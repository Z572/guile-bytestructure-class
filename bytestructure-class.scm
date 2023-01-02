(define-module (bytestructure-class)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-2)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-71)
  #:use-module (ice-9 match)
  #:use-module (bytestructures guile)
  #:use-module ((system foreign) #:prefix ffi:)
  #:use-module (oop goops)
  #:use-module (rnrs bytevectors)
  #:export (bytestructure->pointer
            define-bytestructure-class
            pointer->bytestructure
            bytestructure->bs-instance

            get-bytevector
            get-pointer
            get-bytestructure

            stdbool
            bs:enum
            enum-metadata?
            enum-metadata-field-alist
            .descriptor
            .wrap
            .unwrap))

(define (bytestructure->pointer b)
  (ffi:bytevector->pointer (bytestructure-bytevector b)
                           (bytestructure-offset b)))

(define (pointer->bytestructure pointer struct)
  (make-bytestructure
   (ffi:pointer->bytevector
    pointer
    (bytestructure-descriptor-size struct))
   0
   struct))

;;; <stdbool.h>

(define stdbool
  (make-bytestructure-descriptor
   1 1
   #f
   (lambda (syntax? bytevector offset)
     (if syntax?
         #`(not (zero? (bytevector-s8-ref #,bytevector #,offset)))
         (not (zero? (bytevector-s8-ref bytevector offset)))))
   (lambda (syntax? bytevector offset value)
     (if syntax?
         #`(bytevector-s8-set! #,bytevector #,offset #,(if value 1 0))
         (bytevector-s8-set! bytevector offset (if value 1 0))))))

(define-record-type <enum-metadata>
  (make-enum-metadata field-alist)
  enum-metadata?
  (field-alist enum-metadata-field-alist))

;;; enum

(define (bs:enum fields)
  (define alist (map
                 (let ((num 0))
                   (match-lambda
                     ((? symbol? symbol)
                      (let ((l (cons num symbol)))
                        (set! num (1+ num))
                        l))
                     (((? symbol? symbol) (? integer? integer))
                      (set! num integer)
                      (cons integer symbol))
                     ))
                 fields))
  (define integers (map car alist))
  (define symbols (map cdr alist))
  (let ((h (lambda (a b)
             (when (eq? a b)
               (error "duplicates!" a)))))
    (delete-duplicates symbols h)
    (delete-duplicates integers h))
  (define meta (make-enum-metadata alist))
  (define (getter syntax? bytevector offset)
    (if syntax?
        #`(or (assq-ref #,alist (bytevector-s8-ref #,bytevector #,offset))
              (error "get invalid value!"))
        (or (assq-ref alist (bytevector-s8-ref bytevector offset))
            (error "get invalid value!"))))
  (define (setter syntax? bytevector offset value)
    (define (to-value v)
      (cond ((symbol? v)
             (or (and=> (list-index (lambda (o) (eq? v o)) symbols)
                        (cut list-ref integers <>) )
                 (error "can't not found symbol in enum!" v)))
            ((integer? v)
             (unless (member v integers)
               (error "can't not found integer in enum!" v))
             v)
            (else (error "Is not symbol or "))))
    (if syntax?
        #`(bytevector-s8-set! #,bytevector #,offset #,(to-value value))
        (bytevector-s8-set! bytevector offset (to-value value))))
  (make-bytestructure-descriptor
   4 4 #f getter setter meta))

;; class

(define %bytestructures (make-hash-table 1000))
(define-class <bytestructure-class> (<class>)
  (descriptor #:init-keyword #:descriptor
              #:init-value #f
              #:getter .descriptor)
  (wrap #:getter .wrap)
  (unwrap #:getter .unwrap))

(define (bytestructure->bs-instance bs)
  (and=> (hashv-ref %bytestructures (bytestructure-descriptor bs) #f)
         (lambda (c)
           ((.wrap c) bs))))

(define-method (initialize (object <bytestructure-class>) initargs)
  (next-method)
  (and=> (.descriptor object) (cut hashv-set! %bytestructures <> object))
  (let ()
    (define wrap
      (let ((ptr->obj (make-weak-value-hash-table 3000)))
        (lambda (obj)
          (let ((ptr (cond ((bytestructure? obj) (bytestructure->pointer obj))
                           ((integer? obj) (ffi:make-pointer obj))
                           ((ffi:pointer? obj) obj)
                           (else (goops-error "~S is not a pointer or bytestructure, integer" obj)))))
            (if (ffi:null-pointer? ptr)
                #f
                (or (hash-ref ptr->obj ptr)
                    (let ((o (make object #:pointer ptr)))
                      (hash-set! ptr->obj ptr o)
                      o)))))))
    (define (unwrap o)
      (if o
          (begin (unless (is-a? o object)
                   (error (string-append
                           "not a "
                           (symbol->string (class-name object))
                           " or #f")
                          o))
                 (.pointer o))
          ffi:%null-pointer))
    (slot-set! object 'wrap wrap)
    (slot-set! object 'unwrap unwrap)))

(define (get-field-alist o)
  (cond ((struct-metadata? o) (struct-metadata-field-alist o))
        (else #f)))

(define-inlinable (force-or-nothing o)
  (if (promise? o)
      (force o)
      o))

(define (haneld-pointer-descriptor o)
  (let* ((metadata (bytestructure-descriptor-metadata o))
         (is-pointer? (pointer-metadata? metadata)))
    (values is-pointer? (if is-pointer?
                            (force-or-nothing
                             (pointer-metadata-content-descriptor
                              metadata))
                            o))))

(define-method (compute-get-n-set (class <bytestructure-class>) slot)
  (if (eq? (slot-definition-allocation slot) #:bytestructure)
      (let* ((index (slot-ref class 'nfields))
             (s (slot-definition-options slot))
             (b-name (or (get-keyword #:field-name s #f)
                         (slot-definition-name slot)))
             (descriptor (.descriptor class))
             (alist (get-field-alist
                     (bytestructure-descriptor-metadata descriptor)))
             (field-descriptor
              (assq-ref alist b-name)))
        (unless field-descriptor
          (goops-error "not field name'd `~S' found in class `~S' descriptor "
                       b-name class))
        (slot-set! class 'nfields (+ index 1))
        (let* ((is-pointer? field-descriptor
                            (haneld-pointer-descriptor field-descriptor))
               (ref-handle (if is-pointer? ffi:make-pointer identity))
               (set-handle (if is-pointer? ffi:pointer-address identity))
               (b-class (delay (hashv-ref %bytestructures field-descriptor #f)))
               (wrap (delay (or (and=> (force b-class) .wrap) identity)))
               (unwrap (delay (or (and=> (force b-class) .unwrap) identity))))
          (list (lambda (o)
                  (let ((f (force wrap))
                        (out (bytestructure-ref (get-bytestructure o) b-name)))
                    (f (ref-handle (cond ((bytestructure? out)
                                          (bytestructure->pointer out))
                                         (else out))))))
                (lambda (o v)
                  (let ((f (force unwrap)))
                    (bytestructure-set!
                     (get-bytestructure o) b-name
                     (set-handle (f v))))))))
      (next-method)))

(define-class <bs> ()
  (pointer #:accessor .pointer #:init-keyword #:pointer)
  #:metaclass <bytestructure-class>)

(define-method (initialize (object <bs>) initargs)
  (let ((descriptor(.descriptor (class-of object))))
    (if (get-keyword #:pointer initargs #f)
        (next-method)
        (next-method
         object (append
                 (list #:pointer (ffi:bytevector->pointer
                                  (make-bytevector
                                   (bytestructure-descriptor-size descriptor))))
                 initargs)))))


(define-method (equal? (f <bs>) (l <bs>))
  (equal? (.pointer f)
          (.pointer l)))

(define-method (get-bytestructure (obj <bs>))
  (and-let* ((class (class-of obj))
             (descriptor (.descriptor class))
             (unwrap (.unwrap class)))
    (pointer->bytestructure (unwrap obj) descriptor)))

(define-method (get-bytevector (obj <bs>))
  (and=> (get-bytestructure obj) bytestructure-bytevector))

(define-method (get-pointer (o <bs>))
  (let* ((unwrap (.unwrap (class-of o)))
         (u (unwrap o)))
    (cond ((ffi:pointer? u) u)
          ((bytestructure? u) (bytestructure->pointer u)))))

(define-syntax define-bytestructure-class
  (lambda (x)
    (syntax-case x ()
      ((_ <rtd> (supers ...) descriptor wrap unwrap is?
          othrers ...)
       (and-map identifier? #'(wrap unwrap is?))
       (let ((oo (map (lambda (o)
                        (syntax-case o ()
                          ((slot-name oth ...)
                           #'(slot-name oth ... #:allocation #:bytestructure))
                          (oth #'oth)))
                      #'(othrers ...))))
         #`(begin
             (define-class <rtd> (supers ... <bs>)
               #,@oo

               #,@(if (syntax->datum #'descriptor)
                      #'(#:descriptor descriptor)
                      #'()))
             (export <rtd>)
             (define-public unwrap (.unwrap <rtd>))
             (define-public wrap (.wrap <rtd>))
             (define-public (is? o) (is-a? o <rtd>))))))))
