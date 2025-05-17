(define-module (tests bytestructure-class)
  #:use-module (oop goops)
  #:use-module (bytestructure-class)
  #:use-module (bytestructures guile)
  #:use-module ((system foreign) #:prefix ffi:)
  #:use-module (srfi srfi-64))

(test-group "stdbool"
  (let ((n (bytestructure stdbool #t)))
    (test-eqv "ref #t" #t (bytestructure-ref n))
    (test-eqv "set #f" #f (begin (bytestructure-set! n #f)
                                 (bytestructure-ref n)))))

(test-group "cstring-pointer*"
  (test-equal #f (bytestructure-ref (bytestructure cstring-pointer*)))
  (let* ((s "hh")
         (bs (bytestructure
              cstring-pointer* s))
         (s2 "____"))
    (test-equal s (bytestructure-ref
                   (bytestructure
                    cstring-pointer* s)))
    (test-equal s2 (begin (bytestructure-set! bs s2)
                          (bytestructure-ref bs)))
    (test-equal "abc" (let ((o (bytestructure
                                cstring-pointer* "abc")))
                        (gc)
                        (bytestructure-ref o)))
    (test-equal #f (let ((b (bytestructure
                             cstring-pointer*)))

                     (bytestructure-set! b
                                         #f)
                     (bytestructure-ref b)))))

(define cs (bs:struct `((a ,cstring-pointer*))))
(define-bytestructure-accessors cs
  t-unwrap t-ref t-set!)

(test-group "cstring-pointer*/syntax"
  (test-equal "hello"
    (let ((o (bytestructure-bytevector
              (bytestructure
               cs `((a "abc"))))))
      (t-set! o a "hello")
      (gc)
      (t-ref o a)))
  (test-assert
      (let ((o (bytestructure-bytevector
                (bytestructure
                 cs `((a "abc"))))))
        (t-set! o a #f)
        (gc)
        (equal? #f (t-ref o a)))))

(test-group "bs:enum"
  (define enum
    (bs:enum '((A 1)
               (B 2))))
  (test-eqv "A=1" 'A (bytestructure-ref (bytestructure enum 'A)))
  (test-eqv "B=2" 'B (bytestructure-ref (bytestructure enum 2)))
  (test-error "not have C" 'misc-error (bytestructure-ref (bytestructure enum 'C)))

  (define enum2
    (bs:enum '(A B (C 3))))
  (test-eqv "A=0" 'A (bytestructure-ref (bytestructure enum2 'A)))
  (test-eqv "B=1" 'B (bytestructure-ref (bytestructure enum2 1)))
  (test-eqv "C=3" 'C (bytestructure-ref (bytestructure enum2 3)))

  (test-error "symbol duplicates" #t
              (bs:enum '(A
                         (A 2))))
  (test-error "num duplicates" #t
              (bs:enum '((A 2)
                         (B 2)))))

(test-group "bs:enum->integer"
  (define enum
    (bs:enum '((A 1)
               (B 2))))
  (test-eq "symbol"
    1
    (bs:enum->integer enum 'A))
  (test-error "not symbol" #t
              (bs:enum->integer enum 'C))
  (test-error "not num" #t
              (bs:enum->integer enum 20)))

(test-group "bs-class"
  (define %struct-a (bs:struct `((a ,int))))
  (define-bytestructure-class <struct-a> ()
    %struct-a wrap-struct-a unwrap-struct-a struct-a?
    (a #:accessor .a))
  (define struct-a-1
    (wrap-struct-a
     (bytestructure %struct-a `((a 20)))))
  (test-eqv "ref1" 20 (.a struct-a-1))
  (test-eqv "set1" 30 (begin
                        (set! (.a struct-a-1) 30)
                        (.a struct-a-1)))

  (define %struct-b (bs:struct `((b ,int)
                                 (a ,(bs:pointer %struct-a)))))
  (define-bytestructure-class <struct-b> ()
    %struct-b wrap-struct-b unwrap-struct-b struct-b?
    (a #:accessor .a)
    (b #:accessor .b))
  (define struct-b-1
    (let ((b (bytestructure %struct-b `((b 20)))))
      (bytestructure-set! b 'a (ffi:pointer-address
                                (get-pointer struct-a-1)))
      (wrap-struct-b b)))
  (test-eqv "ref2" struct-a-1 (.a struct-b-1))
  (test-eqv "ref3" #f (begin (set! (.a struct-b-1) #f)
                             (.a struct-b-1)))
  (test-eqv "bytestructure->bs-instance"
    struct-b-1
    (bytestructure->bs-instance (get-bytestructure struct-b-1)))
  (define %struct-c (bs:unknow))
  (define %struct-d (bs:unknow))
  (test-assert "bs:unknow not eq?" (not (eq? %struct-c %struct-d)))
  (test-assert "bs:unknow not eqv?" (not (eqv? %struct-c %struct-d)))
  (define-bytestructure-class <super-class-struct> ()
    %struct-c wrap-sc unwrap-sc sc?)
  (define-bytestructure-class <child-class-struct> (<super-class-struct>)
    %struct-d wrap-cc unwrap-cc cc?)
  (test-assert "wrap" (is-a? (wrap-cc (make <super-class-struct>)) <child-class-struct>))
  (test-assert "wrap2" (ffi:pointer? (unwrap-cc (wrap-cc (make <super-class-struct>)))))
  (test-assert "unwrap-1" (ffi:pointer? (unwrap-sc (make <child-class-struct>))))
  (test-error
   "unwrap: fail" 'misc-error
   (unwrap-cc (make <super-class-struct>))))
