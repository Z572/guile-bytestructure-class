(define-module (guile-bytestructure-class-package)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages guile-xyz)
  #:use-module (gnu packages pkg-config)
  #:use-module (guix build-system gnu)
  #:use-module (guix gexp)
  #:use-module (guix utils)
  #:use-module (guix git-download)
  #:use-module (guix packages))

(define-public guile-bytestructure-class
  (package
    (name "guile-bytestructure-class")
    (version "git")
    (source (local-file "../.." "guile-bytestructure-class-checkout"
                        #:recursive? #t
                        #:select? (or (git-predicate
                                       (string-append
                                        (current-source-directory)
                                        "/../.."))
                                      (const #t))))
    (build-system gnu-build-system)
    (arguments
     (list #:make-flags #~'("GUILE_AUTO_COMPILE=0")))
    (native-inputs
     (list autoconf
           automake
           pkg-config
           guile-3.0-latest))
    (inputs (list guile-3.0-latest))
    (propagated-inputs (list guile-bytestructures))
    (synopsis "")
    (description "")
    (home-page "")
    (license license:gpl3+)))

guile-bytestructure-class
