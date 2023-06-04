(use-modules
 ((guix licenses) #:prefix license:)
 (gnu packages autotools)
 (gnu packages guile)
 (gnu packages guile-xyz)
 (gnu packages pkg-config)
 (guix build-system gnu)
 (guix gexp)
 (guix git-download)
 (guix packages))

(define guile-bytestructure-class
  (package
    (name "guile-bytestructure-class")
    (version "0.2.0")
    (source (local-file "." "guile-bytestructure-class-checkout"
                        #:recursive? #t
                        #:select? (git-predicate (dirname (current-filename)))))
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
