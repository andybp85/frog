#lang info

(define version "0.1")
(define collection 'multi)
(define deps '(["base" #:version "6.1"]
               "find-parent-dir"
               "gregor"
               "html-lib"
               "html-parsing"
               ["markdown" #:version "0.25"]
               "racket-index"
               ["rackjure" #:version "0.9"]
               "reprovide-lang"
               "scribble-lib"
               "scribble-text-lib"
               "srfi-lite-lib"
               "sxml"
               "web-server-lib"
               "yaml"))
(define build-deps '("at-exp-lib"
                     "net-doc"
                     "racket-doc"
                     "rackunit-lib"
                     "scribble-doc"
                     "scribble-text-lib"
                     "web-server-doc"))
(define test-omit-paths '("example/"))
