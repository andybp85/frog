#lang info
(define raco-commands '(("gfrog" (submod gfrog/main main) "run GFrog" #f)))
;(define scribblings '(("frog.scrbl" (multi-page))))
(define clean '("compiled" "doc" "doc/frog"))
