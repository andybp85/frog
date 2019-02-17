#lang racket/base

(require racket/format
         racket/function
         "verbosity.rkt"
         ffi/unsafe
         ffi/unsafe/define)

(provide parse-sass)

(define libsass
  (ffi-lib "libsass" #:fail (thunk  (prn1 "Can't load libsass"))))

(define-ffi-definer define-sass
  (if (ffi-lib? libsass)
      libsass
      #f)
  #:default-make-fail make-not-available)

; I should be using Sass_Data_Context and passing in a string for consistency but
; it has something where you have to strdup the input char*, which I can get
; working in C but not here. See this gist for the C code:
; https://gist.github.com/andybp85/2075671e6403e7c85f19ae2cf3f35fdb

(define _Sass_File_Context-pointer (_cpointer 'Sass_File_Context))
(define _Sass_Context-pointer (_cpointer 'Sass_Context))
(define _Sass_Options-pointer (_cpointer 'Sass_Options))

(define-sass sass_make_file_context (_fun _string -> _Sass_File_Context-pointer))
(define-sass sass_file_context_get_context (_fun _Sass_File_Context-pointer -> _Sass_Context-pointer))
(define-sass sass_context_get_options (_fun _Sass_Context-pointer -> _Sass_Options-pointer))
(define-sass sass_option_set_precision (_fun _Sass_Options-pointer _int -> _void))
(define-sass sass_compile_file_context (_fun _Sass_File_Context-pointer -> _int))
(define-sass sass_context_get_output_string (_fun _Sass_Context-pointer -> _string))
(define-sass sass_context_get_error_message (_fun _Sass_Context-pointer -> _string))
(define-sass sass_delete_file_context (_fun _Sass_File_Context-pointer -> _void))

(define (parse-sass file)

  (define file_ctx
    (sass_make_file_context file))

  (define ctx
    (sass_file_context_get_context file_ctx))

  (define ctx_opt
    (sass_context_get_options ctx))

  (sass_option_set_precision ctx_opt 10)

  (define status
    (sass_compile_file_context file_ctx))

  (define css-or-error
    (if (= 0 status)
        (sass_context_get_output_string ctx)
        (sass_context_get_error_message ctx)))

  (sass_delete_file_context file_ctx)

  (if (= 0 status)
      css-or-error
      (raise-user-error (~a "Sass " css-or-error))))

