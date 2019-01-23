#lang racket/base

(require racket/class
         (planet ryanc/webapi:1:=1/oauth2)
         file/md5
         pkg/lib
         "params.rkt"
         "../paths.rkt")

(provide google-login
         token
         drive-client)

(define token '())
(define token-file
  (build-path (pkg-directory "gfrog") "gfrog" "private" "tokens" (bytes->string/locale (md5 (current-scheme/host)))))

(define drive-client
  (oauth2-client
   #:id (ga-client-id)
   #:secret (ga-client-secret)))

(define (get-new-tokens)
  (let ([boauth (oauth2/request-auth-code/browser
                 google-auth-server
                 drive-client
                 '("https://www.googleapis.com/auth/drive"))])
    (values (send boauth get-access-token) (send boauth get-refresh-token))))

(define (use-refresh-token refresh-token)
  (let ([roauth (oauth2/refresh-token
                 google-auth-server
                 drive-client
                 refresh-token)])
    (values (send roauth get-access-token) (send roauth get-refresh-token))))

(define (set-save-tokens access-token refresh-token)
  (set! token (list (string-append "Authorization: Bearer " access-token)))
  (with-output-to-file token-file #:exists 'truncate
    (λ ()
      (printf refresh-token))))

(define (get-new-and-save-tokens)
  (call-with-values get-new-tokens set-save-tokens))

(define (google-login)
  (if (file-exists? token-file)
      (with-input-from-file token-file
        (λ ()
          (with-handlers ([exn:fail?
                           (λ (ignored)
                             (get-new-and-save-tokens))])
            (call-with-values
             (λ ()
               (use-refresh-token (read-line))) set-save-tokens))))
      (get-new-and-save-tokens)))
