#lang racket

(require net/url
         yaml
         json
         "./params.rkt"
         "./gauth.rkt")

(provide load-posts)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; these funcs copied from https://github.com/fgmart/google-drive-racket
(define (get-files obj)
  (hash-ref obj 'files))

(define (list-children folder-id . next-page-token)
  (read-json
   (get-pure-port
    (string->url (string-append "https://www.googleapis.com/drive/v3/files?"
                                "q='" folder-id "'+in+parents"
                                "&key=" (send drive-client get-id)
                                (if (= 1 (length next-page-token))
                                    (string-append "&pageToken=" (car next-page-token))
                                    "")))
    token)))

(define (list-all-children folder-id . next-page-token)
  (let* ((this-page (if (= 0 (length next-page-token))
                        (list-children folder-id)
                        (list-children folder-id (car next-page-token))))
         (page-token (hash-ref this-page 'nextPageToken #f)))
    (if page-token
        (append (get-files this-page)
                (list-all-children folder-id page-token))
        (get-files this-page))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (get-file mime file-id)
  (get-pure-port
   (string->url (string-append "https://www.googleapis.com/drive/v3/files/"
                               file-id
                               "/export?mimeType="
                               mime))
   token))

;(define (stripbom p)
;  (regexp-replace* (~a "^\uFEFF") p ""))

(define (parse-post post)
  (port->string
   (get-file "text/html"
             (hash-ref post 'id))))

(define (get-post-meta fields file-id)
  (read-json
   (get-pure-port
    (string->url (string-append "https://www.googleapis.com/drive/v3/files/"
                                file-id
                                "?fields="
                                fields))
    token)))

(define (make-post-meta description)
  (if (hash-has-key? description 'description)
      (string->yaml (hash-ref description 'description))
      '()))

(define (load-posts)
  (google-login)
  (map
   (λ (file)
     (hash
      'content (parse-post file)
      'title (hash-ref file 'name)
      'meta (make-post-meta (get-post-meta "description" (hash-ref file 'id)))))
   (list-all-children (ga-posts-folder))))

;(define posts
;  (list
; '#hash((content . ((h3 ((id "hello-new-world")) "Hello New World!") (p () "Second Post"))) (meta . ()) (title . "hello_world2"))
; (hash 'content '((h3 ((id "hello-world")) "Hello World!") (p () "First Post")) 'meta (hash "categories" "world, hello" "date" (date* 0 0 0 3 1 2019 4 2 #f 0 0 "UTC")) 'title "hello_world")))

;(define posts
;  (list
; '#hash((content . ((h3 ((id "hello-new-world")) "Hello New World!") (p () "Second Post"))) (meta . ()) (title . "hello_world2"))
; '#hash((content . ((h3 ((id "hello-new-world")) "Hello New World!") (p () "Second Post"))) (meta . ()) (title . "hello_world2"))))
