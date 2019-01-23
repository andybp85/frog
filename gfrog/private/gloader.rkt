#lang racket/base

(require racket/class
         html-parsing
         json
         net/url
         yaml
         "gauth.rkt"
         "gparser.rkt"
         "params.rkt")

(provide load-posts)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; these funcs copied from https://github.com/fgmart/google-drive-racket
(define (get-files obj)
  (hash-ref obj 'files
            (λ ()
              (eprintf "No files found in Google Drive folder.\n")
              (exit 1))))

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

(define (get-gdoc-file mime file-id)
  (get-pure-port
   (string->url (string-append "https://www.googleapis.com/drive/v3/files/"
                               file-id
                               "/export?mimeType="
                               mime))
   token))

;(define (stripbom p)
;  (regexp-replace* (~a "^\uFEFF") p ""))

(define (get-gdoc-file-meta fields file-id)
  (read-json
   (get-pure-port
    (string->url (string-append "https://www.googleapis.com/drive/v3/files/"
                                file-id
                                "?fields="
                                fields))
    token)))

(define (get-posts)
  (map
   (λ (file)
     (hash
      'content (parse-gdoc/post (get-gdoc-file "text/html" (hash-ref file 'id)))
      'title (hash-ref file 'name)
      'meta (parse-gdoc/meta (get-gdoc-file-meta "description" (hash-ref file 'id)))))
   (list-all-children (ga-posts-folder))))

(define (load-posts)
  (google-login)
  (write (get-posts)))

