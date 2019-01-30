#lang racket/base

(require racket/class
         racket/date
         racket/string
         gregor
         json
         net/url
         "gauth.rkt"
         "gparser.rkt"
         "params.rkt"
         "post-struct.rkt")

(provide load-posts)

; this func mostly copied from https://github.com/fgmart/google-drive-racket
(define (list-all-children folder-id . next-page-token)
  
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

  (let* ((this-page (if (= 0 (length next-page-token))
                        (list-children folder-id)
                        (list-children folder-id (car next-page-token))))
         (page-token (hash-ref this-page 'nextPageToken #f)))
    (if page-token
        (append (get-files this-page)
                (list-all-children folder-id page-token))
        (get-files this-page))))

(define (get-gdoc-file mime file-id)
  (get-pure-port
   (string->url (string-append "https://www.googleapis.com/drive/v3/files/"
                               file-id
                               "/export?mimeType="
                               mime))
   token))

; this func is for stripping control chars when exporting gdocs as text
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

;(struct post (content title description modifiedTime))

(define (get-posts)
  (map
   (λ (file)
     (hash
      'content (parse-gdoc/post (get-gdoc-file "text/html" (hash-ref file 'id)))
      'title (hash-ref file 'name)
      'meta (parse-gdoc/meta (get-gdoc-file-meta "description,modifiedTime" (hash-ref file 'id)))))
   (list-all-children (ga-posts-folder))))

(define (get-posts-list)
  '())

(define (get-post-body id)
  (parse-gdoc/post (get-gdoc-file "text/html" id)))

(define (get-posts-meta)
  (map (λ (file)
         
         (define meta
           (get-gdoc-file-meta "description,modifiedTime,name" (hash-ref file 'id)))
         
         (define post-meta
           (parse-gdoc/meta meta))
         
         (define modified
           (iso8601->datetime (hash-ref meta 'modifiedTime)))
         
         (define (file-name)
           (date-display-format 'iso-8601)
           (string-append*
            (date->string (hash-ref post-meta "date"))
            "-"
            (string-replace
             (string-replace
              (string-downcase (hash-ref meta 'name))
              " " "_")
             #px"[^[:word:]]" "")
            '(".html")))
         
         (hash
          'id (hash-ref file 'id)
          'meta post-meta
          'modified modified
          'filename (file-name)))
       (list-all-children (ga-posts-folder))))

(define (load-posts)

  (google-login)

  ;  (src/posts-path)
  
  (write (get-posts-meta)))

;  (define last-build-time
;    (seconds->date
;     (file-or-directory-modify-seconds
;      (string->path "index.html"))))

;  (define posts (list-all-children (ga-posts-folder)))
;
;  
;  
;  (google-login)
;  (write (get-posts)))

;(load-posts)



       
