#lang racket/base

(require racket/date
         racket/list
         racket/port
         racket/string
         html-parsing
         net/url
         sxml
         yaml
         "paths.rkt"
         "gauth.rkt")

(provide parse-gdoc/post
         parse-gdoc/meta)


(define (parse-gdoc/meta meta)
  (if (hash-has-key? meta 'description)
      (string->yaml (hash-ref meta 'description))
      '()))


(define (parse-span span)
  
  (define (color? style)
    (and (string-prefix? style "color:")
         (not (or
               (string-contains? style "#222222")
               (string-contains? style "#000000")))))

  (define (background-color? style)
    (string-prefix? style "background-color:"))

  (define (underline? style)
    (string=? style "text-decoration:underline"))

  (define (not-link? span)
    (not ((select-first-kid (ntype-names?? '(a))) (sxml:content span))))

  (define (get-decorations styles-list)
    (let ([decos (filter (λ (style)
                           (or
                            (and (color? style) (not-link? span))
                            (background-color? style)
                            (and (underline? style) (not-link? span))))
                         styles-list)])
      (if (empty? decos)
          #f
          decos)))

  (define (make-span colors)
    `(span (@ (style ,(string-join (flatten `(,colors "")) ";")))))

  (define (check-color-and-highlight styles-list)
    (cond
      [(get-decorations styles-list) => make-span]
      [else #f]))

  (define (check-bold styles-list)
    (if (member "font-weight:700" styles-list)
        '(strong)
        #f))

  (define (check-italics styles-list)
    (if (member "font-style:italic" styles-list)
        '(em)
        #f))

  (define (styles-list elm)
    (if (sxml:attr elm 'style)
        (string-split (sxml:attr elm 'style) ";")
        '()))
  
  (define (((make-next-content contents) res) r)
    (sxml:change-content r res))

  (define parsed-contents
    (parse-nodes (sxml:content span)))
    
  (define make-next
    (make-next-content parsed-contents))
    
  (define ((apply-check styles-list) check res)
    (cond
      [(apply check `(,styles-list)) => (make-next res)]
      [else res]))
    
  (foldl
   (apply-check (styles-list span))
   parsed-contents
   `(,check-bold ,check-italics ,check-color-and-highlight)))

(define (parse-img img)

  (define src
    (sxml:attr img' src))

  (define-values (port headers)
    (get-pure-port/headers
     (string->url src)
     token))

  (define filename
    (second (regexp-match #rx"filename=\"(.*?)\"" headers)))

  (define img-path
    (build-path (www/img-path) filename))

  (define out-img
    (sxml:change-attrlist img `((src ,(string-append "/img/" filename)))))  

  (if (not (file-exists? img-path))
      (begin
        (with-output-to-file img-path
          (λ () (display (port->bytes port #:close? #t))))
        out-img)
      out-img))
  
(define (parse-node node)

  (define (empty-attr? attr)
    (not (non-empty-string? (second attr))))

  (define (style? attr)
    (eq? (car attr) 'style))

  (define (id? attr)
    (eq? (car attr) 'id))

  (define (class? attr)
    (eq? (car attr) 'class))

  (define (href? attr)
    (eq? (car attr) 'href))

  (define (fix-href href-attr)

    (define (make-href href)
      (list
       (car href-attr)
       href))

    (define original-href
      (second href-attr))

    (define suggested-href
      (first (string-split (second (string-split original-href "q=")) "&amp;")))

    (displayln (string-append "Original href: " original-href))
    (displayln (string-append "Suggested href: " suggested-href))
    (display "Accept? [Y/<url>] ")

    (define user-input
      (read-line (current-input-port)))

    (if (regexp-match #rx"^Y$|^$" user-input)
        (make-href suggested-href)
        (make-href user-input)))

  (define (parse-attrs attrs)
    (if (empty? attrs)
        empty
        (let ([attr (car attrs)])
          (if (or (empty-attr? attr) (style? attr) (id? attr) (class? attr))
              (parse-attrs (rest attrs))
              (cons 
               (if (href? attr)
                   (fix-href attr)
                   attr)
               (parse-attrs (rest attrs)))))))

  (define (empty-html-element? elm)
    ; img is in here despite being parsed seperately below to accurately reflect the spec
    (member (sxml:element-name elm) '(area base br col embed hr img input link meta param source track wbr)))

  (define (parse-elm-attrs elm)
    (sxml:clean
     (sxml:change-attrlist elm (parse-attrs (sxml:attr-list elm)))))

  (cond
    [((ntype-names?? '(img)) node) (parse-img node)]
    [(and
      ((ntype-names?? '(p)) node)
      (equal? (sxml:text (sxml:content node)) "<!-- more -->"))
     '(*COMMENT* " more ")]
    [(empty-html-element? node) (parse-elm-attrs node)]
    [(sxml:element? node) (sxml:change-content (parse-elm-attrs node) (parse-nodes (sxml:content node)))]
    [else node]))


(define (parse-nodes nodes)

  (define (empty-a? node) 
    (and
     ((ntype-names?? '(a)) node)
     (equal? '() (sxml:content node))))

  (define (fold-node node result)
    (cond
      [(or
        (empty-a? node)
        (equal? '(& nbsp) node)
        (equal? " " node))
       result]
      [((ntype-names?? '(span)) node) (let ([parsed (parse-span node)])
                                        (if (sxml:element? parsed)
                                            `(,@result ,parsed " ")
                                            `(,@result ,@parsed)))]
      [else `(,@result ,(parse-node node))]))

  (foldl fold-node '() nodes))


(define (parse-gdoc/post content post-meta post-path)

  (define html
    (srl:sxml->html-noindent 
     (parse-nodes
      (sxml:content
       (first ((sxpath '("html" "body")) (html->xexp content)))))))

  (date-display-format 'iso-8601)

  (define tags
    (hash-ref (hash-ref post-meta 'info) "tags" #f))

  (with-output-to-file post-path #:exists 'truncate
    (λ () (display (string-append
                    "    Title: "
                    (hash-ref post-meta 'title)
                    "\n"
                    "    Date: "
                    (date->string (hash-ref (hash-ref post-meta 'info) "date"))
                    "T00:00:00"
                    "\n"
                    (if tags
                        (string-append "    Tags: " tags)
                        "")
                    "\n"
                    html)))))
