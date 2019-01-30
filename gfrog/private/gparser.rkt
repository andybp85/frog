#lang racket/base

(require html-parsing
         racket/list
         racket/string
         sxml
         yaml)

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
    (not ((select-first-kid  (ntype-names?? '(a))) (sxml:content span))))

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
    (string-split (sxml:attr elm 'style) ";"))
  
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
    (list
     (car href-attr)
     (first (string-split (second (string-split (second href-attr) "q=")) "&amp;"))))

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
    (member (sxml:element-name elm) '(area base br col embed hr img input link meta param source track wbr)))

  (define (parse-elm-attrs elm)
    (sxml:clean
     (sxml:change-attrlist elm (parse-attrs (sxml:attr-list elm)))))

  (cond
    [(empty-html-element? node) (parse-elm-attrs node)]
    [(sxml:element? node) (sxml:change-content (parse-elm-attrs node) (parse-nodes (sxml:content node)))]
    [else node]))


(define (parse-nodes nodes)

  (define (empty-a? node)
    (and ((ntype-names?? '(a)) node) (sxml:empty-element? node)))

  (define (fold-node node result)
    (cond
      [(or (empty-a? node) (equal? '(& nbsp) node)) result]
      [((ntype-names?? '(span)) node) (let ([parsed (parse-span node)])
                                        (if (sxml:element? parsed)
                                            `(,@result ,parsed)
                                            `(,@result ,@parsed)))]
      [else `(,@result ,(parse-node node))]))

  (foldl fold-node '() nodes))


(define (parse-gdoc/post content)  
  (parse-nodes
   (sxml:content
    (first ((sxpath '("html" "body")) (html->xexp content))))))
