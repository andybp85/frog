#lang racket/base

(require racket/list
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

(define (parse-gdoc/post content post-path)

  (define parsed
    (srl:sxml->html-noindent
     (parse-nodes
      (sxml:content
       (first ((sxpath '("html" "body")) (html->xexp content)))))))

  (with-output-to-file post-path #:exists 'truncate
    (λ () (display parsed))))

;;;
;(define post "<html> <head> <meta content='text/html; charset=UTF-8' http-equiv='content-type'> <style type='text/css'> @import url(https://themes.googleusercontent.com/fonts/css?kit=dpiI8CyVsrzWsJLBFKehGpLhv3qFjX7dUn1mYxfCXhI); ul.lst-kix_wdqwr3v8eg2s-4{list-style-type:none}ul.lst-kix_wdqwr3v8eg2s-5{list-style-type:none}.lst-kix_wdqwr3v8eg2s-8 > li:before{content:'■ '}ul.lst-kix_wdqwr3v8eg2s-6{list-style-type:none}ul.lst-kix_wdqwr3v8eg2s-7{list-style-type:none}ul.lst-kix_wdqwr3v8eg2s-0{list-style-type:none}ul.lst-kix_wdqwr3v8eg2s-1{list-style-type:none}.lst-kix_wdqwr3v8eg2s-6 > li:before{content:'● '}ul.lst-kix_wdqwr3v8eg2s-2{list-style-type:none}ul.lst-kix_wdqwr3v8eg2s-3{list-style-type:none}.lst-kix_wdqwr3v8eg2s-4 > li:before{content:'○ '}.lst-kix_wdqwr3v8eg2s-5 > li:before{content:'■ '}.lst-kix_wdqwr3v8eg2s-3 > li:before{content:'● '}.lst-kix_wdqwr3v8eg2s-0 > li:before{content:'● '}.lst-kix_wdqwr3v8eg2s-1 > li:before{content:'○ '}.lst-kix_wdqwr3v8eg2s-2 > li:before{content:'■ '}ul.lst-kix_wdqwr3v8eg2s-8{list-style-type:none}.lst-kix_wdqwr3v8eg2s-7 > li:before{content:'○ '}</style> </head> <body style='background-color:#ffffff;padding:36pt 36pt 36pt 36pt;max-width:540pt'> <h2 id='h.xs55g8fy96g' style='padding-top:18pt;margin:0;color:#000000;padding-left:0;font-size:16pt;padding-bottom:6pt;line-height:1.15;page-break-after:avoid;font-family:&quot;Arial&quot;;orphans:2;widows:2;text-align:left;padding-right:0'> <span style='color:#000000;font-weight:400;text-decoration:none;vertical-align:baseline;font-size:16pt;font-family:&quot;Arial&quot;;font-style:normal'>Hope - The Disco Biscuits</span> </h2> <p style='padding-top:9pt;margin:0;color:#000000;padding-left:0;font-size:11pt;padding-bottom:9pt;line-height:1.24;font-family:&quot;Arial&quot;;orphans:2;widows:2;height:11pt;text-align:left;padding-right:0'> <span style='color:#222222;font-weight:400;text-decoration:none;vertical-align:baseline;font-size:11pt;font-family:&quot;Roboto&quot;;font-style:normal'> </span> </p><p style='padding-top:9pt;margin:0;color:#000000;padding-left:0;font-size:11pt;padding-bottom:9pt;font-family:&quot;Arial&quot;;line-height:1.24;orphans:2;widows:2;text-align:left;padding-right:0'> <span style='color:#222222;font-weight:400;text-decoration:none;vertical-align:baseline;font-size:11pt;font-family:&quot;Roboto&quot;;font-style:normal'>Mellow was the world when it began,<br>The alphabet and a master plan,<br>Settled in the trees and growing like a vine,<br>A delivery arriving back in time.</span> </p><p style='padding-top:9pt;margin:0;color:#000000;padding-left:0;font-size:11pt;padding-bottom:9pt;font-family:&quot;Arial&quot;;line-height:1.24;orphans:2;widows:2;text-align:left;padding-right:0'> <span style='color:#222222;font-weight:400;text-decoration:none;vertical-align:baseline;font-size:11pt;font-family:&quot;Roboto&quot;;font-style:normal'>Test</span> </p><p style='padding-top:9pt;margin:0;color:#000000;padding-left:0;font-size:11pt;padding-bottom:9pt;font-family:&quot;Arial&quot;;line-height:1.24;orphans:2;widows:2;text-align:left;padding-right:0'> <span style='color:#222222;font-weight:400;text-decoration:none;vertical-align:baseline;font-size:11pt;font-family:&quot;Roboto&quot;;font-style:normal'>Sdfas</span> </p><p style='padding-top:9pt;margin:0;color:#000000;padding-left:0;font-size:11pt;padding-bottom:9pt;font-family:&quot;Arial&quot;;line-height:1.24;orphans:2;widows:2;text-align:left;padding-right:0'> <span style='color:#222222;font-weight:400;text-decoration:none;vertical-align:baseline;font-size:11pt;font-family:&quot;Roboto&quot;;font-style:normal'>Sdafasdf</span> </p><p style='padding-top:9pt;margin:0;color:#000000;padding-left:0;font-size:11pt;padding-bottom:9pt;font-family:&quot;Arial&quot;;line-height:1.24;orphans:2;widows:2;text-align:left;padding-right:0'> <span style='color:#222222;font-weight:400;text-decoration:none;vertical-align:baseline;font-size:11pt;font-family:&quot;Roboto&quot;;font-style:normal'>Asdfdas</span> </p><p style='padding-top:9pt;margin:0;color:#000000;padding-left:0;font-size:11pt;padding-bottom:9pt;font-family:&quot;Arial&quot;;line-height:1.24;orphans:2;widows:2;text-align:left;padding-right:0'> <span style='color:#222222;font-weight:400;text-decoration:none;vertical-align:baseline;font-size:11pt;font-family:&quot;Roboto&quot;;font-style:normal'>asdfasdf</span> </p><p style='padding-top:9pt;margin:0;color:#000000;padding-left:0;font-size:11pt;padding-bottom:9pt;line-height:1.24;font-family:&quot;Arial&quot;;orphans:2;widows:2;height:11pt;text-align:left;padding-right:0'> <span style='color:#222222;font-weight:400;text-decoration:none;vertical-align:baseline;font-size:11pt;font-family:&quot;Roboto&quot;;font-style:normal'> </span> </p><p style='padding-top:9pt;margin:0;color:#000000;padding-left:0;font-size:11pt;padding-bottom:9pt;line-height:1.24;font-family:&quot;Arial&quot;;orphans:2;widows:2;height:11pt;text-align:left;padding-right:0'> <span style='color:#222222;font-weight:400;text-decoration:none;vertical-align:baseline;font-size:11pt;font-family:&quot;Roboto&quot;;font-style:normal'> </span> </p><a id='t.7eaced84113ef2605aad1e1046b7199f68d63b1e'> </a> <a id='t.0'> </a> <table style='margin-left:;border-spacing:0;border-collapse:collapse;margin-right:auto'> <tbody> <tr style='height:0pt'> <td colspan='1' rowspan='1' style='border-right-style:solid;padding:5pt 5pt 5pt 5pt;border-bottom-color:#000000;border-top-width:1pt;border-right-width:1pt;border-left-color:#000000;vertical-align:top;border-right-color:#000000;border-left-width:1pt;border-top-style:solid;border-left-style:solid;border-bottom-width:1pt;width:270pt;border-top-color:#000000;border-bottom-style:solid'> <p style='padding:0;margin:0;color:#000000;font-size:11pt;font-family:&quot;Arial&quot;;line-height:1.0;text-align:left'> <span style='color:#222222;font-weight:400;text-decoration:none;vertical-align:baseline;font-size:11pt;font-family:&quot;Roboto&quot;;font-style:normal'>boo</span> </p></td><td colspan='1' rowspan='1' style='border-right-style:solid;padding:5pt 5pt 5pt 5pt;border-bottom-color:#000000;border-top-width:1pt;border-right-width:1pt;border-left-color:#000000;vertical-align:top;border-right-color:#000000;border-left-width:1pt;border-top-style:solid;border-left-style:solid;border-bottom-width:1pt;width:270pt;border-top-color:#000000;border-bottom-style:solid'> <p style='padding:0;margin:0;color:#000000;font-size:11pt;font-family:&quot;Arial&quot;;line-height:1.0;text-align:left'> <span style='color:#222222;font-weight:400;text-decoration:none;vertical-align:baseline;font-size:11pt;font-family:&quot;Roboto&quot;;font-style:normal'>yah</span> </p></td></tr><tr style='height:0pt'> <td colspan='1' rowspan='1' style='border-right-style:solid;padding:5pt 5pt 5pt 5pt;border-bottom-color:#000000;border-top-width:1pt;border-right-width:1pt;border-left-color:#000000;vertical-align:top;border-right-color:#000000;border-left-width:1pt;border-top-style:solid;border-left-style:solid;border-bottom-width:1pt;width:270pt;border-top-color:#000000;border-bottom-style:solid'> <p style='padding:0;margin:0;color:#000000;font-size:11pt;font-family:&quot;Arial&quot;;line-height:1.0;height:11pt;text-align:left'> <span style='color:#222222;font-weight:400;text-decoration:none;vertical-align:baseline;font-size:11pt;font-family:&quot;Roboto&quot;;font-style:normal'> </span> </p></td><td colspan='1' rowspan='1' style='border-right-style:solid;padding:5pt 5pt 5pt 5pt;border-bottom-color:#000000;border-top-width:1pt;border-right-width:1pt;border-left-color:#000000;vertical-align:top;border-right-color:#000000;border-left-width:1pt;border-top-style:solid;border-left-style:solid;border-bottom-width:1pt;width:270pt;border-top-color:#000000;border-bottom-style:solid'> <p style='padding:0;margin:0;color:#000000;font-size:11pt;font-family:&quot;Arial&quot;;line-height:1.0;height:11pt;text-align:left'> <span style='color:#222222;font-weight:400;text-decoration:none;vertical-align:baseline;font-size:11pt;font-family:&quot;Roboto&quot;;font-style:normal'> </span> </p></td></tr></tbody> </table> <p style='padding-top:9pt;margin:0;color:#000000;padding-left:0;font-size:11pt;padding-bottom:9pt;line-height:1.24;font-family:&quot;Arial&quot;;orphans:2;widows:2;height:11pt;text-align:left;padding-right:0'> <span style='color:#222222;font-weight:400;text-decoration:none;vertical-align:baseline;font-size:11pt;font-family:&quot;Roboto&quot;;font-style:normal'> </span> </p><p style='padding-top:9pt;margin:0;color:#000000;padding-left:0;font-size:11pt;padding-bottom:9pt;font-family:&quot;Arial&quot;;line-height:1.24;orphans:2;widows:2;text-align:left;padding-right:0'> <span style='font-family:&quot;Roboto&quot;;color:#222222;font-weight:400'>The </span> <span style='-webkit-text-decoration-skip:none;color:#222222;font-weight:400;text-decoration:underline;text-decoration-skip-ink:none;font-family:&quot;Roboto&quot;'>officer revolt</span> <span style='font-family:&quot;Roboto&quot;;color:#222222;font-weight:400'>&nbsp;walks the beat with a </span> <span style='font-size:14pt;font-family:&quot;Roboto&quot;;color:#222222;font-weight:400'>bang</span> <span style='font-family:&quot;Roboto&quot;;color:#222222;font-weight:400'>,<br>For a millisecond and a </span> <span style='-webkit-text-decoration-skip:none;color:#1155cc;font-weight:400;text-decoration:underline;text-decoration-skip-ink:none;font-family:&quot;Roboto&quot;'> <a href='https://www.google.com/url?q=https://www.boomerang.com/&amp;sa=D&amp;ust=1547436261915000' style='color:inherit;text-decoration:inherit'>boomerang</a> </span> <span style='font-family:&quot;Roboto&quot;;color:#222222;font-weight:400'>,<br>As they walk away </span> <span style='font-family:&quot;Roboto&quot;;color:#ff0000;font-weight:400'>singing</span> <span style='font-family:&quot;Roboto&quot;;color:#222222;font-weight:400'>&nbsp;the peace is not so strong,<br>To turn the inside to the </span> <span style='background-color:#00ff00;font-family:&quot;Roboto&quot;;color:#222222;font-weight:400'>out and right to</span> <span style='color:#222222;font-weight:400;text-decoration:none;vertical-align:baseline;font-size:11pt;font-family:&quot;Roboto&quot;;font-style:normal'>&nbsp;wrong</span> </p><p style='padding-top:9pt;margin:0;color:#000000;padding-left:0;font-size:11pt;padding-bottom:9pt;font-family:&quot;Arial&quot;;line-height:1.24;orphans:2;widows:2;text-align:left;padding-right:0'> <span style='font-family:&quot;Roboto&quot;;color:#222222;font-weight:700'>But silly as it is, when you can bang your head,</span> <span style='font-family:&quot;Roboto&quot;;color:#222222;font-weight:400'> <br></span> <span style='font-family:&quot;Roboto&quot;;font-style:italic;color:#222222;font-weight:400'>And</span> <span style='font-family:&quot;Roboto&quot;;color:#222222;font-weight:400'>&nbsp;who needs </span> <span style='font-family:&quot;Roboto&quot;;font-style:italic;color:#222222;font-weight:400'>history</span> <span style='font-family:&quot;Roboto&quot;;color:#222222;font-weight:400'>&nbsp;when time just moves ahead,<br></span> <span style='font-family:&quot;Roboto&quot;;color:#222222;font-weight:700'>As</span> <span style='font-family:&quot;Roboto&quot;;color:#222222;font-weight:400'>&nbsp;everything you </span> <span style='font-family:&quot;Roboto&quot;;color:#222222;font-weight:700'>want</span> <span style='color:#222222;font-weight:400;text-decoration:none;vertical-align:baseline;font-size:11pt;font-family:&quot;Roboto&quot;;font-style:normal'>&nbsp;brings something else instead,</span> </p><p style='padding-top:9pt;margin:0;color:#000000;padding-left:0;font-size:11pt;padding-bottom:9pt;font-family:&quot;Arial&quot;;line-height:1.24;orphans:2;widows:2;text-align:left;padding-right:0'> <span style='color:#222222;font-weight:400;text-decoration:none;vertical-align:baseline;font-size:11pt;font-family:&quot;Roboto&quot;;font-style:italic'>A hammerhead might taste the blood,<br>A ladybug might see the red</span> </p><p style='padding-top:9pt;margin:0;color:#000000;padding-left:0;font-size:11pt;padding-bottom:9pt;font-family:&quot;Arial&quot;;line-height:1.24;orphans:2;widows:2;text-align:left;padding-right:0'> <span style='color:#222222;font-weight:400;text-decoration:none;vertical-align:baseline;font-size:11pt;font-family:&quot;Roboto&quot;;font-style:normal'>There was a princess, her friend the mouse, and his little cheese,<br>And she wore these tiny slippers wear you&#39;d think her toes were squeezed,<br>As she smiles on a swing, glides above a flower bed,<br>The gentle nature of a woman gives me hope to rest my head.</span> <span style='overflow: hidden; display: inline-block; margin: 0.00px 0.00px; border: 0.00px solid #000000; transform: rotate(0.00rad) translateZ(0px); -webkit-transform: rotate(0.00rad) translateZ(0px); width: 382.85px; height: 256.50px;'> <img alt='' src='https://lh3.googleusercontent.com/9fcR9YzuWTn1mP0CXk9fkELpDbaRUnGBjuucMdQddt18xXwBMFFD-WdRhBJtP7bgWT_k-s3Ipu3fnKuMSEWn7eng6t3UYjAx4hqKBs73vdovEELsCPHK8tbtn-ZFx1-p4abpGNFP' style='width: 382.85px; height: 256.50px; margin-left: 0.00px; margin-top: 0.00px; transform: rotate(0.00rad) translateZ(0px); -webkit-transform: rotate(0.00rad) translateZ(0px);' title=''> </span> </p><p style='padding-top:0pt;margin:0;color:#000000;padding-left:0;font-size:11pt;padding-bottom:9pt;font-family:&quot;Arial&quot;;line-height:1.24;orphans:2;widows:2;text-align:left;padding-right:0'> <span style='color:#222222;font-weight:400;text-decoration:none;vertical-align:baseline;font-size:11pt;font-family:&quot;Roboto&quot;;font-style:normal'>And hope fuels generations.<br>And hope can start your car.<br>And hope is the root of fantasy.<br>It&#39;s nothing but a star,</span> </p><p style='padding-top:0pt;margin:0;color:#000000;padding-left:0;font-size:11pt;padding-bottom:9pt;font-family:&quot;Arial&quot;;line-height:1.24;orphans:2;widows:2;text-align:left;padding-right:0'> <span style='color:#222222;font-weight:400;text-decoration:none;vertical-align:baseline;font-size:11pt;font-family:&quot;Roboto&quot;;font-style:normal'>Which may be fleeting, may be bright,<br>May keep you staring at the night.<br>Where one might question what life will be.<br>Quietly, I ask myself, &#39;Is there still hope for me?&#39;</span> </p><ul class='lst-kix_wdqwr3v8eg2s-0 start' style='padding:0;margin:0'> <li style='padding-top:0pt;color:#000000;padding-left:0pt;font-size:11pt;padding-bottom:9pt;line-height:1.24;margin-right:0;margin-left:36pt;font-family:&quot;Arial&quot;;margin-top:0;orphans:2;margin-bottom:0;widows:2;text-align:left;padding-right:0'> <span style='color:#222222;font-weight:400;text-decoration:none;vertical-align:baseline;font-size:11pt;font-family:&quot;Roboto&quot;;font-style:normal'>Hope is a generation.</span> </li><li style='padding-top:0pt;color:#000000;padding-left:0pt;font-size:11pt;padding-bottom:9pt;line-height:1.24;margin-right:0;margin-left:36pt;font-family:&quot;Arial&quot;;margin-top:0;orphans:2;margin-bottom:0;widows:2;text-align:left;padding-right:0'> <span style='color:#222222;font-weight:400;text-decoration:none;vertical-align:baseline;font-size:11pt;font-family:&quot;Roboto&quot;;font-style:normal'>Bang your head in the car.</span> </li><li style='padding-top:0pt;color:#000000;padding-left:0pt;font-size:11pt;padding-bottom:9pt;line-height:1.24;margin-right:0;margin-left:36pt;font-family:&quot;Arial&quot;;margin-top:0;orphans:2;margin-bottom:0;widows:2;text-align:left;padding-right:0'> <span style='color:#222222;font-weight:400;text-decoration:none;vertical-align:baseline;font-size:11pt;font-family:&quot;Roboto&quot;;font-style:normal'>And what is the root of fantasy?</span> </li><li style='padding-top:0pt;color:#000000;padding-left:0pt;font-size:11pt;padding-bottom:9pt;line-height:1.24;margin-right:0;margin-left:36pt;font-family:&quot;Arial&quot;;margin-top:0;orphans:2;margin-bottom:0;widows:2;text-align:left;padding-right:0'> <span style='color:#222222;font-weight:400;text-decoration:none;vertical-align:baseline;font-size:11pt;font-family:&quot;Roboto&quot;;font-style:normal'>Lay your bet on a star.</span> </li></ul> <p style='padding:0;margin:0;color:#000000;font-size:11pt;font-family:&quot;Arial&quot;;line-height:1.15;orphans:2;widows:2;height:11pt;text-align:left'> <span style='color:#000000;font-weight:400;text-decoration:none;vertical-align:baseline;font-size:11pt;font-family:&quot;Arial&quot;;font-style:normal'> </span> </p><p style='padding:0;margin:0;color:#000000;font-size:11pt;font-family:&quot;Arial&quot;;line-height:1.15;orphans:2;widows:2;height:11pt;text-align:left'> <span style='color:#000000;font-weight:400;text-decoration:none;vertical-align:baseline;font-size:11pt;font-family:&quot;Arial&quot;;font-style:normal'> </span> </p></body> </html>")
;(parse-gdoc/post post)

;(first ((sxpath '("html" "body")) (html->xexp post)))