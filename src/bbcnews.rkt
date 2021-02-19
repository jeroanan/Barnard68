#lang s-exp "barnard68.rkt"

(require xml
         xml/path
         racket/list
         racket/string
         net/http-client)

(require (for-syntax racket/base))

(require "barnard68.rkt")

(define-syntax (channel-field stx)
  (syntax-case stx ()
    [(_ name init-value)
     (with-syntax ([setter-name
                    (datum->syntax #'name
                                   (string->symbol (format "set-~a" (syntax->datum #'name))))])
       #'(begin
           (define name init-value)
           (define (setter-name x) (set! name (cdata->string x)))))]))

(define rss-host "feeds.bbci.co.uk")
(define rss-path "/news/rss.xml")

(channel-field channel-title "")
(channel-field channel-description "")
(channel-field channel-link "")
(channel-field channel-generator "")
(channel-field channel-last-build-date "")
(channel-field channel-copyright "")
(channel-field channel-language "")
(channel-field channel-ttl "")

(struct item (title
              description
              link
              pubdate))

(define-values (status headers response-port) (http-sendrecv rss-host rss-path #:ssl? #t))

(define x
  (xml->xexpr
   (document-element
    (read-xml/document response-port))))

(define channel (filter list? (se-path*/list '(channel) x)))

(define channel-actions (make-hash))
(define channel-action-mappings (list
                                 (cons 'title set-channel-title)
                                 (cons 'description set-channel-description)
                                 (cons 'link set-channel-link)
                                 (cons 'generator set-channel-generator)
                                 (cons 'lastBuildDate set-channel-last-build-date)
                                 (cons 'copyright set-channel-copyright)
                                 (cons 'language set-channel-language)
                                 (cons 'ttl set-channel-ttl)))

(for ([a channel-action-mappings])
  (hash-set! channel-actions (car a) (cdr a)))

(define (parse-item e)
  (define gt (λ (fn x) (cdata->string (third (fn x)))))
  (define x (filter list? e))
  (define title-text (gt second x))
  (define description-text (gt third x))
  (define link (third (fourth x)))
  (define pubdate (third (sixth x)))
  (item title-text description-text link pubdate))

(define (cdata->string c)
  (if (not (cdata? c))
      ""
      (second
       (regexp-match "CDATA\\[(.*)\\]\\]" (cdata-string c)))))

(define items (list))

(for ([e channel])
  (let ([element-name (first e)])    
    (cond
      [(hash-has-key? channel-actions element-name) ((hash-ref channel-actions element-name) (third e))]
      [(eq? element-name 'image) (void)]
      [(eq? element-name 'item) (set! items (append items (list (parse-item e))))])))

(display-ok-header)
(gemini-title channel-title)
(gemini-link channel-link channel-link)
(for ([i items])
  (displayln (format "=> ~A ~A" (item-title i) (item-link i)))
  (displayln (format "Published ~A" (item-pubdate i)))
  (displayln (format "~A\n\n" (item-description i))))

(displayln (format "\n\nnews content: ~A" channel-copyright))
             