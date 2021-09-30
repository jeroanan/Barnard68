#lang racket/base

(provide syntax-str->config-hash
         channel-field
         (struct-out item)
         cdata->string
         execute)

(require racket/list
         racket/string         
         net/http-client
         xml
         xml/path)

(require (for-syntax racket/base))

;; Makes a variable and setter of the given name and initial value
(define-syntax (channel-field stx)
  (syntax-case stx ()
    [(_ name init-value)
     (with-syntax ([setter-name
                    (datum->syntax #'name
                                   (string->symbol (format "set-~a" (syntax->datum #'name))))])
       #'(begin
           (define name init-value)
           (define (setter-name x) (set! name (cdata->string x)))))]))

;;; Takes the syntax I have defined for bbc languages (keyword value) and put it into a hash table.
(define (syntax-str->config-hash stx-str)
  (define stx-lines (filter (λ (l) (> (string-length (string-trim l)) 0)) (string-split stx-str "\n")))
  (define config-hash (make-hash))
  (for ([sl stx-lines])
           (define sl-split (string-split sl " "))
           (define key (first sl-split))
           (define value (string-join (rest sl-split) " "))
           (hash-set! config-hash key value))
  config-hash)

;;; A struct for holding an item (news item, weather forecast, etc)
(struct item (title
              description
              link
              pubdate))

;; Takes cdata and returns a string containing the actual data.
;; just returns its parameter if it's not a cdata.
(define (cdata->string c)
  (if (not (cdata? c))
      c
      (second
       (regexp-match "CDATA\\[(.*)\\]\\]" (cdata-string c)))))

(define (execute host path channel-action-mappings parse-item)
  (define-values (_ __ response-port) (get-rss-xml host path))
  (define xexpr (response-port->xexpr response-port))
  (define channel (filter list? (se-path*/list '(channel) xexpr)))
  (define channel-actions (channel-action-mappings->channel-actions channel-action-mappings))
  (define items (list))
  (for ([e channel])           
    (let ([element-name (first e)])             
      (cond
        [(hash-has-key? channel-actions element-name) ((hash-ref channel-actions element-name) (third e))]
        [(eq? element-name 'image) (void)]
        [(eq? element-name 'item) (set! items (append items (list (parse-item e))))])))
  items)

(define (get-rss-xml host path)
  (http-sendrecv host path #:ssl? #t))

(define (response-port->xexpr response-port)
  (xml->xexpr
   (document-element
    (read-xml/document response-port))))

(define (channel-action-mappings->channel-actions channel-action-mappings)
  (define channel-actions (make-hash))
  (for ([a channel-action-mappings])
    (hash-set! channel-actions (car a) (cdr a)))
  channel-actions)
