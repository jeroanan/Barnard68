#!/usr/bin/env racket
#lang racket/base

(require racket/port
         syntax/strip-context)

(require (for-syntax racket/base))

(provide (rename-out [literal-read read]
                     [literal-read-syntax read-syntax]))

(define (literal-read in)
  (syntax->datum
   (literal-read-syntax #f in)))

(define (literal-read-syntax src in)
  (with-syntax ([str (port->string in)])    
    (strip-context
     #'(module bbcnews racket
         
         (require racket/list
                  racket/port
                  racket/string
                  net/http-client
                  xml
                  xml/path)

         (require "barnard68.rkt")
         
         (define stx-lines (filter (λ (l) (> (string-length (string-trim l)) 0)) (string-split str "\n")))
         (define config-hash (make-hash))
         
         (for ([sl stx-lines])
           (define sl-split (string-split sl " "))
           (define key (first sl-split))
           (define value (string-join (rest sl-split) " "))
           (hash-set! config-hash key value))
         
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
         (define rss-path (hash-ref config-hash "rss-path"))

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
                                          (cons 'link (λ (x) (set! channel-link x)))
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
         (gemini-link channel-link (format "~A\n\n" channel-link))
         (for ([i items])
           (displayln (format "=> ~A ~A" (item-link i) (item-title i)))
           (displayln (format "Published ~A" (item-pubdate i)))
           (displayln (format "~A\n\n" (item-description i))))

         (displayln (format "\n\nnews content: ~A" channel-copyright))))))
