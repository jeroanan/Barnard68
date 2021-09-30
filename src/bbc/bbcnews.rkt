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

         (require "barnard68.rkt"
                  "bbc/bbc.rkt")

         (define config-hash (syntax-str->config-hash str))
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

         (define channel-action-mappings (list
                                          (cons 'title set-channel-title)
                                          (cons 'description set-channel-description)
                                          (cons 'link (λ (x) (set! channel-link x)))
                                          (cons 'generator set-channel-generator)
                                          (cons 'lastBuildDate set-channel-last-build-date)
                                          (cons 'copyright set-channel-copyright)
                                          (cons 'language set-channel-language)
                                          (cons 'ttl set-channel-ttl)))

         (define (parse-item e)
           (define gt (λ (fn x) (cdata->string (third (fn x)))))
           (define x (filter list? e))
           (define title-text (gt second x))
           (define description-text (gt third x))
           (define link (third (fourth x)))
           (define pubdate (third (sixth x)))
           (item title-text description-text link pubdate))

         (define items (execute rss-host rss-path channel-action-mappings parse-item))

         (display-ok-header)
         (gemini-title channel-title)
         (gemini-link channel-link (format "~A\n\n" channel-link))
         (for ([i items])
           (displayln (format "=> ~A ~A" (item-link i) (item-title i)))
           (displayln (format "Published ~A" (item-pubdate i)))
           (displayln (format "~A\n\n" (item-description i))))

         (displayln (format "\n\nnews content: ~A" channel-copyright))))))
