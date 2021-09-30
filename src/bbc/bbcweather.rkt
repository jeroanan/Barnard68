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
     #'(module bbcweather racket
         
         (require "barnard68.rkt"
                  "bbc/bbc.rkt")
  
         (define config-hash (syntax-str->config-hash str))
         
         (define forecast-type (hash-ref config-hash "forecast-type"))
         (define location-code (hash-ref config-hash "location-code"))
         
         (define rss-host "weather-broker-cdn.api.bbci.co.uk")
         (define language "en")
         (define rss-path (format "~A/rss" (if (string=? forecast-type "3day") "forecast" "observation")))

         (define path-elements (map
                                (λ (x) (string-trim x "/"))
                                (filter (λ (x) (not (string=? "" x))) (list
                                                                       language
                                                                       rss-path
                                                                       (if (string=? forecast-type "3day") forecast-type "")
                                                                       location-code))))

         (define full-path (string-join path-elements "/" #:before-first "/"))

         (channel-field channel-title "")
         (channel-field channel-link "")
         (channel-field channel-description "")
         (channel-field channel-language "")
         (channel-field channel-copyright "")
         (channel-field channel-pub-date "")

         (define channel-action-mappings (list
                                          (cons 'title set-channel-title)
                                          (cons 'description set-channel-description)
                                          (cons 'link (λ (x) (set! channel-link x)))
                                          (cons 'pubDate set-channel-pub-date)
                                          (cons 'copyright set-channel-copyright)
                                          (cons 'language set-channel-language)))
                  
         (define (parse-item e)
           (define gt (λ (fn x) (third (fn x))))
           (define x (filter list? e))
           (define title-text (gt second x))
           (define link (gt third x))
           (define description-text (gt fourth x))
           (define pubdate (third (fifth x)))
           (item title-text description-text "" pubdate))

         (define items (execute rss-host full-path channel-action-mappings parse-item))
         (display-ok-header)
         (gemini-title channel-title)
         (gemini-link channel-link (format "~A\n\n" channel-link))
         (for ([i items])
           (displayln (format "## ~A" (item-title i)))           
           (displayln (format "~A\n\n" (item-description i)))
           (displayln (format "Published ~A\n\n" (item-pubdate i))))

         (displayln (format "\n\nweather content: ~A" channel-copyright))))))