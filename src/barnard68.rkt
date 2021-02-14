#lang racket/base
(provide get-script-root
         get-jsexpr-from-url
         display-ok-header
         gemini-title
         gemini-link
         (all-from-out racket/base))

(require racket/port)
(require net/http-client)
(require json)

(define (get-script-root) (getenv "SCRIPT_NAME"))

(define (get-jsexpr-from-url url path)
  (define-values (status headers response-port) (http-sendrecv url path #:ssl? #t))
  (string->jsexpr (port->string response-port)))

(define (display-ok-header)
  (display "20 text/gemini\r\n"))

(define (gemini-title title-text)
  (display (format "# ~A\n\n" title-text)))

(define (gemini-link url friendly-name)
  (display (format "=> ~A ~A\n\n" url friendly-name)))

