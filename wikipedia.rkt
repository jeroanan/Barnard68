#!/usr/bin/env racket
#lang racket/base

(require racket/bool)
(require racket/list)
(require racket/port)
(require racket/string)
(require net/http-client)
(require net/uri-codec)

(require json)

(define script-root (getenv "SCRIPT_NAME"))

(define (text/gemini-header code)
  (format "~A text/gemini\r\n" code))
  
(define (display-ok-header)
  (display (text/gemini-header 20)))

(define (internal-error msg)
  (display (format "50 ~A\r\n" msg)))

(define (gemini-link-line url friendly-name)
  (format "=> ~A ~A\n\n" url friendly-name))

(define (gemini-title title-text)
  (format "# ~A\n\n" title-text))

(define (wikipedia-get-jsexpr-from-path path)
  (define-values (status headers response-port) (http-sendrecv "en.wikipedia.org" path #:ssl? #t))
  (string->jsexpr (port->string response-port)))

(define (display-search-link)
  (display (gemini-link-line (format "/~A" script-root) "Search")))

(define (process-subheading l pattern fmt)
  (if (false? (regexp-match pattern l))
      l
      (format fmt (string-replace l "=" ""))))

(define (process-subheading-1 l)
  (process-subheading l #rx"== .* ==$" "## ~A"))

(define (process-subheading-2 l)
  (process-subheading l #rx"=== .* ===$" "## ~A"))

(define (process-line l)
  (define (loop l fs)
    (if (empty? fs)
        l
        (loop ((first fs) l) (rest fs))))

  (define fs (list
               process-subheading-1
               process-subheading-2))
  (loop l fs))

(define (get-article name)
  (define article-path (format "/w/api.php?action=query&format=json&titles=~A&prop=extracts&explaintext" name))
  (define article-json (wikipedia-get-jsexpr-from-path article-path))
  (define query-obj (hash-ref article-json 'query))
  (define pages-obj (hash-ref query-obj 'pages))
  (define num-obj (hash-ref pages-obj (first (hash-keys pages-obj))))
  
  (define title (hash-ref num-obj 'title))
  (define article (hash-ref num-obj 'extract))
  (define article-lines 
    (string-join
      (map 
        process-line
      (string-split article "\n")) "\n"))

  (display-ok-header)
  (display-search-link)
  (display (gemini-title (format "~A" title)))
  (display article-lines))

(define (search search-term)
  (define search-path (format "/w/api.php?action=opensearch&search=~A" search-term))

  (define search-json (wikipedia-get-jsexpr-from-path search-path))
  (define received-search-term (first search-json))
  (define search-results (second search-json))

  (display-ok-header)
  (display (gemini-title (format "Search results for \"~A\"" search-term)))
  (display-search-link)

  (for ([sr search-results])
    (display (gemini-link-line (format "/~A?article=~A" script-root (uri-encode sr)) sr)))
  (display "\n\n")
  (display (gemini-link-line (format "/~A" script-root) "Search")))

(define (default qs-dict)
  (if (eq? (string-length qs) 0)
      (display "10 search\r\n")
      (begin
        (if (hash-has-key? qs-dict "search")
            (search (hash-ref qs-dict "search"))
            (internal-error "invalid query string")))))
  
(define (neq? p1 p2)
  (not (eq? p1 p2)))

(define (query-string->hashtable qs)
  (define ht (make-hash))
  (define entries (string-split qs "&"))
  (for ([e entries])
    (define kv (string-split e "="))
    (hash-set!
     ht
     (first kv)
     (if (> (length kv) 1) (second kv) "")))
  ht)
                   
(define (dispatch query-string)
  (define query-entries (query-string->hashtable query-string))

  (define (has-key? k)
    (hash-has-key? query-entries k))

  (define (get-val k)
    (hash-ref query-entries k))
  
  (cond
    [(has-key? "search") (search (get-val "search"))]
    [(has-key? "article") (get-article (get-val "article"))]
    [else (search query-string)])
  (void))

(define qs (getenv "QUERY_STRING"))

(if (or (eq? qs #f) (eq? (string-length qs) 0))
    (default "")
    (dispatch qs))
