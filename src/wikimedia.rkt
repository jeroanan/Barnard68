#lang racket/base

(require racket/port)
(require syntax/strip-context)

(provide (rename-out [literal-read read]
                     [literal-read-syntax read-syntax]))

(define (literal-read in)
  (syntax->datum
   (literal-read-syntax #f in)))

(define (literal-read-syntax src in)
  (with-syntax ([str (port->string in)])    
    (strip-context
     #'(module wikimedia racket
         
         (require racket/list)
         (require racket/string)
         (require racket/bool)
         (require net/uri-codec)
         (require "barnard68.rkt")

         (define base-url (make-parameter ""))
         (define qs (getenv "QUERY_STRING"))

         (define (wikipedia-get-jsexpr-from-path path)
           (get-jsexpr-from-url (base-url) path))

         (define (display-search-link)
           (gemini-link (format "/~A" (get-script-root)) "Search"))

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
           (gemini-title (format "~A" title))
           (display article-lines))

         (define (search search-term)
           (define search-path (format "/w/api.php?action=opensearch&search=~A" search-term))

           (define search-json (wikipedia-get-jsexpr-from-path search-path))
           (define received-search-term (first search-json))
           (define search-results (second search-json))

           (display-ok-header)
           (gemini-title (format "Search results for \"~A\"" search-term))
           (display-search-link)

           (for ([sr search-results])
             (gemini-link (format "/~A?article=~A" (get-script-root) (string-downcase (uri-encode sr))) sr))
           (display "\n\n")
           (gemini-link (format "/~A" (get-script-root)) "Search"))

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

         (define (default qs-dict)
           (if (eq? (string-length qs) 0)
               (display "10 search\r\n")
               (begin
                 (if (hash-has-key? qs-dict "search")
                     (search (hash-ref qs-dict "search"))
                     (internal-error "invalid query string")))))

         (define (dispatch)
           (define query-entries (query-string->hashtable qs))

           (define (has-key? k)
             (hash-has-key? query-entries k))

           (define (get-val k)
             (hash-ref query-entries k))
  
           (cond
             [(has-key? "search") (search (get-val "search"))]
             [(has-key? "article") (get-article (get-val "article"))]
             [else (search qs)])
           (void))

         (define (wikimedia-begin)
           (when (not (false? qs))
             (if (eq? (string-length qs) 0)
                 (default "")
                 (dispatch))))
         
         (define lines (filter (λ (l) (> (string-length (string-trim l)) 0)) (string-split str "\n")))
         (for ([sl lines])
           (define words (string-split sl " "))
           (define keyword (first words))
           (define operand (string-join (rest words) " "))
           (cond
             [(string=? keyword "wikimedia-base-url") (base-url operand)]))
         (wikimedia-begin)))))


