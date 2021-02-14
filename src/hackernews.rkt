#!/usr/bin/env racket
#lang s-exp "barnard68.rkt"

(require racket/bool)
(require racket/list)
(require db)
(require json)

(define database-file "hn.db") ;; remember to run enclosed db build shell script
(define id-cache-expiry-seconds 2600)

(define base-url "hacker-news.firebaseio.com")

(struct item (id title url))
(struct item-ids (seconds ids))

(define (get-item-from-db db-conn id)
  (define db-item (query-rows db-conn "SELECT * FROM Item WHERE ID=?" id))
  (if (empty? db-item)
      #f
      (let ([row (first db-item)])
        (item (vector-ref row 0) (vector-ref row 1) (vector-ref row 2)))))

(define (add-item-to-db db-conn item)
  (query-exec db-conn
              "INSERT INTO Item (Id, Title, Url) VALUES (?, ?, ?)"
              (item-id item)
              (item-title item)
              (item-url item)))

(define (get-item-from-api id)
  (define j (get-jsexpr-from-url base-url (format "/v0/item/~A.json" id)))
  (define i (item
             (hash-ref j 'id)
             (hash-ref j 'title)
             (if (hash-has-key? j 'url) (hash-ref j 'url) "")))
  i)

(define (get-item db-conn id)
  (define db-item (get-item-from-db db-conn id))
  (if (false? db-item)
      (let ([api-item (get-item-from-api id)])
        (add-item-to-db db-conn api-item)
        api-item)
      db-item))

(define (get-top-item-ids-from-db db-conn)
  (define rows (query-rows
                db-conn
                "SELECT * FROM IdCache"))
  (if (empty? rows)
      #f
      (let ([row (first rows)])
        (item-ids (vector-ref row 0) (string->jsexpr (vector-ref row 1))))))

(define (clear-item-id-cache db-conn)
  (query-exec db-conn "DELETE FROM IdCache"))

(define (get-top-item-ids-from-api db-conn)
  (define ids (get-jsexpr-from-url base-url "/v0/beststories.json"))
  (clear-item-id-cache db-conn)
  (query-exec db-conn "INSERT INTO IdCache VALUES (?, ?)" (current-seconds) (jsexpr->string ids))
  ids)
  
(define (get-top-story-ids db-conn)
  (define id-cache (get-top-item-ids-from-db db-conn))
  (if (or (false? id-cache)
          (> (- (current-seconds) (item-ids-seconds id-cache)) id-cache-expiry-seconds))
      (get-top-item-ids-from-api db-conn)
      (item-ids-ids id-cache)))
  
(define (get-top-stories)
  (define db-conn (sqlite3-connect #:database database-file))
  (define story-ids (get-top-story-ids db-conn))
  (display-ok-header)
  (gemini-title "Top Stories")
  (for ([id story-ids])
    (let ([i (get-item db-conn id)])
      (gemini-link (item-url i) (item-title i)))))

(get-top-stories)