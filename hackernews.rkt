#lang s-exp "barnard68.rkt"

(require racket/bool)
(require racket/list)
(require db)

(define database-file "hn.db") ;; remember to run enclosed db build shell script
(define base-url "hacker-news.firebaseio.com")

(struct item (id title url))

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

(define (get-top-stories)
  (define db-conn (sqlite3-connect #:database database-file))
  (define story-ids (get-jsexpr-from-url base-url "/v0/beststories.json"))
  (display-ok-header)
  (gemini-title "Top Stories")
  (for ([id story-ids])
    (let ([i (get-item db-conn id)])
      (gemini-link (item-url i) (item-title i)))))
