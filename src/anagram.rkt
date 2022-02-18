#!/usr/bin/env racket
#lang racket/base

(require racket/file
         racket/string)
(require "barnard68.rkt")

(define wordlist-file "./lang-english.txt")

(unless (file-exists? wordlist-file)
  (error (format "wordlist-file ~A not found" wordlist-file)))

(define (sort-word word)
  (list->string (sort (string->list (string-downcase word)) char<?)))

(define (find-anagrams word)
  (define words
    (filter (λ (w) (= (string-length word) (string-length w)))
            (file->lines wordlist-file)))
  (define word-sorted (sort-word word))
  (filter (λ (w) (and
                  (string=? word-sorted (sort-word w))
                  (not (string=? w word))))
          words))

(define (show-anagrams w)
  (define the-anagrams (map (λ (a) (format "* ~A\n" a)) (find-anagrams w)))
  (display-ok-header)
  (gemini-title (format "Anagrams of ~A" w))
  (if (eq? (length the-anagrams) 0)
      (display "None found\n")
      (display (string-join the-anagrams "")))
  (display "\n\n")
  (gemini-link "anagram.rkt" "Search again"))

(define (search-page)
  (if (string=? qs "")
      (display "10 search\r\n")
      (show-anagrams qs)))

(define qs (getenv "QUERY_STRING"))

(search-page)