#lang s-exp "wikipedia.rkt"

(base-url "en.wiktionary.org")

(define qs (getenv "QUERY_STRING"))

(when (not (false? qs))
  (if (eq? (string-length qs) 0)
      (default "")
      (dispatch qs)))
