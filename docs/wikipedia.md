# wikipedia.rkt

Provides a CGI script to search for and read Wikipedia articles.

## 1. usage

Output depends on the url used to call the script:

* No query string – requests a search term

* search=<searchterm> – calls search api on wikipedia and returns list
  of results

* article=<articlename> – calls wikipedia api and returns article text

## 2. license

GPLV3+
