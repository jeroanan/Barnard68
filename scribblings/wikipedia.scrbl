#lang scribble/base

@title{wikipedia.rkt}

Provides a CGI script to search for and read Wikipedia articles.

@section{usage}

Output depends on the url used to call the script:

@itemlist[
 @item{No query string -- requests a search term}
 @item{search=<searchterm> -- calls search api on wikipedia and returns list of results}
 @item{article=<articlename> -- calls wikipedia api and returns article text}]

@section{license}

GPLV3+
          