#lang scribble/base

@title{Barnard68}

Barnard 68 provides scripts that can be used with CGI-enabled Gemini Protocol servers to act as gateways to web APIs. The scripts call their respective APIs and output gemini documents.

I may add more scripts to act as API gateways to other places later.

@section{The Scripts}

hackernews.rkt -- get stories from Hackernews
wikipedia.rkt -- search and read Wikipedia articles

barnard68.rkt provides a module language to the other scripts and so isn't designed to be run directly.

@section{How to use}

@itemlist[
 @item{Unless you're running binary versions of the scripts (not really recommended) you'll need Racket}
 @item{You need a Gemini server that can run CGI scripts. I have developed and tested this using dezhemini, as I really enjoy Racket}
 @item{Put barnard68.rkt as well as any/all of the other rkt files you want to use in whichever directory you keep CGI scripts in on your Gemini server}
 @item{All .rkt files other than barnard68.rkt should already be executable, but worth checking}]

@section{License}

GPLV3+




