#lang scribble/base

@title{Barnard68}

Barnard 68 provides scripts that can be used with CGI-enabled Gemini Protocol servers to act as gateways to web APIs. The scripts call their respective APIs and output gemini documents.

I may add more scripts to act as API gateways to other places later.

@section{The Scripts}

hackernews.rkt -- get top stories from Hackernews

wikimedia.rkt -- search and read wikimedia articles. This script provides a reader language that can be used to easily
integrate to wikimedia. The following very simple scripts are provided with this repo that demonstrate how to do this:

@itemlist[
 @item{wikipedia.rkt}
 @item{wiktionary.rkt}]

bbcnews.rkt -- read BBC news RSS feeds. This script also provids a very simple reader language that allows for easy
integration with furhter BBC news RSS feeds. The following are provided here:

@itemlist[
 @item{bbcnews-tech.rkt}
 @item{bbcnews-topstories.rkt}
 @item{bbcnews-uk.rkt}
 @item{bbcnews-world.rkt}]

Note that since the bbcnews* scripts just fetch and parse RSS feeds, the links that they present are merely http links to
the BBC news webesite.
           
@section{How to use}

@itemlist[
 @item{Unless you're running binary versions of the scripts (not really recommended) you'll need Racket}
 @item{You need a Gemini server that can run CGI scripts. I have developed and tested this using dezhemini, as I really enjoy Racket}
 @item{Put barnard68.rkt as well as any/all of the other rkt files you want to use in whichever directory you keep CGI scripts in on your Gemini server}
 @item{All .rkt files other than barnard68.rkt, wikimedia.rkt and bbcnews.rkt should already be executable, but worth checking}]

@section{License}

GPLV3+




