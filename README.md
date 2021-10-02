# Barnard68

Barnard 68 provides scripts that can be used with CGI-enabled Gemini
Protocol servers to act as gateways to web APIs. The scripts call their
respective APIs and output gemini documents.

I may add more scripts to act as API gateways to other places later.

## 1. The Scripts

hackernews.rkt – get top stories from Hackernews

wikimedia.rkt – search and read wikimedia articles. This script provides
a reader language that can be used to easily integrate to wikimedia. The
following very simple scripts are provided with this repo that
demonstrate how to do this:

* wikipedia.rkt

* wiktionary.rkt

bbcnews.rkt – read BBC news RSS feeds. This script also provids a very
simple reader language that allows for easy integration with furhter BBC
news RSS feeds. The following are provided here:

* bbcnews-tech.rkt

* bbcnews-topstories.rkt

* bbcnews-uk.rkt

* bbcnews-world.rkt

Note that since the bbcnews\* scripts just fetch and parse RSS feeds,
the links that they present are merely http links to the BBC news
webesite.

bbcweather.rkt – Get the weather from the BBC. This repo includes two
examples to get the three-day forecast and latest observatons from
manchester. To get the weather for your location, copy/edit these files
to contain your location code. You can get your location code by search
for your location on the BBC weather website, and then copying the
7-digit number from the end of the resulting URL.

## 2. How to use

* Unless you’re running binary versions of the scripts (not really
  recommended) you’ll need Racket

* You need a Gemini server that can run CGI scripts. I have developed
  and tested this using dezhemini, as I really enjoy Racket

* Put barnard68.rkt as well as any/all of the other rkt files you want
  to use in whichever directory you keep CGI scripts in on your Gemini
  server

* All .rkt files other than barnard68.rkt, wikimedia.rkt and bbcnews.rkt
  should already be executable, but worth checking

## 3. License

GPLV3+
