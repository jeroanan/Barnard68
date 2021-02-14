#lang scribble/base

@title{hackernews.rkt}

Provides a CGI script to get the top stories from Hacker News.

@section{usage}

Calling the script will download the top stories.

Getting the top stories consists of two steps:

@itemlist[
 @item{/v0/beststories.json is called to retrieve the items ids of the top stories}
 @item{/v0/item/<itemid>.json is called for each id returned by the previous call to get story title and URL}]

The initial call to /v0/beststories.json returns, according to the API documentation, a list of up to 500 item Ids. I have found that it's more like 200, but regardles /v0/item must then be called for each individual item id to retrieve the title and URL. This can take a while so to mitigate the script saves results from /v0/item to a sqlite database and only retrieves from the API if it hasn't gotten it before.

The sqlite database can be made by running the make-hn-db.sh shell script in the tools directory of this project. You may have to tweak that script, hackernews.rkt and/or symlinks in your filesystem to get the database working from the desired place. If hackernews.rkt is run and it can't find its database it's probably going to fall over as things stand. :). I might improve this in future but it's not a priority at the moment.

@section{License}

GPLV3+