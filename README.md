# Barnard68

Barnard 68 currently provides just one script that allows a Gemini server with CGI support to search and read Wikipedia articles.

I may add more scripts to act as API gateways to other places later.

# How to use

You need a Gemini server that can run CGI scripts. I have developed and tested this using dezhemini, as I really enjoy Racket. The Wikipedia script offers three urls:

/ -- Gives a search prompt
/search?<search_term> -- Searches Wikipedia for the given search term and presents the results as links
/article?<article_name> -- Displays the article

# Gemini Link

I don't plan to host anything here myself in Geminispace. Doing so seems a little too centralised for me. Rather, the contents of this repository are intended to be self-hosted. I myself host this stuff on a private Gemini server running on a Raspberry Pi in my house.

# License

GPLV3+
