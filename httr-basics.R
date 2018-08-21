library(httr)

# there are two important parts to http:
# 1) the request: the data sent to the server
# 2) the response: the data sent back from the server

# to make a request
r = GET("http://httpbin.org/get")
r

# you can pull out important parts of the response with various helper
# methods, or dig directly into the object
status_code(r)
headers(r)

# THE RESPONSE
# the data sent back from the server consists of three parts:
# 1) the status line
# 2) the headers
# 3) the body
# the most important part of the status line is the http status code
# it tells you whether or not the request was successful
# you can access the status code along with a descriptive message using
# http_status
http_status(r)

r$status_code

# THE BODY
# three ways to access the body of the request, all using content
# 1) access the body as a character vector
# 2) access the body as a raw vector
# 3) httr provides a number of default parsers for common file types
content(r, "text", encoding = 'ISO-8859-1')
content(r, 'raw')
str(content(r, 'parsed'))

# THE HEADERS
headers(r)

# Cookies
r = GET("http://httpbin.org/cookies/set", query = list(a = 1))
cookies(r)
# cookies are automatically persisted between requests to the same domain
r = GET("http://httpbin.org/cookies/set", query = list(b = 1))
cookies(r)


# THE REQUEST
# the url query string
# a common way of sending simple key-value pairs to the server is the
# query string: e.g. /httpbin.org/get?key=val.

r = GET("http://httpbin.org/get",
  query = list(key1 = "value1", key2 = "value2")
)
content(r)$args

# any NULL elements are automatically dropped from the list
r <- GET("http://httpbin.org/get", 
         query = list(key1 = "value 1", "key 2" = "value2", key2 = NULL))
content(r)$args

# custom headers
# you can add custom headers to a request with add_headers()
r <- GET("http://httpbin.org/get", add_headers(Name = "Hadley"))
str(content(r)$headers)

# cookies are simple key-value pairs like the query string, but they persist across
# multiple requests in a session
r <- GET("http://httpbin.org/cookies", set_cookies("MeWant" = "cookies"))
content(r)$cookies

# request body
# when POST()  ing, you can include data in the body of the request
# httr allows you to supply this in a number of different ways, the most
# common way is a named list
r = POST('http://httpbin.org/post', body = list(a=1, b=2, c=3))
r <- POST("http://httpbin.org/post", body = list(a = 1, b = 2, c = 3))

# you can use the encode argument to determine how this data is sent to the server
url <- "http://httpbin.org/post"
body <- list(a = 1, b = 2, c = 3)


# Form encoded
r <- POST(url, body = body, encode = "form")
# Multipart encoded
r <- POST(url, body = body, encode = "multipart")
# JSON encoded
r <- POST(url, body = body, encode = "json")

POST(url, body = body, encode = "multipart", verbose()) # the default
POST(url, body = body, encode = "form", verbose())
POST(url, body = body, encode = "json", verbose())
