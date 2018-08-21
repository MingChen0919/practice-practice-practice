library(httr)

# Overall design

# the key features of any API are the structure of the requests and structure
# of the responses. An HTTP request consists of the following parts
# 1) HTTP verb (GET, POST, DELETE, etc.)
# 2) The base URL for the API
# 3) The URL path or endpoint
# 4) URL query arguments (e.g., ?foo=bar)
# 5) Optional headers
# 6) An optional request body

# FIRST STEPS
# Send a simple request
# first, find a simple API endpoint that doesn't require authentication
# for this example, we'll use the list of httr issues which requires sending
# a GET request to repos/hadley/httr
github_api = function(path) {
  url = modify_url("https://api.github.com", path = path)
  GET(url)
}

resp = github_api("/repos/hadley/httr")
resp

# PARSE THE RESPONSE
# next, you need to take the response returned by the API and turn it into a useful
# object. Any API with return an HTTP response that consists of headers and a body.
# while the response can come in multiple forms (see above), two of the most common
# structured formats are XML and JSON

# some API allows you to which response format to use
GET("http://www.colourlovers.com/api/color/6B4106?format=xml")
GET("http://www.colourlovers.com/api/color/6B4106?format=json")

# others use 'content negotiation' to determine what sort of data to send back.
# in these cases, you will need to include one of accept_json() and accept_xml in
# your request.

# if you have a choice, choose json: it's usually much easier to work with than xml
# to determine what type of content is returned
http_type(resp)

# I recommend checking that the type is as you expect in your helper function.
# this will ensure that you get a clear error message if the API changes:
github_api = function(path) {
  url <- modify_url("https://api.github.com", path = path)
  
  resp <- GET(url)
  if (http_type(resp) != "application/json") {
    stop("API did not return json", call. = FALSE)
  }
  
  resp
}

# next we need to parse the output into an R object. httr provides some default parsers with
# content(..., as = "auto") but i don't recommend using them inside a package. Instead it's
# better to explicityly parse it yourself.
github_api = function(path) {
  url <- modify_url("https://api.github.com", path = path)
  
  resp <- GET(url)
  if (http_type(resp) != "application/json") {
    stop("API did not return json", call. = FALSE)
  }
  
  jsonlite::fromJSON(content(resp, "text"), simplifyVector = FALSE)
}

# return a helpful object
# rather than simply returning the response as a list, i think it's a good practice to make
# a simple S3 object. This will make debugging later on much much much more pleasant
github_api = function(path) {
  url <- modify_url("https://api.github.com", path = path)
  
  resp <- GET(url)
  if (http_type(resp) != "application/json") {
    stop("API did not return json", call. = FALSE)
  }
  
  parsed <- jsonlite::fromJSON(content(resp, "text"), simplifyVector = FALSE)
  
  structure(
    list(
      content = parsed,
      path = path,
      response = resp
    ),
    class = "github_api"
  )
}

print.github_api = function(x, ...) {
  cat("<GitHub ", x$path, ">\n", sep = "")
  str(x$content)
  invisible(x)
}

github_api("/users/hadley")

# Turn API errors into R errors
# next, you need to make sure that your API wrapper throws an error if the
# request failed. Using a web API introduces additional possible points of
# failure into R code aside from those occuring in R itself. These include:
# 1. client-side exceptions
# 2. network / communication exceptions
# 3. server-side exceptions
github_api <- function(path) {
  url <- modify_url("https://api.github.com", path = path)
  
  resp <- GET(url)
  if (http_type(resp) != "application/json") {
    stop("API did not return json", call. = FALSE)
  }
  
  parsed <- jsonlite::fromJSON(content(resp, "text"), simplifyVector = FALSE)

  if (http_error(resp)) {
    stop(
      sprintf(
        "GitHub API request failed [%s]\n%s\n<%s>",
        status_code(resp),
        parsed$message,
        parsed$documentation_url
      ),
      call. = FALSE
    )
  }
  
  structure(
    list(
      content = parsed,
      path = path,
      response = resp
    ),
    class = "github_api"
  )
  
}

github_api("/user/hadley")

# SET A USER AGENT
# a good default for an R API package wrapper is to make it the URL to your GitHub repo:
ua = user_agent("http://github.com/hadley/httr")
ua

github_api <- function(path) {
  url <- modify_url("https://api.github.com", path = path)
  
  resp <- GET(url, ua)
  if (http_type(resp) != "application/json") {
    stop("API did not return json", call. = FALSE)
  }
  
  parsed <- jsonlite::fromJSON(content(resp, "text"), simplifyVector = FALSE)
  
  if (status_code(resp) != 200) {
    stop(
      sprintf(
        "GitHub API request failed [%s]\n%s\n<%s>", 
        status_code(resp),
        parsed$message,
        parsed$documentation_url
      ),
      call. = FALSE
    )
  }
  
  structure(
    list(
      content = parsed,
      path = path,
      response = resp
    ),
    class = "github_api"
  )
}

# PASSING PARAMETERS
# 1. URL path: modify_url()
# 2. Query arguments: The query argument to GET(), POST(), etc.
# 3. HTTP headers: add_headers()
# 4. Request body: The body argument to GET(), POST(), etc.
# modify_url
POST(modify_url("https://httpbin.org", path = "/post"))

# query arguments
POST("http://httpbin.org/post", query = list(foo = "bar"))

# headers
POST("http://httpbin.org/post", add_headers(foo = "bar"))

# body
## as form
POST("http://httpbin.org/post", body = list(foo = "bar"), encode = "form")
## as json
POST("http://httpbin.org/post", body = list(foo = "bar"), encode = "json")

# AUTHENTICATION
# most common forms of authentication
# 1. "basic" authentication: username + password
# 2. "basic" authentication with an API key/token: token is generated with username and password
# 3. OAuth: OAuth is a protocol for generating a user- or session-specific authenticaiton token


# Pagination (handling multi-page responses)

# Rate limiting
rate_limit <- function() {
  github_api("/rate_limit")
}
rate_limit()

rate_limit <- function() {
  req <- github_api("/rate_limit")
  core <- req$content$resources$core
  
  reset <- as.POSIXct(core$reset, origin = "1970-01-01")
  cat(core$remaining, " / ", core$limit,
      " (Resets at ", strftime(reset, "%H:%M:%S"), ")\n", sep = "")
}

rate_limit()
