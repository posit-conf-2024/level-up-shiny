# This file contains a general toolkit for routing and combining bits of
# HTTP-handling logic. It is similar in spirit to Rook (and Rack, and WSGI, and
# Connect, and...) but adds cascading and routing.
#
# This file is called "middleware" because that's the term used for these bits
# of logic in these other frameworks. However, our code uses the word "handler"
# so we'll stick to that for the rest of this document; just know that they're
# basically the same concept.
#
# ## Intro to handlers
#
# A **handler** (or sometimes, **httpHandler**) is a function that takes a
# `req` parameter--a request object as described in the Rook specification--and
# returns `NULL`, or an `httpResponse`.
#
## ------------------------------------------------------------------------

#' Create an HTTP response object
#'
#' @param status HTTP status code for the response.
#' @param content_type The value for the `Content-Type` header.
#' @param content The body of the response, given as a single-element character
#'   vector (will be encoded as UTF-8) or a raw vector.
#' @param headers A named list of additional headers to include. Do not include
#'   `Content-Length` (as it is automatically calculated) or `Content-Type` (the
#'   `content_type` argument is used instead).
#'
#' @examples
#' httpResponse(
#'   status = 405L,
#'   content_type = "text/plain",
#'   content = "The requested method was not allowed"
#' )
#'
#' @keywords internal
#' @export
httpResponse <- function(
    status = 200L,
    content_type = "text/html; charset=UTF-8",
    content = "",
    headers = list()) {
  # Make sure it's a list, not a vector
  headers <- as.list(headers)
  if (is.null(headers$`X-UA-Compatible`)) {
    headers$`X-UA-Compatible` <- "IE=edge,chrome=1"
  }
  resp <- list(
    status = status, content_type = content_type, content = content,
    headers = headers
  )
  class(resp) <- "httpResponse"
  return(resp)
}

#
# You can think of a web application as being simply an aggregation of these
# functions, each of which performs one kind of duty. Each handler in turn gets
# a look at the request and can decide whether it knows how to handle it. If
# so, it returns an `httpResponse` and processing terminates; if not, it
# returns `NULL` and the next handler gets to execute. If the final handler
# returns `NULL`, a 404 response should be returned.
#
# We have a similar construct for websockets: **websocket handlers** or
# **wsHandlers**. These take a single `ws` argument which is the websocket
# connection that was just opened, and they can either return `TRUE` if they
# are handling the connection, and `NULL` to pass responsibility on to the next
# wsHandler.
#
# ### Combining handlers
#
# Since it's so common for httpHandlers to be invoked in this "cascading"
# fashion, we'll introduce a function that takes zero or more handlers and
# returns a single handler. And while we're at it, making a directory of static
# content available is such a common thing to do, we'll allow strings
# representing paths to be used instead of handlers; any such strings we
# encounter will be converted into `staticHandler` objects.
#
## ------------------------------------------------------------------------
joinHandlers <- function(handlers) {
  # Zero handlers; return a null handler
  if (length(handlers) == 0) {
    return(function(req) NULL)
  }

  # Just one handler (function)? Return it.
  if (is.function(handlers)) {
    return(handlers)
  }

  handlers <- lapply(handlers, function(h) {
    if (is.character(h)) {
      return(staticHandler(h))
    } else {
      return(h)
    }
  })

  # Filter out NULL
  handlers <- handlers[!sapply(handlers, is.null)]

  if (length(handlers) == 0) {
    return(function(req) NULL)
  }
  if (length(handlers) == 1) {
    return(handlers[[1]])
  }

  function(req) {
    for (handler in handlers) {
      response <- handler(req)
      if (!is.null(response)) {
        return(response)
      }
    }
    return(NULL)
  }
}

#
# Note that we don't have an equivalent of `joinHandlers` for wsHandlers. It's
# easy to imagine it, we just haven't needed one.
#
# ### Handler routing
#
# Handlers do not have a built-in notion of routing. Conceptually, given a list
# of handlers, all the handlers are peers and they all get to see every request
# (well, up until the point that a handler returns a response).
#
# You could implement routing in each handler by checking the request's
# `PATH_INFO` field, but since it's such a common need, let's make it simple by
# introducing a `routeHandler` function. This is a handler
# [decorator](http://en.wikipedia.org/wiki/Decorator_pattern) and it's
# responsible for 1) filtering out requests that don't match the given route,
# and 2) temporarily modifying the request object to take the matched part of
# the route off of the `PATH_INFO` (and add it to the end of `SCRIPT_NAME`).
# This way, the handler doesn't need to figure out about what part of its URL
# path has already been matched via routing.
#
# (BTW, it's safe for `routeHandler` calls to nest.)
#
## -----------------------------------------------------------------------