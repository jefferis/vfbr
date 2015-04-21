#' Generic query against VFB API
#'
#' This function epects an R list or vector describing a query and constructs an
#' appropriate query url embedding a JSON query, GETs the server response and
#' (by default) parses the JSON result.
#'
#' @param query A key-value list specifying the query
#' @param path The path on the server containing the query page
#' @param server The base url of the server
#' @param parse.json Whether or no to parse the response (default: TRUE)
#' @param ... additional arguments passed to
#' @export
#' @examples
#' vfb_generic_query(list(query_type="descendant_class", query="FBbt:00003679"))
#'
#' vfb_generic_query(list(query_type="individuals", query="FBbt:00003679"))
vfb_generic_query<-function(query, path="do/jsonQuery.html?json=",
                             server= getOption("vfbr.server"), parse.json=TRUE, ...) {
  queryj=minify(toJSON(query, auto_unbox=TRUE))
  queryj=utils::URLencode(queryj)
  url=paste0(server, "/", path, queryj)
  if(is.null(server)) stop ("You must specify a server!")
  res=GET(url)

  if(parse.json) {
    vfb_parse_json(res, ...)
  } else {
    res
  }
}

vfb_parse_json <- function(req, ...) {
  text <- content(req, as = "text")
  if (identical(text, "")) stop("No output to parse", call. = FALSE)
  jsonlite::fromJSON(text, simplifyVector = FALSE, ...)
}
