#' Generic query against VFB API
#'
#' @description This function epects an R list or vector describing a query and
#'   constructs an appropriate query url embedding a JSON query, GETs the server
#'   response and (by default) parses the JSON result.
#'
#' @details Note that the VFB OWL query endpoint default wraps all results
#'   inside in a JSON result object called \code{results}. When
#'   \code{parse.json=TRUE}, the returned results will be unwrapped to remove
#'   this outer layer.
#'
#' @param query A key-value list specifying the query
#' @param path The path on the server containing the query page
#' @param server The base url of the server
#' @param parse.json Whether or no to parse the response (default: TRUE)
#' @param ... additional arguments passed to
#'   \code{jsonlite::\link[jsonlite]{fromJSON}}
#' @export
#' @seealso \code{\link[jsonlite]{fromJSON}}
#' @examples
#' # query for descendant classes of Fan-Shaped Body
#' vfb_generic_query(list(query_type="descendant_class", query="FBbt:00003679"))
#'
#' # query for individual neurons overlapping with Fan-Shaped Body
#' neurondf=vfb_generic_query(list(query_type="individuals", query="FBbt:00003679"))
#' # show the first few rows of the returned data.frame
#' head(neurondf)
#'
vfb_generic_query<-function(query, path="do/jsonQuery.html?json=",
                            server= getOption("vfbr.server"), parse.json=TRUE, ...) {
  queryj=minify(toJSON(query, auto_unbox=TRUE))
  queryj=utils::URLencode(queryj)
  url=paste0(server, "/", path, queryj)
  if(is.null(server)) stop ("You must specify a server!")
  res=GET(url)

  if(parse.json) {
    res=vfb_parse_json(res, ...)
    if(is.list(res) && length(res)==1 && names(res)=='results')
      res$results else res
  } else {
    res
  }
}

vfb_parse_json <- function(req, simplifyVector = TRUE, ...) {
  text <- content(req, as = "text")
  if (identical(text, "")) stop("No output to parse", call. = FALSE)
  jsonlite::fromJSON(text, simplifyVector = simplifyVector, ...)
}

#' Title
#'
#' @inheritParams vfb_generic_query
#' @param sort Character vector naming one or more fields (+ delimited) to use
#'   for sorting the results.
#' @param rows Maximum number of rows to return
#' @param fields Which fields to return (+delimited). A value of \code{""}
#'   implies all fields.
#' @return When \code{parse.json=TRUE}, a data.frame containing the parsed
#'   response (originally the \code{response$docs} field in the parsed JSON)
#'   along with additional attributes including
#'
#'   \itemize{
#'
#'   \item numFound
#'
#'   \item start
#'
#'   \item responseHeader
#'
#'   }
#'
#'   When \code{parse.json=FALSE} an \code{httr::response} object
#' @export
#'
#' @examples
#' # Find VFB ids matching a given GMR line
#' vfb_solr_query("fq=VFB_*&q=label:GMR_10A07*")
#' \dontrun{
#' # VFB id for all GMR lines
#' vfb_solr_query("fq=VFB_*&q=label:GMR_*", rows=4000)
#'
#' #' # VFB id for all FlyCircuit neurons
#' y=vfb_solr_query("fq=VFB_*&q=source_data_link_annotation:*flycircuit*",
#'   rows=20000)
#' }
#' @seealso \code{\link[httr]{response}}
vfb_solr_query<-function(query, path="search/select?wt=json&df=short_form",
                         fields="label+short_form", sort="score+desc", rows=30L,
                         server= getOption("vfbr.server"), parse.json=TRUE, ...) {
  fullquery=paste(paste0("sort=", sort), paste0("fl=", fields), paste0("rows=",rows), sep = "&", query)
  url=paste0(server, "/", path, "&", fullquery)
  url=utils::URLencode(url)
  if(is.null(server)) stop ("You must specify a server!")
  res=GET(url)

  if(parse.json) {
    rawres=vfb_parse_json(res, ...)
    # get main response data.frame
    response=rawres[['response']]
    res=response[['docs']]
    # make an empty data.frame if we got no response
    if(!length(res)) res=data.frame()
    # copy over other fields
    otherfields=setdiff(names(response),'docs')
    for(n in otherfields) attr(res, n)=response[[n]]
    # add response header as attribute as well
    attr(res, 'responseHeader')=rawres[['responseHeader']]
  }
  res
}
