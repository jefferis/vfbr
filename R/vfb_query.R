#' Generic query against VFB OWL API
#'
#' @description This function expects an R list or vector describing a query and
#'   constructs an appropriate query url embedding a JSON query, GETs the server
#'   response and (by default) parses the JSON result.
#'
#' @details Note that the VFB OWL query endpoint by default wraps all results
#'   inside a JSON result object called \code{results}. When
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
#' \dontrun{
#' # query for descendant classes of Fan-Shaped Body
#' vfb_owl_query(list(query_type="descendant_class", query="FBbt:00003679"))
#'
#' # query for individual neurons overlapping with Fan-Shaped Body
#' neurondf=vfb_owl_query(list(query_type="individuals", query="FBbt:00003679"))
#' # show the first few rows of the returned data.frame
#' head(neurondf)
#' }
vfb_owl_query<-function(query, path="do/jsonQuery.html?json=",
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

#' Query VFB via solr indexing system
#'
#' @description Solr provides an extremely fast way to query all key content on
#'   VFB and is the backend used for most queries run on the website. It is
#'   pre-populated from the OWL ontology documents describing all the
#'   information on the site and their relationships. More details on Solr and
#'   its query syntax can be found at \url{http://lucene.apache.org/solr/}.
#' @details The \code{query} arguments maps onto the general solr \code{q=}
#'   query while \code{filterqueries} maps onto one or more \code{fl=} terms.
#'   The
#'   \href{https://cwiki.apache.org/confluence/display/solr/Common+Query+Parameters}{solr
#'    wiki} says this about the difference:
#'
#'   The fq parameter defines a query that can be used to restrict the superset
#'   of documents that can be returned, without influencing score. It can be
#'   very useful for speeding up complex queries, since the queries specified
#'   with fq are cached independently of the main query. When a later query uses
#'   the same filter, there's a cache hit, and filter results are returned
#'   quickly from the cache.
#' @inheritParams vfb_owl_query
#' @param filterquery A character vector (of length one or more) describing
#'   filter queries for solr (see Details for regular vs filter queries)
#' @param sort Character vector naming one or more fields (+ delimited) to use
#'   for sorting the results.
#' @param defaultfield Character vector naming default field used for filter
#'   queries (defaults to \code{short_form})
#' @param rows Maximum number of rows to return. The special value of Inf
#'   implies all matching rows.
#' @param fields Which fields to return (+delimited). A value of \code{""}
#'   implies all fields.
#' @param ... additional solr query arguments
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
#' vfb_solr_query(filterquery="VFB_*",query="label:GMR_10A07*")
#'
#' # how many GMR lines can we find
#' # note use of rows = 0 so we do not fetch results (but still get totals)
#' r=vfb_solr_query(filterquery="VFB_*",query="label:GMR_*", rows=0)
#' attr(r,'numFound')
#' \dontrun{
#' # VFB id for all GMR lines
#' vfb_solr_query(filterquery="VFB_*",query="label:GMR_*", rows=4000)
#'
#' # VFB id for all FlyCircuit neurons
#' # note use of rows=Inf to fetch all rows
#' y=vfb_solr_query(filterquery="VFB_*",
#'   query="source_data_link_annotation:*flycircuit*", rows=Inf)
#' }
#' @seealso \code{\link[httr]{response}}
vfb_solr_query<-function(query="*:*", filterquery=NULL,
                         fields="label+short_form", sort="score+desc",
                         defaultfield="short_form", rows=30L,
                         path="search/select?wt=json",
                         server= getOption("vfbr.server"), parse.json=TRUE, ...) {
  if(!is.finite(rows)) {
    # check how many rows there are
    rowr=vfb_solr_query(query=query, filterquery = filterquery, fields = fields,
                        sort=sort, defaultfield = defaultfield, rows=0L, path=path,
                        server = server, parse.json = T)
    rows=attr(rowr,'numFound')
    # now we will return them all
  }
  params=c(fl=fields, sort=sort, rows=rows, df=defaultfield, q=query)
  # filterquery can be vectorised
  de_vectorise<-function(x, name) {
    if(length(x))
      names(x)=rep(name, length(x))
    x
  }
  params=c(params, de_vectorise(filterquery, "fq"))

  apl=pairlist(...)
  if(length(apl)){
    # interpret as extra SOLR params
    for(n in names(apl))  params=c(params, de_vectorise(apl[[n]], n))
  }

  fullquery=paste(names(params), sep="=", params, collapse = "&")
  url=paste0(server, "/", path, "&", fullquery)
  url=utils::URLencode(url)
  if(is.null(server)) stop ("You must specify a server!")
  res=GET(url)

  if(parse.json) {
    rawres=vfb_parse_json(res)
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



#' Query VFB's Neo4J graph database
#'
#' @details Under the hood, this uses the \code{\link[RNeo4j]{cypher}} function
#'   to call a Neo4J service running on the specified VFB server. The \code{path} and \code{server} arguments should be
#' @param x A character query in Neo4J's cypher language
#' @param ... Additional query arguments of the form \code{key=value}
#' @param path The relative path on the server for the Neo4J endpoint
#' @param server The server's root URL
#'
#' @return A data.frame of query results
#' @export
#' @importFrom RNeo4j startGraph cypher
#' @seealso \code{\link[RNeo4j]{RNeo4j}}, \code{\link[RNeo4j]{cypher}}
#' @family query
#' @examples
#' \donttest{
#' # ask for all neuronal classes
#' nclasses=vfb_neo4j_query("MATCH (n:Neuron:Class) RETURN n.label")
#' nrow(nclasses)
#' head(nclasses)
#'
#' # Find all images with an associated neuronal class
#' q=paste0("MATCH (n:Class:VFB { label : 'neuron' })",
#'   "<-[:SUBCLASSOF*]-(p)<-[:INSTANCEOF]-(i:Individual)",
#'   "RETURN distinct i.label, p.label;")
#' nclasses_image=vfb_neo4j_query(q)
#'
#' }
#' @references \url{https://neo4j.com/developer/cypher-query-language/}
vfb_neo4j_query <- function(x, ..., path="neo4jdb/data", server= getOption("vfbr.server")){
  url=file.path(server, path)
  g <- try(startGraph(url))
  cypher(g, x)

}
