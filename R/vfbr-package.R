#'Programmatic Access to the virtualflybrain.org website
#'
#'@section Queries:
#'
#'  \itemize{
#'
#'  \item{\code{\link{vfb_solr_query}}} Allows you to query VFB's SOLR end
#'  point. This gives you programmatic access to the great majority of searches
#'  that you can perform interactively on the VFB website.
#'
#'  \item{\code{\link{vfb_neo4j_query}}} Allows you to query VFB's Neo4J end
#'  point. This gives you programmatic access to many sophisticated searches
#'  based on the VFB ontology, including searches that may not be easily
#'  accessible via the website.
#'
#'  \item{\code{\link{vfb_synonym_query}}} A higher level function that allows
#'  you to search for canonical terms with synonyms matching a given query.
#'
#'  }
#'
#'@section Package Options: The following options can be set to specify default
#'  behaviour. They will be set to sensible defaults on package startup, unless
#'  you have previously set a value (e.g. in your \code{\link[base]{Rprofile}}).
#'
#'  \itemize{
#'
#'  \item{\code{vfbr.server}}{ URL of main VFB server}
#'
#'  \item{\code{vfbr.server.neo4j}}{ URL for Neo4J graph database queries}
#'
#'  \item{\code{vfbr.server.solr}}{ URL for SOLR queries}
#'
#'  \item{\code{vfbr.server.owl}}{ URL for OWL ontology queries}
#'
#'  \item{\code{vfbr.server.gepetto}}{ URL of VFB's gepetto server for 3D
#'  visualisation (see \code{\link{vfb_3dbrowser_url}}.)}
#'
#'  \item{\code{vfbr.server.r}}{ URL for opencpu/R queries - not for users at the moment}
#'
#'  \item{\code{vfbr.stack.gmr_url}}{URL containing listing of registered GMR
#'  Gal4 confocal stacks. See \code{\link{gmr_stack_urls}}}
#'
#'  \item{\code{vfbr.stack.downloads}}{Location of downloaded stacks. See
#'  \code{\link{download_gmr_stacks}}}
#'
#'  }
#'
#'@references See \url{http://virtualflybrain.org}
#'@seealso \code{\link{vfb_solr_query}}
#'@name vfbr-package
#'@aliases vfbr
#' @examples
#' # Show state of vfbr package options
#' options()[grep('^vfbr', names(options()))]
#'
#' \dontrun{
#' example(vfb_solr_query)
#' }
#'@docType package
#'@keywords package
#'@import httr jsonlite
NULL
