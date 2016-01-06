#' Find canononical value for synonyms using VFB SOLR query
#'
#' @param x The search term. Multiple search terms are also allowed - see
#'   details. Wildcards (\code{?*}) can be used, but
#' @param exact Whether to do an exact match (default) or to allow partial
#'   matches against synonyms.
#' @param fields The fields to return (defaults to 'short_form label synonym').
#'   Setting \code{fields=""} implies all fields (but note that not all results
#'   may have the same fields - see discussion in details)
#' @param ... Additional arguments passed to \code{\link{vfb_solr_query}}
#' @return A data.frame containing one or more result rows or when
#' @details Note that data.frame that is returned may contain a list in the
#'   synonym column because there will likely be multiple synonyms for a given
#'   query.
#'
#'   When \code{x} has length > 1 i.e. multiple query terms, then multiple calls
#'   to \code{\link{vfb_solr_query}} are wrapped in a \code{\link[base]{sapply}}
#'   statement. You can pass arguments to \code{sapply} in \code{...} such as
#'   \code{simplify=FALSE} if you wish. By default the return value will be a
#'   matrix with rows that you can index by field name and columns that you can
#'   index by queries. However the form of this is not guaranteed if you ask for
#'   fields that are present only for some of the results.
#'
#'   See
#'   \url{https://github.com/EBISPOT/OLS/blob/master/ols-solr/src/main/solr-conf/ontology/conf/schema.xml}
#'    for definition of the of the synonym_s exact match field.
#' @seealso \code{\link{vfb_solr_query}}
#' @export
#' @examples
#' vfb_synonym_query("SOG")
#' # anything with a synonym that includes SOG
#' vfb_synonym_query("SOG", exact=FALSE)
#'
#' \donttest{
#' # query for PAM exactly
#' vfb_synonym_query("PAM")
#' # any term with a synonym containing PAM
#' vfb_synonym_query("PAM", exact = FALSE)
#' }
#' # terms with synonyms that start with "PAM-"
#' vfb_synonym_query("PAM-*")
#' # nb this doesn't return any results when exact=FALSE because solr matches
#' # against tokenised strings (i.e. strings that have broken on spaces and
#' # other non word characters such as dashes and underscores).
#' vfb_synonym_query("PAM-*", exact = FALSE)
#' \donttest{
#' # Search for MBON-01 to MBON-22 (sprintf is used to 0 pad the numbers)
#' vfb_synonym_query(sprintf("MBON-%02d",1:22))
#' # You can also use a wild card search, which is much faster since it only
#' # makes a single solr query but the hits are returned in an arbitrary order.
#' vfb_synonym_query("MBON-??")
#' }
vfb_synonym_query<-function(x, exact=TRUE, fields='short_form label synonym', ...){
  if(length(x)>1) return(sapply(x, vfb_synonym_query, exact=exact, fields=fields, ...))
  qfield=ifelse(exact, "synonym_s", "synonym")
  vfb_solr_query(filterquery=x, fields=fields, defaultfield = qfield, ...)
}
