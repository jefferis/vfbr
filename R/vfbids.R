#' Convert between VFB and external identifiers
#'
#' @details Note that these functions are not appropriate for fetching more than
#'   ~ 200 identifiers individually specified identifiers. If a large number of
#'   identifiers are required, it is a much better idea to fetch all ids and
#'   then filter locally.
#' @param vfbids One or more vfb identifiers. Solr wildcards can also be used
#'   when \code{fixed=FALSE}- see examples and \code{\link{vfb_solr_query}}
#' @param ... Additional arguments passed to \code{\link{vfb_solr_query}}.
#' @return A character vector of vfb ids (for \code{vfb_fromvfbids}) or external
#'   identifiers (for \code{vfb_tovfbids}) in the same order as the input query.
#'   Missing values will be denoted by \code{NA}s when \code{fixed=TRUE}. When
#'   \code{fixed=FALSE} and a wild-card search returns no results a character
#'   vector of length 0 will be returned.
#' @export
#' @seealso \code{\link{gmr_vfbid}}, \code{\link{vfb_solr_query}}, which powers
#'   the underlying queries.
#' @examples
#' # some flycircuit ids
#' fcids=c("VGlut-F-000304", "VGlut-F-200278", "fru-F-200121", "TH-F-300016")
#' vfbids=vfb_tovfbids(fcids)
#' vfb_fromvfbids(vfbids)
vfb_fromvfbids<-function(vfbids, ...){
  q=paste(vfbids, collapse = " ")
  rdf=vfb_solr_query(query=q, fields = "short_form+label", rows=Inf, ...)
  if(nrow(rdf)==0) {
    rep(NA_character_, length(vfbids))
  } else {
    # make sure results are in correct order with NAs for missing results
    rdf$label[match(vfbids, rdf$short_form)]
  }
}

#' @param ids One or more external identifiers. Solr wildcards can also be used
#'   when \code{fixed=FALSE}- see examples and \code{\link{vfb_solr_query}}
#' @param fixed Whether to insist on exact matches. When \code{fixed=FALSE} solr
#'   wildcards can be used.
#' @rdname vfb_fromvfbids
#' @export
#' @examples
#' # Some GMR GAL4 lines
#' gmrs=c('93D09', '87F10')
#' vfb_tovfbids(sprintf("GMR_%s*", gmrs), fixed=FALSE)
vfb_tovfbids<-function(ids, fixed=TRUE, ...){
  query_field=ifelse(fixed, "label_s:", "label:")
  q=paste0(query_field, ids, collapse = " ")
  rdf=vfb_solr_query(filterquery="VFB_*",query=q, fields = "short_form+label", rows = Inf, ...)
  if(nrow(rdf)==0) {
    if(fixed) rep(NA_character_, length(ids)) else character()
  } else if(fixed){
    # make sure results are in correct order with NAs for missing results
    rdf$short_form[match(ids, rdf$label)]
  } else {
    # wild card results - ordering/missing values undefined
    rdf$short_form
  }
}

is.vfb_id <- function(x) {
  substr(x,1,4)=="VFB_"
}

extract_gmr_id <- function(ids) stringr::str_extract(ids, "[0-9]{1,2}[A-H][0-9]{2}")

#' Convert GMR identifiers to VFB ids
#'
#' Note that this function currently is vectorised and operates by pulling a
#' list of all GMR ids on VFB. Therefore if you have 50 (or 1000) GMR lines to
#' query, you should run the query in a single call to \code{gmr_vfbid}.
#'
#' @param ids A character vector of GMR Gal4 ids
#' @export
#' @examples
#' gmr_vfbid("11H09")
#' @seealso \code{\link{vfb_fromvfbids}}
gmr_vfbid<-function(ids){
  shortids=extract_gmr_id(ids)
  r=vfb_solr_query(filterquery=c("VFB_*","synonym_autosuggest:GMR_*"), fields = "short_form+label", rows = Inf)
  r$gmr_ids=extract_gmr_id(r$label)
  r$short_form[match(shortids, r$gmr_ids)]
}

