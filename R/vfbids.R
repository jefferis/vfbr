#' Convert between VFB and external identifiers
#'
#' @details Note that these functions are not appropriate for fetching more than
#'   ~ 200 identifiers individually specified identifiers. If a large number of
#'   identifiers are required, it is a much better idea to fetch all ids and
#'   then filter locally.
#' @param vfbids One or more vfb identifiers. Solr wildcards can also be used
#'   when \code{fixed=FALSE}- see examples and \code{\link{vfb_solr_query}}
#' @param mustWork logical: if \code{TRUE} then an error is given if there are
#'   missing results; if {NA} then a warning; if \code{FALSE} then there will be
#'   no message but mising values will still be denoted by \code{NA} values.
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
#'
#' # make up a fake id for testing - will give a warning
#' \donttest{
#' vfb_fromvfbids(c(vfbids, "VFB_10013392"))
#' }
vfb_fromvfbids<-function(vfbids, ..., mustWork=NA){
  q=paste(vfbids, collapse = " ")
  rdf=vfb_solr_query(query=q, fields = "short_form+label", rows=Inf, ...)
  if(nrow(rdf)==0) {
    res=rep(NA_character_, length(vfbids))
  } else {
    # make sure results are in correct order with NAs for missing results
    res=rdf$label[match(vfbids, rdf$short_form)]
  }
  if(!isFALSE(mustWork)){
    if(any(is.na(res)))
      if(isTRUE(mustWork)) stop("Some ids could not be translated!")
      else warning("Some ids could not be translated!")
  }
  res
}

#' @param ids One or more external identifiers. Solr wildcards can also be used
#'   when \code{fixed=FALSE}- see examples and \code{\link{vfb_solr_query}}
#' @param fixed Whether to insist on one exact match for each input identifier
#'   in \code{ids}. When \code{fixed=FALSE} solr wildcards can be used
#'   (returning an arbitrary number of identifiers) but query ids cannot be
#'   matched to returned ids.
#'
#' @param synonyms whether to compare with the synonym field rather than the
#'   canonical label.
#' @rdname vfb_fromvfbids
#' @export
#' @examples
#' ## FlyCircuit ids
#' # these "gene_name" ids are a secondary id on FlyCircuit
#' fcgns=c("DvGlutMARCM-F1106_seg1", "DvGlutMARCM-F1448_seg1",
#'   "FruMARCM-F001735_seg002", "THMARCM-160F_seg2")
#'
#' # note the difference in order between fixed = TRUE/FALSE
#' vfb_tovfbids(fcgns, fixed=TRUE)
#' vfb_tovfbids(fcgns, fixed=FALSE)
#' # depends on searching synonym field
#' vfb_tovfbids(fcgns, fixed=TRUE, synonyms=FALSE)
#' vfb_tovfbids(fcids, fixed=TRUE)
#' # these are the canonical form, so don't depend on searching synonym field
#' vfb_tovfbids(fcids, synonyms=FALSE)
#'
#' # Some GMR GAL4 lines
#' gmrs=c('93D09', '87F10')
#' vfb_tovfbids(sprintf("GMR_%s*", gmrs), fixed=FALSE)
#' # unfortunately these don't work with fixed=TRUE to define ordering
#' vfb_tovfbids(sprintf("GMR_%s*", gmrs), fixed=TRUE)
vfb_tovfbids<-function(ids, synonyms=TRUE, fixed=TRUE, ...){
  query_field=ifelse(synonyms, "synonym_autosuggest:", "label_s:")
  q=paste0(query_field, ids, collapse = " ")
  rdf = vfb_solr_query(
    filterquery = "VFB_*",
    query = q,
    #fetch synonyms if we need to match against them
    fields = paste0("short_form+label",ifelse(synonyms,"+synonym","")),
    rows = Inf,
    ...
  )
  if(nrow(rdf)==0) {
    if(fixed) rep(NA_character_, length(ids)) else character()
  } else if(fixed){
    if(nrow(rdf)!=length(ids))
      stop("mismatch between returned number of solr results and queries!")
    # make sure results are in correct order with NAs for missing results
    if(synonyms) {
      # the synonym field may have multiple entries per row
      indices=rep(seq_len(nrow(rdf)), sapply(rdf$synonym, length))
      all_syns=unlist(rdf$synonym, use.names = FALSE)
      targetdf=data.frame(index=indices, syn=all_syns, stringsAsFactors = FALSE)
      qdf=data.frame(id=ids, stringsAsFactors = FALSE)
      m=merge(qdf, targetdf, by.x='id', by.y='syn', all.x=TRUE, sort=FALSE)
      rdf$short_form[m$index]
    } else rdf$short_form[match(ids, rdf$label)]
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
#' # Note that gmr_vfbid is quite relaxed about the form of the GMR ids
#' gmr_vfbid("11H09")
#' gmr_vfbid("R11H09")
#' gmr_vfbid("GMR11H09")
#' @seealso \code{\link{vfb_fromvfbids}}
gmr_vfbid<-function(ids){
  shortids=extract_gmr_id(ids)
  r=vfb_solr_query(filterquery=c("VFB_*","synonym_autosuggest:GMR_*"), fields = "short_form+label", rows = Inf)
  r$gmr_ids=extract_gmr_id(r$label)
  r$short_form[match(shortids, r$gmr_ids)]
}

