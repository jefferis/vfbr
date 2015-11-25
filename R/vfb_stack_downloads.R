#' Return the urls for GMR-Gal4 stacks
#' @description \code{gmr_stack_urls} returns all URLs. This is expensive but
#'   can be memoised (cached).
#' @details You probably won't want to use this directly
#' @return A character vector of URLs each named by their GMR ID
#' @param gmr_url The URL of the directory containing all registered GMR images
#'   for download.
#' @export
#' @examples
#' u=gmr_stack_urls()
#' length(u)
gmr_stack_urls<-function(gmr_url='http://flybrain.mrc-lmb.cam.ac.uk/vfb/jfrc/fl/reformatted-quant/') {
  h=xml2::read_html(gmr_url)
  urls=rvest::html_attr(rvest::html_nodes(h, css = "a"), 'href')
  nrrds=grep("^JFRC2_.*nrrd$",urls, value = TRUE)
  nrrd_urls=file.path(gmr_url, nrrds)
  names(nrrd_urls)=extract_gmr_id(nrrd_urls)
  nrrd_urls
}

#' Return the urls for specified GMR-Gal4 stacks
#' @description \code{gmr_stack_urls_for_ids} returns URLs for specified GMR
#'   Gal4 lines
#' @rdname gmr_stack_urls
#' @param ids Character vector of GMR ids specified in any way
#' @return Character vector of named URLs
gmr_stack_urls_for_ids<-function(ids){
  all_urls=R.cache::memoizedCall(gmr_stack_urls)
  all_urls[extract_gmr_id(ids)]
}
