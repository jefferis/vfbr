#' Return the urls for all registered GMR-Gal4 stacks
#'
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
  nrrd_urls=grep("^JFRC2_.*nrrd$",urls, value = TRUE)
  names(nrrd_urls)=extract_gmr_id(nrrd_urls)
  nrrd_urls
}

# vfbr.stack.downloads
#
# gmr_nrrds
