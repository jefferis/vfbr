#' Make a URL for a set of VFB ids to be displayed in the stack browser
#'
#' @param ids A character vector of IDs
#' @param add Whether to add stacks to the existing scene (the default, when
#'   \code{add=TRUE}) or to start a new scene.
#' @inheritParams vfb_solr_query
#'
#' @return character vector to open stack browser
#' @export
#'
#' @examples
#' # some gmr lines
#' ids=c("VFBi_00004657","VFBi_00023207","VFBi_00023120","VFBi_00022264")
#' template="VFBt_00100000"
#' u=vfb_stack_url(c(template, ids), add=FALSE)
#' \dontrun{
#' browseURL(u)
#' }
vfb_stack_url<-function(ids, path='/site/stacks', server= getOption("vfbr.server")) {
  paste0(server, path, "/", "index.htm?add=", paste(ids, collapse = ","))
}
