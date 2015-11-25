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

memoised_download=R.cache::addMemoization(function(...) try(download.file(...)))
regular_download=function(...) try(download.file(...))

#' Download the registered GMR stacks for given ids
#' @details Note that the downloading tries to be a little bit clever, by
#'   \enumerate{
#'
#'   \item wrapping the \code{download.file} call in a try expression in case it
#'   fails
#'
#'   \item memoising the \code{download.file} call so if it is called with the
#'   same url and destination value it will not re-download the file.
#'
#'   }
#'
#'   If memoisation gives you unexpected behaviour, you can set
#'   \code{Force=TRUE}.
#'
#' @inheritParams gmr_stack_urls
#' @param download.dir The download directory
#' @param Force Whether to force the download.
#' @param ... Additional arguments passed to \code{\link{download.file}}
#' @return Named character vector of paths to downloaded files
download_gmr_stacks<-function(ids, download.dir=getOption('vfbr.stack.downloads'),
                              Force=FALSE, ...){
  if(is.null(download.dir)) stop("You must specify a download directory")
  if(!file.exists(download.dir)) dir.create(download.dir, recursive = TRUE)
  if(!file.info(download.dir)$isdir)
    stop("You must specify a valid download directory")
  urls=gmr_stack_urls_for_ids(ids)
  dests=file.path(download.dir, basename(urls))
  names(dests)=names(urls)
  mapply(ifelse(Force, regular_download, memoised_download),
         urls, dests, MoreArgs = list(...))
  dests
}

#
#
# gmr_nrrds
