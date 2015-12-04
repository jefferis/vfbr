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
#' \dontrun{
#' gmr_stack_urls_memo=memoise::memoise(gmr_stack_urls)
#' system.time(u2 <- gmr_stack_urls_memo())
#' # and again
#' system.time(u3 <- gmr_stack_urls_memo())
#' stopifnot(all.equal(u2,u3))
#' }
gmr_stack_urls<-function(gmr_url=getOption('vfbr.stack.gmr_url')) {
  h=xml2::read_html(gmr_url)
  urls=rvest::html_attr(rvest::html_nodes(h, css = "a"), 'href')
  nrrds=grep("^JFRC2_.*nrrd$",urls, value = TRUE)
  nrrd_urls=file.path(gmr_url, nrrds)
  names(nrrd_urls)=extract_gmr_id(nrrd_urls)
  nrrd_urls
}

#' @importFrom memoise memoise
gmr_stack_urls_memo=memoise::memoise(function(...) {
  message("Caching all gmr stack urls. This may take a minute ...")
  gmr_stack_urls(...)
})

#' Return the urls for specified GMR-Gal4 stacks
#' @description \code{gmr_stack_urls_for_ids} returns URLs for specified GMR
#'   Gal4 lines
#' @rdname gmr_stack_urls
#' @param ids Character vector of GMR ids specified in any way
#' @return Character vector of named URLs
gmr_stack_urls_for_ids<-function(ids){
  all_urls=gmr_stack_urls_memo()
  all_urls[extract_gmr_id(ids)]
}

cached_download=function(url, destfile, ...) {
  if(file.exists(destfile)) return(invisible(0L))
  try(download.file(url, destfile, ...))
}

regular_download=function(...) try(download.file(...))

#' Download the registered GMR stacks for given ids
#' @details Note that the downloading tries to be a little bit clever, by
#'   \enumerate{
#'
#'   \item wrapping the \code{download.file} call in a try expression in case it
#'   fails
#'
#'   \item caching the \code{download.file} call so if the destination file
#'   already exists it will not re-download the file.
#'
#'   }
#'
#'   If caching gives you unexpected behaviour, you can set \code{Force=TRUE}.
#'
#' @inheritParams gmr_stack_urls
#' @param download.dir The download directory
#' @param Force Whether to force the download.
#' @param ... Additional arguments passed to \code{\link{download.file}}
#' @return Named character vector of paths to downloaded files
#' @seealso \code{\link{gmr_stack_urls_for_ids}}
#' @export
#' @examples
#' \dontrun{
#' stacks=download_gmr_stacks(c('9A09', '95H11'))
#' stacks
#'
#' # on Mac and Windows systems, this will open in the default application
#' # e.g. Fiji/ImageJ if you have associated ".nrrd" files
#' open_stack<-function(x) system(paste("open", paste(shQuote(x), collapse = " ")))
#' open_stack(stacks)
#' # open the downloads directory in the Finder
#' system(paste("open -R", shQuote(getOption('vfbr.stack.downloads'))))
#'
#' # Set the package option to control where files are downloaded
#' # see also ?vfbr
#' option(vfbr.stack.downloads="/path/to/my/stacks/folder")
#' }
download_gmr_stacks<-function(ids, download.dir=getOption('vfbr.stack.downloads'),
                              Force=FALSE, ...){
  if(is.null(download.dir)) stop("You must specify a download directory")
  if(!file.exists(download.dir)) dir.create(download.dir, recursive = TRUE)
  if(!file.info(download.dir)$isdir)
    stop("You must specify a valid download directory")
  urls=na.omit(gmr_stack_urls_for_ids(ids))
  if(!length(urls)) return(invisible(NULL))
  dests=file.path(download.dir, basename(urls))
  names(dests)=names(urls)
  mapply(if(Force) regular_download else cached_download,
         urls, dests, MoreArgs = list(mode='wb', ...))
  dests
}
