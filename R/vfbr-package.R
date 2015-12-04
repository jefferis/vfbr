#' Programmatic Access to the virtualflybrain.org website
#'
#' @section Package Options: The following options can be set to specify default
#'   behaviour. They will be set to sensible defaults on package startup, unless
#'   you have previously set a value (e.g. in your
#'   \code{\link[base]{Rprofile}}).
#'
#'   \itemize{
#'
#'   \item{\code{vfbr.server}}{ URL of main VFB server}
#'
#'   \item{\code{vfbr.server.gepetto}}{ URL of VFB's gepetto server for 3D
#'   visualisation (see \code{\link{vfb_3dbrowser_url}}.)}
#'
#'   \item{\code{vfbr.stack.gmr_url}}{URL containing listing of registered GMR
#'   Gal4 confocal stacks. See \code{\link{gmr_stack_urls}}}
#'
#'   \item{\code{vfbr.stack.downloads}}{Location of downloaded stacks. See
#'   \code{\link{download_gmr_stacks}}}
#'
#'   }
#'
#' @references See \url{http://virtualflybrain.org}
#' @seealso \code{\link{vfb_solr_query}}
#' @name vfbr-package
#' @aliases vfbr
#' @examples
#' # Show state of vfbr package options
#' options()[grep('^vfbr', names(options()))]
#'
#' \dontrun{
#' example(vfb_solr_query)
#' }
#' @docType package
#' @keywords package
#' @import httr jsonlite
NULL
