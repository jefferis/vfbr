#' Programmatic access to the VFB API
#'
#' @section package options:
#'
#'   You can set a default server to connect to with the option
#'   \code{vfbr.server}. This is presently set on package load as follows:
#'   \code{options(vfbr.server="http://vfbsandbox.inf.ed.ac.uk")} unless you
#'   have previously set it (e.g. in your \code{\link[base]{Rprofile}}) to a
#'   different value.
#' @name vfbr-package
#' @aliases vfbr
#' @docType package
#' @keywords package
#' @import httr jsonlite
#' @examples
#' # Show state of vfbr package options
#' options()[grep('^vfbr', names(options()))]
#' \dontrun{
#' example(vfb_solr_query)
#' }
#' @references See \url{http://virtualflybrain.org}
#' @seealso \code{\link{vfb_solr_query}}
NULL
