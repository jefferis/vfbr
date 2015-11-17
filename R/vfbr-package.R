#' Programmatic Access to the virtualflybrain.org website
#'
#' @section Package Options: The following options can be set to specify default
#'   behaviour.
#'
#'   \itemize{
#'
#'   \item{\code{vfbr.server}}{ URL of main VFB server}
#'
#'   \item{\code{vfbr.server.gepetto}}{ URL of VFB's gepetto server for 3D
#'   visualisation (see \code{\link{vfb_3dbrowser_url}}.)}
#'
#'   }
#' @name vfbr-package
#' @aliases vfbr
#' @seealso \code{\link{vfb_solr_query}}
#' @examples
#' # Show state of vfbr package options
#' options()[grep('^vfbr', names(options()))]
#' @docType package
#' @keywords package
NULL
