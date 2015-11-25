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
#'   \item{\code{vfbr.stack.downloads}}{Location of downloaded stacks. See
#'   \code{\link{download_gmr_stacks}}}

#'   }
#' @name vfbr-package
#' @aliases vfbr
#' @examples
#' # Show state of vfbr package options
#' options()[grep('^vfbr', names(options()))]
#' @docType package
#' @keywords package
NULL
