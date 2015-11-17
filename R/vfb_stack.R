#' Make a URL for a set of VFB ids to be displayed in the stack browser
#'
#' @param ids A character vector of IDs
#' @param terminfo A VFB id used to choose which term information will be
#'   displayed next to the stack browser.
#' @param clear Whether to clear items already displayed in the stack browser
#'   (default FALSE)
#' @inheritParams vfb_solr_query
#'
#' @return character vector to open stack browser
#' @export
#'
#' @examples
#' # some gmr lines
#' ids=c("VFB_00004657","VFB_00023207","VFB_00023120","VFB_00022264")
#' template="VFBt_00100000"
#' u=vfb_stack_url(c(template, ids))
#'
#' # find vfb id for this class of neuron often called fruitless mAL neuron
#' resdf=vfb_solr_query("synonym:mAL",filterquery = c("label:female","label:neuron"))
#' u=vfb_stack_url(c(template, ids), terminfo=resdf$short_form[1], clear=TRUE)
#' \dontrun{
#' browseURL(u)
#' }
vfb_stack_url<-function(ids, terminfo=NULL, clear=FALSE, path='/site/stacks', server= getOption("vfbr.server")) {
  u=paste0(server, path, "/", "index.htm?add=", paste(ids, collapse = ","))
  if(!is.null(terminfo)) u=paste0(u, "&id=", terminfo)
  if(clear) u=paste0(u, "&clear=true")
  u
}


#' Make a URL for a set of VFB ids to be displayed in the 3D browser
#'
#' @param ids A character vector of IDs
#' @param template VFB id of the template brain
#' @inheritParams vfb_solr_query
#'
#' @return character vector to open 3D browser
#' @export
#'
#' @examples
#' # some flycircuit neurons
#' ids=c("VFB_00004657","VFB_00023207","VFB_00023120","VFB_00022264")
#' u=vfb_3dbrowser_url(ids)
#'
#' fcids=c("VGlut-F-000304", "VGlut-F-200278", "fru-F-200121", "TH-F-300016")
#' u2=vfb_3dbrowser_url(vfb_tovfbids(fcids))
#' \dontrun{
#' browseURL(u)
#' browseURL(u2)
#' }
vfb_3dbrowser_url<-function(ids, template="VFBt_001", server=getOption('vfbr.server.gepetto')) {
  baseurl="%s/org.geppetto.frontend/geppetto?load_project_from_url=http://vfbdev.inf.ed.ac.uk/do/geppettoJson.json?i=%s&t=%s"
  sprintf(baseurl, server, paste(ids, collapse = ","), template)
}
