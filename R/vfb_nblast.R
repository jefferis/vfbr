#' Query vfb opencpu server to obtain pre-computed nblast scores
#'
#' @param query A single flycircuit identifier
#' @param n The number of hits to return
#' @param target The target data set to query
#' @param ... Additional arguments to be included in the POST
#'
#' @return a data.frame in descending score order with columns \itemize{
#'
#'   \item id Flycircuit Neuron name
#'
#'   \item score NBLAST score
#'
#'   }
#' @export
#' @examples
#' vfb_nblast('fru-M-200266')
#'
#' \dontrun{
#' top10=vfb_nblast('fru-M-200266', target="GMR-Gal4", n = 10)
#' library(nat.amira)
#' open_stack_viewer(getOption("vfbr.stack.downloads"), download_gmr_stacks(top10$id))
#' }
vfb_nblast<-function(query, n=50, target=c("FlyCircuit", "GMR-Gal4"), ...){
  target=match.arg(target)
  server="http://vfbdev.inf.ed.ac.uk"
  topn_path=paste0("/ocpu/library/flynblastscores/R/flycircuit_",
                   ifelse(target=='FlyCircuit', "topn", "gmr_topn"),
                   '/json')
  posturl=paste0(server,topn_path)
  listbody=list(query=query, n=n, ...)
  r=POST(posturl, body=listbody, encode = 'json')
  stop_for_status(r)
  jsonlite::fromJSON(content(r,as='text'))
}
