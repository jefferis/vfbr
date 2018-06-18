#' Query vfb opencpu server to obtain pre-computed nblast scores
#'
#' @param query A single flycircuit identifier
#' @param n The number of hits to return
#' @param target The target data set to query
#' @param vfb_ids Whether to expect and return VFB identifiers rather than the
#'   ids used in the original dataset. Default value \code{vfb_ids=NA} attempts
#'   to autodetect and return VFB ids.
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
#' @inheritParams vfb_solr_query
#' @examples
#' vfb_nblast('fru-M-200266')
#' # using a VFB id
#' vfb_nblast('VFB_00014755', n=10)
#'
#' \donttest{
#' top10=vfb_nblast('fru-M-200266', target="GMR-Gal4", n = 10)
#' head(top10)
#' }
#' \dontrun{
#' library(nat.amira)
#' open_stack_viewer(getOption("vfbr.stack.downloads"), download_gmr_stacks(top10$id))
#' }
#'
#' \dontrun{
#' # to use ocpu instance as an API
#' curl http://r.virtualflybrain.org/ocpu/library/vfbr/R/vfb_nblast/json \
#'   -d 'query="FruMARCM-M002262_seg001"'
#' # specify number of results to return
#' curl http://r.virtualflybrain.org/ocpu/library/vfbr/R/vfb_nblast/json \
#'   -d 'query="FruMARCM-M002262_seg001&n=10"'
#' # all results
#' curl http://r.virtualflybrain.org/ocpu/library/vfbr/R/vfb_nblast/json \
#'   -d 'query="FruMARCM-M002262_seg001&n=0"'
#' # with VFB id
#' curl http://r.virtualflybrain.org/ocpu/library/vfbr/R/vfb_nblast/json \
#'   -d 'query="VFB_00014755&n=10"'
#' }
vfb_nblast<-function(query, n=50, target=c("FlyCircuit", "GMR-Gal4"),
                     vfb_ids=NA, server=getOption("vfbr.server.r"), ...){
  target=match.arg(target)
  topn_path=paste0("/ocpu/library/flynblastscores/R/flycircuit_",
                   ifelse(target=='FlyCircuit', "topn", "gmr_topn"),
                   '/json')
  posturl=paste0(server,topn_path)

  if(is.na(vfb_ids)) {
    vfb_ids <- all(is.vfb_id(query))
  }
  if(isTRUE(vfb_ids)) {
    query <- vfb_fromvfbids(query)
  }

  listbody=list(query=query, n=n, ...)
  r=POST(posturl, body=listbody, encode = 'json')
  stop_for_status(r)
  res=jsonlite::fromJSON(content(r,as='text'))
  if(isTRUE(vfb_ids) && nrow(res)>0) {
    res[['id']]=vfb_tovfbids(res[['id']])
  }
  res
}
