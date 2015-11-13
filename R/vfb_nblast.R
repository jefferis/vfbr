#' Query vfb opencpu server to obtain pre-computed nblast scores
#'
#' @param query A single flycircuit identifier
#' @param n The number of hits to return
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
#' vfb_nblast('fru-M-200266', n=10)
vfb_nblast<-function(query, n=50, ...){
  server="http://vfbdev.inf.ed.ac.uk"
  topn_path="/ocpu/library/flynblastscores/R/flycircuit_topn"
  posturl=paste0(server,topn_path)
  r=POST(posturl, body=list(query=query, n=n, ...))
  stop_for_status(r)
  response_path=scan(text=content(r), n = 1, what="", sep = "\n", quiet = TRUE)
  geturl=paste0(server,response_path,"/csv")
  read.csv(geturl, stringsAsFactors = FALSE)
}
