.onLoad <- function(libname, pkgname) {
  op.vfbr=list(
    vfbr.server="http://vfbdev.inf.ed.ac.uk",
    vfbr.server.gepetto="http://129.215.164.244:8084",
    vfbr.stack.gmr_url='http://flybrain.mrc-lmb.cam.ac.uk/vfb/jfrc/fl/reformatted-quant/',
    vfbr.stack.downloads=file.path(
      rappdirs::user_data_dir('rpkg-vfbr', appauthor=NULL), 'stacks')
  )
  op<-options()
  toset <- !(names(op.vfbr) %in% names(op))
  if(any(toset)) options(op.vfbr[toset])

  invisible()
}
