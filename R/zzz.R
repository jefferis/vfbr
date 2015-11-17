.onLoad <- function(libname, pkgname) {
  op.vfbr=list(
    vfbr.server="http://vfbdev.inf.ed.ac.uk",
    vfbr.server.gepetto="http://129.215.164.244:8084"
  )
  op<-options()
  toset <- !(names(op.vfbr) %in% names(op))
  if(any(toset)) options(op.vfbr[toset])

  invisible()
}
