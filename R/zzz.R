.onLoad <- function(libname, pkgname) {
  op.vfbr=list(
    vfbr.server="http://vfbsandbox.inf.ed.ac.uk"
  )
  op<-options()
  toset <- !(names(op.vfbr) %in% names(op))
  if(any(toset)) options(op.vfbr[toset])

  invisible()
}
