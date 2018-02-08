.onLoad <- function(libname, pkgname) {
  op.vfbr=list(
    vfbr.server="http://virtualflybrain.org",
    vfbr.server.owl="http://owl.virtualflybrain.org",
    vfbr.server.neo4j="http://pdb.virtualflybrain.org",
    vfbr.server.r="http://r.virtualflybrain.org",
    vfbr.server.gepetto="http://vfbsandbox3.inf.ed.ac.uk:8180",
    vfbr.stack.gmr_url='http://flybrain.mrc-lmb.cam.ac.uk/vfb/jfrc/fl/reformatted-quant/',
    vfbr.stack.downloads=file.path(
      rappdirs::user_data_dir('rpkg-vfbr', appauthor=NULL), 'stacks')
  )
  op<-options()
  toset <- !(names(op.vfbr) %in% names(op))
  if(any(toset)) options(op.vfbr[toset])

  invisible()
}
