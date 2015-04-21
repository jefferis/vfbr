# vfbr

## Installation
Currently there isn't a released version on [CRAN](http://cran.r-project.org/), 
but you  can use the **devtools** [1] package to install the development version:

```r
if (!require("devtools")) install.packages("devtools")
devtools::install_github("jefferis/vfbr")
```

## Use

```r
library(vfbr)
example(vfb_generic_query)
```

[1] Note: Windows users need [Rtools](http://www.murdoch-sutherland.com/Rtools/) and [devtools](http://CRAN.R-project.org/package=devtools) to install via devtools.
