# vfbr
[![Travis-CI Build Status](https://travis-ci.org/jefferis/vfbr.svg?branch=master)](https://travis-ci.org/jefferis/vfbr)
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

# generic SOLR queries
example(vfb_solr_query)
# queries to find terms matching a synonym
example(vfb_synonym_query)

# package help
?vfbr
```

[1] Note: Windows users need [Rtools](http://www.murdoch-sutherland.com/Rtools/) and [devtools](http://CRAN.R-project.org/package=devtools) to install via devtools.
