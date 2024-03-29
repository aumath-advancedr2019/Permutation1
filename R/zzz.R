.onLoad <- function(libname, pkgname){
  if ("ggplot2" %in% rownames(installed.packages())){
    cat('Load required: ggplot2','\n')
    library("ggplot2")
  } else {
    cat('Some functions may require ggplot2','\n')
  }
  if ("magrittr" %in% rownames(installed.packages())){
    cat('Load required: magrittr','\n')
    library("magrittr")
  } else {
    cat('Some functions may require magrittr','\n')
  }
}

.onDetach <- function(libname, pkgname){
  if ("ggplot2" %in% devtools::loaded_packages()[,1]){
    detach("package:ggplot2")
  }
  if ("magrittr" %in% devtools::loaded_packages()[,1]){
    detach("package:magrittr")
  }
}