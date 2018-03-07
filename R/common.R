## collection of common used helper functions and default packages to be loaded



## define %ni% analogous to %in% (see ?match)
<<<<<<< HEAD
#' Title
#'
#' @param x
#' @param table
#'
#' @return
#' @export
#'
#' @examples
'%ni%' <- function(x, table) {
  match(x, table, nomatch = 0) == 0
}


## calc ppm according to Pelz2005
#' Title
#'
#' @param vp
#' @param dilution
#'
#' @return
#' @export
#'
#' @examples
=======
'%ni%' <- function(x, table) match(x, table, nomatch = 0) == 0


## calc ppm according to Pelz2005
>>>>>>> f5ecebce5c8cd90c7769029890e1099edb3b7a6e
ppmCalc <- function(vp, dilution) {
  loga <- 1.359 * log10(vp) + 2.0404
  a <- 10 ^ loga

  res <- a * dilution * 100

  return(res)
}


## transform all levels of a data.frame to upper case
<<<<<<< HEAD
#' Title
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples
=======
>>>>>>> f5ecebce5c8cd90c7769029890e1099edb3b7a6e
alltoupper <- function(x) {
  for (i in 1:length(x))
    levels(x[,i]) <- toupper(levels(x[,i]))
  return(x)
}


## lifetime kurtosis
<<<<<<< HEAD
#' Title
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples
=======
>>>>>>> f5ecebce5c8cd90c7769029890e1099edb3b7a6e
ltk <- function(x) {
  x <- na.omit(x)
  (sum(((x - mean(
    x
  )) / as.numeric(sd(
    x
  ))) ^ 4)  / length(x)) - 3
}


## function to find extremum
<<<<<<< HEAD
#' Title
#'
#' @param x
#' @param na.rm
#'
#' @return
#' @export
#'
#' @examples
=======
>>>>>>> f5ecebce5c8cd90c7769029890e1099edb3b7a6e
extremum <- function(x, na.rm = F) {
  min <- min(x, na.rm = na.rm)
  #	minpos <- which.min(x)
  min.abs <- abs(min)
  max <- max(x, na.rm = na.rm)
  #	maxpos <- which.max(x)
  ifelse (min.abs > max, return(min), return(max))
}

<<<<<<< HEAD
#' Title
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples
=======
>>>>>>> f5ecebce5c8cd90c7769029890e1099edb3b7a6e
which.extremum <- function(x)
{
  min <- min(x)
  minpos <- which.min(x)
  min.abs <- abs(min)
  max <- max(x)
  maxpos <- which.max(x)
  ifelse (min.abs > max, return(minpos), return(maxpos))
}


# ## packages
# require(ggplot2)
# require(reshape2)
# require(gridExtra)
# require(scales)
# require(dplyr)


## g_legend
<<<<<<< HEAD
#' Title
#'
#' @param a.gplot
#'
#' @return
#' @export
#'
#' @examples
=======
>>>>>>> f5ecebce5c8cd90c7769029890e1099edb3b7a6e
g_legend <- function(a.gplot) {
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <-
    which(sapply(tmp$grobs, function(x)
      x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}
