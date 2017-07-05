

#' @importFrom magrittr %>%
#' @export
magrittr::`%>%`

#' @import methods
NULL



#' @import GenomicFeatures
#' @import GenomicRanges
#' @import GenomeInfoDb
#' @import IRanges
#' @import S4Vectors
#' @import Biobase
#' @import jsonlite
NULL




#' @export
ul <- function(x)
    unlist(x, recursive = FALSE, use.names = FALSE)

#' @export
`ul<-` <- function (x, value)
    relist(value, x)

