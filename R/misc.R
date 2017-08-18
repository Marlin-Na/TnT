

#' @importFrom magrittr %>%
#' @export
magrittr::`%>%`

#' @importFrom biovizBase crunch
#' @export
biovizBase::crunch


#' @import methods
#' @import GenomicFeatures
#' @import GenomicRanges
#' @import GenomeInfoDb
#' @import IRanges
#' @import S4Vectors
#' @import Biobase
#' @import jsonlite
#' @importFrom gplots col2hex
#' @importFrom stats na.fail setNames
#' @importFrom utils capture.output
NULL



#' Scale Qualitative Values to Color
#' 
#' A simple util function that scales a factor to color based on the palette function.
#'
#' @param value A factor or character vector that may have n unique values.
#' @param palette.fun The palette function to generate colors.
#'     For example, \code{\link[grDevices]{terrain.colors}}.
#' @param ... Extra arguments passed to the palette function.
#'
#' @return
#'     A character vector as colors, with the same length of \code{value}. Same values
#'     in \code{value} will have the same color.
#' @export
#' @examples
#' mapcol(iris$Species)
mapcol <- function (value, palette.fun = grDevices::rainbow, ...) {
    uniqueV <- unique(value)
    mappedC <- palette.fun(length(uniqueV), ...)
    mappedC[match(value, uniqueV)]
}

#' Display Labels with Strand
#' 
#' A simple util function that used internally to generate display labels of GeneTrack and
#' TxTrack.
#'
#' @param labels Character vector, names of each feature.
#' @param strands Factor or character vector with the same length of \code{labels}, can
#'     be "+", "-" or "*".
#' @return
#'     A character vector that combines the labels with strand information.
#' @export
#' @examples
#' strandlabel(c("gene1", "gene2", "gene3"), c("+", "-", "*"))
strandlabel <- function (labels, strands) {
    if (length(labels))
        ifelse(strands == "+", paste(labels, ">"),
            ifelse(strands == "-", paste("<", labels), labels))
    else
        ifelse(strands == "+", ">",
            ifelse(strands == "-", "<", ""))
}






splitdf <- function (df, f) {
    # We need to speed up this function
    if (requireNamespace("data.table", quietly = TRUE)) {
        # relative faster
        df[["___FACTOR___"]] <- f
        ldf <- split(   # data.table:::split.data.table(
            data.table::as.data.table(df),
            drop = TRUE, by = "___FACTOR___", keep.by = FALSE, flatten = FALSE
        )
    }
    else {
        warning("Install data.table could speed up the spliting process")
        ldf <- split(df, f, drop = TRUE)
    }
    ldf
}






#### The following functions are no longer used. -------------------------------

# ul <- function(x)
#     unlist(x, recursive = FALSE, use.names = FALSE)
# 
# `ul<-` <- function (x, value)
#     relist(value, x)
# 
# expose_all <- function (package = "TnT") {
#     attachname <- paste0(package, "_all")
#     while(attachname %in% search())
#         detach(attachname, character.only = TRUE)
#     
#     pkgns <- loadNamespace(package)
#     attach(pkgns, name = attachname)
#     invisible(pkgns)
# }
# 
# .removeAsIs <- function (df) {
#     # The nested data frame converted from GRanges/DataFrame will have a
#     # "AsIs" class, which will cause the data frame can not be shown and
#     # can not be converted to JSON correctly.
#     for (i in seq_along(df)) {
#         element <- df[[i]]
#         if (is.data.frame(element)) {
#             class(element) <- class(element)[class(element) != "AsIs"]
#             df[[i]] <- .removeAsIs(element)
#         }
#     }
#     df
# }
# 
# .JSONFilter <- function (colname) {
#     stopifnot(is.character(colname))
#     escapeColname <- sapply(colname, function (s) as.character(toJSON(unbox(s))))
#     condfilter <- paste(sprintf("[%s]", escapeColname), collapse = "")
#     condfilter
# }
# if (interactive()) .JSONFilter(colname = c("data", "start"))
