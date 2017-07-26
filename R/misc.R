

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
#' @importFrom gplots col2hex
NULL



#' @export
mapcol <- function (value, palette.fun = grDevices::rainbow, ...) {
    uniqueV <- unique(value)
    mappedC <- palette.fun(length(uniqueV), ...)
    mappedC[match(value, uniqueV)]
}


#' @export
ul <- function(x)
    unlist(x, recursive = FALSE, use.names = FALSE)

#' @export
`ul<-` <- function (x, value)
    relist(value, x)


#' @export
splitdf <- function (df, f) {
    # We need to speed up this function
    if (requireNamespace("data.table", quietly = TRUE)) {
        # relative faster
        df[["___FACTOR___"]] <- f
        ldf <- data.table:::split.data.table(
            data.table::as.data.table(df),
            by = "___FACTOR___", keep.by = FALSE, flatten = FALSE
        )
    }
    else {
        warning("Install data.table could speed up the spliting process")
        ldf <- split(df, f)
    }
    ldf
}


#' @export
strandlabel <- function (labels, strands) {
    if (length(labels))
        ifelse(strands == "+", paste(labels, ">"),
            ifelse(strands == "-", paste("<", labels), labels))
    else
        ifelse(strands == "+", ">",
            ifelse(strands == "-", "<", ""))
}

#' @export
.removeAsIs <- function (df) {
    # The nested data frame converted from GRanges/DataFrame will have a
    # "AsIs" class, which will cause the data frame can not be shown and
    # can not be converted to JSON correctly.
    for (i in seq_along(df)) {
        element <- df[[i]]
        if (is.data.frame(element)) {
            class(element) <- class(element)[class(element) != "AsIs"]
            df[[i]] <- .removeAsIs(element)
        }
    }
    df
}


#' @export
.JSONFilter <- function (colname) {
    stopifnot(is.character(colname))
    escapeColname <- sapply(colname, function (s) as.character(toJSON(unbox(s))))
    condfilter <- paste(sprintf("[%s]", escapeColname), collapse = "")
    condfilter
}
# Example
if (interactive()) .JSONFilter(colname = c("data", "start"))
