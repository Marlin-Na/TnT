

#' @importFrom magrittr %>%
#' @export
magrittr::`%>%`

#' @importFrom biovizBase crunch
#' @export
biovizBase::crunch


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


ul <- function(x)
    unlist(x, recursive = FALSE, use.names = FALSE)

`ul<-` <- function (x, value)
    relist(value, x)


expose_all <- function (package) {
    attachname <- paste0(package, "_all")
    while(attachname %in% search())
        detach(attachname, character.only = TRUE)
    
    pkgns <- loadNamespace(package)
    attach(pkgns, name = attachname)
    invisible(pkgns)
}





splitdf <- function (df, f) {
    # We need to speed up this function
    if (requireNamespace("data.table", quietly = TRUE)) {
        # relative faster
        df[["___FACTOR___"]] <- f
        ldf <- data.table:::split.data.table(
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


#' @export
strandlabel <- function (labels, strands) {
    if (length(labels))
        ifelse(strands == "+", paste(labels, ">"),
            ifelse(strands == "-", paste("<", labels), labels))
    else
        ifelse(strands == "+", ">",
            ifelse(strands == "-", "<", ""))
}







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


.JSONFilter <- function (colname) {
    stopifnot(is.character(colname))
    escapeColname <- sapply(colname, function (s) as.character(toJSON(unbox(s))))
    condfilter <- paste(sprintf("[%s]", escapeColname), collapse = "")
    condfilter
}
# Example
if (interactive()) .JSONFilter(colname = c("data", "start"))
