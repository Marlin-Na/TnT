

# #' @importFrom magrittr %>%
# #' @export
# magrittr::`%>%`

# #' @importFrom biovizBase crunch
# #' @export
# biovizBase::crunch


#' @import methods
#' @import GenomicRanges
#' @import GenomeInfoDb
#' @import IRanges
#' @import S4Vectors
#' @import Biobase
#' @import jsonlite
#' @importFrom stats na.fail setNames
#' @importFrom utils capture.output
#' @importFrom knitr knit_print
#' @import htmlwidgets
NULL



match.class <- function (class, choices) {
    # Match a class to a list of known classes
    #   (i.e. matching the first direct superclass in that list)
    #   It can be used to simulate a method dispatch.
    super.classes <- c(class, getAllSuperClasses(getClass(class)))
    matched <- choices[which.min(match(choices, super.classes))]   
    
    if (!length(matched))
        stop(sprintf("Unmatched class %s", sQuote(class)))
    if (! matched %in% choices) # Sanity check
        stop("<internal> Wrongly matched class")
    matched
}


#' Save a TnTBoard to an HTML file
#' 
#' A simple wrapper of \code{\link[htmlwidgets]{saveWidget}}, which saves a
#' TnTBoard/TnTGenome object to an HTML file (e.g. for sharing with others).
#'
#' @param tntdef A TnTBoard/TnTGenome object to save.
#' @param file,selfcontained,libdir,background,knitrOptions
#'     Passed to \code{\link[htmlwidgets]{saveWidget}}.
#'
#' @return Return NULL.
#' @export
#' @examples
#' data <- GRanges("chr2", IRanges(c(6,9,42), width = 1),
#'                 value = c(0.3, 0.5, 0.9))
#' track <- PinTrack(data, label = NULL, background = "green")
#' genome <- TnTGenome(list(track))
#' destfile <- tempfile(fileext = ".html")
#' destfile
#' saveTnT(genome, destfile)
#' \dontrun{
#' utils::browseURL(destfile)
#' }
saveTnT <- function (tntdef, file, selfcontained = TRUE, libdir = NULL,
                     background = "white", knitrOptions = list()) {
    stopifnot(is(tntdef, "TnTBoard"))
    tntdef <- trackWidget(tntdef)
    htmlwidgets::saveWidget(tntdef, file, selfcontained, libdir, background, knitrOptions)
}



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


col2hex <- function (colors) {
    mat <- grDevices::col2rgb(colors)
    grDevices::rgb(mat[1, ], mat[2, ], mat[3, ], maxColorValue = 255)
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



.consolidateSeqinfo <- function (li.tracks) {
    # The function accepts a list of tracks,
    # returns a list of tracks with the same Seqinfo
    stopifnot(is.list(li.tracks))
    
    if (!length(li.tracks))
        return(li.tracks)
    s <- .mergeSeqinfo(li.tracks)
    for (i in seq_along(li.tracks))
        if (!identical(seqinfo(li.tracks[[i]]), s))
            # The merged Seqinfo should be a superset of individual Seqinfo
            seqinfo(li.tracks[[i]], new2old = match(seqlevels(s), seqlevels(li.tracks[[i]])),
                    pruning.mode = "error") <- s
    li.tracks
}

.mergeSeqinfo <- function (li.seqinfo) {
    # It accepts a list of Seqinfo object or a list of tracks, returns the merged Seqinfo.
    # It checks whether the objects are identical before applying `merge`,
    # so that it is fast in some cases.
    if (!length(li.seqinfo))
        return(Seqinfo())
    for (i in seq_along(li.seqinfo))
        if (!is(li.seqinfo[[i]], "Seqinfo"))
            li.seqinfo[[i]] <- seqinfo(li.seqinfo[[i]])

    target <- li.seqinfo[[1]]
    for (i in seq_along(li.seqinfo)) {
        new <- li.seqinfo[[i]]
        if (identical(target, new))
            target <- new
        else
            target <- merge(target, new)
    }
    target
}




