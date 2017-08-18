
#### Track Constructor      ========



new_track <- function (class, data, display = list(), background = NULL, height = NULL, label = NULL, ...) {
    t <- new(Class = class, Data = data, Display = display, ...)
    if (length(label) > 1L)
        label <- paste(label, collapse = " ")
    trackSpec(t, "background") <- background
    trackSpec(t, "height") <- height
    trackSpec(t, "label") <- label
    t
}


#' Track Constructors
#' 
#' @name track-constructors
#' 
#' @param range,pos GRanges or IRanges object. For pos, all the width should be one.
#' @param value,domain `value` is a numeric vector that is parallel to pos, which indicates
#'     height of features at each position for PinTrack, LineTrack and AreaTrack. `domain` is
#'     a length-two numeric vector which sets the lower and upper limit of `value` (i.e. the limit on y-axis).
#'
#' @param label Character, shown as label of the track on the left, could be NULL.
#' @param tooltip A data frame that is parallel to range or pos.
#' @param color Character vector that sets the color of the features.
#'     It can be parallel to the data (i.e. have the same length) thus sets colors
#'     of each individual feature.
#' @param background Length-one character vector that sets background of the track.
#' @param height Length-one numeric vector that sets height of the track.
#' 
#' @return Returns an object that inherits "TnTTrack" class.
#' @seealso
#'     You can find various examples at \url{https://marlin-na.github.io/TnT/examples/},
#'     also see \code{\link{composite-track}} on how to create a composite track.
#'      
#' @export
#' @examples
#' BlockTrack(range = GRanges("chr1", IRanges(199, 4000)),
#'            color = "green", background = "red", height = 100)
BlockTrack <- function (range, label = deparse(substitute(range)),
                        tooltip = mcols(range), color = "blue", background = NULL,
                        height = 30) {
    
    data <- RangeTrackData(range = range, tooltip = tooltip, color = color)
    new_track("BlockTrack",
              b = background, h = height, l = label, da = data, di = list())
}

#' @rdname track-constructors
#' @export
VlineTrack <- function (pos, label = deparse(substitute(pos)), tooltip = mcols(pos),
                        color = "green", background = NULL, height = 40) {
    ## TODO: remove tooltip?
    data <- PosTrackData(pos = pos, color = color, tooltip = tooltip)
    new_track("VlineTrack", b = background, h = height, l = label, da = data, di = list())
}

#' @rdname track-constructors
#' @export
PinTrack <- function (pos, value = mcols(pos)$value, domain = c(0, max(value)),
                      label = deparse(substitute(pos)),
                      tooltip = mcols(pos), color = "red", background = NULL,
                      height = 40) {
    
    if (is.null(value))
        stop("Value (i.e. height) at each position not specified.")
    stopifnot(length(domain) == 2)
    
    data <- PosValTrackData(pos = pos, val = value, tooltip = tooltip, color = color)
    new_track("PinTrack",
              b = background, h = height, l = label, da = data, di = list(), Domain = domain)
}

#' @rdname track-constructors
#' @export
LineTrack <- function (pos, value = mcols(pos)$value, domain = c(0, max(value)),
                       label = deparse(substitute(pos)), color = "yellow", background = NULL,
                       height = 70) {
    if (is.null(value))
        stop("Value (i.e. height) at each position not specified.")
    stopifnot(length(domain) == 2)
    
    ## Do not need tooltip
    data <- PosValTrackData(pos = pos, val = value, tooltip = NULL, color = color)
    new_track("LineTrack",
              b = background, h = height, l = label, da = data, di = list(), Domain = domain)
}

#' @rdname track-constructors
#' @export
AreaTrack <- function (pos, value = mcols(pos)$value, domain = c(0, max(value)),
                       label = deparse(substitute(pos)), color = "pink", background = NULL,
                       height = 70) {
    arglist <- as.list(environment())
    linetrack <- do.call(LineTrack, arglist)
    ans <- as(as(linetrack, "DomainValTrack"), "AreaTrack")
    validObject(ans)
    ans
}

#' @rdname track-constructors
#' @param txdb,seqlevel The TxDb and seqlevel to extract gene or transcript from.
#' @export
GeneTrackFromTxDb <- function (txdb, seqlevel = seqlevels(txdb),
                       label = deparse(substitute(txdb)), # TODO: tooltip?
                       color = "black", background = NULL, height = 100) {
    
    data <- GeneTrackDataFromTxDb(txdb = txdb, seqlevel = seqlevel, color = color)
    new_track("GeneTrack",
              b = background, h = height, l = label, da = data, di = list())
}

#' @rdname track-constructors
#' @param names Character vector with the same length of data, which is used to generate display labels
#'     shown together with features when zooming in.
#' @export
FeatureTrack <- function (range, label = deparse(substitute(range)),
                          tooltip = mcols(range),
                          names = base::names(range),
                          color = "black", background = NULL, height = 200) {
    force(tooltip)
    force(names)
    data <- GeneTrackData(range, labels = names,
                          ids = seq_along(range), tooltip = tooltip, color = color)
    new_track("GeneTrack",
              b = background, h = height, l = label, da = data, di = list())
}
##EXAMPLE
if (FALSE) {
    library(GenomicFeatures)
    txdb <- TxDb.Hsapiens.UCSC.hg19.knownGene::TxDb.Hsapiens.UCSC.hg19.knownGene
    gr <- genes(txdb)
    ft <- FeatureTrack(gr)
    seqlevels(ft, pru = "coarse") <- "chr12"
    compileTrack(ft)
    TnTBoard(ft, GRanges("chr12", IRanges(1,10000)))
    
}

#' @rdname track-constructors
#' @param grl For `GroupFeatureTrack` function, a GRangesList object that represents grouped ranges
#'     as data source. It is assumed that ranges in each group are on the same strand and do not overlap.
#' @export
GroupFeatureTrack <- function (grl, label = deparse(substitute(grl)),
                               tooltip = mcols(grl),
                               names = base::names(grl),
                               color = "black", background = NULL, height = 200) {
    force(tooltip)
    force(names)
    
    # # Check wheter strands within each group are consistent
    # strands <- as(unique(strand(grl)), "CharacterList")
    # strands[lengths(strands) == 0L] <- "" # may be empty
    # if (any(lengths(strands) != 1L))
    #     stop("Strands are not consistent within each group")
    # strands <- as.character(strands)
    # 
    # labels <- strandlabel(names, strands)
    
    data <- TxTrackDataFromGRangesList(grl, tooltip = tooltip, color = color,
                                       labels = names)
    new_track("TxTrack",
              b = background, h = height, l = label, da = data, di = list())
}
# EXAMPLE
if (FALSE) {
    library(GenomicFeatures)
    txdb <- TxDb.Hsapiens.UCSC.hg19.knownGene::TxDb.Hsapiens.UCSC.hg19.knownGene
    exby <- exonsBy(txdb)
    names(exby) <- paste("tx", names(exby))
    gft <- GroupFeatureTrack(exby)
    TnTBoard(gft, viewrange = GRanges("chrX", IRanges(1000, 100000)))
}

#' @rdname track-constructors
#' @export
TxTrackFromTxDb <- function (txdb, seqlevel = seqlevels(txdb),
                     label = deparse(substitute(txdb)),
                     color = "red", background = NULL, height = 300) {
    
    data <- TxTrackDataFromTxDb(txdb, seqlevel = seqlevel, color = color)
    
    new_track("TxTrack",
              b = background, h = height, l = label, da = data, di = list())
}

#' @rdname track-constructors
#' @param gr For `TxTrackFromGRanges` function, a GRanges object that represents exons and cds as data
#'     source, and will be rendered as transcripts. Two meta-columns ("type", "tx_id") are required,
#'     "type" can be "exon" or "cds" by which ranges of "cds" will be filled with color, "tx_id" indicates
#'     the grouping.
#' @export
TxTrackFromGRanges <- function (gr, label = deparse(substitute(gr)),
                                color = "red", background = NULL, height = 300) {
    data <- TxTrackDataFromGRanges(gr, color = color)
    new_track("TxTrack",
              b = background, h = height, l = label, da = data, di = list())
}
    
