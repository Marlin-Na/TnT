
#### Track Constructor      ========



#' @export
new_track <- function (class, data, display = list(), background = NULL, height = NULL, label = NULL, ...) {
    t <- new(Class = class, Data = data, Display = display, ...)
    trackSpec(t, "background") <- background
    trackSpec(t, "height") <- height
    trackSpec(t, "label") <- label
    t
}

#' @export
BlockTrack <- function (range, label = deparse(substitute(range)),
                        tooltip = mcols(range), color = "blue", background = NULL,
                        height = 30) {
    
    data <- RangeTrackData(range = range, tooltip = tooltip, color = color)
    new_track("BlockTrack",
              b = background, h = height, l = label, da = data, di = list())
}

#' @export
VlineTrack <- function (pos, label = deparse(substitute(pos)), tooltip = mcols(pos),
                        color = "green", background = NULL, height = 40) {
    ## TODO: remove tooltip?
    data <- PosTrackData(pos = pos, color = color, tooltip = tooltip)
    new_track("VlineTrack", b = background, h = height, l = label, da = data, di = list())
}

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

#' @export
LineTrack <- function (pos, value = mcols(pos)$value, domain = c(0, max(value)),
                       label = deparse(substitute(pos)),
                       tooltip = mcols(pos), color = "yellow", background = NULL,
                       height = 70) {
    if (is.null(value))
        stop("Value (i.e. height) at each position not specified.")
    stopifnot(length(domain) == 2)
    
    ## TODO: remove tooltip?
    data <- PosValTrackData(pos = pos, val = value, tooltip = tooltip, color = color)
    new_track("LineTrack",
              b = background, h = height, l = label, da = data, di = list(), Domain = domain)
}

#' @export
AreaTrack <- function (pos, value = mcols(pos)$value, domain = c(0, max(value)),
                       label = deparse(substitute(pos)),
                       tooltip = mcols(pos), color = "pink", background = NULL,
                       height = 70) {
    arglist <- as.list(environment())
    linetrack <- do.call(LineTrack, arglist)
    ans <- as(as(linetrack, "DomainValTrack"), "AreaTrack")
    validObject(ans)
    ans
}

#' @export
GeneTrackFromTxDb <- function (txdb, seqlevel = seqlevels(txdb),
                       label = deparse(substitute(txdb)), # TODO: tooltip?
                       color = "black", background = NULL, height = 100) {
    
    data <- GeneTrackDataFromTxDb(txdb = txdb, seqlevel = seqlevel, color = color)
    new_track("GeneTrack",
              b = background, h = height, l = label, da = data, di = list())
}

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

#' @export
TxTrackFromTxDb <- function (txdb, seqlevel = seqlevels(txdb),
                     label = deparse(substitute(txdb)),
                     color = "red", background = NULL, height = 300) {
    
    data <- TxTrackDataFromTxDb(txdb, seqlevel = seqlevel, color = color)
    
    new_track("TxTrack",
              b = background, h = height, l = label, da = data, di = list())
}

#' @export
TxTrackFromGRanges <- function (gr, label = deparse(substitute(gr)),
                                color = "red", background = NULL, height = 300) {
    data <- TxTrackDataFromGRanges(gr, color = color)
    new_track("TxTrack",
              b = background, h = height, l = label, da = data, di = list())
}
    
