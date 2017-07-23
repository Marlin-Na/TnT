
#### Track Constructor      ========



#' @export
new_track <- function (class, data, display = list(), background = NULL, height = NULL, label = NULL) {
    t <- new(Class = class, Data = data, Display = display)
    trackSpec(t, "background") <- background
    trackSpec(t, "height") <- height
    trackSpec(t, "label") <- label
    t
}

.parseCol <- function (col) {
    len <- length(col)
    # TODO: set colors for every range?
    if (len > 1L)
        stop("Length of color > 1")
    
    if (len) gplots::col2hex(col)
    else     col
}

#' @export
BlockTrack <- function (range, label = deparse(substitute(range)),
                        tooltip = mcols(range), color = NULL, background = NULL,
                        height = 30) {
    
    color <- .parseCol(color)
    
    data <- RangeTrackData(range = range, tooltip = tooltip)
    display <- list(
        tnt.board.track.feature.block = ma(),
        color = color,
        # The index slot is actually added on JS side
        index = js("function (d) {return d['.index.'];}")
    )
    new_track("BlockTrack",
              b = background, h = height, l = label, da = data, di = display)
}

#' @export
PinTrack <- function (pos, value = mcols(pos)$value, domain = c(0, max(value)),
                      label = deparse(substitute(pos)),
                      tooltip = mcols(pos), color = NULL, background = NULL,
                      height = 40) {
    color <- .parseCol(color)
    
    if (is.null(value))
        stop("Value (i.e. height) at each position not specified.")
    stopifnot(length(domain) == 2)
    
    data <- PosValTrackData(pos = pos, val = value, tooltip = tooltip)
    display <- list(
        tnt.board.track.feature.pin = ma(),
        domain = domain,
        color = color,
        index = js("function (d) {return d['.index.'];}")
    )
    new_track("PinTrack",
              b = background, h = height, l = label, da = data, di = display)
}

#' @export
GeneTrack <- function (txdb, seqlevel = seqlevels(txdb),
                       label = deparse(substitute(txdb)), # TODO: tooltip?
                       color = NULL, background = NULL, height = 100) {
    color <- .parseCol(color)
    
    data <- GeneTrackDataFromTxDb(txdb = txdb, seqlevel = seqlevel)
    display <- list(
        tnt.board.track.feature.genome.gene = ma(),
        # TODO: color for each range
        color = color
    )
    new_track("GeneTrack",
              b = background, h = height, l = label, da = data, di = display)
}

#' @export
FeatureTrack <- function (range, label = deparse(substitute(range)),
                          tooltip = mcols(range),
                          names = base::names(range),
                          color = NULL, background = NULL, height = 200) {
    color <- .parseCol(color)
    
    force(tooltip)
    force(names)
    data <- GeneTrackData(range, labels = names,
                          ids = seq_along(range), tooltip = tooltip)
    display <- list(
        tnt.board.track.feature.genome.gene = ma(),
        # TODO
        color = color
    )
    new_track("GeneTrack",
              b = background, h = height, l = label, da = data, di = display)
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
GroupFeatureTrack <- function (grangelist, label = deparse(substitute(grangelist)),
                               tooltip = mcols(grangelist),
                               names = base::names(grangelist),
                               color = NULL, background = NULL, height = 200) {
    color <- .parseCol(color)
    
    force(tooltip)
    force(names)
    
    data <- TxTrackDataFromGRangesList(grangelist, tooltip = tooltip,
                                       labels = names)
    display <- list(
        tnt.board.track.feature.genome.transcript = ma(),
        # TODO: this is not the correct approach
        color = if (is.null(color)) js('function (t) { return "red" }')
                else js(sprintf('function (t) { return "%s" }', color))
    )
    new_track("TxTrack",
              b = background, h = height, l = label, da = data, di = display)
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
TxTrack <- function (txdb, seqlevel = seqlevels(txdb),
                     label = deparse(substitute(txdb)),
                     color = NULL, background = NULL, height = 300) {
    color <- .parseCol(color)
    
    data <- TxTrackDataFromTxDb(txdb, seqlevel = seqlevel)
    display <- list(
        tnt.board.track.feature.genome.transcript = ma(),
        # TODO: this is not the correct approach
        color = if (is.null(color)) js('function (t) { return "red" }')
                else js(sprintf('function (t) { return "%s" }', color))
    )
    
    new_track("TxTrack",
              b = background, h = height, l = label, da = data, di = display)
}
