
#### Track Constructor      ========

#' @export
.mkScalarOrNull <- function (x)
    if (is.null(x)) NULL else Biobase::mkScalar(x)

#' @export
BlockTrack <- function (range, label = deparse(substitute(range)),
                        tooltip = mcols(range), color = NULL, background = NULL,
                        height = 30) {
    label <- .mkScalarOrNull(label)
    background <- .mkScalarOrNull(background)
    height <- .mkScalarOrNull(height)
    data <- RangeTrackData(range = range, tooltip = tooltip)
    display <- list(
        tnt.board.track.feature.block = ma(),
        color = color,
        # The index slot is actually added on JS side
        index = js("function (d) {return d['.index.'];}")
    )
    new("BlockTrack", Label = label, Background = background, Height = height,
        Data = data, Display = display)
}

#' @export
PinTrack <- function (pos, value = mcols(pos)$value, domain = c(0, max(value)),
                      label = deparse(substitute(pos)),
                      tooltip = mcols(pos), color = NULL, background = NULL,
                      height = 40) {
    if (is.null(value))
        stop("Value (i.e. height) at each position not specified.")
    label <- .mkScalarOrNull(label)
    background <- .mkScalarOrNull(background)
    height <- .mkScalarOrNull(height)
    force(domain)
    stopifnot(length(domain) == 2)
    data <- PosValTrackData(pos = pos, val = value, tooltip = tooltip)
    display <- list(
        tnt.board.track.feature.pin = ma(),
        domain = domain,
        color = color,
        index = js("function (d) {return d['.index.'];}")
    )
    new("PinTrack", Label = label, Background = background, Height = height,
        Data = data, Display = display)
}

#' @export
GeneTrack <- function (txdb, seqlevel = seqlevels(txdb),
                       label = deparse(substitute(txdb)), # TODO: tooltip?
                       color = NULL, background = NULL, height = 100) {
    label <- .mkScalarOrNull(label)
    background <- .mkScalarOrNull(background)
    height <- .mkScalarOrNull(height)
    data <- GeneTrackDataFromTxDb(txdb = txdb, seqlevel = seqlevel)
    display <- list(
        tnt.board.track.feature.genome.gene = ma(),
        # TODO: color for each range
        color = color
    )
    new("GeneTrack", Label = label, Background = background, Height = height,
        Data = data, Display = display)
}

#' @export
FeatureTrack <- function (range, label = deparse(substitute(range)),
                          tooltip = mcols(range),
                          names = base::names(range),
                          color = NULL, background = NULL, height = 200) {
    force(tooltip)
    force(names)
    label <- .mkScalarOrNull(label)
    background <- .mkScalarOrNull(background)
    height <- .mkScalarOrNull(height)
    data <- GeneTrackData(range, labels = names,
                          ids = seq_along(range), tooltip = tooltip)
    display <- list(
        tnt.board.track.feature.genome.gene = ma(),
        # TODO
        color = color
    )
    new("GeneTrack", Label = label, Background = background, Height = height,
        Data = data, Display = display)
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
    force(tooltip)
    force(names)
    label <- .mkScalarOrNull(label)
    background <- .mkScalarOrNull(background)
    height <- .mkScalarOrNull(height)
    data <- TxTrackDataFromGRangesList(grangelist, tooltip = tooltip,
                                       labels = names)
    display <- list(
        tnt.board.track.feature.genome.transcript = ma(),
        # TODO: this is not the correct approach
        color = if (is.null(color)) js('function (t) { return "red" }')
                else js(sprintf('function (t) { return "%s" }', color))
    )
    new("TxTrack", Label = label, Background = background, Height = height,
        Data = data, Display = display)
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
    label <- .mkScalarOrNull(label)
    background <- .mkScalarOrNull(background)
    height <- .mkScalarOrNull(height)
    data <- TxTrackDataFromTxDb(txdb, seqlevel = seqlevel)
    display <- list(
        tnt.board.track.feature.genome.transcript = ma(),
        # TODO: this is not the correct approach
        color = if (is.null(color)) js('function (t) { return "red" }')
                else js(sprintf('function (t) { return "%s" }', color))
    )
    new("TxTrack", Label = label, Background = background, Height = height,
        Data = data, Display = display)
}