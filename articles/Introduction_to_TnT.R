## ---- eval=FALSE---------------------------------------------------------
#  devtools::install_github("marlin-na/TnT")

## ---- eval=FALSE---------------------------------------------------------
#  source("https://bioconductor.org/biocLite.R")
#  biocLite(c("Biobase", "GenomeInfoDb", "IRanges", "GenomicRanges", "S4Vectors", "biovizBase"))

## ------------------------------------------------------------------------
suppressPackageStartupMessages(library(TnT))

## ------------------------------------------------------------------------
gr <- GRanges("chr7",
    ranges = IRanges(
        start = c(26549019L, 26564119L, 26585667L, 26591772L, 26594192L, 26623835L,
                  26659284L, 26721294L, 26821518L, 26991322L),
        end =   c(26550183L, 26564500L, 26586158L, 26593309L, 26594570L, 26624150L,
                  26660352L, 26721717L, 26823297L, 26991841L)),
    ID = 1:10,
    Name = paste("My Range", 1:10)
)
btrack <- BlockTrack(gr)
btrack

## ------------------------------------------------------------------------
args(BlockTrack)

## ------------------------------------------------------------------------
TnTGenome(btrack)

## ---- echo=FALSE---------------------------------------------------------
df <- data.frame(stringsAsFactors = FALSE,
    Constructor = c("BlockTrack", "VlineTrack", "PinTrack", "LineTrack", "AreaTrack",
                    "GeneTrackFromTxDb", "FeatureTrack", "GroupFeatureTrack",
                    "TxTrackFromTxDb", "TxTrackFromGRanges", "merge")
)
map.source <- c(
    BlockTrack   = "GRanges",
    FeatureTrack = "GRanges",
    VlineTrack   = "Width-one GRanges",
    PinTrack     = "Width-one GRanges paired with values",
    LineTrack    = "Width-one GRanges paired with values",
    AreaTrack    = "Width-one GRanges paired with values",
    GeneTrackFromTxDb  = "TxDb",
    TxTrackFromTxDb    = "TxDb",
    TxTrackFromGRanges = "GRanges paired with 'type' and 'tx_id'",
    GroupFeatureTrack  = "GRangesList",
    merge = "Two or more tracks"
)
map.feature <- c(
    BlockTrack   = "block",
    VlineTrack   = "vline",
    PinTrack     = "pin",
    LineTrack    = "line",
    AreaTrack    = "area",
    GeneTrackFromTxDb  = "gene",
    FeatureTrack       = "gene",
    GroupFeatureTrack  = "tx",
    TxTrackFromTxDb    = "tx",
    TxTrackFromGRanges = "tx",
    merge = "composite"
)
map.link <- c(
    BlockTrack   = "tracktype-BlockTrack.html",
    VlineTrack   = "tracktype-VlineTrack.html",
    PinTrack     = "tracktype-PinTrack.html",
    LineTrack    = "tracktype-LineTrack-AreaTrack.html",
    AreaTrack    = "tracktype-LineTrack-AreaTrack.html",
    GeneTrackFromTxDb  = "tracktype-GeneTrack.html",
    FeatureTrack       = "tracktype-GeneTrack.html",
    GroupFeatureTrack  = "tracktype-TxTrack.html",
    TxTrackFromTxDb    = "tracktype-TxTrack.html",
    TxTrackFromGRanges = "tracktype-TxTrack.html",
    merge = "track-CompositeTrack.html"
)
genlink <- function (base) {
    sprintf("[link](%s)", paste0("https://marlin-na/TnT/examples/", base))
}
df$Source <- map.source[df$Constructor]
df$`Feature type` <- map.feature[df$Constructor]
df$`Example` <- genlink(map.link[df$Constructor])
knitr::kable(df)

## ------------------------------------------------------------------------
trackSpec(btrack, "background")
btrack2 <- btrack
trackSpec(btrack2, "background") <- "blanchedalmond"
trackSpec(btrack2, "label")      <- "My Ranges"
trackSpec(btrack2, "height")     <- 50

## ------------------------------------------------------------------------
btrack2$color                     # Equivalent to `trackData(btrack2)$color`
btrack2$color <- "darkseagreen4"  # Equivalent to `trackData(btrack2)$color <- "darkseagreen4"`

## ------------------------------------------------------------------------
trackData(btrack2) <- shift(trackData(btrack2), 10000)

## ------------------------------------------------------------------------
TnTBoard(list(btrack, btrack2))

## ------------------------------------------------------------------------
tooltip(btrack2) <- cbind(tooltip(btrack2), as.data.frame(trackData(btrack2)))
TnTGenome(btrack2, view.range = trackData(btrack2)[4] * .05)

## ------------------------------------------------------------------------
set.seed(6)
pintrack <- PinTrack(GRanges("chr7", IRanges(start = sample(26300000:27000000, 4), width = 1)),
                     value = c(1,3,2,4), color = c("blue", "yellow", "green", "red"))
TnTGenome(
    list(pintrack, btrack2),
    view.range = GRanges("chr7", IRanges(26550000, 26600000)),
    coord.range = IRanges(26350000, 27050000),
    zoom.allow = IRanges(50000, 200000)
)

