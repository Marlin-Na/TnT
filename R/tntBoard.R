

#' @include tntTracks.R

###   TnT Board     ############################################################

####  Class Def for TnT Board   ========
setClass("TnTBoard",
    slots = c(
        ViewRange = "GRanges",
        CoordRange = "IRanges",
        ZoomAllow = "IRanges",
        # Drag should always be allowed.
        #AllowDrag = "logical",
        TrackList = "list"
    )
)

#### TnT Board Constructor      ========



#' @export
TnTBoard <- function (tracklist, view.range = GRanges(),
                      coord.range = IRanges(), zoom.allow = IRanges()) {
    
    if (is(tracklist, "TnTTrack"))
        tracklist <- list(tracklist)
    else
        stopifnot(all(sapply(tracklist, inherits, what = "TnTTrack")))
    
    b <- new("TnTBoard", ViewRange = view.range, CoordRange = coord.range,
             ZoomAllow = zoom.allow, TrackList = tracklist)
    b
}




#### TnT Board Compilation      ========

#' @export
compileBoard <- function (tntboard) {
    ## Modification to tntboard
    b <- .selectTrackSeq(tntboard)
    b <- .determineCoordRange(b)
    b <- .consolidateBackground(b)
    
    spec <- .compileBoardSpec(b)
    tklst <- .compileTrackList(b)
    tntdef <- c(spec, tklst)
    tntdef
}

.consolidateBackground <- function (tntboard) {
    # By the time of construction of each tnt track, the background color can
    # be either set to "NULL" or a scalar character.
    #
    # Before compilation of tntboard, this function examines these settings in
    # each track, replace the NULLs with a more suitable value.
    tracklist <- tntboard@TrackList
    li.colors <- lapply(tracklist, function (t) t@Background@.Data)
    colors <- unique(unlist(li.colors))
    
    if (length(colors) == 0L || length(colors) >= 2L)
        default <- Biobase::mkScalar("white")
    else
        default <- Biobase::mkScalar(colors)
    
    tracklist <- lapply(tracklist, replace = default,
        function (track, replace) {
            if (is.null(track@Background))
                track@Background <- replace
            track
        }
    )
    tntboard@TrackList <- tracklist
    tntboard
}

#' @export
.compileBoardSpec <- function (tntboard) {
    .checkBoardSpec <- function (tntboard) {
        b <- tntboard
        stopifnot(
            # These three slots should be prepared before converted to JS
            length(b@ViewRange) == 1,
            length(b@CoordRange) == 1,
            length(b@ZoomAllow) == 1
        )
        b
    }
    b <- .checkBoardSpec(tntboard)
    jc.board.spec <- jc(
        tnt.board = ma(),
        from     = start(b@ViewRange),
        to       = end(b@ViewRange),
        min      = start(b@CoordRange),
        max      = end(b@CoordRange),
        zoom_out = end(b@ZoomAllow),
        zoom_in  = start(b@ZoomAllow)
    )
    jc.board.spec
}

#' @export
.selectTrackSeq <- function (tntboard) {
    tracklist0 <- tntboard@TrackList
    viewrange0 <- tntboard@ViewRange
    if (length(viewrange0)) {
        stopifnot(length(viewrange0) == 1)
        seqlv <- seqlevelsInUse(viewrange0)
    }
    else {
        # TODO: Find a proper view range
        stop()
    }
    tracklist <- lapply(tracklist0, keepSeqlevels,
                        value = seqlv, pruning.mode = "coarse")
    tntboard@TrackList <- tracklist
    tntboard
}

#' @export
.determineCoordRange <- function (tntboard) {
    coordrange0 <- tntboard@CoordRange
    
    seqlens <- vapply(tntboard@TrackList, seqlengths, integer(1))
    seqlen <- unique(seqlens[!is.na(seqlens)])
    
    if (length(seqlen) >= 2L) {
        stop()
    }
    else if (length(seqlen) == 1L) {
        coordrange <- IRanges(start = 0L, end = seqlen)
    }
    else {
        li.rgs <- lapply(tntboard@TrackList,
            function (track) range(trackData(track))
        )
        rg <- do.call('c', li.rgs)
        rg <- range(rg)
        stopifnot(length(rg) == 1)
        coordrange <- ranges(rg)
    }
    tntboard@CoordRange <- coordrange
    tntboard@ZoomAllow <- IRanges(start = 10, end = width(coordrange))
    tntboard
}

#' @export
.compileTrackList <- function (tntboard) {
    tracklist <- tntboard@TrackList
    li.jc <- lapply(tracklist, compileTrack)
    names(li.jc) <- replicate(length(li.jc), "add_track")
    jc <- asJC(li.jc)
    jc
}

## Printing     ====
#' @export
setMethod("show", signature = c("TnTBoard"),
    function (object) {
        # TODO: Have to provide renderTnT and TnTOutput
        widget <- trackWidget(object, elementId = NULL)
        print(widget)
    }
)
#' @export
knit_print.TnTBoard <- function (x, ..., options = NULL) {
    # Redirect method to htmlwidget
    x <- trackWidget(x, elementId = NULL)
    knitr::knit_print(x, ..., options = options)
}

## EXAMPLE      ====
if (FALSE) local({
    data("cpgIslands", package = "Gviz")
    txdb <- TxDb.Hsapiens.UCSC.hg19.knownGene::TxDb.Hsapiens.UCSC.hg19.knownGene
    viewrg <- range(cpgIslands)
    ct <- BlockTrack(cpgIslands, color= "blue", height = 30)
    gt <- GeneTrack(txdb, color = "grey")
    txt <- TxTrack(txdb, color = "red", height = 300)
    TnTBoard(tracklist = list(ct), viewrange = viewrg)
    TnTBoard(tracklist = list(txt), viewrange = viewrg)
    TnTBoard(tracklist = list(gt, ct), viewrange = viewrg)
})




###   TnT Genome    ############################################################

# setClass("TnTGenome", contains = "TnTBoard",
#     slots = c(
#         
#     )
# )
