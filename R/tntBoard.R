

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

## EXAMPLE
if (FALSE) {
    library(GenomicFeatures)
    gr <- GRanges("chr12", IRanges(1:4, width = 2))
    t <- BlockTrack(gr)
    b <- TnTBoard(t)
    tracklist(b)
}


#### Accessors                  ========

#' @export
tracklist <- function (tntboard) {
    tntboard@TrackList
}

#' @export
`tracklist<-` <- function (tntboard, value) {
    tntboard@TrackList <- value
    tntboard
}




#### TnT Board Compilation      ========

#' @export
compileBoard <- function (tntboard) {
    b <- wakeupBoard(tntboard)
    
    spec <- .compileBoardSpec(b)
    tklst <- .compileTrackList(b)
    tntdef <- c(spec, tklst)
    tntdef
}


#' @export
wakeupBoard <- function (tntboard) {
    if (inherits(tntboard, "TnTGenome"))
        stop() # TODO
    
    tntboard <- .selectView(tntboard)
    tntboard <- .filterSeq(tntboard)
    tntboard <- .selectCoord(tntboard)
    tntboard <- .selectZoom(tntboard)
    
    tntboard
}


#' @export
.filterSeq <- function (tntboard, use.seq = seqlevelsInUse(tntboard@ViewRange)) {
    stopifnot(length(use.seq) == 1)
    
    li.t <- tracklist(tntboard)
    li.t <- lapply(li.t, keepSeqlevels, value = use.seq, pruning.mode = "coarse")
    
    tntboard@TrackList <- li.t
    tntboard
}

#' @export
.selectCoord <- function (tntboard) {
    if (length(tntboard@CoordRange) == 1)
        return(tntboard)
    if (length(tntboard@CoordRange) > 1)
        stop()
    
    viewrg <- tntboard@ViewRange
    stopifnot(length(viewrg) == 1)
    
    # Use seqlevel from the view range
    seqlv <- seqlevelsInUse(viewrg)
    
    agg.seqinfo <- {
        li.t <- tracklist(tntboard)
        li.seqinfo <- lapply(li.t, seqinfo)
        do.call(merge, li.seqinfo)
    }
    
    seqlen <- {
        if (!seqlv %in% seqlevels(agg.seqinfo))
            stop("Seqlevel of the view range can not be found in the track list")
        seqlengths(agg.seqinfo)[as.character(seqlv)]
    }
    
    if (!is.na(seqlen)) {
        tntboard@CoordRange <- IRanges(0, seqlen + 1)
        return(tntboard)
    }
    
    # Then seqlength is not known
    coord <- {
        li.t <- tracklist(tntboard)
        if (all(sapply(li.t, inherits, what = "RangeTrack"))) {
            # TODO: include the cases that not all of the tracks are RangeTrack
            ## Aggregate from track data
            li.rgs <- lapply(li.t,
                function (track) range(trackData(track))
            )
            rg <- do.call('c', li.rgs)
            rg <- range(rg)
            rg <- keepSeqlevels(rg, seqlv, pruning.mode = "coarse")
            stopifnot(length(rg) == 1) # TODO: However, there will be cases that all the tracks are empty
            coord <- ranges(rg) * .7
        }
        # TODO: other cases?
        else {
            ## Use the view range
            coord <- ranges(tntboard@ViewRange) * .7
        }
        coord
    }
    
    msg <- sprintf("Seqlenth is unknown, automatically set coordinate range to %s-%s",
                   start(coord), end(coord))
    message(msg)
    
    tntboard@CoordRange <- coord
    tntboard
}

#' @export
.selectView <- function (tntboard) {
    viewrange0 <- tntboard@ViewRange
    if (length(viewrange0) == 1)
        return(tntboard) # Already specified
    if (length(viewrange0) > 1)
        stop()
    
    # ViewRange is not set
    tracklist0 <- tracklist(tntboard)
    
    li.tseqs <- lapply(tracklist0, seqlevelsInUse)
    li.tseqs <- li.tseqs[lengths(li.tseqs) != 0]
    
    if (length(li.tseqs) == 0) {
        # No "InUse" seqlevels
        li.tseqs <- lapply(tracklist0, seqlevels)
        li.tseqs <- li.tseqs[lengths(li.tseqs) != 0]
    }
    
    if (length(li.tseqs) == 0)
        # All seqlevels are empty...
        commonseqs <- character()
    else
        commonseqs <- Reduce(intersect, li.tseqs)
    
    if (length(commonseqs) == 0)
        stop("No common seqlevel is found in the track list.")
    
    if (length(commonseqs) == 1)
        sel.seq <- commonseqs
    else
        sel.seq <- commonseqs[1]
    
    viewrg <- {
        # TODO
        GRanges(sel.seq, IRanges(1, 1000))
    }
    
    tntboard@ViewRange <- viewrg
    
    message <- sprintf("View range is not specified, automatically selecting %s to %s on seqlevel %s",
                   start(viewrg), end(viewrg), seqlevels(viewrg))
    message(message)
    
    tntboard
}

#' @export
.selectZoom <- function (tntboard) {
    zoomalo <- tntboard@ZoomAllow
    if (length(zoomalo) == 1)
        return(tntboard)
    if (length(zoomalo) > 1)
        stop()
    
    # Then ZoomAllow is not set
    coord <- tntboard@CoordRange
    stopifnot(length(coord) == 1)
    
    tntboard@ZoomAllow <- IRanges(10, width(coord) + 1)
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
.compileTrackList <- function (tntboard) {
    tracklist <- tntboard@TrackList
    li.jc <- lapply(tracklist, compileTrack)
    names(li.jc) <- replicate(length(li.jc), "add_track")
    jc <- asJC(li.jc)
    jc
}


# .consolidateBackground <- function (tntboard) {
#     # By the time of construction of each tnt track, the background color can
#     # be either set to "NULL" or a scalar character.
#     #
#     # Before compilation of tntboard, this function examines these settings in
#     # each track, replace the NULLs with a more suitable value.
#     tracklist <- tntboard@TrackList
#     li.colors <- lapply(tracklist, function (t) t@Background@.Data)
#     colors <- unique(unlist(li.colors))
#     
#     if (length(colors) == 0L || length(colors) >= 2L)
#         default <- Biobase::mkScalar("white")
#     else
#         default <- Biobase::mkScalar(colors)
#     
#     tracklist <- lapply(tracklist, replace = default,
#         function (track, replace) {
#             if (is.null(track@Background))
#                 track@Background <- replace
#             track
#         }
#     )
#     tntboard@TrackList <- tracklist
#     tntboard
# }




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
