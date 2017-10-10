
#' @include tntTracks.R

###   TnT Board     ############################################################

####  Class Def for TnT Board   ========

# A TnT Board is the object that holds a list of tracks and settings for the browser,
# it contains the following slots:
#
#   o ViewRange
#       Class "GRanges" that specifies the initial viewing range of rendered browser.
#       According to the its seqlevel, trackdata that on extra chromosomes will be
#       dropped before converted to JavaScript. It should be suppiled when constructing
#       the Board.
#   o CoordRange
#       Class "IRanges" that defines the left and right coordinate of the board.
#   o ZoomAllow
#       Class "IRanges" that describes the minimal and maximal extent of the
#       board (i.e. the limit when zooming in and out).
#   o AllowDrag
#       Logical, whether allow drag and zoom
#   o TrackList
#       List of tracks.

setClass("TnTBoard",
    slots = c(
        ViewRange = "GRanges",
        CoordRange = "IRanges",
        ZoomAllow = "IRanges",
        AllowDrag = "logical",
        TrackList = "list"
    )
)

setClass("TnTGenome", contains = "TnTBoard",
    slots = c(
        Species = "character",
        Chromosome = "character"
    )
)

#### TnT Board Constructor      ========

#' TnTBoard
#' 
#' A TnTBoard or TnTGenome object stores a list of tracks and can be automatically
#' shown in an interactive R session or in rmarkdown output.
#' 
#' @param tracklist One track or a list of tracks to view.
#' @param view.range Length-one GRanges object, sets the initial view range.
#' @param coord.range Length-one IRanges object or length-two numeric vector,
#'     sets the coordinate limit of the board (i.e. minimum/maximum possible coordinate).
#' @param zoom.allow Length-one IRanges object or length-two numeric vector,
#'     sets the minimum and maximum extent of the board (i.e. the limit when zooming in and zooming out).
#' @param allow.drag Logical, whether drag should be allowed? Default TRUE.
#' @param use.tnt.genome Logical, whether to add axis and location. `TnTGenome(...)` is essentially
#'     a wrapper to `TnTBoard(..., use.tnt.genome = TRUE)`.
#'
#' @return
#'     Returns a TnTBoard or TnTGenome object which has printing method to be rendered
#'     as a htmlwidget.
#' @export
#' 
#' @name tntboard
#' @aliases tntgenome
#' 
#' @examples
#' track <- BlockTrack(GRanges("chr1", IRanges(start = c(100, 300, 500), width = c(10, 100, 200))))
#' \dontrun{
#' TnTGenome(track)
#' }
TnTBoard <- function (tracklist, view.range = GRanges(),
                      coord.range = IRanges(), zoom.allow = IRanges(), allow.drag = TRUE,
                      use.tnt.genome = FALSE) {
    
    if (is(tracklist, "TnTTrack"))
        tracklist <- list(tracklist)
    else
        stopifnot(all(vapply(tracklist, is, logical(1L), what = "TnTTrack")))
    
    if (is.numeric(coord.range)) {
        if (length(coord.range) == 2L)
            coord.range <- IRanges(coord.range[1], coord.range[2])
        else
            stop("coord.range should be a length-one IRanges or a length-two numeric vector")
    }
    if (is.numeric(zoom.allow)) {
        if (length(zoom.allow) == 2L)
            zoom.allow <- IRanges(zoom.allow[1], zoom.allow[2])
        else
            stop("zoom.allow should be a length-one IRanges or a length-two numeric vector")
    }
    
    b <- new(if (use.tnt.genome) "TnTGenome" else "TnTBoard",
             ViewRange = view.range, CoordRange = coord.range,
             ZoomAllow = zoom.allow, TrackList = tracklist, AllowDrag = allow.drag)
    b
}

#' @rdname tntboard
#' @export
TnTGenome <- function (tracklist, view.range = GRanges(),
                       coord.range = IRanges(), zoom.allow = IRanges(), allow.drag = TRUE) {
    TnTBoard(tracklist, view.range = view.range,
             coord.range = coord.range, zoom.allow = zoom.allow, allow.drag = allow.drag,
             use.tnt.genome = TRUE)
}


## EXAMPLE
if (FALSE) {
    library(GenomicFeatures)
    gr <- GRanges("chr12", IRanges(1:4, width = 2))
    bt <- BlockTrack(gr)
    b <- TnTBoard(bt)
    tracklist(b)
}




#' Range of TnTBoard
#' 
#' Get combined range of all tracks in a TnTBoard, used internally.
#'
#' @param x TnTBoard. 
#' @param ...,with.revmap,ignore.strand,na.rm
#'     Passed to \code{\link[GenomicRanges]{range,GenomicRanges-method}}.
#' @return GRanges.
setMethod(range, signature = c(x = "TnTBoard"),
    function (x, ..., with.revmap=FALSE, ignore.strand=FALSE, na.rm=FALSE) {
        if (length(list(...)))
            warning("Extra arguments ignored.")
        li.track <- tracklist(x)
        
        rg <- do.call(range, c(unname(li.track),
            list(with.revmap=with.revmap, ignore.strand=ignore.strand, na.rm=na.rm)))
        rg
    }
)



#### Accessors                  ========

#' Track List in TnTBoard
#' 
#' The tracks of a TnTBoard are stored as a list which can be accessed or modified
#' with these functions. 
#'
#' @param tntboard A TnTBoard or TnTGenome object
#' 
#' @name tracklist
#' @return \code{tracklist} returns a list of tracks.
#' @export
#' @examples
#' bt <- BlockTrack(GRanges("chr21", IRanges(100, 1200)))
#' li.tracks <- list(bt, bt)
#' board <- TnTBoard(li.tracks)
#' tracklist(board)
#' \dontrun{
#' show(board)
#' }
#' tracklist(board) <- list(bt)
#' \dontrun{
#' show(board)
#' }
tracklist <- function (tntboard) {
    tntboard@TrackList
}

#' @rdname tracklist
#' @param value A list of tracks
#' @export
`tracklist<-` <- function (tntboard, value) {
    tntboard@TrackList <- value
    tntboard
}


#### SeqInfo                    ========

#' @rdname seqinfo
setMethod("seqinfo", signature = "TnTBoard",
    function (x) {
        li.tracks <- tracklist(x)
        li.seqinfo <- lapply(li.tracks, seqinfo)
        do.call(merge, unname(li.seqinfo))
    }
)



#### TnT Board Compilation      ========

compileBoard <- function (tntboard) {
    b <- wakeupBoard(tntboard)
    
    if (is(b, "TnTGenome"))
        spec <- .compileBoardSpec(b, use.tnt.genome = TRUE)
    else
        spec <- .compileBoardSpec(b)
    tklst <- .compileTrackList(b)
    tntdef <- c(spec, tklst)
    tntdef
}


wakeupBoard <- function (tntboard) {
    
    tntboard <- .selectView(tntboard)
    tntboard <- .fillGenome(tntboard)
    tntboard <- .filterSeq(tntboard)
    tntboard <- .selectCoord(tntboard)
    tntboard <- .selectZoom(tntboard)
    
    tntboard
}



.filterSeq <- function (tntboard, use.seq = seqlevelsInUse(tntboard@ViewRange)) {
    stopifnot(length(use.seq) == 1)
    
    li.t <- tracklist(tntboard)
    li.t <- lapply(li.t, keepSeqlevels, value = use.seq, pruning.mode = "coarse")
    
    tntboard@TrackList <- li.t
    tntboard
}

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
        do.call(merge, unname(li.seqinfo))
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
        ## Aggregate from track data
        rg <- range(tntboard, ignore.strand = TRUE)
        rg <- keepSeqlevels(rg, seqlv, pruning.mode = "coarse")
        
        stopifnot(length(rg) == 1) # TODO: However, there will be cases that all the tracks are empty
        
        ranges(rg) * .7
    }
    
    msg <- sprintf(paste("- Missing argument `coord.range` and seqlength is unknown:",
                         "  automatically set coordinate limit to %s..%s ...", sep = "\n"),
                   start(coord), end(coord))
    message(msg)
    
    tntboard@CoordRange <- coord
    tntboard
}


.selectView <- function (tntboard) {
    ## TODO: view range may support strands, i.e. only showing features on one strand
    
    viewrange0 <- tntboard@ViewRange
    tracklist0 <- tracklist(tntboard)
    
    if (length(viewrange0) == 1) {
        # Already specified
        ## Update the combined seqinfo
        comb.seqinfo <- do.call(merge, unname(lapply(tracklist0, seqinfo)))
        comb.seqinfo <- merge(comb.seqinfo, seqinfo(viewrange0))
        
        seqinfo(tntboard@ViewRange,
            new2old = match(seqlevels(comb.seqinfo), seqlevels(tntboard@ViewRange))) <- comb.seqinfo
        
        return(tntboard)
    }
    if (length(viewrange0) > 1)
        stop()
    
    # Then length(viewrange0) == 0
    
    commonseqs <- {
        li.tseqs <- lapply(tracklist0, function (t) seqlevelsInUse(t))
        li.tseqs <- li.tseqs[lengths(li.tseqs) != 0]
        
        if (length(li.tseqs) == 0) {
            # No "InUse" seqlevels
            li.tseqs <- lapply(tracklist0, seqlevels)
            li.tseqs <- li.tseqs[lengths(li.tseqs) != 0]
        }
        
        if (length(li.tseqs) == 0)
            # All seqlevels are empty...
            character()
        else
            Reduce(intersect, li.tseqs)
    }
    
    sel.seq <- {
        if (length(commonseqs) == 0)
            stop("No common seqlevel is found in the track list.")
        if (length(commonseqs) == 1)
            sel.seq <- commonseqs
        else
            sel.seq <- commonseqs[1]
        unname(sel.seq)
    }
    
    find.viewrg <- function (tntboard, sel.seq) {
        li.track <- tracklist(tntboard)
        comb.seqinfo <- do.call(merge, unname(lapply(li.track, seqinfo)))
        
        li.rg <- lapply(li.track, range, ignore.strand = TRUE)
        li.rg <- unname(lapply(li.rg, keepSeqlevels, value = sel.seq, pruning.mode = "coarse"))
        
        stopifnot(all(lengths(li.rg) %in% c(1L, 0L)))
        
        viewrg <- {
            # Find the intersection of the ranges and use it as view range
            viewrg <- Reduce(intersect, li.rg[lengths(li.rg) != 0])
            
            if (length(viewrg)) {
                # The intersection exists
                ## TODO
                stopifnot(length(viewrg) == 1)
                viewrg <- viewrg * .8
            }
            else {
                # There is no intersection
                ## TODO
                viewrg <- do.call(range, c(unname(li.rg), ignore.strand = TRUE))
                stopifnot(length(viewrg) == 1)
                viewrg <- viewrg * .8
            }
            
            seqinfo(viewrg, new2old = match(seqlevels(comb.seqinfo), seqlevels(viewrg))) <- comb.seqinfo
            
            viewrg
        }
        viewrg
    }
    
    viewrg <- find.viewrg(tntboard = tntboard, sel.seq = sel.seq)
    tntboard@ViewRange <- viewrg
    
    message <- sprintf(paste("- Missing argument `view.range`:",
                             "  automatically select %i..%i on seqlevel %s...", sep = "\n"),
                   start(viewrg), end(viewrg), seqlevelsInUse(viewrg))
    message(message)
    
    tntboard
}

.fillGenome <- function (tntboard) {
    if (!is(tntboard, "TnTGenome"))
        return(tntboard)
    
    stopifnot(length(tntboard@ViewRange) == 1)
    # Seqinfo of ViewRange is combined from the track list
    seqinfo <- seqinfo(tntboard@ViewRange)
    seqlv   <- seqlevelsInUse(tntboard@ViewRange)
    tntboard@Species <- unname(genome(seqinfo)[seqlv])
    tntboard@Chromosome <- seqlv
    tntboard
}


.selectZoom <- function (tntboard) {
    zoomalo <- tntboard@ZoomAllow
    if (length(zoomalo) == 1)
        return(tntboard)
    if (length(zoomalo) > 1)
        stop()
    
    # Then ZoomAllow is not set
    coord <- tntboard@CoordRange
    stopifnot(length(coord) == 1)
    
    tntboard@ZoomAllow <- if (width(coord) <= 15) IRanges(1, 100)
                          else IRanges(10, width(coord) + 100)
    tntboard
}

.compileBoardSpec <- function (tntboard, use.tnt.genome = FALSE) {
    .checkBoardSpec <- function (tntboard) {
        b <- tntboard
        stopifnot(
            # These three slots should be prepared before converted to JS
            length(b@ViewRange) == 1,
            length(b@CoordRange) == 1,
            length(b@ZoomAllow) == 1,
            length(b@AllowDrag) == 1,
            if (is(b, "TnTGenome"))
                length(b@Species) == 1 else TRUE,
            if (is(b, "TnTGenome"))
                length(b@Chromosome) == 1 else TRUE
        )
        b
    }
    b <- .checkBoardSpec(tntboard)
    if (use.tnt.genome)
        jc.board.spec <- jc(
            tnt.board.genome = ma(),
            from = start(b@ViewRange),
            to   = end(b@ViewRange),
            species = if (is.na(b@Species)) "Unknown" else b@Species,
            chr     = b@Chromosome,
            min_coord = js(sprintf('new Promise (function (resolve) { resolve (%i); })',
                                   start(b@CoordRange))),
            max_coord = js(sprintf('new Promise (function (resolve) { resolve (%i); })',
                                   end(b@CoordRange))),
            zoom_out = end(b@ZoomAllow),
            zoom_in  = start(b@ZoomAllow),
            allow_drag = b@AllowDrag
        )
    else
        jc.board.spec <- jc(
            tnt.board = ma(),
            from     = start(b@ViewRange),
            to       = end(b@ViewRange),
            min      = start(b@CoordRange),
            max      = end(b@CoordRange),
            zoom_out = end(b@ZoomAllow),
            zoom_in  = start(b@ZoomAllow),
            allow_drag = b@AllowDrag
        )
    jc.board.spec
}
    
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

setMethod("show", signature = c("TnTBoard"),
    function (object) {
        # TODO: Have to provide renderTnT and TnTOutput
        widget <- trackWidget(object, elementId = NULL)
        print(widget)
    }
)


#' Printing TnTBoard in Rmarkdown
#' 
#' S3 method to automatically render a TnTBoard with knitr.
#' 
#' @param x A TnTBoard or TnTGenome object.
#' @param ...,options Passed to \code{htmlwidget:::knit_print.htmlwidget}.
#' 
#' @return \code{htmlwidget:::knit_print.htmlwidget} invisibly returns a character
#'     vector with "browsable_html" S3 class.
#' @references \code{\link[knitr]{knit_print}}
#' @export
#' @examples
#' track <- BlockTrack(GRanges("chr12", IRanges(c(100, 400, 700), width = 100)),
#'                     color = c("green", "red", "blue"))
#' tntboard <- TnTGenome(track)
#' \dontrun{
#' knitr::knit_print(tntboard)
#' }
knit_print.TnTBoard <- function (x, ..., options = NULL) {
    # Redirect method to htmlwidget
    x <- trackWidget(x, elementId = NULL)
    knitr::knit_print(x, ..., options = options)
}


# setMethod("knit_print", signature = c(x = "TnTBoard"), knit_print.TnTBoard)
