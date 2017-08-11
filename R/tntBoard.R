

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

setClass("TnTGenome", contains = "TnTBoard",
    slots = c(
        Species = "character",
        Chromosome = "character"
    )
)

#### TnT Board Constructor      ========



#' @export
TnTBoard <- function (tracklist, view.range = GRanges(),
                      coord.range = IRanges(), zoom.allow = IRanges(), use.tnt.genome = FALSE) {
    
    if (is(tracklist, "TnTTrack"))
        tracklist <- list(tracklist)
    else
        stopifnot(all(sapply(tracklist, inherits, what = "TnTTrack")))
    
    b <- new(if (use.tnt.genome) "TnTGenome" else "TnTBoard",
             ViewRange = view.range, CoordRange = coord.range,
             ZoomAllow = zoom.allow, TrackList = tracklist)
    b
}

#' @export
TnTGenome <- function (tracklist, view.range = GRanges(),
                       coord.range = IRanges(), zoom.allow = IRanges()) {
    TnTBoard(tracklist, view.range = GRanges(),
             coord.range = IRanges(), zoom.allow = IRanges(), use.tnt.genome = TRUE)
}


## EXAMPLE
if (FALSE) {
    library(GenomicFeatures)
    gr <- GRanges("chr12", IRanges(1:4, width = 2))
    t <- BlockTrack(gr)
    b <- TnTBoard(t)
    tracklist(b)
}


setMethod(range, signature = c(x = "TnTBoard"),
    function (x, ..., with.revmap=FALSE, ignore.strand=FALSE, na.rm=FALSE) {
        if (length(list(...)))
            warning("Extra arguments ignored.")
        li.track <- tracklist(x)
        stopifnot(all(sapply(li.track, inherits, what = c("RangeTrack", "CompositeTrack"))))
        
        rg <- do.call(range, c(unname(li.track),
            list(with.revmap=with.revmap, ignore.strand=ignore.strand, na.rm=na.rm)))
        rg
    }
)



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
    
    if (inherits(b, "TnTGenome"))
        spec <- .compileBoardSpec(b, use.tnt.genome = TRUE)
    else
        spec <- .compileBoardSpec(b)
    tklst <- .compileTrackList(b)
    tntdef <- c(spec, tklst)
    tntdef
}


#' @export
wakeupBoard <- function (tntboard) {
    
    tntboard <- .selectView(tntboard)
    tntboard <- .fillGenome(tntboard)
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
    
    msg <- sprintf(paste("- Coordinate limit is not specified and seqlength is unknown:",
                         "  automatically set coordinate range to %s..%s ...", sep = "\n"),
                   start(coord), end(coord))
    message(msg)
    
    tntboard@CoordRange <- coord
    tntboard
}


#' @export
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
    
    # Then ViewRange is not set
    
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
    
    message <- sprintf(paste("- View range is not specified:",
                             "  automatically select %i..%i on seqlevel %s ...", sep = "\n"),
                   start(viewrg), end(viewrg), seqlevelsInUse(viewrg))
    message(message)
    
    tntboard
}

#' @export
.fillGenome <- function (tntboard) {
    if (!inherits(tntboard, "TnTGenome"))
        return(tntboard)
    
    stopifnot(length(tntboard@ViewRange) == 1)
    # Seqinfo of ViewRange is combined from the track list
    seqinfo <- seqinfo(tntboard@ViewRange)
    seqlv   <- seqlevelsInUse(tntboard@ViewRange)
    tntboard@Species <- unname(genome(seqinfo)[seqlv])
    tntboard@Chromosome <- seqlv
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
    
    tntboard@ZoomAllow <- if (width(coord) <= 15) IRanges(1, 100)
                          else IRanges(10, width(coord) + 100)
    tntboard
}

#' @export
.compileBoardSpec <- function (tntboard, use.tnt.genome = FALSE) {
    .checkBoardSpec <- function (tntboard) {
        b <- tntboard
        stopifnot(
            # These three slots should be prepared before converted to JS
            length(b@ViewRange) == 1,
            length(b@CoordRange) == 1,
            length(b@ZoomAllow) == 1,
            if (inherits(b, "TnTGenome"))
                length(b@Species) == 1 else TRUE,
            if (inherits(b, "TnTGenome"))
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
            species = if (is.na(b@Species)) "Unknown sequence" else b@Species,
            chr     = b@Chromosome,
            min_coord = js(sprintf('new Promise (function (resolve) { resolve (%i); })',
                                   start(b@CoordRange))),
            max_coord = js(sprintf('new Promise (function (resolve) { resolve (%i); })',
                                   end(b@CoordRange))),
            zoom_out = end(b@ZoomAllow),
            zoom_in  = start(b@ZoomAllow)
        )
    else
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

setMethod("show", signature = c("TnTBoard"),
    function (object) {
        # TODO: Have to provide renderTnT and TnTOutput
        widget <- trackWidget(object, elementId = NULL)
        print(widget)
    }
)
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




