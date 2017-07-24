

#' @include tntTracks.R




### Track Compilation       =======


#' @export
.initDisplay <- function (track, feaname, extra = list()) {
    # For RangeTrack
    
    di.init <- setNames(list(ma()), feaname)
    di.color <- list(color = js("function (d) {return d.color;}"))
    di.index <- list(index = js("function (d) {return d.key;}"))
    di.extra <- extra
    ## For certain types of track
    ## TODO: we should use method dispatch instead
    di.domain <- list(domain = if (inherits(track, "DomainValTrack")) track@Domain
                               else NULL)
    di.tooltip <- {
        toolti <- tooltip(track)
        stopifnot(
            is.data.frame(toolti),
            #all(sapply(toolti, is.atomic)),
            !any(duplicated(names(toolti)))
        )
        toolti.header <- trackSpec(track, "label")
        toolti.entries <- colnames(toolti)
        list(on = ma("click",
            tooltipCallback(header = toolti.header, entries = toolti.entries)
        ))
    }
    
    display <- c(di.init, di.color, di.index, di.extra, di.domain, di.tooltip)
    track@Display <- display
    track
}

#' @export
.convertCol <- function (track) {
    # TODO: if all the colors are identical,
    #       we may modify the color callback to reduce the size of file
    col <- trackData(track)$color
    if (length(col) == 0)
        return(track)
    col <- gplots::col2hex(col)
    trackData(track)$color <- col
    track
}


#' @export
setGeneric("wakeupTrack", function (track) standardGeneric("wakeupTrack"))

setMethod("wakeupTrack", signature = c(track = "RangeTrack"),
    function (track) {
        class <- class(track)
        feaname <- switch(class,
            BlockTrack = "tnt.board.track.feature.block",
            GeneTrack = "tnt.board.track.feature.genome.gene",
            TxTrack = "tnt.board.track.feature.genome.transcript",
            PinTrack = "tnt.board.track.feature.pin",
            stop()
        )
        track <- .initDisplay(track, feaname = feaname)
        track <- .convertCol(track)
        track
    }
)
# EXAMPLE
if (FALSE) {
    gr <- GRanges("chr12", IRanges(1:4 * 10 + 1, width = 5))
    mcols(gr) <- data.frame(check.names = FALSE,
        Location = "",
        Chromosome = 12, Start = start(gr), End = end(gr),
        Description = "",
        "What's for?" = "Unknown"
    )
    track <- BlockTrack(gr)
    
    track
    wakeupTrack(track)
    
    TnTBoard(list(track), view.range = GRanges("chr12", IRanges(1, 100)))
}


# setGeneric("compileTrack",
#            function (tntTrack) standardGeneric("compileTrack"))

#' @export
compileTrack <- function (tntTrack) {
    # Wake up
    tntTrack <- wakeupTrack(tntTrack)
    
    li.spec <- list(
        tnt.board.track = ma(),
        label  = trackSpec(tntTrack, "label"),
        color  = trackSpec(tntTrack, "background"),  # rename to "color"
        height = trackSpec(tntTrack, "height")
    )
    
    jc.spec <- asJC(li.spec)
    jc.display <- jc(
        display = asJC(
            tntTrack@Display
        )
    )
    jc.data <- jc(data = compileTrackData(trackData(tntTrack)))
    c(jc.spec, jc.display, jc.data)
}

## Example
if (FALSE) local({
    btrack <- BlockTrack(GRanges("chr12", IRanges(21,234)))
    compileTrack(btrack)
    txdb <- TxDb.Hsapiens.UCSC.hg19.knownGene::TxDb.Hsapiens.UCSC.hg19.knownGene
    gtrack <- GeneTrack(txdb, seqlevel = "chr3")
    compileTrack(gtrack)
    ptrack <- PinTrack(GRanges("chr21", IRanges(1:10, width = 1), value = runif(10)))
    compileTrack(ptrack)
})




