

#' @include tntTracks.R




### Track Compilation       =======


.initDisplay <- function (track, feaname, extra = list()) {
    # For RangeTrack
    
    di.init <- setNames(list(ma()), feaname)
    di.color <- list(color = {
        ## TODO: temporary solution for LineTrack and AreaTrack
        if (inherits(track, c("LineTrack", "AreaTrack"))) {
            co <- unique(trackData(track)$color)
            if (length(co) > 1) {
                # TODO: add informative warning in constructor
                warning("LineTrack and AreaTrack do not support multiple color values")
                co <- co[1]
            }
            if (length(co) == 0)
                NULL  ## empty track?
            else
                co
        }
        else
            js('function (d) {return d.color;}')
    })
    di.index <- list(index = js('function (d) {return d.key;}'))
    di.extra <- extra
    ## For certain types of track
    ## TODO: we should use method dispatch instead
    di.domain <- list(domain = {
        if (inherits(track, "DomainValTrack"))
            ## TODO: temporary solution for LineTrack and AreaTrack
            if (inherits(track, c("LineTrack", "AreaTrack")))
                NULL
            else
                track@Domain
        else NULL
    })
    di.tooltip <- {
        toolti <- tooltip(track)
        stopifnot(
            is.data.frame(toolti),
            #all(sapply(toolti, is.atomic)),
            !any(duplicated(names(toolti)))
        )
        toolti.header <- {
            label <- trackSpec(track, "label")
            if (is.null(label)) ""
            else                label
        }
        toolti.entries <- colnames(toolti)
        list(on = ma("click",
            tooltipCallback(header = toolti.header, entries = toolti.entries)
        ))
    }
    
    display <- c(di.init, di.color, di.index, di.extra, di.domain, di.tooltip)
    track@Display <- display
    track
}

.convertCol <- function (track) {
    # TODO: if all the colors are identical,
    #       we may modify the color callback to reduce the size of file
    col <- trackData(track)$color
    if (length(col) == 0)
        return(track)
    col <- col2hex(col)
    trackData(track)$color <- col
    track
}


# #' Wake Up a Track
# #' 
# #' Internal function.
# #'
# #' @param track A TnTTrack.
# #' @return A TnTTrack
setGeneric("wakeupTrack", function (track) standardGeneric("wakeupTrack"))

setMethod("wakeupTrack", signature = c(track = "RangeTrack"),
    function (track) {
        class <- class(track)
        
        # Simulate a method dispatch
        super.classes <- getAllSuperClasses(getClass(class))
        use.classes <- c("BlockTrack", "GeneTrack", "TxTrack", "VlineTrack",
                         "PinTrack", "LineTrack", "AreaTrack")
        use.class <- use.classes[which.min(match(use.classes, super.classes))]
        
        if (length(use.class) == 0)
            stop(sprintf("Method for %s class not implemented", class))
        
        feaname <- switch(use.class,
            BlockTrack   = "tnt.board.track.feature.block",
            GeneTrack    = "tnt.board.track.feature.genome.gene",
            TxTrack      = "tnt.board.track.feature.genome.transcript",
            VlineTrack   = "tnt.board.track.feature.vline",
            PinTrack     = "tnt.board.track.feature.pin",
            LineTrack    = "tnt.board.track.feature.line",
            AreaTrack    = "tnt.board.track.feature.area",
            stop()
        )
        
        ### TEMP: TO REMOVE IN FUTURE
        if (use.class == "VlineTrack") {
            trackData(track)$key <- start(trackData(track))
        }
        ### TEMP: TO REMOVE IN FUTURE
        
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

compileTrack <- function (tntTrack, wakeup = TRUE) {
    # Wake up
    if (wakeup)
        tntTrack <- wakeupTrack(tntTrack)
    
    label <- trackSpec(tntTrack, "label")
    label <- if (is.null(label)) "" else label
    
    background <- trackSpec(tntTrack, "background")
    background <- if (is.null(background)) "white" else col2hex(background)
    
    height <- trackSpec(tntTrack, "height")
    height <- if (is.null(height)) 100 else height
    
    li.spec <- list(
        tnt.board.track = ma(),
        label  = label,
        color  = background,  # rename to "color"
        height = height
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




