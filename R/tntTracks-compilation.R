

#' @include tntTracks.R




### Track Compilation       =======


.initDisplay <- function (track, feaname, extra = list()) {
    # For RangeBasedTrack
    
    di.init <- setNames(list(ma()), feaname)
    di.color <- list(color = {
        ## LineTrack and AreaTrack do not support multiple color values
        ## This is a tempoary solution
        if (is(track, "LineTrack") || is(track, "AreaTrack")) {
            co <- unique(trackData(track)$color)
            if (length(co) > 1) {
                # TODO: Also add warning in constructor?
                warning("LineTrack and AreaTrack do not support multiple color values")
                co <- co[1]
            }
            if (!length(co))
                NULL  ## empty track?
            else
                co
        }
        else
            js('function (d) {return d.color;}')
    })
    di.index <- list(index = js('function (d) {return d.key;}'))
    di.extra <- extra
    
    # Do not set domain in display any more, but normalize data from domain to 0..1
    di.domain <- list(domain = {
        if (is(track, "PinTrack")) # TODO: This can be removed when
            c(0L, 1L)                    #       default value in upstream is fixed.
        else
            NULL
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
    if (!length(col))
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

setMethod("wakeupTrack", signature = c(track = "RangeBasedTrack"),
    function (track) {
        class <- class(track)
        
        # Simulate a method dispatch
        use.class <- match.class(class,
            c("BlockTrack", "GeneTrack", "TxTrack", "VlineTrack",
              "PinTrack", "LineTrack", "AreaTrack")
        )
        
        feaname <- switch(use.class,
            BlockTrack   = "tnt.board.track.feature.block",
            GeneTrack    = "tnt.board.track.feature.genome.gene",
            TxTrack      = "tnt.board.track.feature.genome.transcript",
            VlineTrack   = "tnt.board.track.feature.vline",
            PinTrack     = "tnt.board.track.feature.pin",
            LineTrack    = "tnt.board.track.feature.line",
            AreaTrack    = "tnt.board.track.feature.area",
            stop("<internal> Unmatched track class")
        )
        
        ### TEMP: TO REMOVE IN FUTURE, AND MAKE SURE KEY IS UNIQUE
        if (use.class == "VlineTrack") {
            trackData(track)$key <- start(trackData(track))
        }
        ### TEMP: TO REMOVE IN FUTURE
        
        track <- .initDisplay(track, feaname = feaname)
        track <- .convertCol(track)
        track
    }
)

# setGeneric("compileTrack",
#            function (tntTrack) standardGeneric("compileTrack"))

compileTrack <- function (tntTrack, wakeup = TRUE) {
    # Wake up
    if (wakeup)
        tntTrack <- wakeupTrack(tntTrack)
    
    label <- trackSpec(tntTrack, "label")
    label <- if (is.null(label)) "" else label
    
    background <- trackSpec(tntTrack, "background")
    background <- if (is.null(background)) col2hex("white")
                  else                     col2hex(background)
    
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



