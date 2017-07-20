

#' @include tntTracks.R




### Track Compilation       =======


#' @export
setGeneric("wakeupTrack", function (track) standardGeneric("wakeupTrack"))

setMethod("wakeupTrack", signature = c(track = "RangeTrack"),
    function (track) {
      # Update tooltip setting
      # TODO: move the template to js side
        toolti <- tooltip(track)
        stopifnot(
            is.data.frame(toolti),
            #all(sapply(toolti, is.atomic)),
            !any(duplicated(names(toolti)))
        )
        toolti.header <- trackSpec(track, "label")
        toolti.colnames <- colnames(toolti)
        js.callback <- tooltipCallback(header = toolti.header, labels = toolti.colnames,
                                       colnames = toolti.colnames)
        toolti.spec <- list(on = ma("click", js.callback))
        # Append to "Display"
        track@Display <- c(track@Display, toolti.spec)
        
      # TODO2: May optionally simplify the "color" callback
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
    
    TnTBoard(list(track), viewrange = GRanges("chr12", IRanges(1, 100)))
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




