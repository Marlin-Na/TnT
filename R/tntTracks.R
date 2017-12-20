
#' @include tntTracks-trackData.R

# A TnT Track consists of Five slots:
#   o Background:       Background color
#   o Height:           Height of the track
#   o Label:            Label of the track
#   o Data:
#       - Virtual class "TrackData", with subclasses that have different methods
#         for conversion to JS. Most of which also extend "GRanges" class.
#       - A list of tracks, for CompositeTrack
#   o Display:
#       A list, describes the display options for different types of track.
#       In the current implementation, it only exists as a placeholder for
#       these options. The content does not matter at all and will always be
#       overwritten before rendering.
#
# TnT Track supports `seqinfo` and `seqinfo<-` method so that before rendering,
# only track data on the intended chromosome can be kept.


## Templates for JS Callback and Promise     ------------------------------------

tooltipCallback <- function (header, entries) {
    # Example:
    #   tooltipCallback(header = "Tooltip Header",
    #                   entries = c("Start", "End", "Description"))
    stopifnot(length(header) == 1)
    jc(tnr.tooltip_callback = ma(header, entries))
}




###  TnT Tracks   ##############################################################

#### Track classes          ========
setClassUnion("ScalarCharacterOrNull", c("ScalarCharacter", "NULL"))
setClassUnion("ScalarNumericOrNull", c("ScalarNumeric", "ScalarInteger", "NULL"))

setClass("TnTTrack", slots = c(
    # TODO: add ID slot?
    Background = "ScalarCharacterOrNull",
    Height = "ScalarNumericOrNull",
    Label = "ScalarCharacterOrNull",
    
    Data = "ANY",
    Display = "list"
))

setClass("RangeBasedTrack", contains = "TnTTrack", slots = c(Data = "RangeTrackData"))

setClass("BlockTrack", contains = "RangeBasedTrack", slots = c(Data = "RangeTrackData"))
setClass("GeneTrack", contains = "RangeBasedTrack", slots = c(Data = "GeneTrackData"))
setClass("TxTrack", contains = "RangeBasedTrack", slots = c(Data = "TxTrackData"))

setClass("VlineTrack", contains = "RangeBasedTrack", slots = c(Data = "PosTrackData"))

setClass("DomainValTrack", contains = "RangeBasedTrack")

setClass("PinTrack", contains = "DomainValTrack", slots = c(Data = "PosValTrackData"))
setClass("LineTrack", contains = "DomainValTrack", slots = c(Data = "PosValTrackData"))
setClass("AreaTrack", contains = "DomainValTrack", slots = c(Data = "PosValTrackData"))



#### Seqinfo Methods        ========


#' Seqinfo of TnTTrack and TnTBoard
#' 
#' @param x A TnTTrack or TnTBoard object. 
#' @param new2old,pruning.mode,value Passed to seqinfo method for GenomicRanges.
#' @name seqinfo
#' @aliases seqinfo<-,RangeBasedTrack-method
#' @return \code{seqinfo} returns a SeqInfo object.
#' @examples
#' btrack1 <- BlockTrack(GRanges("chr1", IRanges(1, 123)))
#' btrack2 <- BlockTrack(GRanges("chr2", IRanges(3, 599)))
#' ctrack <- merge(btrack1, btrack2)
#' board <- TnTBoard(list(btrack1, btrack2))
#' 
#' seqinfo(btrack1)
#' seqinfo(btrack2)
#' seqinfo(ctrack)
#' seqinfo(board)
setReplaceMethod("seqinfo", signature = c(x = "RangeBasedTrack"),
    function (x, new2old, pruning.mode, value) {
        trackData(x) <- `seqinfo<-`(x = trackData(x), new2old = new2old,
                                    pruning.mode = pruning.mode, value = value)
        x
    }
)

#' @rdname seqinfo
setMethod("seqinfo", signature = c("RangeBasedTrack"),
    function (x) {
        seqinfo(trackData(x))
    }
)

#' @rdname seqinfo
setMethod("seqlevelsInUse", signature = c(x = "RangeBasedTrack"),
    function (x) seqlevelsInUse(trackData(x))
)


#### TrackData Accessor          ========

#' Access Track Data
#' 
#' Access and modify the track data. \code{x$name} and \code{x$name <- value} are
#' just shortcuts for \code{trackData(x)$name} and \code{trackData(x)$name <- value},
#' respectively.
#' 
#' @name trackdata
#' @param x A TnTTrack object.
#' 
#' @return \code{trackData} on all track types except "CompositeTrack" returns an
#'     object that inherits GRanges class, which means they should behave like a GRanges.
#'     While \code{trackData} on "CompositeTrack" returns a list of tracks.
#'
#' @export
#' @examples
#' track <- BlockTrack(GRanges("chr1", IRanges(6, 54)))
#' trackData(track) # track data of block track is an object that inherits GRanges.
#' ctrack <- merge(track, track)
#' trackData(ctrack) # track data of composite track is a list of tracks
trackData <- function (x) {
    x@Data
}

#' @rdname trackdata
#' @param value Replaced value.
#' @export
`trackData<-` <- function (x, value) {
    # TODO: convert value to the needed class?
    x@Data <- value
    validObject(x)
    x
}

#### `[`, `[[`, etc.        ========

#' @rdname trackdata
setMethod("$", signature = c(x = "TnTTrack"),
    function (x, name) {
        s <- as.call(list(`$`, trackData(x), name))
        eval.parent(s)
    }
)

#' @rdname trackdata
#' @param name Passed to the inner method for track data.
setReplaceMethod("$", signature = c(x = "TnTTrack"),
    function (x, name, value) {
        s <- match.call()
        s$x <- bquote(TnT::trackData(.(s$x)))
        trackData(x) <- eval.parent(s)
        x
    }
)

# setMethod("[", signature = c(x = "TnTTrack"),
#     function (x, i, j, ..., drop = TRUE) {
#         #trackData(x)[i, j, ..., drop = TRUE]
#         s <- match.call()
#         s$x <- bquote(TnT::trackData(.(s$x)))
#         eval.parent(s)
#     }
# )

# setMethod("[<-", signature = c(x = "TnTTrack"),
#     function (x, i, j, ..., value) {
#         s <- match.call()
#         s$x <- bquote(TnT::trackData(.(s$x)))
#         trackData(x) <- eval.parent(s)
#         x
#     }
# )

# setMethod("[[", signature = c(x = "TnTTrack"),
#     function (x, ...) {
#         s <- match.call()
#         s$x <- bquote(TnT::trackData(.(s$x)))
#         eval.parent(s)
#     }
# )

# setMethod("[[<-", signature = c(x = "TnTTrack"),
#     function (x, i, j, ..., value) {
#         s <- match.call()
#         s$x <- bquote(TnT::trackData(.(s$x)))
#         trackData(x) <- eval.parent(s)
#         x
#     }
# )


#### TrackSpec Accessor     ========

.mkScalarOrNull <- function (x)
    if (is.null(x)) x else Biobase::mkScalar(x)

#' Track Spec
#' 
#' Height, background and label are common options of all tracks, use these functions
#' to get and set them.
#' 
#' @name trackSpec
#'
#' @param track A TnTTrack object.
#' @param which Character vector, can be "background", "height" or "label".
#' @param value Value to set: background should be character, height should be numeric,
#'     label should be character. If length of \code{which} is bigger than one, \code{value}
#'     should be a list with the same length.
#' @return
#'     For \code{trackSpec}, if length of \code{which} equals to one, return a
#'     scalar character or numeric, if length of \code{which} is bigger than one,
#'     return as a list.
#' @export
#' @examples
#' track <- BlockTrack(GRanges("chr13", IRanges(6, 9)))
#' trackSpec(track, "background")
#' trackSpec(track, c("height", "label"))
#' trackSpec(track, c("height", "label")) <- list(100, "my range")
#' trackSpec(track, "background") <- "green"
#' trackSpec(track)
trackSpec <- function (track, which = c("background", "height", "label")) {
    if (length(which) == 1)
        switch(which,
            background = track@Background@.Data,
            height = track@Height@.Data,
            label = track@Label@.Data,
            stop(which, "is not an available track option")
        )
    else
        `names<-`(lapply(which, trackSpec, track = track), which)
}


#' @rdname trackSpec
#' @export
`trackSpec<-` <- function (track, which = c("background", "height", "label"), value) {
    if (length(which) == 1) {
        switch(which,
            background = track@Background <- .mkScalarOrNull(value),
            height     = track@Height <- .mkScalarOrNull(value),
            label      = track@Label <- .mkScalarOrNull(value),
            warning(which, " is not an available track option")
        )
        return(track)
    }
    
    stopifnot(length(which) == length(value))
    
    for (i in seq_along(which))
        trackSpec(track, which = which[[i]]) <- value[[i]]
    track
}




#### Track Tooltip          ========

#' Access Track Tooltips
#'
#' @param x A TnTTrack object.
#' 
#' @return \code{tooltip} returns a data frame.
#'
#' @export
#' @examples
#' gr <- GRanges("chr12", IRanges(c(6, 69), c(42, 135)), Name = c("my range 1", "my range 2"))
#' track <- BlockTrack(gr)
#' tooltip(track)
#' tooltip(track)$Width <- width(gr)
#' tooltip(track)
setGeneric("tooltip", function (x) standardGeneric("tooltip"))

#' @rdname tooltip
#' @export
#' @param value A data frame to replace, its row number should equal to length of data.
setGeneric("tooltip<-", function (x, value) standardGeneric("tooltip<-"))

#' @rdname tooltip
setMethod("tooltip", signature = "TrackData",
    function (x) {
        x$tooltip
    }
)
#' @rdname tooltip
setMethod("tooltip", signature = "TnTTrack",
    function (x) {
        tooltip(trackData(x))
    }
)
#' @rdname tooltip
setReplaceMethod("tooltip", signature = c(x = "TrackData", value = "data.frame"),
    function (x, value) {
        x$tooltip <- value
        x
    }
)
#' @rdname tooltip
setReplaceMethod("tooltip", signature = c(x = "TnTTrack", value = "data.frame"),
    function (x, value) {
        tooltip(trackData(x)) <- value
        x
    }
)



###   Show Method   ############################################################

setMethod("show", signature = "RangeBasedTrack",
    function (object) {
        background <- trackSpec(object, "background")
        if (is.null(background))
            background <- paste("missing, use", sQuote("white"))
        height     <- trackSpec(object, "height")
        label      <- trackSpec(object, "label")
        cat("A", class(object), "\n")
        cat("| Label:\t", label, "\n", sep="")
        cat("| Background:\t", background, "\n", sep="")
        cat("| Height:\t", height, "\n", sep="")
        cat("| Data:\t")
        dout <- capture.output(show(trackData(object)))
        dout[-1] <- paste0("|  ", dout[-1])
        dout <- paste(dout, collapse = "\n")
        cat(dout)
        cat("\n")
    }
)





###   View          ############################################################

view <- function (...) {
    # TODO
}

