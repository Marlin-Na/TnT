


### TrackData Class     --------------------------------------------------------

setClass("TrackData", slots = c(Source = "ANY"))

## 0. Empty Track Data                      --------

NoTrackData <- setClass("NoTrackData", contains = "TrackData")
setMethod("asJC", c(object = "NoTrackData"),
    function (object) jc(tnt.board.track.data.empty = NoArg)
)


## 1. "Range" or "Pos" based track data     --------

# These types should be able to be converted to a data.frame, then
# use jsonlite::toJSON to convert them to json string.
#
# An example is shown in the following:
if (interactive()) local({
    gr <- GenomicRanges::GRanges("chr12", IRanges::IRanges(1:4, 40))
    gr$value <- c(42)
    df.td <- .asDfTrackData(gr, PosBased = FALSE, SelectSeq = "chr12")
    print(df.td)
    print(.selfRetriever(df.td))
})



setClassUnion("GRangesOrIRanges", members = c("GRanges", "IRanges"))
#setClassUnion("GPosOrIRanges", members = c("GPos", "IRanges"))

setClass("RangeOrPosTrackData", contains = "TrackData", slots = c(Source = "GRangesOrIRanges"))

RangeTrackData <- setClass("RangeTrackData", contains = "RangeOrPosTrackData")
PosTrackData <- setClass("PosTrackData", contains = "RangeOrPosTrackData")

# TODO: set validity method




.jcAsyncData <- function (js.retriever) {
    # Async method requires a javascript callback that returns a promise object
    # which can be useful when retrieving remote data
    jc(tnt.board.track.data.async = ma(),
       retriever = js.retriever)
}
.jcSyncData <- function (js.retriever) {
    jc(tnt.board.track.data.sync = ma(),
       retriever = js.retriever)
}
.selfRetriever <- function (df.trackdata) {
    json <- jsonlite::toJSON(df.trackdata, dataframe = "rows", pretty = TRUE)
    ans <- sprintf("function () {  return ( %s )  }", json)
    js(ans)
}
.asDfTrackData <- function (GRangesOrIRanges, PosBased, SelectSeq = NULL) {
    x <- GRangesOrIRanges
    
    ir2df <- function (IR, PosBased, AdditionalCols = NULL) {
        if (PosBased) {
            stopifnot(all(IRanges::width(IR) == 1))
            ans <- data.frame(.pos = IRanges::start(IR))
        }
        else ans <- data.frame(.start = IRanges::start(IR), .end = IRanges::end(IR))
        
        if (!is.null(AdditionalCols))
            ans <- as.data.frame(cbind(ans, AdditionalCols))
        ans
    }
    
    if (is(x, "IRanges")) {
        stopifnot(is.null(SelectSeq))
        ## But note that IRanges normally does not use meta columns, and it lacks
        ## methods (like `$`) to operate on metacolumns.
        df <- ir2df(IR = x, PosBased = PosBased,
                    AdditionalCols = GenomicRanges::mcols(x))
    }
    else if (is(x, "GRanges")) {
        gr <- GenomeInfoDb::keepSeqlevels(x, SelectSeq)
        ir <- GenomicRanges::ranges(gr)
        
        mcol <- GenomicRanges::mcols(gr)
        othcol <- data.frame(
            # seqnames may be unnecessary to be included
            .strand = GenomicRanges::strand(gr)
        )
        df <- ir2df(IR = ir, PosBased = PosBased, AdditionalCols = cbind(othcol, mcol))
    }
    else stop()
    
    df
}


## 2. Others                                --------



### TrackData Convert   --------------------------------------------------------

setMethod("asJC", signature = c(object = "RangeTrackData"),
          
    function (object, selectSeq) {
        posbased <- FALSE
        df.data <- .asDfTrackData(object@Source,
                                  PosBased = posbased, SelectSeq = selectSeq)
        .jcSyncData(.selfRetriever(df.data))
    }
)

setMethod("asJC", signature = c(object = "PosTrackData"),
          
    function (object, selectSeq) {
        posbased <- TRUE
        df.data <- .asDfTrackData(object@Source,
                                  PosBased = posbased, SelectSeq = selectSeq)
        .jcSyncData(.selfRetriever(df.data))
    }
)


###  TnT Tracks     ------------------------------------------------------------


TnTTrack <- setClass(
    "TnTTrack",
    slots = c(
        Spec = "list",
        Data = "TrackData",
        Display = "list"
    )
)



compileTrack <- function (tntTrack, prefSpec = NULL, prefDis = NULL, selectSeq = NULL) {
    
    .combinePref <- function (lst, pref) {
        # Subsititute "NULL" elements with the corresponding values in "pref"
        # TODO:
        #   Dispatch different "pref" based on name of the first element of "lst" so that
        #   different types of track will have corresponding preference (a theme setting).
        null.lst <- lst[unlist(lapply(lst, is.null))]
        fit.pref <- pref[names(pref) %in% names(null.lst)]
        lst[names(fit.pref)] <- fit.pref
        lst
    }
    
    jc.spec    <- asJC(.combinePref(tntTrack@Spec, prefSpec))
    jc.display <- jc(display = asJC(.combinePref(tntTrack@Display, prefDis)))
    jc.data    <- jc(data    = asJC(tntTrack@Data, selectSeq = selectSeq))
    
    c(jc.spec, jc.display, jc.data)
}


###  Track Construction     ----------------------------------------------------

## TODO: composite track, will it fit into the TnTTrack class?

## TODO: These functions will be directly exported to create tracks, thus we need
##       to add more arguments for each type.

## Data-less tracks ---- location track and axis track
setClass("DataLessTrack", contains = "TnTTrack")

setClass("LocationTrack", contains = "DataLessTrack")
LocationTrack <- function (height = 30, color = "white") {
    new("LocationTrack",
        Spec = list(
            # TODO:
            #   1. Set color to NULL? So that the color can be later modified with pref,
            #      also for AxisTrack.
            #   2. Is there any need to set label? Also for AxisTrack.
            tnt.board.track = NoArg,
            color = color, height = height,
            label = NULL, id = NULL
        ),
        Data = NoTrackData(),
        Display = list(tnt.board.track.feature.location = NoArg)
    )
}

setClass("AxisTrack", contains = "DataLessTrack")
AxisTrack <- function (orientation = c("top", "bottom"),
                       height = 30, color = "white") {
    orientation <- match.arg(orientation)
    new("AxisTrack",
        Spec = list(
            tnt.board.track = NoArg,
            color = color, height = height,
            label = NULL, id = NULL
        ),
        Data = NoTrackData(),
        Display = list(
            tnt.board.track.feature.axis = NoArg,
            orientation = orientation
        )
    )
}



## At current stage, we may not need to consider index of the data

## Block track
setClass("BlockTrack", contains = "TnTTrack")
BlockTrack <- function (data) {
    trackdata <- new("RangeTrackData", Source = data)
    new("BlockTrack",
        Spec = list(
            tnt.board.track = NoArg,
            color = NULL, height = NULL,
            label = NULL, id = NULL
        ),
        Data = trackdata,
        Display = list(
            ## TODO: Change the default name convension of "start" and "end"
            tnt.board.track.feature.block = NoArg,
            color = NULL
        )
    )
}



## Pin track
setClass("PinTrack", contains = "TnTTrack")
PinTrack <- function (data) {
    ## TODO:   Note that pin track will require an additional "val" column
    trackdata <- new("PosTrackData", Source = data)
    new("PinTrack",
        Spec = list(
            tnt.board.track = NoArg,
            color = NULL, height = NULL,
            label = NULL, id = NULL
        ),
        Data = trackdata,
        Display = list(
            ## TODO: Change the default name convension of "pos" and "val"
            tnt.board.track.feature.pin = NoArg,
            color = NULL,
            domain = NULL # TODO: When to set domain? Or just use default value?
        )
    )
}


## vline track
setClass("VlineTrack", contains = "TnTTrack")
VlineTrack <- function (data) {
    trackdata <- new("PosTrackData", Source = data)
    new("VlineTrack",
        Spec = list(
            tnt.board.track = NoArg,
            color = NULL, height = NULL,
            label = NULL, id = NULL
        ),
        Data = trackdata,
        Display = list(
            ## TODO: Change the default name convension of "pos"
            tnt.board.track.feature.vline = NoArg,
            color = NULL
        )
    )
}







###  TnT Board      ------------------------------------------------------------













