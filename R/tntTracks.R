


### TrackData Class     --------------------------------------------------------

setClass("TrackData", slots = c(Source = "ANY"))


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
    
    ir2df <- function (IRanges, PosBased, AdditionalCols) {
        if (PosBased) {
            stopifnot(all(IRanges::width(x) == 1))
            ans <- data.frame(.pos = IRanges::start(x))
        }
        else ans <- data.frame(.start = IRanges::start(x), .end = IRanges::end(x))
        
        if (!missing(AdditionalCols))
            ans <- as.data.frame(cbind(ans, AdditionalCols))
        ans
    }
    
    if (is(x, "IRanges")) {
        stopifnot(is.null(SelectSeq))
        ## But note that IRanges normally does not use meta columns, and it lacks
        ## methods (like `$`) to operate on metacolumns.
        df <- ir2df(IRanges = x, PosBased = PosBased,
                    AdditionalCols = GenomicRanges::mcols(IRanges))
    }
    else if (is(x, "GRanges")) {
        gr <- GenomeInfoDb::keepSeqlevels(x, SelectSeq)
        ir <- GenomicRanges::ranges(gr)
        
        mcol <- GenomicRanges::mcols(gr)
        othcol <- data.frame(
            # seqnames may be unnecessary to be included
            .strand = GenomicRanges::strand(gr)
        )
        df <- ir2df(IRanges = ir, PosBased = PosBased, AdditionalCols = cbind(othcol, mcol))
    }
    else stop()
    
    df
}


## 2. Others                                --------



### TrackData Convert   --------------------------------------------------------

setMethod("asJC", signature = c(object = "RangeTrackData"),

    function (object, ...) {
        
        localf <- function (object, selectSeq, ...) {
            posbased <- FALSE
            df.data <- .asDfTrackData(object@Source,
                                      PosBased = posbased, SelectSeq = selectSeq)
            .jcSyncData(.selfRetriever(df.data))
        }
        
        checkArgs(localf, ...); localf(object, ...)
    }
)

setMethod("asJC", signature = c(object = "PosTrackData"),
          
    function (object, ...) {
        
        localf <- function (object, selectSeq, ...) {
            posbased <- TRUE
            df.data <- .asDfTrackData(object@Source,
                                      PosBased = posbased, SelectSeq = selectSeq)
            .jcSyncData(.selfRetriever(df.data))
        }
        
        checkArgs(localf, ...); localf(object, ...)
    }
)


###  TnT Tracks     ------------------------------------------------------------


setClass(
    "TnTTrack",
    slots = c(
        Spec = "list",
        Data = "TrackData",
        Display = "list"
    )
)
TnTTrack <- function (Spec, Data, Display) {
    new("TnTTrack", Spec = Spec, Data = Data, Display = Display)
}


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


###  TnT Board      ------------------------------------------------------------













