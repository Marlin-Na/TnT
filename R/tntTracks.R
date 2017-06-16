

# A TnT Track consists of three slots:
#   o Spec:
#       Class "TrackSpec" with three slots (i.e. "background", "height", "label").
#       It describes the common options across tracks (background color, height
#       and label for each track).
#
#   o Data:
#       Class "TrackData", it is a virtual class with subclasses that have different
#       methods for conversion to JavaScript. Currently all subclasses also extend
#       "GRanges" class.
#
#   o Display:
#       A list, describes the display options for different types of track,
#       e.g. color of the feature, height "domain" of AreaTrack, and in future, will
#            also include the settings for tooltips
#
# TnT Track supports `seqinfo` and `seqinfo<-` method so that before compilation,
# only track data on the intended chromosome can be kept.
#
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
#       It is not suppied when constructing the board.
#       But before compilation, it is determined by the "seqlength" of "seqinfo"
#       from ViewRange and also the track list, if not found, use value of
#       minimal and maximal range of track data list.
#   o ZoomAllow
#       Class "IRanges" that describes the minimal and maximal extent of the
#       board (i.e. the limit when zooming in and out).
#       Like CoordRange, it is not setted when constructing board.
#       Before compilation, the maximal extent is set to width of CoordRange, the
#       minimal extent is set to 10.
#   o TrackList
#       List of tracks.
# 
# In summary, before compilation, TnT should do the following work:
#   1. Drop trackdata that are not on the seqlevel (chromosome) of ViewRange.
#   2. Determine the coordinate range and maximal limit of zooming
#   3. Check TrackSpec of each track, fill with default values if some of them
#      are not specified.
#   Finally, the TnTBoard object can be compiled to a valid definition of TnT instance.









## Template for JS Callback and Promise     ------------------------------------

#' @export
JSCallback <- function (result, toJSON = TRUE) {
    # This function provides a template for constructing
    # a JS callback as a data retriever
    
    if (is(result, "JavaScript"))
        retstring <- result
    else if (toJSON)
        ## TODO:
        ## We may want to firstly convert the dataframe by cols, then use
        ## HTMLWidgets.dataframeToD3() to convert it on JS side.
        ## Refer: http://www.htmlwidgets.org/develop_advanced.html#htmlwidgets.dataframetod3
        retstring <- jsonlite::toJSON(result, dataframe = "rows", pretty = TRUE)
    else
        retstring <- .convertToJSChar(result)
    
    jsstring <- sprintf("function () {  return ( %s )  }", retstring)
    JavaScript(jsstring)
}

#' @export
.JSONMap <- function (colname, constant) {
    stopifnot(any(missing(colname), missing(constant)))
    if (missing(constant)) {
        # TODO: use toJSON to perform escape
        condfilter <- paste(sprintf('["%s"]', colname), collapse = "")
        string <- sprintf('function (d) { return (d%s); }', condfilter)
        ans <- JavaScript(string)
    }
    else
        ans <- constant
    ans
}
# Example
if (interactive()) .JSONMap(colname = c("data", "start"))







###  Track Data   ##############################################################


#### TrackData classes      ========
setClass("TrackData")

setClass("NoTrackData", contains = c("NULL", "TrackData"))
setClass("RangeTrackData", contains = c("GRanges", "TrackData"))
setClass("PosTrackData", contains = c("RangeTrackData"))
setClass("GeneTrackData", contains = "RangeTrackData")



#### TrackData constructors     ========

#' @export
NoTrackData <- function () new("NoTrackData")

#' @export
RangeTrackData <- function (range, tooltip = mcols(range)) {
    tooltip <- as.data.frame(tooltip)
    if (is(range, "IRanges")) {
        range <- GRanges(seqnames = "UnKnown", ranges = range, strand = "*")
    }
    range <- as(range, "GRanges")
    # Avoid possible duplicated colname
    while (!is.null(range$tooltip))
        range$tooltip <- NULL
    range$tooltip <- tooltip
    new("RangeTrackData", range)
}

#' @export
PosTrackData <- function (pos, tooltip = mcols(pos)) {
    tooltip <- as.data.frame(tooltip)
    trackdata <- RangeTrackData(range = pos, tooltip = tooltip)
    trackdata <- as(trackdata, "PosTrackData")
    validObject(trackdata) # Ensure all the width equals to one
    trackdata
}

#' @export
GeneTrackDataFromTxDb <- function (txdb, seqlevel = seqlevels(txdb)) {
    seqlevel.ori <- seqlevels(txdb)     # Set and restore the seqlevels
    seqlevels(txdb) <- seqlevel         #+++++++++++++++++++++++++++++++++++++++
    
    # TODO: use "single.strand.genes.only = FALSE" ?
    gr <- GenomicFeatures::genes(txdb)
    # We must restore the seqlevel of the txdb since it is a reference class
    seqlevels(txdb) <- seqlevel.ori     #---------------------------------------
    
    gr$display_label <- {
        strands <- strand(gr)
        ifelse(strands == "+", paste("Gene", gr$gene_id, ">"),
            ifelse(strands == "-", paste("<", "Gene", gr$gene_id),
                paste("Gene", gr$gene_id)))
    }
    gr$id <- {
        # Gene id may not be unique if "single.strand.genes.only = FALSE"
        li.geneid <- split(gr$gene_id, list(seqnames(gr), strand(gr)))
        mod.li.geneid <- lapply(li.geneid, function (x) make.unique(x))
        unsplit(mod.li.geneid, list(seqnames(gr), strand(gr)))
    }
    # TODO: what about tooltip?
    gr$tooltip <- data.frame(
        "Location" = as.character(gr),
        "Gene ID" = gr$gene_id,
        check.names = FALSE
    )
    gr$gene_id <- NULL
    
    new("GeneTrackData", gr)
}

setValidity("PosTrackData",
    function (object) {
        if (all(width(object) == 1)) TRUE
        else return("Width of PosTrackData should be 1.")
    }
)


#### TrackData Compilation  ========

#' @export
setGeneric("compileTrackData",
           function (trackData) standardGeneric("compileTrackData"))

#' @export
setMethod("compileTrackData", signature = "data.frame",
    function (trackData) {
        removeAsIs <- function (df) {
            # The nested data frame converted from GRanges/DataFrame will have a
            # "AsIs" class, which will cause the data frame can not be shown and
            # can not be converted to JSON correctly.
            for (i in seq_along(df)) {
                element <- df[[i]]
                if (is.data.frame(element)) {
                    class(element) <- class(element)[class(element) != "AsIs"]
                    df[[i]] <- removeAsIs(element)
                }
            }
            df
        }
        df <- removeAsIs(trackData)
        
        js.retriever <- JSCallback(df)
        jc.syncdata <-  jc(tnt.board.track.data.sync = ma(),
                           retriever = js.retriever)
        jc.syncdata
    }
)

#' @export
setMethod("compileTrackData", signature = "NoTrackData",
    function (trackData)
        jc(tnt.board.track.data.empty = NoArg)
) 

#' @export
setMethod("compileTrackData", signature = "RangeTrackData",
    function (trackData) {
        ## TODO: Have to select seq
        stopifnot(length(unique(seqnames(trackData))) == 1)
        df <- as.data.frame(trackData)[c("start", "end", "strand", "tooltip")]
        compileTrackData(df)
    }
)

#' @export
setMethod("compileTrackData", signature = "PosTrackData",
    function (trackData) {
        ## TODO: Have to select seq
        stopifnot(length(unique(seqnames(trackData))) == 1)
        stopifnot(all(width(trackData) == 1))
        
        df <- as.data.frame(trackData)[c("start", "strand", "val", "tooltip")]
        df <- S4Vectors::rename(df, c(start = "pos"))
        compileTrackData(df)
    }
)

#' @export
setMethod("compileTrackData", signature = "GeneTrackData",
    function (trackData) {
        stopifnot(length(unique(seqnames(trackData))) == 1)
        
        df <- as.data.frame(trackData)
        df[c("seqnames", "width", "strand")] <- NULL
        
        compileTrackData(df)
    }
)

###  TnT Tracks   ##############################################################

#### Track Spec Class       ========
#  TrackSpec contains the common track options, namely
#      o color   background color
#      o height
#      o label
#      o id
#  Before compilation, these slots need to be consolidated across tracks:
#      1. If the background color is not specified for one track, use the
#         default option or from the board option.
#      ~ 2. Make the id unique across tracks. ~ Forget about id
setClass("TrackSpec",
    slots = c(
        background = "character", # renamed to color upon compilation
        height = "numeric",
        label = "character"
        #id = "character"
    )
)

#' @export
TrackSpec <- function (background = character(0), height = numeric(0),
                       label = character(0), .trackspecfromlist) {
    if (!missing(.trackspecfromlist)) {
        ans <- do.call("TrackSpec", .trackspecfromlist)
        return(ans)
    }
    new("TrackSpec", background = background, height = height, label = label)
}


#' @export
setMethod("as.list", signature = "TrackSpec",
    function (x) {
        li.spec <- list(
            background = x@background,
            height     = x@height,
            label      = x@label
            #id         = x@id
        )
        li.spec
    }
)
#' @export
.compileTrackSpec <- function (trackspec) {
    li.spec <- as.list(trackspec)
    stopifnot(all(lengths(li.spec) == 1))
    # Rename background to color
    names(li.spec)[names(li.spec) == "background"] <- "color"
    # Add header
    li.spec <- c(list(tnt.board.track = ma()), li.spec)
    jc.spec <- asJC(li.spec)
    jc.spec
}
# Example
if (interactive()) local({
    .compileTrackSpec(TrackSpec("white", 30, label = "track"))
})
#' @export
.prepareTrackSpec <- function (tntboard) {
    # TODO: Track spec should be consolidated with regard to the setting from board
    
    # Fill with default values
    # TODO: how to set withregard to track type?
    defaultspec <- list(
        background = "white",
        height     = 100
    )
    tracklist <- tntboard@TrackList
    for (i in seq_along(tracklist)) {
        track <- tracklist[[i]]
        tspec <- as.list(track@Spec)
        null.spec <- tspec[lengths(tspec) == 0]
        if (length(null.spec)) {
            fit.default <-  defaultspec[names(defaultspec) %in% names(null.spec)]
            tspec[names(fit.default)] <- fit.default
            track@Spec <- TrackSpec(.trackspecfromlist = tspec)
            tracklist[[i]] <- track
        }
    }
    tntboard@TrackList <- tracklist
    tntboard
}


#### Track classes          ========
setClass("TnTTrack", slots = c(Spec = "TrackSpec", Data = "TrackData", Display = "list"))

setClass("BlockTrack", contains = "TnTTrack", slots = c(Data = "RangeTrackData"))
setClass("PinTrack", contains = "TnTTrack", slots = c(Data = "PosTrackData"))
setClass("GeneTrack", contains = "TnTTrack", slots = c(Data = "GeneTrackData"))

#### Seqinfo Methods        ========
#' @export
setMethod("seqinfo", signature = c("TnTTrack"),
    function (x) seqinfo(x@Data)
)
#' @export
setMethod("seqinfo<-", signature = c(x = "TnTTrack"),
    function (x, new2old, force, pruning.mode, value) {
        x@Data <- `seqinfo<-`(x = x@Data, new2old = new2old,
                              force = force, pruning.mode = pruning.mode, value = value)
        x
    }
)

#### Track Constructor      ========

#' @export
BlockTrack <- function (range, label = deparse(substitute(range)),
                        tooltip = mcols(range), color = NULL, ...) {
    force(label)
    spec <- TrackSpec(label = label, ...)
    data <- RangeTrackData(range = range, tooltip = tooltip)
    display <- list(
        tnt.board.track.feature.block = ma(),
        color = color
        # from = .JSONMap(colname = "from"),
        # to = .JSONMap(colname = "to")
    )
    new("BlockTrack", Spec = spec, Data = data, Display = display)
}

#' @export
PinTrack <- function (pos, value = mcols(pos)$value, domain = c(0, max(value)),
                      label = deparse(substitute(pos)),
                      tooltip = mcols(pos), color = NULL, ...) {
    if (is.null(value))
        stop("Value (i.e. height) at each position not specified.")
    force(domain)
    force(label)
    stopifnot(length(domain) == 2)
    data <- PosTrackData(pos = pos, tooltip = tooltip)
    data$val <- value
    spec <- TrackSpec(label = label, ...)
    display <- list(
        tnt.board.track.feature.pin = ma(),
        domain = domain,
        color = color
    )
    new("PinTrack", Spec = spec, Data = data, Display = display)
}

#' @export
GeneTrack <- function (txdb, seqlevel = seqlevels(txdb),
                       label = deparse(substitute(txdb)), # TODO: tooltip?
                       color = NULL, ...) {
    force(label)
    spec <- TrackSpec(label = label, ...)
    data <- GeneTrackDataFromTxDb(txdb = txdb, seqlevel = seqlevel)
    display <- list(
        tnt.board.track.feature.genome.gene = ma(),
        color = color
    )
    new("GeneTrack", Data = data, Spec = spec, Display = display)
}


#### Track Compilation      ========

# setGeneric("compileTrack",
#            function (tntTrack) standardGeneric("compileTrack"))

#' @export
compileTrack <- function (tntTrack) {
    jc.spec <- .compileTrackSpec(tntTrack@Spec)
    jc.display <- jc(display = asJC(tntTrack@Display))
    jc.data <- jc(data = compileTrackData(tntTrack@Data))
    c(jc.spec, jc.display, jc.data)
}




###   TnT Board     ############################################################

####  Class Def for TnT Board   ========
setClass("TnTBoard",
    slots = c(
        ViewRange = "GRanges",
        CoordRange = "IRanges",
        ZoomAllow = "IRanges",
        # TODO: How does the width correspond to pixel?
        #       In fact, we should not specify the width here,
        #       but use the resize method on JS side.
        #Width = "integer",
        # Drag should always be allowed.
        #AllowDrag = "logical",
        TrackList = "list"
    )
)

#### TnT Board Constructor      ========
#' @export
TnTBoard <- function (tracklist, viewrange = GRanges()) {
    stopifnot(
        all(sapply(tracklist, inherits, what = "TnTTrack"))
    )
    b <- new("TnTBoard", ViewRange = viewrange, CoordRange = IRanges(),
             ZoomAllow = IRanges(), TrackList = tracklist)
    b
}

#### TnT Board Compilation      ========
#' @export
compileBoard <- function (tntboard) {
    ## Modification to tntboard
    b <- .selectTrackSeq(tntboard)
    b <- .determineCoordRange(b)
    b <- .prepareTrackSpec(b)
    
    spec <- .compileBoardSpec(b)
    tklst <- .compileTrackList(b)
    tntdef <- c(spec, tklst)
    tntdef
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
.selectTrackSeq <- function (tntboard) {
    tracklist0 <- tntboard@TrackList
    viewrange0 <- tntboard@ViewRange
    if (length(viewrange0)) {
        stopifnot(length(viewrange0) == 1)
        seqlv <- seqlevelsInUse(viewrange0)
    }
    else {
        # TODO: Find a proper view range
        stop()
    }
    tracklist <- lapply(tracklist0, keepSeqlevels,
                        value = seqlv, pruning.mode = "coarse")
    tntboard@TrackList <- tracklist
    tntboard
}

#' @export
.determineCoordRange <- function (tntboard) {
    coordrange0 <- tntboard@CoordRange
    
    seqlens <- vapply(tntboard@TrackList, seqlengths, integer(1))
    seqlen <- unique(seqlens[!is.na(seqlens)])
    
    if (length(seqlen) >= 2L) {
        stop()
    }
    else if (length(seqlen) == 1L) {
        coordrange <- IRanges(start = 0L, end = seqlen)
    }
    else {
        li.rgs <- lapply(tntboard@TrackList,
            function (track) range(track@Data)
        )
        rg <- do.call('c', li.rgs)
        rg <- range(rg)
        stopifnot(length(rg) == 1)
        coordrange <- ranges(rg)
    }
    tntboard@CoordRange <- coordrange
    tntboard@ZoomAllow <- IRanges(start = 10, end = width(coordrange))
    tntboard
}

#' @export
.compileTrackList <- function (tntboard) {
    tracklist <- tntboard@TrackList
    li.jc <- lapply(tracklist, compileTrack)
    names(li.jc) <- replicate(length(li.jc), "add_track")
    jc <- asJC(li.jc)
    jc
}

## Printing     ====
#' @export
setMethod("show", signature = c("TnTBoard"),
    function (object) {
        # TODO: Also have to edit renderTnT and TnTOutput
        tntdef <- compileBoard(object)
        print(TnT(tntdef))
    }
)
#' @export
knit_print.TnTBoard <- function (x, ..., options = NULL) {
    # Redirect method to htmlwidget
    x <- TnT(compileBoard(x))
    knitr::knit_print(x, ..., options = options)
}

## EXAMPLE      ====
if (FALSE) local({
    data("cpgIslands", package = "Gviz")
    txdb <- TxDb.Hsapiens.UCSC.hg19.knownGene::TxDb.Hsapiens.UCSC.hg19.knownGene
    viewrg <- range(cpgIslands)
    ct <- BlockTrack(cpgIslands, color= "blue", height = 30)
    gt <- GeneTrack(txdb, color = "red", height = 250)
    b <- TnTBoard(tracklist = list(ct), viewrange = viewrg)
    b
    b <- TnTBoard(tracklist = list(gt, ct), viewrange = viewrg)
    b
})








###   TnT Genome    ############################################################

# setClass("TnTGenome", contains = "TnTBoard",
#     slots = c(
#         
#     )
# )



