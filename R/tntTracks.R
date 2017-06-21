

# A TnT Track consists of Five slots:
#   o Background:       Background color
#   o Height:           Height of the track
#   o Label:            Label of the track
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
# In summary, before compilation, TnT should do the following work
# (see function compileBoard):
#   1. Drop trackdata that are not on the seqlevel (chromosome) of ViewRange.
#   2. Determine the coordinate range and maximal limit of zooming
#   3. Consolidate background color:
#       If background color for any track is set as NULL, then replace it with
#       default value -- the background color from other tracks Or "white" if none
#       of them is available.
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
setClass("TxTrackData", contains = "RangeTrackData")



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
strandlabel <- function (labels, strands) {
    ifelse(strands == "+", paste(labels, ">"),
        ifelse(strands == "-", paste("<", labels), labels))
}


#' @export
GeneTrackDataFromTxDb <- function (txdb, seqlevel = seqlevels(txdb)) {
    seqlevel.ori <- seqlevels(txdb)
    seqlevels(txdb) <- seqlevel         # Set and restore the seqlevels
    # We must restore the seqlevel of the txdb since it is a reference class
    on.exit(seqlevels(txdb) <- seqlevel.ori)
    
    # TODO: use "single.strand.genes.only = FALSE" ?
    gr <- GenomicFeatures::genes(txdb)
    
    gr$display_label <-
        strandlabel(labels = paste("Gene", gr$gene_id), strands = strand(gr))
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


#' @export
TxTrackDataFromTxDb <- function (txdb, seqlevel = seqlevels(txdb),
                                 use.data.table = TRUE) {
    ## Set and restore seqlevels of txdb
    seqlevel.ori <- seqlevels(txdb)
    seqlevels(txdb) <- seqlevel
    on.exit(seqlevels(txdb) <- seqlevel.ori)
    
    ## Extract features from txdb
    gr.txs <- transcripts(txdb, columns = c("tx_id", "tx_name", "gene_id"))
    gr.txs$gene_id <- as.character(gr.txs$gene_id) # contain NA values
    gr.cds <- cds(txdb, columns = c("tx_id"))
    gr.exons <- exons(txdb, columns = c("tx_id"))
    
    ## Unlist tx_id column of gr.cds and gr.exons
    flatGRByTxID <- function (gr) {
        txid <- gr$tx_id
        gr$tx_id <- NULL
        gr <- rep(gr, lengths(txid))
        gr$tx_id <- unlist(txid)
        gr
    }
    gr.cds <- flatGRByTxID(gr.cds)
    gr.exons <- flatGRByTxID(gr.exons)
    
    ## Area of coding exons will be filled with color, non-coding exons will not,
    ## so CDS can mask on the parent exons.
    gr.cds$coding <- if (length(gr.cds)) TRUE else logical(0)
    gr.exons$coding <- if (length(gr.exons)) FALSE else logical(0)
    
    ## Combine cds and exons
    combined.exons <- c(gr.exons, gr.cds)
    
    ## Prepare a list of data frame as a meta column of the result
    df.exons <- as.data.frame(combined.exons)
    df.exons$tx_start <- {
        txstart <- setNames(start(gr.txs), as.character(gr.txs$tx_id))
        txstart[as.character(df.exons$tx_id)]
    }
    df.exons$offset <- df.exons$start - df.exons$tx_start
    factor_txid <- df.exons$tx_id
    df.exons <- df.exons[c("start", "end", "offset", "coding", "tx_id")]
    
    # Slow in this step
    if (use.data.table && requireNamespace("data.table"))
        # Relative faster approach
        ldf.exons <- data.table:::split.data.table(
            data.table::as.data.table(df.exons),
            # use "by" not "f" to be relative faster
            by = "tx_id", keep.by = FALSE, flatten = FALSE
        )
    else
        ldf.exons <- split(
            df.exons[names(df.exons) != "tx_id"],
            factor_txid
        )
    
    gr.txs$exons <- {
        toassign <- vector("list", length = length(gr.txs))
        names(toassign) <- as.character(gr.txs$tx_id)
        toassign[names(ldf.exons)] <- ldf.exons
        toassign
    }
    gr.txs$display_label <- {
        labels <- ifelse(is.na(gr.txs$gene_id), gr.txs$tx_name,
                         paste(gr.txs$gene_id, gr.txs$tx_name))
        strandlabel(labels, strand(gr.txs))
    }
    gr.txs$key <- as.integer(gr.txs$tx_id)
    gr.txs$id <- as.character(gr.txs$tx_id)
    gr.txs$tooltip <- data.frame(
        "Location" = as.character(gr.txs),
        "Transcript Name" = gr.txs$tx_name,
        check.names = FALSE
    )
    gr.txs$gene_id <- NULL
    new("TxTrackData", gr.txs)
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
# EXAMPLE
if (FALSE) local({
    compileTrackData(head(iris))
})

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
# EXAMPLE
if (FALSE) local({
    data <- RangeTrackData(range = IRanges::IRanges(1:4, 5:8),
                           tooltip = data.frame(start = 1:4, width = 5))
    compileTrackData(data)
})

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

#' @export
setMethod("compileTrackData", signature = "TxTrackData",
    function (trackData) {
        stopifnot(length(unique(seqnames(trackData))) == 1)
        
        df <- as.data.frame(trackData)
        df <- df[c("start", "end", "display_label", "key", "id", "exons", "tooltip")]
        
        compileTrackData(df)
    }
)

# EXAMPLE
if (FALSE) local({
    txdb <- TxDb.Hsapiens.UCSC.hg19.knownGene::TxDb.Hsapiens.UCSC.hg19.knownGene
    tdata <- TxTrackDataFromTxDb(txdb, "chrUn_gl000221")
    compileTrackData(tdata)
})



###  TnT Tracks   ##############################################################

#### Track classes          ========
setClassUnion("ScalarCharacterOrNull", c("ScalarCharacter", "NULL"))
setClass("TnTTrack", slots = c(
    # TODO: add ID slot?
    Background = "ScalarCharacterOrNull",
    Height = "ScalarNumeric",
    Label = "ScalarCharacterOrNull",
    
    Data = "TrackData",
    Display = "list")
)

setClass("BlockTrack", contains = "TnTTrack", slots = c(Data = "RangeTrackData"))
setClass("PinTrack", contains = "TnTTrack", slots = c(Data = "PosTrackData"))
setClass("GeneTrack", contains = "TnTTrack", slots = c(Data = "GeneTrackData"))
setClass("TxTrack", contains = "TnTTrack", slots = c(Data = "TxTrackData"))

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
.mkScalarOrNull <- function (x)
    if (is.null(x)) NULL else Biobase::mkScalar(x)

#' @export
BlockTrack <- function (range, label = deparse(substitute(range)),
                        tooltip = mcols(range), color = NULL, background = NULL,
                        height = 30) {
    label <- .mkScalarOrNull(label)
    background <- .mkScalarOrNull(background)
    height <- Biobase::mkScalar(height)
    data <- RangeTrackData(range = range, tooltip = tooltip)
    display <- list(
        tnt.board.track.feature.block = ma(),
        color = color
        # from = .JSONMap(colname = "from"),
        # to = .JSONMap(colname = "to")
    )
    new("BlockTrack", Label = label, Background = background, Height = height,
        Data = data, Display = display)
}

#' @export
PinTrack <- function (pos, value = mcols(pos)$value, domain = c(0, max(value)),
                      label = deparse(substitute(pos)),
                      tooltip = mcols(pos), color = NULL, background = NULL,
                      height = 40) {
    if (is.null(value))
        stop("Value (i.e. height) at each position not specified.")
    label <- .mkScalarOrNull(label)
    background <- .mkScalarOrNull(background)
    height <- Biobase::mkScalar(height)
    force(domain)
    stopifnot(length(domain) == 2)
    data <- PosTrackData(pos = pos, tooltip = tooltip)
    data$val <- value
    display <- list(
        tnt.board.track.feature.pin = ma(),
        domain = domain,
        color = color
    )
    new("PinTrack", Label = label, Background = background, Height = height,
        Data = data, Display = display)
}

#' @export
GeneTrack <- function (txdb, seqlevel = seqlevels(txdb),
                       label = deparse(substitute(txdb)), # TODO: tooltip?
                       color = NULL, background = NULL, height = 100) {
    label <- .mkScalarOrNull(label)
    background <- .mkScalarOrNull(background)
    height <- Biobase::mkScalar(height)
    data <- GeneTrackDataFromTxDb(txdb = txdb, seqlevel = seqlevel)
    display <- list(
        tnt.board.track.feature.genome.gene = ma(),
        color = color
    )
    new("GeneTrack", Label = label, Background = background, Height = height,
        Data = data, Display = display)
}

#' @export
TxTrack <- function (txdb, seqlevel = seqlevels(txdb),
                     label = deparse(substitute(txdb)),
                     color = NULL, background = NULL, height = 100) {
    label <- .mkScalarOrNull(label)
    background <- .mkScalarOrNull(background)
    height <- Biobase::mkScalar(height)
    data <- TxTrackDataFromTxDb(txdb, seqlevel = seqlevel, use.data.table = TRUE)
    display <- list(
        tnt.board.track.feature.genome.transcript = ma(),
        # TODO: this is not the correct approach
        color = if (is.null(color)) js('function (t) { return "red" }')
                else js(sprintf('function (t) { return "%s" }', color))
    )
    new("TxTrack", Label = label, Background = background, Height = height,
        Data = data, Display = display)
}


#### Track Tooltip          ========

#' @export
setGeneric("tooltip", function (x) standardGeneric("tooltip"))
#' @export
setGeneric("tooltip<-", function (x, value) standardGeneric("tooltip<-"))

setMethod("tooltip", signature = "TrackData",
    function (x) {
        x$tooltip
    }
)
setMethod("tooltip", signature = "TnTTrack",
    function (x) {
        tooltip(x@Data)
    }
)
setMethod("tooltip<-", signature = c(x = "TrackData", value = "data.frame"),
    function (x, value) {
        x$tooltip <- value
        x
    }
)
setMethod("tooltip<-", signature = c(x = "TnTTrack", value = "data.frame"),
    function (x, value) {
        tooltip(x@Data) <- value
        x
    }
)

# EXAMPLE
if (FALSE) local({
    # TODO: to fix this error when deparse produce non-scalar character
    t <- BlockTrack(GRanges("chr12", IRanges(1:15 * 100, width = 10), label = paste("range", 1:15)))
    gr <- GRanges("chr12", IRanges(1:15 * 100, width = 10), label = paste("range", 1:15))
    t <- BlockTrack(gr)
    t@Data
    # Warning messages:
    #     1: In (function (..., row.names = NULL, check.rows = FALSE, check.names = TRUE,  :
    #        row names were found from a short variable and have been discarded
    #     2: In (function (..., row.names = NULL, check.rows = FALSE, check.names = TRUE,  :
    #        row names were found from a short variable and have been discarded
    tooltip(t)
    tooltip(t) <- c(1:15)
    tooltip(t) <- data.frame(start = start(gr))
    
    compileTrack(t)
    TnTBoard(list(t), viewrange = range(t@Data))
})


#### Track Compilation      ========

# setGeneric("compileTrack",
#            function (tntTrack) standardGeneric("compileTrack"))

#' @export
compileTrack <- function (tntTrack) {
    li.spec <- list(
        tnt.board.track = ma(),
        label  = tntTrack@Label,
        color  = tntTrack@Background,  # rename to "color"
        height = tntTrack@Height
    )
    
    jc.spec <- asJC(li.spec)
    jc.display <- jc(display = asJC(tntTrack@Display))
    jc.data <- jc(data = compileTrackData(tntTrack@Data))
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
    b <- .consolidateBackground(b)
    
    spec <- .compileBoardSpec(b)
    tklst <- .compileTrackList(b)
    tntdef <- c(spec, tklst)
    tntdef
}

.consolidateBackground <- function (tntboard) {
    # By the time of construction of each tnt track, the background color can
    # be either set to "NULL" or a scalar character.
    #
    # Before compilation of tntboard, this function examines these settings in
    # each track, replace the NULLs with a more suitable value.
    tracklist <- tntboard@TrackList
    li.colors <- lapply(tracklist, slot, name = "Background")
    colors <- unique(unlist(li.colors))
    
    if (length(colors) == 0L || length(colors) >= 2L)
        default <- Biobase::mkScalar("white")
    else
        default <- Biobase::mkScalar("colors")
    
    tracklist <- lapply(tracklist, replace = default,
        function (track, replace) {
            if (is.null(track@Background))
                track@Background <- replace
            track
        }
    )
    tntboard@TrackList <- tracklist
    tntboard
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
        # TODO: Have to provide renderTnT and TnTOutput
        widget <- trackWidget(object, elementId = NULL)
        print(widget)
    }
)
#' @export
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








###   TnT Genome    ############################################################

# setClass("TnTGenome", contains = "TnTBoard",
#     slots = c(
#         
#     )
# )



