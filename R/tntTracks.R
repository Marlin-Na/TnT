
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









## Templates for JS Callback and Promise     ------------------------------------



#' @export
tooltipCallback <- function (header, entries) {
    stopifnot(length(header) == 1)
    
    jc(tnr.tooltip_callback = ma(header, entries))
    
    
    #js.colmap <- {
    #    df.colmap <- data.frame(label = labels, colname = colnames)
    #    js(as.character(toJSON(df.colmap, pretty = 2)))
    #}
    #
    #js.func <- js(paste(collapse = "\n", c(
    #    '                                                     ',
    #    ' var getTooltipCall = function (d, header, colmap) { ',
    #    '     var rows = [];                                  ',
    #    '     for (var i = 0; i < colmap.length; i++) {       ',
    #    '         var colname = colmap[i].colname;            ',
    #    '         var row = {                                 ',
    #    '             "label": colmap[i].label,               ',
    #    '             "value": d.tooltip[colname]             ',
    #    '         };                                          ',
    #    '         rows.push(row);                             ',
    #    '     }                                               ',
    #    '     return { header: header, rows: rows };          ',
    #    ' };                                                  ',
    #    '                                                     '
    #)))
    #js.do <- asJS(jc(
    #    tnt.tooltip.table = ma(),
    #    width = 120,
    #    call = ma(
    #        js("this"),
    #        jc(getTooltipCall = ma(js("d"), header, js.colmap))
    #    )
    #))
    #callback <- sprintf('function (d) {\n  %s \n  %s; \n}',
    #                    js.func, js.do)
    #js(callback)
}
# EXAMPLE
if (interactive()) local({
    tooltipCallback(header = "Tooltip Header", entries = c("Start", "End", "Description"))
})








###  Track Data   ##############################################################


#### TrackData classes      ========
setClass("TrackData")

setClass("NoTrackData", contains = c("NULL", "TrackData"))
setClass("RangeTrackData", contains = c("GRanges", "TrackData"))
setClass("PosTrackData", contains = "RangeTrackData")
setClass("PosValTrackData", contains = "PosTrackData")
setClass("GeneTrackData", contains = "RangeTrackData")
setClass("TxTrackData", contains = "RangeTrackData")



#### TrackData constructors     ========

#' @export
NoTrackData <- function () new("NoTrackData")

#' @export
RangeTrackData <- function (range, tooltip = mcols(range)) {
    tooltip <-
        if (is.null(tooltip))
            data.frame(matrix( , nrow = length(range), ncol = 0))
        else 
            as.data.frame(tooltip, optional = TRUE)
    
    range <-
        if (is(range, "IRanges"))
            GRanges(seqnames = "UnKnown", ranges = range, strand = "*")
        else
            as(range, "GRanges")
    
    mcols(range) <- NULL
    range$tooltip <- tooltip
    new("RangeTrackData", range)
}

#' @export
PosTrackData <- function (pos, tooltip = mcols(pos)) {
    trackdata <- RangeTrackData(range = pos, tooltip = tooltip)
    trackdata <- as(trackdata, "PosTrackData")
    validObject(trackdata) # Ensure all the width equals to one
    trackdata
}

#' @export
PosValTrackData <- function (pos, val, tooltip = mcols(pos)) {
    mcols(pos) <- NULL
    
    trackdata <- RangeTrackData(range = pos, tooltip = tooltip)
    trackdata$val <- val
    
    trackdata <- as(trackdata, "PosValTrackData")
    validObject(trackdata)
    trackdata
}


#' @export
GeneTrackData <- function (range, labels = paste("Gene", mcols(range)$gene_id),
                           ids = make.unique(labels), tooltip = mcols(range)) {
    force(labels)
    force(ids)
    force(tooltip)
    mcols(range) <- NULL
    
    range <- RangeTrackData(range, tooltip)
    range$display_label <- strandlabel(labels, strand(range))
    range$id <- ids
    
    range <- as(range, "GeneTrackData")
    validObject(range)
    range
}
### EXAMPLE
if (FALSE) {
    txdb <- TxDb.Hsapiens.UCSC.hg19.knownGene::TxDb.Hsapiens.UCSC.hg19.knownGene
    gene <- genes(txdb, columns = c("gene_id", "tx_id"))
    GeneTrackData(gene[1:4])
    ir <- IRanges(1:3, width = 10)
    GeneTrackData(ir, labels = paste("gene", 1:3))
}

#' @export
GeneTrackDataFromTxDb <- function (txdb, seqlevel = seqlevels(txdb)) {
    seqlevel.ori <- seqlevels(txdb)
    seqlevels(txdb) <- seqlevel         # Set and restore the seqlevels
    # We must restore the seqlevel of the txdb since it is a reference class
    on.exit(seqlevels(txdb) <- seqlevel.ori)
    
    # TODO: use "single.strand.genes.only = FALSE" ?
    gr <- GenomicFeatures::genes(txdb)
    labels <- gr$gene_id
    tooltip <- data.frame(
        # TODO: choose proper tooltip
        "Location" = as.character(gr),
        "Gene ID" = gr$gene_id,
        check.names = FALSE
    )
    GeneTrackData(range = gr, labels = labels, tooltip = tooltip)
}
### EXAMPLE
if (FALSE) {
    txdb <- TxDb.Hsapiens.UCSC.hg19.knownGene::TxDb.Hsapiens.UCSC.hg19.knownGene
    GeneTrackDataFromTxDb(txdb, c("chrX", "chrY"))
}

#' @export
TxTrackDataFromGRangesList <- function (grl, tooltip = mcols(grl),
                                        labels = names(grl)) {
    force(tooltip)
    force(labels)
    stopifnot(nrow(tooltip) == length(grl))
    stopifnot(length(labels) == length(grl))
    tx <- range(grl)
    if (any(lengths(tx) > 1))
        stop("seqlevels and strands within each group are not consistent")
    
    gr.txs <- unlist(tx)
    mcols(gr.txs) <- NULL
    
    gr.txs$tooltip <- as.data.frame(tooltip, optional = TRUE)[
        seq_along(grl)[lengths(grl) != 0], ]
    gr.txs$key <- seq_along(grl)[lengths(grl) != 0]
    gr.txs$id <- as.character(gr.txs$key)
    gr.txs$display_label <- strandlabel(labels[lengths(grl) != 0], strand(gr.txs))
    
    gr.exons <- unlist(grl, use.names = FALSE)
    keys.gr.exons <- rep(seq_along(grl), lengths(grl))
    
    mcols(gr.exons) <- NULL
    gr.exons$coding <- if (length(gr.exons)) TRUE else logical(0)
    gr.exons$offset <-
        start(gr.exons) - start(gr.txs)[match(keys.gr.exons, gr.txs$key)]
    
    df.exons <- as.data.frame(gr.exons)[c("start", "end", "offset", "coding")]
    ldf.exons <- splitdf(df.exons, keys.gr.exons)[as.character(gr.txs$key)]
    gr.txs$exons <- ldf.exons
    new("TxTrackData", gr.txs)
}
### EXAMPLE
if (FALSE) {
    txdb <- TxDb.Hsapiens.UCSC.hg19.knownGene::TxDb.Hsapiens.UCSC.hg19.knownGene
    grl <- exonsBy(txdb)[1:3]
    a <- TxTrackDataFromGRangesList(grl)
    a[1]$key
    # When one of the GRanges is empty
    grl$`1` <- grl$`1`[numeric(0)]
    b <- TxTrackDataFromGRangesList(grl)
    b[1]$key
    compileTrackData(b)
}



#' @export
TxTrackDataFromTxDb <- function (txdb, seqlevel = seqlevels(txdb)) {
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
    df.exons <- as.data.frame(combined.exons, optional = TRUE)
    df.exons$tx_start <- {
        txstart <- setNames(start(gr.txs), as.character(gr.txs$tx_id))
        txstart[as.character(df.exons$tx_id)]
    }
    df.exons$offset <- df.exons$start - df.exons$tx_start
    factor_txid <- df.exons$tx_id
    df.exons <- df.exons[c("start", "end", "offset", "coding")]
    
    ldf.exons <- splitdf(df.exons, factor_txid)
    
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

setValidity("PosValTrackData",
    function (object) {
        if (is.null(mcols(object)$val))
            return("Missing 'val' meta-column in PosValTrackData")
        else TRUE
    }
)

setValidity("RangeTrackData",
    function (object) {
        if (!is.data.frame(object$tooltip))
            if (is.null(object$tooltip))
                return("Missing 'tooltip' meta-column in RangeTrackData")
            else
                return("The 'tooltip' meta-column should be a data frame")
        TRUE
    }
)

setValidity("GeneTrackData",
    function (object) {
        if (is.null(object$display_label))
            return("Missing 'display_label' meta-column in GeneTrackData")
        if (is.null(object$id))
            return("Missing 'id' meta-column in GeneTrackData")
        TRUE
    }
)

setValidity("TxTrackData",
    function (object) {
        if (is.null(object$display_label))
            return("Missing 'display_label' meta-column in TxTrackData")
        if (is.null(object$id))
            return("Missing 'id' meta-column in TxTrackData")
        if (is.null(object$key))
            return("Missing 'key' meta-column in TxTrackData")
            
        # In fact, we should check whether all the elements are data frame
        if (!is.list(object$exons))
            if (is.null(object$exons))
                return("Missing 'exons' meta-column in TxTrackData")
            else
                return("The 'exons' meta-column should be a list of data frame")
        
        return(TRUE)
    }
)


#### TrackData Compilation  ========

#' @export
setGeneric("compileTrackData",
           function (trackData) standardGeneric("compileTrackData"))

#' @export
setMethod("compileTrackData", signature = "NoTrackData",
    function (trackData)
        jc(tnt.board.track.data.empty = NoArg)
) 

#' @export
setMethod("compileTrackData", signature = "RangeTrackData",
    function (trackData) {
        stopifnot(length(unique(seqnames(trackData))) == 1)
        df <- as.data.frame(trackData, optional = TRUE)[
            c("start", "end", "tooltip")]
        jc.data <- jc(
            tnt.board.track.data.sync = ma(),
            retriever = jc(tnr.range_data_retriever =
                               jc(tnr.add_index = df))
        )
        jc.data
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
        stopifnot(length(unique(seqnames(trackData))) == 1)
        stopifnot(all(width(trackData) == 1))
        
        df <- as.data.frame(trackData, optional = TRUE)[
            c("start", "tooltip")]
        df <- S4Vectors::rename(df, c(start = "pos"))
        
        jc.data <- jc(
            tnt.board.track.data.sync = ma(),
            retriever = jc(tnr.pos_data_retriever =
                               jc(tnr.add_index = df))
        )
        jc.data
    }
)

#' @export
setMethod("compileTrackData", signature = "PosValTrackData",
    function (trackData) {
        stopifnot(length(unique(seqnames(trackData))) == 1)
        stopifnot(all(width(trackData) == 1))
        
        df <- as.data.frame(trackData, optional = TRUE)[
            c("start", "val","tooltip")]
        df <- S4Vectors::rename(df, c(start = "pos"))
        
        jc.data <- jc(
            tnt.board.track.data.sync = ma(),
            retriever = jc(tnr.pos_data_retriever =
                               jc(tnr.add_index = df))
        )
        jc.data
    }
)
##EXAMPLE
if (FALSE) {
    gpos <- GRanges("chr12", IRanges(seq(1, 10, 3), width = 1))
    mcols(gpos) <- as.data.frame(gpos)
    pt <- PosTrackData(gpos)
    compileTrackData(pt)
    pt <- PosValTrackData(gpos)
    pt <- PosValTrackData(gpos, val = start(gpos))
    compileTrackData(pt)
}

#' @export
setMethod("compileTrackData", signature = "GeneTrackData",
    function (trackData) {
        stopifnot(length(unique(seqnames(trackData))) == 1)
        
        df <- as.data.frame(trackData, optional = TRUE)[
            c("start", "end", "tooltip", "display_label", "id")]
        
        jc.data <- jc(
            tnt.board.track.data.sync = ma(),
            # Unlike RangeTrackData or PosTrackData, here is no need to add index
            retriever = jc(tnr.range_data_retriever = df)
        )
        jc.data
        
    }
)

#' @export
setMethod("compileTrackData", signature = "TxTrackData",
    function (trackData) {
        stopifnot(length(unique(seqnames(trackData))) == 1)
        df <- as.data.frame(trackData, optional = TRUE)
        df <- df[c("start", "end", "display_label", "key", "id", "exons", "tooltip")]
        jc.data <- jc(
            tnt.board.track.data.sync = ma(),
            retriever = jc(tnr.range_data_retriever = df)
        )
        jc.data
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
setClassUnion("ScalarNumericOrNull", c("ScalarNumeric", "ScalarInteger", "NULL"))

setClass("TnTTrack", slots = c(
    # TODO: add ID slot?
    Background = "ScalarCharacterOrNull",
    Height = "ScalarNumericOrNull",
    Label = "ScalarCharacterOrNull",
    
    Data = "TrackData",
    Display = "list"
))

setClass("RangeTrack", contains = "TnTTrack", slots = c(Data = "RangeTrackData"))

setClass("BlockTrack", contains = "RangeTrack", slots = c(Data = "RangeTrackData"))
setClass("PinTrack", contains = "RangeTrack", slots = c(Data = "PosValTrackData"))
setClass("GeneTrack", contains = "RangeTrack", slots = c(Data = "GeneTrackData"))
setClass("TxTrack", contains = "RangeTrack", slots = c(Data = "TxTrackData"))

#### Seqinfo Methods        ========
#' @export
setMethod("seqinfo", signature = c("TnTTrack"),
    function (x) seqinfo(trackData(x))
)
#' @export
setMethod("seqinfo<-", signature = c(x = "TnTTrack"),
    function (x, new2old, force, pruning.mode, value) {
        trackData(x) <- `seqinfo<-`(x = trackData(x), new2old = new2old,
                                    force = force, pruning.mode = pruning.mode, value = value)
        x
    }
)

#### TrackData Accessor          ========

#' @export
trackData <- function (track) {
    track@Data
}

#' @export
`trackData<-` <- function (track, value) {
    track@Data <- value
    validObject(track)
    track
}


#### TrackSpec Accessor     ========

#' @export
.mkScalarOrNull <- function (x)
    if (is.null(x)) x else Biobase::mkScalar(x)

#' @export
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
        tooltip(trackData(x))
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
        tooltip(trackData(x)) <- value
        x
    }
)

# EXAMPLE
if (FALSE) local({
    # TODO: to fix this error when deparse produce non-scalar character
    t <- BlockTrack(GRanges("chr12", IRanges(1:15 * 100, width = 10), label = paste("range", 1:15)))
    gr <- GRanges("chr12", IRanges(1:15 * 100, width = 10), label = paste("range", 1:15))
    t <- BlockTrack(gr)
    trackData(t)
    # Warning messages:
    #     1: In (function (..., row.names = NULL, check.rows = FALSE, check.names = TRUE,  :
    #        row names were found from a short variable and have been discarded
    #     2: In (function (..., row.names = NULL, check.rows = FALSE, check.names = TRUE,  :
    #        row names were found from a short variable and have been discarded
    tooltip(t)
    tooltip(t) <- c(1:15)
    tooltip(t) <- data.frame(start = start(gr))
    
    compileTrack(t)
    TnTBoard(list(t), viewrange = range(trackData(t)))
})



###   Show Method   ############################################################

setMethod("show", signature = "TnTTrack",
    function (object) {
        tracktype <- class(object)
        cat("A", tracktype, "\n")
    }
)

setMethod("show", signature = "RangeTrack",
    function (object) {
        background <- trackSpec(object, "background")
        height     <- trackSpec(object, "height")
        label      <- trackSpec(object, "label")
        callNextMethod()
        cat("| Label:\t", label, "\n", sep="")
        cat("| Background:\t", background, "\n", sep="")
        cat("| Height:\t", height, "\n", sep="")
        cat("| Data:\t")
        dout <- capture.output(show(trackData(object)))
        dout[-1] <- paste0("|  ", dout[-1])
        dout <- paste(dout, collapse = "\n")
        cat(dout)
    }
)





###   View          ############################################################

view <- function (...) {
    # TODO
}

