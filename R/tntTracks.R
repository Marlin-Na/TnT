
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
RangeTrackData <- function (range, color = "black", tooltip = mcols(range), key = seq_along(range)) {
    range <-
        if (is(range, "IRanges"))
            GRanges(seqnames = "UnKnown", ranges = range, strand = "*")
        else
            as(range, "GRanges")
    
    color <- if (length(range)) color else character(0)
    
    tooltip <-
        if (is.null(tooltip))
            data.frame(matrix( , nrow = length(range), ncol = 0))
        else 
            as.data.frame(tooltip, optional = TRUE)
    
    mcols(range) <- NULL
    range$tooltip <- tooltip
    range$color <- color
    range$key <- key
    new("RangeTrackData", range)
}

#' @export
PosTrackData <- function (pos, color = "black", tooltip = mcols(pos)) {
    trackdata <- RangeTrackData(range = pos, color = color, tooltip = tooltip)
    trackdata <- as(trackdata, "PosTrackData")
    validObject(trackdata) # Ensure all the width equals to one
    trackdata
}

#' @export
PosValTrackData <- function (pos, val, color = "black", tooltip = mcols(pos)) {
    mcols(pos) <- NULL
    
    trackdata <- RangeTrackData(range = pos, color = color, tooltip = tooltip)
    trackdata$val <- val
    
    trackdata <- as(trackdata, "PosValTrackData")
    validObject(trackdata)
    trackdata
}


#' @export
GeneTrackData <- function (range, labels = paste("Gene", mcols(range)$gene_id),
                           ids = make.unique(labels), color = "black", tooltip = mcols(range)) {
    force(labels)
    force(ids)
    force(tooltip)
    
    mcols(range) <- NULL
    
    range <- RangeTrackData(range, color, tooltip)
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
GeneTrackDataFromTxDb <- function (txdb, seqlevel = seqlevels(txdb), color = "black") {
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
    
    if (length(color) > 1) {
        color <- color[[1]]
        msg <- sprintf("GeneTrackDataFromTxDb does not support multiple color values, use %s", color)
        warning(msg)
    }
    
    GeneTrackData(range = gr, labels = labels, color = color, tooltip = tooltip)
}
### EXAMPLE
if (FALSE) {
    txdb <- TxDb.Hsapiens.UCSC.hg19.knownGene::TxDb.Hsapiens.UCSC.hg19.knownGene
    GeneTrackDataFromTxDb(txdb, c("chrX", "chrY"))
}

#' @export
TxTrackDataFromGRanges <- function (gr, type = gr$type, tx_id = gr$tx_id, tx_name = gr$tx_name,
                                    color = "red") {
    # Check columns
    check_col <- function(a, name = c("type", "tx_id")) {
        if (!is.null(a))
            return(TRUE)
        name <- match.arg(name)
        msg <- sprintf("%s is required to construct TxTrackData", dQuote(name))
        stop(msg)
    }
    check_col(type,  "type")
    check_col(tx_id, "tx_id")
    force(tx_name) # tx_name is optional
    
    mcols(gr)  <- NULL
    gr$type    <- type
    gr$tx_id   <- tx_id
    gr$tx_name <- tx_name
    rm(type, tx_id, tx_name)
    
    gr <- gr[gr$type %in% c("exon", "cds")]
    gr.tx <- {
        gr.tx <- split(gr, gr$tx_id, drop = TRUE)
        gr.tx <- range(gr.tx)
        gr.tx <- if (any(lengths(gr.tx) != 1L)) stop() else unlist(gr.tx)
        
        gr.tx$tx_id <- gr$tx_id[match(names(gr.tx), gr$tx_id)] # match character to integer
        gr.tx$tx_id <- na.fail(gr.tx$tx_id)
        
        gr.tx$tx_name <- if (is.null(gr$tx_name)) NULL else {
            gr$tx_name[match(gr.tx$tx_id, gr$tx_id)]
        }
        
        unname(gr.tx)
    }
    
    gr.tx$exons <- {
        exons <- data.frame(start = start(gr), end = end(gr),
                            offset = start(gr) - start(gr.tx)[match(gr$tx_id, gr.tx$tx_id)],
                            coding = ifelse(gr$type == "cds", TRUE, FALSE))
        exons <- splitdf(exons, gr$tx_id)
        exons <- unname(exons[match(names(exons), gr.tx$tx_id)])
        exons
    }
    
    gr.tx$display_label <- strandlabel(
        labels = if (is.null(gr.tx$tx_name)) gr.tx$tx_id else gr.tx$tx_name,
        strands = strand(gr.tx)
    )
    
    gr.tx$key <- as.integer(as.factor(gr.tx$tx_id))
    gr.tx$id  <- as.character(gr.tx$tx_id)
    
    if (is.null(gr.tx$tx_name))
        gr.tx$tooltip <- data.frame(stringsAsFactors = FALSE,
            tx_id    = gr.tx$tx_id,
            location = as.character(gr.tx) 
        )
    else
        gr.tx$tooltip <- data.frame(
            tx_id    = gr.tx$tx_id,
            tx_name  = gr.tx$tx_name,
            location = as.character(gr.tx) 
        )
    
    if (length(color) != 1)
        stop("Currently do not support multiple color values")
    gr.tx$color <- if (length(gr.tx)) color else character(0)
    new("TxTrackData", gr.tx)
}


#' @export
TxTrackDataFromGRangesList <- function (grl, color = "red", tooltip = mcols(grl),
                                        labels = names(grl)) {
    ## TODO: check overlap within each group
    
    force(tooltip)
    force(labels)
    stopifnot(nrow(tooltip) == length(grl))
    stopifnot(length(labels) == length(grl))
    tx <- range(grl)
    if (any(lengths(tx) > 1))
        stop("seqlevels and strands within each group are not consistent")
    
    gr.txs <- unlist(tx)
    mcols(gr.txs) <- NULL
    
    gr.txs$color <- if (length(gr.txs)) color else character(0)
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
TxTrackDataFromTxDb <- function (txdb, seqlevel = seqlevels(txdb), color = "red") {
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
    
    if (length(color) > 1) {
        color <- color[[1]]
        msg <- sprintf("TxTrackDataFromTxDb does not support multiple color values, use %s", color)
        warning(msg)
    }
    gr.txs$color <- if (length(gr.txs)) color else character(0)
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
        if (!is.character(object$color))
            if (is.null(object$color))
                return("Missing 'color' meta-column in RangeTrackData")
            else
                return("The 'color' meta-column should be a character")
        
        k <- object$key
        if (is.null(k))
            return("Missing 'key' meta-column in RangeTrackData")
        if (length(k) != length(unique(k)))
            return("'key' is not unique in RangeTrackData")
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
        df <- as.data.frame(trackData, optional = TRUE) [
            c("start", "end", colnames(mcols(trackData)))]
        
        if (inherits(trackData, "TxTrackData"))
            jc.data <- jc(
                tnt.board.track.data.sync = ma(),
                retriever = jc(tnr.range_data_retriever = jc(tnr.cp_tx_color_to_exon = df))
            )
        else
            jc.data <- jc(
                tnt.board.track.data.sync = ma(),
                retriever = jc(tnr.range_data_retriever = df)
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
        
        df <- as.data.frame(trackData, optional = TRUE)[c("start", colnames(mcols(trackData)))]
        df <- S4Vectors::rename(df, c(start = "pos"))
        
        jc.data <- jc(
            tnt.board.track.data.sync = ma(),
            retriever = jc(tnr.pos_data_retriever = df)
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
    
    Data = "ANY",
    Display = "list"
))

setClass("RangeTrack", contains = "TnTTrack", slots = c(Data = "RangeTrackData"))

setClass("BlockTrack", contains = "RangeTrack", slots = c(Data = "RangeTrackData"))
setClass("GeneTrack", contains = "RangeTrack", slots = c(Data = "GeneTrackData"))
setClass("TxTrack", contains = "RangeTrack", slots = c(Data = "TxTrackData"))

setClass("VlineTrack", contains = "RangeTrack", slots = c(Data = "PosTrackData"))

setClass("DomainValTrack", contains = "RangeTrack", slots = c(Domain = "numeric"))
setValidity("DomainValTrack",
    function (object) if (length(object@Domain) != 2)
        "Domain must be a length-two numeric vector" else TRUE
)

setClass("PinTrack", contains = "DomainValTrack", slots = c(Data = "PosValTrackData"))
setClass("LineTrack", contains = "DomainValTrack", slots = c(Data = "PosValTrackData"))
setClass("AreaTrack", contains = "DomainValTrack", slots = c(Data = "PosValTrackData"))



#### Seqinfo Methods        ========
#' @export
setMethod("seqinfo", signature = c("RangeTrack"),
    function (x) seqinfo(trackData(x))
)
#' @export
setMethod("seqinfo<-", signature = c(x = "RangeTrack"),
    function (x, new2old, force, pruning.mode, value) {
        trackData(x) <- `seqinfo<-`(x = trackData(x), new2old = new2old,
                                    force = force, pruning.mode = pruning.mode, value = value)
        x
    }
)
#' @export
setMethod("seqlevelsInUse", signature = c(x = "RangeTrack"),
    function (x) seqlevelsInUse(trackData(x))
)


#### TrackData Accessor          ========

#' @export
trackData <- function (track) {
    track@Data
}

#' @export
`trackData<-` <- function (track, value) {
    # TODO: convert value to the needed class
    track@Data <- value
    validObject(track)
    track
}

#### `[`, `[[`, etc.        ========

setMethod("[", signature = c(x = "TnTTrack"),
    function (x, i, j, ..., drop = TRUE) {
        #trackData(x)[i, j, ..., drop = TRUE]
        s <- match.call()
        s$x <- bquote(TnT::trackData(.(s$x)))
        eval.parent(s)
    }
)

setMethod("[[", signature = c(x = "TnTTrack"),
    function (x, ...) {
        s <- match.call()
        s$x <- bquote(TnT::trackData(.(s$x)))
        eval.parent(s)
    }
)

setMethod("$", signature = c(x = "TnTTrack"),
    function (x, name) {
        s <- as.call(list(`$`, trackData(x), name))
        eval.parent(s)
    }
)

setMethod("[<-", signature = c(x = "TnTTrack"),
    function (x, i, j, ..., value) {
        s <- match.call()
        s$x <- bquote(TnT::trackData(.(s$x)))
        trackData(x) <- eval.parent(s)
        x
    }
)

setMethod("[[<-", signature = c(x = "TnTTrack"),
    function (x, i, j, ..., value) {
        s <- match.call()
        s$x <- bquote(TnT::trackData(.(s$x)))
        trackData(x) <- eval.parent(s)
        x
    }
)

setMethod("$<-", signature = c(x = "TnTTrack"),
    function (x, name, value) {
        s <- match.call()
        s$x <- bquote(TnT::trackData(.(s$x)))
        trackData(x) <- eval.parent(s)
        x
    }
)

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

setMethod("show", signature = "RangeTrack",
    function (object) {
        background <- trackSpec(object, "background")
        height     <- trackSpec(object, "height")
        label      <- trackSpec(object, "label")
        cat("A", class(object), "\n")
        cat("| Label:\t", label, "\n", sep="")
        cat("| Background:\t", background, "\n", sep="")
        cat("| Height:\t", height, "\n", sep="")
        if (inherits(object, "DomainValTrack"))
            cat("| Domain:\t", object@Domain, "\n", sep="")
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

