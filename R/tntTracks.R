
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

NoTrackData <- function () new("NoTrackData")

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

PosTrackData <- function (pos, color = "black", tooltip = mcols(pos)) {
    trackdata <- RangeTrackData(range = pos, color = color, tooltip = tooltip)
    trackdata <- as(trackdata, "PosTrackData")
    validObject(trackdata) # Ensure all the width equals to one
    trackdata
}

PosValTrackData <- function (pos, val, domain = numeric(), color = "black", tooltip = mcols(pos)) {
    force(tooltip)
    mcols(pos) <- NULL
    
    trackdata <- RangeTrackData(range = pos, color = color, tooltip = tooltip)
    trackdata$val <- val
    metadata(trackdata)$domain <- domain
    
    trackdata <- as(trackdata, "PosValTrackData")
    validObject(trackdata)
    trackdata
}

.is.nodomain <- function (t) {
    if (is(t, "TnTTrack"))
        t <- trackData(t)
    d <- metadata(t)$domain
    return(is.null(d) || length(d) == 0)
}
getdomain <- function (t) {
    if (is(t, "TnTTrack"))
        t <- trackData(t)
    
    if (!is(t, "PosValTrackData"))
        return(NULL)
    
    if (.is.nodomain(t)) {
        # Domain is not specified, automatically generate one
        val <- t$val
        if (length(na.omit(val)) == 0) {
            min <- 0L
            max <- 1L
        }
        else if (length(na.omit(val)) == 1) {
            a <- na.omit(val)[1]
            min <- min(c(a, 0))
            max <- max(c(a, 0))
        }
        else if (all(val >= 0, na.rm = TRUE)) {
            min <- 0L
            max <- max(val, na.rm = TRUE)
        }
        else {
            min <- min(val, na.rm = TRUE)
            max <- max(val, na.rm = TRUE)
        }
        
        return(c(min, max))
    }
    else
        return(metadata(t)$domain)
}

setMethod("show", signature = "PosValTrackData",
    function (object) {
        out <- capture.output(callNextMethod())
        domain <- getdomain(object)
        add <- sprintf("  domain: %s %s to %s",
                       if (.is.nodomain(object)) "not specified, use" else "", domain[1], domain[2])
        cat(out, add, sep = "\n")
        invisible(object)
    }
)


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

GeneTrackDataFromTxDb <- function (txdb, seqlevel = seqlevels(txdb), color = "black") {
    seqlevel.ori <- seqlevels(txdb)
    seqlevels(txdb) <- seqlevel         # Set and restore the seqlevels
    # We must restore the seqlevel of the txdb since it is a reference class
    on.exit(seqlevels(txdb) <- seqlevel.ori)
    
    if (!requireNamespace("GenomicFeatures", quietly = TRUE))
        stop("GenomicFeatures package is not available, ",
             "please install with \"BiocInstaller::biocLite('GenomicFeatures')\"")
    # TODO: use "single.strand.genes.only = FALSE" ?
    gr <- GenomicFeatures::genes(txdb)
    labels <- gr$gene_id
    tooltip <- data.frame(stringsAsFactors = FALSE,
        "gene_id" = gr$gene_id,
        #"location" = as.character(gr),
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
                            coding = na.fail(ifelse(gr$type == "cds", TRUE,
                                             ifelse(gr$type == "exon", FALSE, NA))))
        exons <- splitdf(exons, gr$tx_id)
        exons <- unname(exons[match(gr.tx$tx_id, names(exons))])
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
            tx_id    = gr.tx$tx_id
            # location = as.character(gr.tx) 
        )
    else
        gr.tx$tooltip <- data.frame(stringsAsFactors = FALSE,
            tx_id    = gr.tx$tx_id,
            tx_name  = gr.tx$tx_name
            # location = as.character(gr.tx) 
        )
    
    if (length(color) != 1)
        stop("Currently do not support multiple color values")
    gr.tx$color <- if (length(gr.tx)) color else character(0)
    new("TxTrackData", gr.tx)
}


TxTrackDataFromGRangesList <- function (grl, color = "red", tooltip = mcols(grl),
                                        labels = names(grl)) {
    ## TODO: check overlap within each group
    
    force(tooltip)
    force(labels)
    if (is.null(labels))
        labels <- rep("", length(grl))
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
    
    df.exons <- as.data.frame(gr.exons, optional = TRUE)[c("start", "end", "offset", "coding")]
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



TxTrackDataFromTxDb <- function (txdb, seqlevel = seqlevels(txdb), color = "red") {
    ## Set and restore seqlevels of txdb
    seqlevel.ori <- seqlevels(txdb)
    seqlevels(txdb) <- seqlevel
    on.exit(seqlevels(txdb) <- seqlevel.ori)
    
    ## Extract features from txdb
    if (!requireNamespace("GenomicFeatures", quietly = TRUE))
        stop("GenomicFeatures package is not available, ",
             "please install with \"BiocInstaller::biocLite('GenomicFeatures')\"")
    gr.txs <- GenomicFeatures::transcripts(txdb, columns = c("tx_id", "tx_name", "gene_id"))
    gr.txs$gene_id <- as.character(gr.txs$gene_id) # contain NA values
    gr.cds <- GenomicFeatures::cds(txdb, columns = c("tx_id"))
    gr.exons <- GenomicFeatures::exons(txdb, columns = c("tx_id"))
    
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
    gr.txs$tooltip <- data.frame(stringsAsFactors = FALSE,
        "tx_id"   = gr.txs$tx_id,
        "tx_name" = gr.txs$tx_name,
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
        val <- mcols(object)$val
        domain <- metadata(object)$domain
        if (is.null(val))
            return("Missing 'val' meta-column in PosValTrackData")
        if (!is.numeric(val))
            return("'val' meta-column should be a numeric vector")
        
        if (is.null(domain) || length(domain) == 0)
            NULL # Domain is not specified, but it's okay
        else {
            if (!is.numeric(domain) || length(domain) != 2)
                return("Domain is not a length-two numeric vector")
            if (domain[1] > min(val)) # only give warnings
                warning(
                    sprintf("The minimum value (%s) of track data is smaller than domain boundary %s-%s",
                            min(val), domain[1], domain[2])
                )
            if (domain[2] < max(val))
                warning(
                    sprintf("The maximum value (%s) of track data is larger than domain boundary %s-%s",
                            max(val), domain[1], domain[2])
                )
        }
        
        return(TRUE)
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

setGeneric("compileTrackData",
           function (trackData, ...) standardGeneric("compileTrackData"))

setMethod("compileTrackData", signature = "NoTrackData",
    function (trackData)
        jc(tnt.board.track.data.empty = na)
)

setMethod("compileTrackData", signature = "RangeTrackData",
    function (trackData, full = FALSE) {
        stopifnot(length(unique(seqnames(trackData))) == 1)
        df <- as.data.frame(trackData, optional = TRUE) [
            c("start", "end", colnames(mcols(trackData)))]
        
        if (is(trackData, "TxTrackData"))
            jc.data <- jc(
                tnt.board.track.data.sync = ma(),
                retriever = jc(tnr.range_data_retriever = jc(tnr.cp_tx_color_to_exon = df))
            )
        else
            jc.data <- jc(
                tnt.board.track.data.sync = ma(),
                retriever = jc(tnr.range_data_retriever =
                                   ma(df, if (full) TRUE else FALSE))
            )
        jc.data
    }
)
# EXAMPLE
if (FALSE) local({
    data <- RangeTrackData(range = IRanges::IRanges(1:4, 5:8),
                           tooltip = data.frame(start = 1:4, width = 5))
    compileTrackData(data)
    compileTrackData(data, full = TRUE)
})

setMethod("compileTrackData", signature = "PosTrackData",
    function (trackData, full = FALSE) {
        stopifnot(length(unique(seqnames(trackData))) == 1)
        stopifnot(all(width(trackData) == 1))
        
        df <- as.data.frame(trackData, optional = TRUE)[c("start", colnames(mcols(trackData)))]
        df <- S4Vectors::rename(df, c(start = "pos"))
        
        jc.data <- jc(
            tnt.board.track.data.sync = ma(),
            retriever = jc(tnr.pos_data_retriever =
                               ma(df, if (full) TRUE else FALSE))
        )
        jc.data
    }
)

setMethod("compileTrackData", signature = "PosValTrackData",
    function (trackData, full = FALSE) {
        stopifnot(length(unique(seqnames(trackData))) == 1)
        stopifnot(all(width(trackData) == 1))
        validObject(trackData)
        
        df <- as.data.frame(trackData, optional = TRUE)[c("start", colnames(mcols(trackData)))]
        df <- S4Vectors::rename(df, c(start = "pos"))
        
        domain <- getdomain(trackData)
        
        jc.data <- jc(
            tnt.board.track.data.sync = ma(),
            retriever = jc(
                tnr.pos_data_retriever = ma(
                    jc(tnr.scale_val = ma(df, domain)), # scale data from domain to [0, 1]
                    if (full) TRUE else FALSE  # return full data or not
                )
            )
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
    compileTrackData(pt, full = TRUE)
    pt <- PosValTrackData(gpos, val = start(gpos))
    compileTrackData(pt)
    compileTrackData(pt, full = TRUE)
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

setClass("DomainValTrack", contains = "RangeTrack")

setClass("PinTrack", contains = "DomainValTrack", slots = c(Data = "PosValTrackData"))
setClass("LineTrack", contains = "DomainValTrack", slots = c(Data = "PosValTrackData"))
setClass("AreaTrack", contains = "DomainValTrack", slots = c(Data = "PosValTrackData"))



#### Seqinfo Methods        ========


#' Seqinfo of TnTTrack and TnTBoard
#' 
#' @param x A TnTTrack or TnTBoard object. 
#' @param new2old,pruning.mode,value Passed to seqinfo method for GenomicRanges.
#' @name seqinfo
#' @aliases seqinfo<-,RangeTrack-method
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
setMethod("seqinfo<-", signature = c(x = "RangeTrack"),
    function (x, new2old, pruning.mode, value) {
        trackData(x) <- `seqinfo<-`(x = trackData(x), new2old = new2old,
                                    pruning.mode = pruning.mode, value = value)
        x
    }
)

#' @rdname seqinfo
setMethod("seqinfo", signature = c("RangeTrack"),
    function (x) {
        seqinfo(trackData(x))
    }
)

#' @rdname seqinfo
setMethod("seqlevelsInUse", signature = c(x = "RangeTrack"),
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
setMethod("$<-", signature = c(x = "TnTTrack"),
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
setMethod("tooltip<-", signature = c(x = "TrackData", value = "data.frame"),
    function (x, value) {
        x$tooltip <- value
        x
    }
)
#' @rdname tooltip
setMethod("tooltip<-", signature = c(x = "TnTTrack", value = "data.frame"),
    function (x, value) {
        tooltip(trackData(x)) <- value
        x
    }
)

# EXAMPLE
if (FALSE) local({
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
    TnTBoard(list(t), view.range = range(trackData(t)))
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

