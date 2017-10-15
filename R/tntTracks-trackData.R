
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
    
    color <- {
        # TODO: Convert factor to integer or character? And which is better?
        if (!length(range))
            color <- character(0)
        if (is.numeric(color))
            color <- as.integer(color)
        color
    }
    
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

.is.nodomain <- function (track) {
    if (is(track, "TnTTrack"))
        track <- trackData(track)
    d <- metadata(track)$domain
    return(is.null(d) || !length(d))
}
getdomain <- function (track) {
    if (is(track, "TnTTrack"))
        track <- trackData(track)
    
    if (!is(track, "PosValTrackData"))
        return(NULL)
    
    if (.is.nodomain(track)) {
        # Domain is not specified, automatically generate one
        val <- track$val
        if (!length(na.omit(val))) {
            min <- 0L
            max <- 1L
        }
        else if (length(na.omit(val)) == 1L) {
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
        return(metadata(track)$domain)
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
    
    if (length(color) > 1L) {
        color <- color[[1]]
        msg <- sprintf("GeneTrackDataFromTxDb does not support multiple color values, use %s", color)
        warning(msg)
    }
    
    GeneTrackData(range = gr, labels = labels, color = color, tooltip = tooltip)
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
        gr.tx <- range(gr.tx)  # Becomes a GRangesList
        if (any(lengths(gr.tx) != 1L)) {
            # TODO: to be more verbose
            stop("Features in the same group can not locate on different strands")
        }
        gr.tx <- unlist(gr.tx) # To GRanges
        
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
        
        if (is.null(domain) || !length(domain))
            NULL # Domain is not specified, but it's okay
        else {
            if (!is.numeric(domain) || length(domain) != 2L)
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
        if (!is.character(object$color) && !is.integer(object$color)) {
            if (is.null(object$color))
                return("Missing 'color' meta-column in RangeTrackData")
            else
                return("The 'color' meta-column should be either character or integer")
        }
        
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
