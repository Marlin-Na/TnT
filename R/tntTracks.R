


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








###  Track Data      ------------------------------------------------------------

setClass("TrackData")

setClass("NoTrackData",
         contains = c("NULL", "TrackData"))
setClass("RangeTrackData",
         contains = c("GRanges", "TrackData"))
setClass("PosTrackData",
         contains = c("RangeTrackData"))

NoTrackData <- function () new("NoTrackData")

RangeTrackData <- function (range, tooltip = mcols(range)) {
    tooltip <- as.data.frame(tooltip)
    if (is(range, "IRanges")) {
        range <- GRanges(seqnames = "UnKnown", ranges = range, strand = "*")
    }
    range <- as(range, "GRanges")
    range$.tooltip <- tooltip
    new("RangeTrackData", range)
}

PosTrackData <- function (pos, tooltip = mcols(pos)) {
    tooltip <- as.data.frame(tooltip)
    trackdata <- RangeTrackData(range = pos, tooltip = tooltip)
    trackdata <- as(trackdata, "PosTrackData")
    validObject(trackdata) # Ensure all the width equals to one
    trackdata
}

setValidity("PosTrackData",
    function (object) {
        if (all(width(object) == 1)) TRUE
        else return("Width of PosTrackData should be 1.")
    }
)


setGeneric("compileTrackData",
           function (trackData, ...) standardGeneric("compileTrackData"))

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

setMethod("compileTrackData", signature = "NoTrackData",
    function (trackData)
        jc(tnt.board.track.data.empty = NoArg)
) 

setMethod("compileTrackData", signature = "RangeTrackData",
    function (trackData) {
        ## TODO: Have to select seq
        stopifnot(length(unique(seqnames(trackData))) == 1)
        df <- as.data.frame(trackData)[c("start", "end", "strand", ".tooltip")]
        df <- S4Vectors::rename(df, c(start = "from", end = "to"))
        compileTrackData(df)
    }
)

setMethod("compileTrackData", signature = "PosTrackData",
    function (trackData) {
        ## TODO: Have to select seq
        stopifnot(length(unique(seqnames(trackData))) == 1)
        stopifnot(all(width(trackData) == 1))
        
        df <- as.data.frame(trackData)[c("start", "strand", ".tooltip")]
        df <- S4Vectors::rename(df, c(start = "pos"))
        compileTrackData(df)
    }
)



setClass("TxDbTrackData",
         contains = c("TrackData"),
         slots = c(TxDb = "TxDb", Target = "character", SeqLevel = "character"))
# TODO: valiad whether SeqLevel are valid seqlevels for the TxDb

TxDbTrackData <- function (txdb, target = c("tx", "gene")) {
    target <- match.arg(target)
    new("TxDbTrackData", TxDb = txdb, Target = target, SeqLevel = seqlevels(txdb))
}

setMethod("compileTrackData", signature = "TxDbTrackData",
    function (trackData) {
        seqlevel <- trackData@SeqLevel
        txdb <- trackData@TxDb
        target <- trackData@Target
        stopifnot(length(seqlevel) == 1)
        
        seqlevels(txdb) <- seqlevel
        
        if (target == "gene") {
            # TODO: use "single.strand.genes.only = FALSE" ?
            gr.gene <- genes(txdb)
            # TODO: Note that gene id may not be unique if "single.strand.genes.only = FALSE"
            df <- as.data.frame(gr.gene)[
                c("seqnames", "start", "end", "strand", "gene_id")]
            df$display_label <- with(df, {
                ifelse(strand == "+", paste("Gene", gene_id, ">"),
                    ifelse(strand == "-", paste("<", "Gene", gene_id), gene_id)
                )
            })
            df <- S4Vectors::rename(df, c(gene_id = "id"))
            df
        }
        if (target == "tx") {
            # TODO
            stop()
        }
        
        # We must restore the seqlevel of the txdb since it is a reference class
        seqlevels(txdb) <- seqlevels0(txdb)
        
        compileTrackData(df)
    }
)


# // transcripts data
# var transcripts = [
#     {
#         start: 32336637,
#         end: 32367637,
#         display_label: "Gene name 1>",
#         key: 1,
#         id: 'Gene1',
#         exons: [
#             {
#                 start: 32337637,
#                 end: 32338637,
#                 offset: 32337637 - 32336637,
#                 coding: false,
#                 transcript: {
#                     Parent: 'Gene1'
#                 }
#             },
#             {
#                 start: 32339637,
#                 end: 32357637,
#                 offset: 32339637 - 32336637,
#                 coding: true,
#                 transcript: {
#                     Parent: 'Gene1'
#                 }
#             },
#             {
#                 start: 32360637,
#                 end: 32360737,
#                 offset: 32360637 - 32336637,
#                 coding: true,
#                 transcript: {
#                     Parent: 'Gene1'
#                 }
#             },
#             {
#                 start: 32363637,
#                 end: 32367637,
#                 offset: 32363637 - 32336637,
#                 coding: false
#             }
#             ]
#     },
#     {
#         start: 32346637,
#         end: 32393637,
#         display_label: "Overlapping gene",
#         key: 3,
#         id: 'OG1',
#         exons: [
#             {
#                 start: 32346637,
#                 end: 32393637,
#                 offset: 0,
#                 coding: true
#             }
#             ]
#     },
#     {
#         start: 32393637,
#         end: 32402637,
#         display_label: "Gene name 2 >",
#         key: 2,
#         id: 'Gene2',
#         exons: [
#             {
#                 start: 32396637,
#                 end: 32400637,
#                 // Offset comes from the fact that in Ensembl, exons coordinates are relative to the parent transcript and not the gene
#                 // So an offset equals to o = exonStart - transcriptStart is applied if necessary (otherwise, set it to 0)
#                 offset: 32396637 - 32393637,
#                 coding: true,
#                 transcript: {
#                     Parent: 'Gene2',
#                 }
#             }
#             ]
#     }
#     ];



### TnT Tracks  ----------------------------------------------------------------


setClass("TnTTrack", slots = c(Spec = "list", Data = "TrackData", Display = "list"))

compileTrack <- function (tntTrack) {
    jc.spec <- asJC(tntTrack@Spec)
    jc.display <- jc(display = asJC(tntTrack@Display))
    jc.data <- jc(data = compileTrackData(tntTrack@Data))
    c(jc.spec, jc.display, jc.data)
}


setClass("ReferenceTrack", contains = "TnTTrack")

setClass("GeneTrack", contains = "ReferenceTrack", slots = c(Data = "TxDbTrackData"))

GeneTrack <- function (txdb, label = deparse(substitute(txdb)), # TODO: tooltip?
                       id = NULL, height = NULL, color = NULL, color.background = NULL) {
    force(label)
    data <- TxDbTrackData(txdb, target = "gene")
    spec <- list(
        tnt.board.track = ma(),
        height = height,
        color = color.background,
        label = label,
        id = id
    )
    display <- list(
        tnt.board.track.feature.genome.gene = ma(),
        color = color
    )
    new("GeneTrack", Data = data, Spec = spec, Display = display)
}


setClass("BlockTrack", contains = "TnTTrack", slots = c(Data = "RangeTrackData"))

setClass("PinTrack", contains = "TnTTrack", slots = c(Data = "PosTrackData"))

setGeneric("compileTrack", function (tntTrack) standardGeneric("compileTrack"))

BlockTrack <- function (range, label = deparse(substitute(range)),
                        tooltip = mcols(range), id = NULL,
                        height = NULL, color = NULL, color.background = NULL) {
    force(label)
    data <- RangeTrackData(range = range, tooltip = tooltip)
    spec <- list(
        tnt.board.track = ma(),
        color = color.background,
        height = height,
        label = label,
        id = id
    )
    display <- list(
        tnt.board.track.feature.block = ma(),
        color = color
        # from = .JSONMap(colname = "from"),
        # to = .JSONMap(colname = "to")
    )
    new("BlockTrack", Spec = spec, Data = data, Display = display)
}


PinTrack <- function (pos, value = mcols(pos)$value, domain = c(min(value), max(value)),
                      label = deparse(substitute(pos)), tooltip = mcols(pos),
                      id = NULL, height = NULL, color = NULL,
                      color.background = NULL) {
    if (is.null(value))
        stop("Value (height) at each position not specified.")
    force(domain)
    force(label)
    stopifnot(length(domain) == 2)
    data <- PosTrackData(pos = pos, tooltip = tooltip)
    data$val <- value
    spec <- list(
        tnt.board.track = ma(),
        color = color.background,
        height = height,
        label = label,
        id = id
    )
    display <- list(
        tnt.board.track.feature.pin = ma(),
        domain = domain,
        color = color
    )
    new("PinTrack", Spec = spec, Data = data, Display = display)
}





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






###   TnT Board     ------------------------------------------------------------

#setClassUnion("GRangesOrIRanges", members = c("GRanges", "IRanges"))


#setClass("TnTBoard",
#    slots = c(
#        PreSetViewRange = "GRangesOrIRanges",
#        PreSetCoordRange = "GRangesOrIRanges",
#        # Ideally, the allowed zoom range should depend on the width and maxium coordinate
#        PreSetZoomAllow = "IRanges",
#        PreSetZoom = "numeric",
#        # TODO: How does the width correspond to pixel?
#        #       In fact, we should not specify the width here,
#        #       but use the resize method on JS side.
#        #Width = "integer",
#        PreSetAllowDrag = "logical",
#        TrackList = "list"
#    )
#)
#
#TnTBoard <- function (track.list, prezoom = 1, allow.drag = TRUE) {
#    
#}

###   TnT Genome    ------------------------------------------------------------

# setClass("TnTGenome", contains = "TnTBoard",
#     slots = c(
#         
#     )
# )






















