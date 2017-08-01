

#' @include tntTracks-compilation.R

setClass("CompositeTrackData", contains = c("list", "TrackData"))
setClass("CompositeTrack", contains = "TnTTrack", slots = c(Data = "CompositeTrackData"))

setValidity("CompositeTrackData",
    function (object) {
        if (!all(sapply(object, inherits, what = "RangeTrack")))
            return("All components of CompositeTrack should be RangeTrack")
        return(TRUE)
    }
)

#' @export
CompositeTrackData <- function (tracklist) {
    new("CompositeTrackData", tracklist)
}

setMethod("merge", signature = c(x = "TnTTrack", y = "TnTTrack"),
    function (x, y, ...) {
        tracklist <- list(x, y, ...)
        merge_tracklist(tracklist)
    }
)
setMethod("merge", signature = c(x = "TnTTrack", y = "missing"),
    function (x, y, ...) {
        tracklist <- list(x, ...)
        merge_tracklist(tracklist)
    }
)

#' @export
merge_tracklist <- function (tracklist) {
    stopifnot(all(sapply(tracklist, inherits, what = c("RangeTrack", "CompositeTrack"))))
    
    tracklist <- as.list(tracklist)
    which.comp <- sapply(tracklist, inherits, what = "CompositeTrack")
    tracklist[which.comp] <- lapply(tracklist[which.comp], trackData)
    tracklist <- c(tracklist, recursive = TRUE, use.names = FALSE)
    
    .merge_tracklist <- function (tracklist) {
        spec <- .mergeSpec(tracklist)
        ans <- new("CompositeTrack", Data = CompositeTrackData(tracklist))
        trackSpec(ans, which = names(spec)) <- spec
        ans
    }
    .mergeSpec <- function (tracklist) {
        labels      <- unique(sapply(tracklist, trackSpec, which = "label"))
        heights     <- unique(sapply(tracklist, trackSpec, which = "height"))
        backgrounds <- unique(sapply(tracklist, trackSpec, which = "background"))
        
        stopifnot(is.atomic(labels), is.atomic(heights), is.atomic(backgrounds))
        
        f <- function(x, w = c("label", "height", "background")) {
            w <- match.arg(w)
            if (length(x) == 1)
                return(x)
            if (length(x) == 0)
                return(NULL)
            if (w == "label")
                return(paste(paste(x[-length(x)], collapse = ", "), x[length(x)], sep = " and "))
            if (w == "height")
                return(na.fail(max(na.omit(x))))
            if (w == "background")
                return(x[1])
            stop()
        }
        list(
            label      = f(labels,      "label"),
            height     = f(heights,     "height"),
            background = f(backgrounds, "background")
        )
    }
    .merge_tracklist(tracklist)
}

.mkref <- function (l)
    paste0("subtrack", seq_along(l))

setMethod("compileTrackData", signature = "CompositeTrackData",
    function (trackData) {
        li.t <- as.list(trackData)
        
        li.retriever <- lapply(li.t,
            function (t) {
                cd <- compileTrackData(trackData(t))
                stopifnot(
                    length(cd) == 2,
                    identical(names(cd), c("tnt.board.track.data.sync", "retriever"))
                )
                cd[[2]]
            }
        )
        
        jc.retriever <- {
            jc.init <- jc(tnr.composite_data_retriever = ma())
            jc.add <- {
                li.add <- mapply(ref = .mkref(li.t), func = li.retriever, USE.NAMES = FALSE,
                    function (ref, func)
                        jc(add = ma(ref, func))
                )
                do.call(c, unname(li.add))
            }
            jc.end <- jc(done = ma())
            
            c(jc.init, jc.add, jc.end)
        }
        
        jc(tnt.board.track.data.sync = ma(),
           retriever = jc.retriever)
    }
)


# setMethod("wakeupTrack", signature = c(track = "CompositeTrack"),
#     function (track) {
#         li.track <- trackData(track)
#         li.track <- lapply(li.track, wakeupTrack)
#         li
#     }
# )