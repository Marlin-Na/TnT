

#' @include tntTracks-compilation.R

setClass("CompositeTrack", contains = "TnTTrack", slots = c(Data = "list"))

setValidity("CompositeTrack",
    function (object) {
        data <- trackData(object)
        if (!all(sapply(data, inherits, what = "RangeTrack")))
            return("All components of CompositeTrack should be RangeTrack")
        return(TRUE)
    }
)

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
    
    which.comp <- sapply(tracklist, inherits, what = "CompositeTrack")
    tracklist[which.comp] <- lapply(tracklist[which.comp], trackData)
    tracklist <- c(tracklist, recursive = TRUE, use.names = FALSE)
    
    .merge_tracklist <- function (tracklist) {
        spec <- .mergeSpec(tracklist)
        ans <- new("CompositeTrack", Data = tracklist)
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


