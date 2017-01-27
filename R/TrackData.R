

# @examples
if (interactive() && requireNamespace("IRanges")) {
    ir <- IRanges::IRanges(start = c(12,34), end = c(12,34))
    td <- asTrackData(ir, use.pos = TRUE,
                      additional.cols = list(val = c(0.5, 0.7)))
    selfRetriever(td)
}



# It may take an argument of data.frame which contains appropriate columns and
# then returns a javascript callback function as a character string of class
# "JS_EVAL".
selfRetriever <- function (trackData) {
    json <- jsonlite::toJSON(trackData)
    ans <- {
        ans <- sprintf("function () {  return %s  }", json)
        JS(ans)
    }
    ans
}

# The track data is represented as a data.frame containing essential columns like
# "start", "end", "pos" and "val". it can be further converted to "json" using jsonlite::toJSON.
asTrackData <- function (x, ...) UseMethod("asTrackData")

# This method can be applied to IRanges but not GRanges
asTrackData.Ranges <- function (x, use.pos = FALSE, additional.cols, ...) {
    df <- {
        if (use.pos) {
            stopifnot(all(BiocGenerics::width(x) == 1))
            data.frame(pos = BiocGenerics::start(x))
        }
        else
            data.frame(start = BiocGenerics::start(x), end = BiocGenerics::end(x))
    }

    ans <- if (!missing(additional.cols)) cbind(df, additional.cols) else df
    class(ans) <- c("tntTrackData", "data.frame")
    ans
}


asTrackData.data.frame <- function (x, preserved.cols = c("start","end","pos","val"), ...) {
    whichcols <- colnames(x) %in% preserved.cols
    ans <- x[, whichcols, drop = FALSE]
    class(ans) <- c("tntTrackData", "data.frame")
    ans
}
