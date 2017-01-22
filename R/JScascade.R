
#' Construct a Javascript Function Cascade
#'
#' \code{JScascade} constructs a list which can represent the defination of a
#' TnT browser/tree as a functional cascade, it can then be parsed as javascript
#' code by \code{\link{asJS}}.
#'
#' @name JScascade
#' 
#' @param ... 
#'
#' @return
#'     The function returns a JScascade S3 object which can be further utilized
#'     by \code{asJS()} and \code{JScascade()}/\code{jc()} itself.
#' 
#' 
#' @export
#'
#' @examples
#' axisTrack <- jc(
#'     tnt.board.track = "",
#'     height = 20,
#'     color = quo('white'),
#'     display = jc(tnt.board.track.feature.axis = "")
#' )
#' blockTrack <- jc(
#'     tnt.board.track = "",
#'     height = 30,
#'     color = quo("#FFCFDD"),
#'     data = jc(
#'         tnt.board.track.data.sync = "",
#'         retriever = "function() {return [{start : 200, end : 350}]}"
#'     ),
#'     display = jc(
#'         tnt.board.track.feature.block = "",
#'         color = quo("blue"),
#'         index = "function (d) {return d.start}"
#'     )
#' )
#' tntdef <- jc(
#'     tnt.board.genome = "",
#'     from = 0,
#'     to = 500,
#'     min = 0,
#'     max = 1000,
#'     add_track = axisTrack,
#'     add_track = blockTrack
#' )
#' tntdef
#' asJS(tntdef)
JScascade <- function (...) {
    ans <- list(...)
    if (is.null(names(ans)) || any(names(ans) == ""))
        stop ("Names of the arguments must be specified.")
    class(ans) <- "JScascade"
    ans
}

#' @export
#' @rdname JScascade
jc <- function (...) JScascade(...)

#' @export
print.JScascade <- function(x) {
    calls <- names(x)
    arguments <- vapply(x, FUN.VALUE = character(1),
        function (e) {
            if (is(e, "JScascade"))
                return("<S3: JScascade>")
            else
                return(as.character(e))
        }
    )
    out <- data.frame(JScalls = calls)
    out$Arguments <- arguments
    rownames(out) <- paste("step", seq_along(x))
    cat("<S3: JScascade>:\n")
    print(out)
    invisible(x)
}

#' Parse \code{\link{JScascade}} Class as Literal Javascript Code
#'
#' @param x 
#'
#' @return It will return a character string of the class "JS_EVAL".
#' @export
asJS <- function (x) UseMethod("asJS")

#' @export
asJS.JScascade <- function (x) {
    calls <- names(x)
    arguments <- vapply(x, asJS, character(1))
    each <- sprintf("%s(%s)", calls, arguments)
    ans <- 
    ans <- htmlwidgets::JS(
        paste(each, collapse = "\n."))
    ans
}

#' @export
asJS.default <- function (x) 
    htmlwidgets::JS(as.character(x))



#' @export
print.JS_EVAL <- function (x) {
    cat(x)
    invisible(x)
}

#' Add Quotation Mark to a String
#' 
#' Utility to facilitate the construction of \code{\link{JScascade}}.
#' 
#' @rdname quo
#'
#' @param string 
#' @param x It will be subsitituted and deparsed as a string. Then \code{equo} will
#'     call \code{quo} on it returning a string with additional quotation marks.
#'
#' @return
#' 
#' @export
#'
#' @examples
#' identical(
#'     quo("white"),
#'     equo(white)
#' )
#' q <- quo("hell('world')")
#' cat(q)
#' if (requireNamespace("jsonlite"))
#'     jsonlite::toJSON(q)
quo <- function (string) {
    # Escape the existing quotation marks before adding 
    s <- gsub("'", "\\'", string, fixed = TRUE)
    sprintf("'%s'", s)
}

#' @rdname quo
#' @export
equo <- function (x)
    quo(deparse(substitute(x)))



