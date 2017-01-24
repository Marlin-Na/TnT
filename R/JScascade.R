

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
#' @export
#'
#' @examples
#' axisTrack <- jc(
#'     tnt.board.track = NULL,
#'     height = 20,
#'     color = "white",
#'     display = jc(tnt.board.track.feature.axis = NULL)
#' )
#' blockTrack <- jc(
#'     tnt.board.track = NULL,
#'     height = 30,
#'     color = "yellow",
#'     data = jc(
#'         tnt.board.track.data.sync = NULL,
#'         retriever = JS("function() {return [{start : 200, end : 350}]}")
#'     ),
#'     display = jc(
#'         tnt.board.track.feature.block = NULL,
#'         color = "blue",
#'         index = JS("function (d) {return d.start}")
#'     )
#' )
#' tntdef <- jc(
#'     tnt.board.genome = NULL,
#'     from = 0,
#'     to = 500,
#'     min = 0,
#'     max = 1000,
#'     add_track = axisTrack,
#'     add_track = blockTrack
#' )
#' tntdef
#' asJS(tntdef)
#' TnT(asJS(tntdef))
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


# Function to combine multiple "JScascade" objects
#' @export
c.JScascade <- function(...) {
    jclist <- lapply(list(...),
        function (x) {
            stopifnot(is(x, "JScascade"))
            unclass(x)
        }
    )
    ans <- do.call(c, jclist)
    class(ans) <- "JScascade"
    ans
}

#' @export
`[.JScascade` <- function (x, condition) {
    li <- unclass(x)
    ans <- li[condition]
    class(ans) <- "JScascade"
    ans
}

#' @export
print.JScascade <- function(x, ...) {
    calls <- names(x)
    arguments <- vapply(x, FUN.VALUE = character(1),
        function (e) {
            if (is(e, "JScascade"))
                return("<S3: JScascade>")
            else 
                return(asJS(e))
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
    ans <- JS(paste(each, collapse = "\n."))
    ans
}


#' @export
asJS.logical <- function (x)
    JS(if (x) "true" else "false")

#' @export
asJS.NULL <- function (x)
    character(1)

#' @export
asJS.character <- function (x)
    quo(x)

#' @export
asJS.default <- function (x) 
    JS(as.character(x))


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
    # TODO: There is a problem, try not to subsititute existing "\'" with "\\'"?
    s <- gsub("'", "\\'", string, fixed = TRUE)
    sprintf("'%s'", s)
}

#' @rdname quo
#' @export
equo <- function (x)
    quo(deparse(substitute(x)))



