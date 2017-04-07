

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
#'     tnt.board = NULL,
#'     from = 0,
#'     to = 500,
#'     min = 50,
#'     max = 1000,
#'     width = 500,
#'     add_track = axisTrack,
#'     add_track = blockTrack
#' )
#' tntdef
#' asJS(tntdef)
#' if (interactive())
#'     TnT(asJS(tntdef))
JScascade <- function (...) {
    JCfromlst(list(...))
}

#' @export
JCfromlst <- function (lst) {
    if (!is.list(lst))
        #lst <- as.list(lst)
        stop("The argument is not a list.")
    if (is.null(names(lst)) || any(names(lst) == ""))
        stop("Names of the function calls must be specified.")
    class(lst) <- "JScascade"
    lst
}


#' @export
#' @rdname JScascade
jc <- JScascade


# Function to combine multiple "JScascade" objects
#' @export
c.JScascade <- function(...) {
    jclist <- lapply(list(...), function (x) unclass(x))
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
    funs <- names(x)
    arguments <- vapply(x, FUN.VALUE = character(1),
        function (e) {
            if (is(e, "JScascade"))
                return("<S3: JScascade>")
            else 
                return(asJS(e))
        }
    )
    main <- {
        main <- data.frame(js_functions = funs)
        main$arguments <- arguments
        rownames(main) <- paste("step", seq_along(x))
        main
    }
    head <- c("Javascript function cascade:\n")
    tail <- c("You can use", sQuote('asJS'), "to convert it to javascript code\n")
    cat(head)
    print(main)
    cat("---------\n")
    cat(tail)
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
asJS.numeric <- function (x)
    JS(as.character(x))

#' @export
asJS.default <- function (x) 
    JS(as.character(x))





#' @export
asJS.multiArgs <- function (x) {
    if (is.null(names(x)) || all(names(x) == "")) {
        if (length(x) == 1)
            return(asJS(x[[1]]))
        else {
            l <- vapply(x, FUN = asJS, FUN.VALUE = character(1))
            return(JS(paste(l, collapse = ", ")))
        }
    }
    else {
        warning("The names will be ignored at present.")
        names(x) <- NULL
        return(asJS(x))
    }
}


#' @export
mafromlst <- function (lst) {
    #
    if (!is.list(lst))
        stop("The argument is not a list.")
    class(lst) <- "multiArgs"
    lst
}

#' @export
multiArgs <- function (...) {
    mafromlst(list(...))
}

#' @export
ma <- multiArgs




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
    #       Also take a look at "shQuote()".
    s <- gsub("'", "\\'", string, fixed = TRUE)
    sprintf("'%s'", s)
}

#' @rdname quo
#' @export
equo <- function (x)
    quo(deparse(substitute(x)))



