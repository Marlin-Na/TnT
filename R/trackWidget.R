


#' Convert a TnTBoard to htmlwidget
#' 
#' This function is only provided for users who are familiar with the concept of
#' \link[htmlwidgets]{htmlwidgets-package}. It explicitly convets a TnTBoard or
#' TnTGenome object to a htmlwidget object. You do not need it in most cases.
#'
#' @param tntdef A TnTBoard/TnTGenome object or a htmlwidget object. If it is
#'     a htmlwidget object, the function will return it as is.
#' @param elementId An id for the htmlwidget (random by default).
#' 
#' @return
#' The function returns a htmlwidget object.
#' 
#' @export
#' @examples
#' b <- TnTBoard(BlockTrack(GRanges("chr12", IRanges(1, 123))))
#' widget <- trackWidget(b)
#' class(widget)
#' identical(widget, trackWidget(widget))
trackWidget <- function (tntdef, elementId = NULL) {
    # Determine the class of tntdef
    if (is(tntdef, "TnTBoard"))
        tntdef <- compileBoard(tntdef) # becomes a JSCascade
    if (is(tntdef, "JSCascade"))
        tntdef <- asJS(tntdef)
    if (is(tntdef, "JavaScript"))
        tntdef <- as.character(tntdef)
    
    if (inherits(tntdef, "htmlwidget"))
        return(tntdef)
    
    stopifnot(is.character(tntdef))
    
    # forward options using x
    x <- list(
        tntdef = tntdef
    )
    
    # Sizing policy
    sizepolicy <- htmlwidgets::sizingPolicy(
        browser.fill = TRUE,
        knitr.figure = FALSE,
        knitr.defaultWidth = "100%",
        knitr.defaultHeight = "auto"
    )
    
    # create widget
    htmlwidgets::createWidget(
        name = 'trackWidget',
        x,
        # The width of widget can be automatically resized.
        width = NULL,
        # The height (of the container) can be automatically set in knitr
        height = NULL,
        sizingPolicy = sizepolicy,
        package = 'TnT',
        elementId = elementId
    )
}
## Example
if (interactive()) local({
    trackWidget('
    var axisTrack = tnt.board.track()
        .height(20)
        .color("white")
        .display(tnt.board.track.feature.axis());

    var blockTrack = tnt.board.track()
        .height(30)
        .color("grey")
        .data(tnt.board.track.data.sync()
            .retriever(function () {
                return [
                    {start: 200, end: 350},
                    {start: 400, end: 450}
                ]
            })
        )
        .display(
            tnt.board.track.feature.block()
            .color("red")
        );

    var board = tnt.board()
        .from(100)
        .to(500)
        .min(0)
        .max(1000)
        .add_track(axisTrack)
        .add_track(blockTrack);
    board
    ')
})


# TODO: provide shiny bindings


#' Shiny bindings for TnT
#'
#' Output and render functions for using TnT within Shiny
#' applications and interactive Rmd documents.
#'
#' @param outputId output variable to read from
#' @param width,height Must be a valid CSS unit (like \code{'100\%'},
#'   \code{'400px'}, \code{'auto'}) or a number, which will be coerced to a
#'   string and have \code{'px'} appended.
#' @param expr An expression that generates a TnTBoard/TnTGenome object.
#' @param env The environment in which to evaluate \code{expr}.
#' @param quoted Is \code{expr} a quoted expression (with \code{quote()})? This
#'   is useful if you want to save an expression in a variable.
#'
#' @name TnT-shiny
#' @return An output or render function that enables the use of the converted
#'     htmlwidget within Shiny applications.
#' @export
#' @examples
#' if (interactive() && require(shiny)) {
#'     ui <- fluidPage(fluidRow(
#'         column(width = 2, {
#'             "A Simple Example Here"
#'         }),
#'         column(width = 10, {
#'             TnTOutput("out")
#'         })
#'     ))
#'     server <- function (input, output) {
#'         re.btrack <- reactive({
#'             gr <- GRanges("chr12", IRanges(100, 1000))
#'             BlockTrack(gr)
#'         })
#'         output$out <- renderTnT({
#'             TnTBoard(re.btrack())
#'         })
#'     }
#'     shinyApp(ui = ui, server = server)
#' }
TnTOutput <- function (outputId, width = '100%', height = 'auto') {
    htmlwidgets::shinyWidgetOutput(outputId, 'trackWidget', width, height, package = "TnT")
}

#' @rdname TnT-shiny
#' @export
renderTnT <- function (expr, env = parent.frame(), quoted = FALSE) {
    if (!quoted) expr <- substitute(expr) # force quoted
    
    # A little bit hack, maybe there are better ways
    expr <- bquote(TnT::trackWidget(.(expr)))
    
    htmlwidgets::shinyRenderWidget(expr, TnTOutput, env, quoted = TRUE)
}

