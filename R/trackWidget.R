


#' @export
trackWidget <- function (tntdef, elementId = NULL) {
    # Determine the class of tntdef
    if (inherits(tntdef, "TnTBoard"))
        tntdef <- compileBoard(tntdef) # becomes a JSCascade
    if (inherits(tntdef, "JSCascade"))
        tntdef <- asJS(tntdef)
    if (inherits(tntdef, "JavaScript"))
        tntdef <- as.character(tntdef)
    
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


# #' Shiny bindings for TnT
# #'
# #' Output and render functions for using TnT within Shiny
# #' applications and interactive Rmd documents.
# #'
# #' @param outputId output variable to read from
# #' @param width,height Must be a valid CSS unit (like \code{'100\%'},
# #'   \code{'400px'}, \code{'auto'}) or a number, which will be coerced to a
# #'   string and have \code{'px'} appended.
# #' @param expr An expression that generates a TnT
# #' @param env The environment in which to evaluate \code{expr}.
# #' @param quoted Is \code{expr} a quoted expression (with \code{quote()})? This
# #'   is useful if you want to save an expression in a variable.
# #'
# #' @name TnT-shiny
# #'
# #' @export
# #' @examples
# #' if (interactive() && require(shiny)) {
# #'     ui <- fluidPage(
# #'         fluidRow(TnTOutput("out"))
# #'     )
# #'     server <- function (input, output) {
# #'         output$out <- renderTnT({
# #'             TnT("
# #'               tnt.board.genome()
# #'                 .from(0)
# #'                 .to(500)
# #'                 .min(0)
# #'                 .max(1000)
# #'                 .add_track(tnt.board.track()
# #'                            .height(20)
# #'                            .color('white')
# #'                            .display(tnt.board.track.feature.axis()))
# #'                 .add_track(tnt.board.track()
# #'                            .height(30)
# #'                            .color('#FFCFDD')
# #'                            .data(tnt.board.track.data.sync()
# #'                                  .retriever(function() {return [{start : 200, end : 350}]}))
# #'                            .display(tnt.board.track.feature.block()
# #'                                     .color('blue')
# #'                                     .index(function (d) {return d.start})))
# #'             ")
# #'         })
# #'     }
# #'     shinyApp(ui = ui, server = server)
# #' }
# TnTOutput <- function(outputId, width = '100%', height = '400px'){
#     htmlwidgets::shinyWidgetOutput(outputId, 'TnT', width, height, package = 'TnT')
# }
# 
# #' @rdname TnT-shiny
# #' @export
# renderTnT <- function(expr, env = parent.frame(), quoted = FALSE) {
#     if (!quoted) { expr <- substitute(expr) } # force quoted
#     htmlwidgets::shinyRenderWidget(expr, TnTOutput, env, quoted = TRUE)
# }

