#' <Add Title>
#'
#' <Add Description>
#'
#' @param tntdef
#'     A string of javascript code representing the definition of TnT browser/tree.
#'     It can be constructed using \code{\link{JScascade}} and \code{\link{asJS}}.
#'
#' @param width,height,elementId
#' 
#' @export
#' @examples
#' if (interactive())
#' TnT("
#'   tnt.board.genome()
#'     .from(0)
#'     .to(500)
#'     .min(0)
#'     .max(1000)
#'     .add_track(tnt.board.track()
#'                .height(20)
#'                .color('white')
#'                .display(tnt.board.track.feature.axis()))
#'     .add_track(tnt.board.track()
#'                .height(30)
#'                .color('#FFCFDD')
#'                .data(tnt.board.track.data.sync()
#'                      .retriever(function() {return [{start : 200, end : 350}];}))
#'                .display(tnt.board.track.feature.block()
#'                         .color('blue')
#'                         .index(function (d) {return d.start})))
#' ")
TnT <- function(tntdef, width = NULL, height = NULL, elementId = NULL) {

    #stopifnot(is(tntdef,"JS_EVAL"))
    tntdef <- JS(tntdef)

    # forward options using x
    x <- list(
        tntdef = tntdef
    )

    # create widget
    htmlwidgets::createWidget(
        name = 'TnT',
        x,
        width = width,
        height = height,
        package = 'TnT',
        elementId = elementId
    )
}

#' Shiny bindings for TnT
#'
#' Output and render functions for using TnT within Shiny
#' applications and interactive Rmd documents.
#'
#' @param outputId output variable to read from
#' @param width,height Must be a valid CSS unit (like \code{'100\%'},
#'   \code{'400px'}, \code{'auto'}) or a number, which will be coerced to a
#'   string and have \code{'px'} appended.
#' @param expr An expression that generates a TnT
#' @param env The environment in which to evaluate \code{expr}.
#' @param quoted Is \code{expr} a quoted expression (with \code{quote()})? This
#'   is useful if you want to save an expression in a variable.
#'
#' @name TnT-shiny
#'
#' @export
#' @examples
#' if (interactive() && require(shiny)) {
#'     ui <- fluidPage(
#'         fluidRow(TnTOutput("out"))
#'     )
#'     server <- function (input, output) {
#'         output$out <- renderTnT({
#'             TnT("
#'               tnt.board.genome()
#'                 .from(0)
#'                 .to(500)
#'                 .min(0)
#'                 .max(1000)
#'                 .add_track(tnt.board.track()
#'                            .height(20)
#'                            .color('white')
#'                            .display(tnt.board.track.feature.axis()))
#'                 .add_track(tnt.board.track()
#'                            .height(30)
#'                            .color('#FFCFDD')
#'                            .data(tnt.board.track.data.sync()
#'                                  .retriever(function() {return [{start : 200, end : 350}]}))
#'                            .display(tnt.board.track.feature.block()
#'                                     .color('blue')
#'                                     .index(function (d) {return d.start})))
#'             ")
#'         })
#'     }
#'     shinyApp(ui = ui, server = server)
#' }
TnTOutput <- function(outputId, width = '100%', height = '400px'){
    htmlwidgets::shinyWidgetOutput(outputId, 'TnT', width, height, package = 'TnT')
}

#' @rdname TnT-shiny
#' @export
renderTnT <- function(expr, env = parent.frame(), quoted = FALSE) {
    if (!quoted) { expr <- substitute(expr) } # force quoted
    htmlwidgets::shinyRenderWidget(expr, TnTOutput, env, quoted = TRUE)
}
