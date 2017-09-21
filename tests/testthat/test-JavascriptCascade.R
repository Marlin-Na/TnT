context("JavascriptCascade")

library(testthat)


test_that("JSCascade Construction", {
    expect_identical(jc, JSCascade)
    expect_identical(ma, MultiArgs)
    expect_identical(js, JavaScript)
    
    expect_error(jc(23L))
    expect_error(jc(ir = IRanges::IRanges(1,2)))
    
    
    call_list <- list(integer = 42L, numeric = .05, logical = TRUE, null = NULL,
                  character = "character", jscascade = jc(multiargs = ma(1, "chr")),
                  javascript = js("'jscharacter'"))
    
    j <- jc(.listData = call_list)
    
    expect_identical(j, do.call(jc, call_list))
    expect_identical(j, asJC(call_list))
    
    expect_false(all(names(j) %in% "null"), label = "NULL value should have been removed")
})


expect_js_identical <- function (jc1, jc2, ...) {
    js1 <- asJS(jc1)
    js2 <- asJS(jc2)
    expect_identical(js1, js2, ...)
}

test_that("JSCascade to JS Conversion", {
    expect_js_identical(jc(char = "char"),
                        jc(char = js("\"char\"")))
    expect_js_identical(jc(num = 12L),
                        jc(num = js("12")))
    expect_js_identical(jc(logical = TRUE),
                        jc(logical = js("true")))
    expect_js_identical(jc(logical = FALSE),
                        jc(logical = js("false")))
    expect_js_identical(jc(emptyarg = ma()),
                        jc(emptyarg = js("")))
})

library(jsonlite)

test_that("Conversion of JSON", {
    json <- toJSON(iris, pretty = TRUE)
    expect_js_identical(
        jc(x = json),
        jc(x = js(unclass(json)))
    )
    
    json <- toJSON("chr", auto_unbox = TRUE)
    expect_js_identical(
        jc(x = json),
        jc(x = "chr")
    )
    expect_js_identical(
        jc(x = json),
        jc(x = js('"chr"'))
    )
})

test_that("Conversion of data frame", {
    df <- {
        df <- data.frame(x = 1:3)
        df$l <- list(a = 1, b = 1:2, c = 1:3)
        df$d <- data.frame(dx = 1:3, dy = 4:6)
        df$ldf <- split(data.frame(x = 1:6, group = rep(1:3, 1:3)), rep(1:3, 1:3))
#            list(d1 = df[, 1:2], d2 = data.frame(x = 3), d3 = df[3, 1:2])
        df
    }
    # In practice, we may need to convert data frame that have columns of:
    #   1. a nested data frame (e.g. tooltip)
    #   2. a list of data frame (exons of each transcript)
    
    # TODO
    expect_js_identical(
        jc(data = df['d']),
        jc(data = toJSON(df['d'], pretty = 2))
    )
    expect_js_identical(
        jc(data = df['ldf']),
        jc(data = toJSON(df['ldf'], auto_unbox = TRUE, pretty = 2))
    )
    expect_identical(
        toJSON(df['ldf'], auto_unbox = FALSE),
        toJSON(df['ldf'], auto_unbox = TRUE)
    )
})


test_that("JSCascade Combination", {
    j <- jc(x = 1)
    k <- jc(y = jc(a = "chr"))
    expect_identical(c(j, k), jc(x = 1, y = jc(a = "chr")))
})
