context("JavascriptCascade")

library(testthat)


test_that("JSCascade Construction", {
    expect_identical(jc, JSCascade)
    expect_identical(ma, MultiArgs)
    expect_identical(js, JavaScript)
    
    expect_error(jc(23L))
    expect_error(jc(ir = IRanges::IRanges(1,2)))
    
    # TODO: This error is associated with the "show" method, we may:
    #    1. Add a validity check when the jc is constructed, OR
    #    2. If it is not a scalar, use `jsonlite::toJSON` to convert the object
    expect_error(show(jc(x = c(1,2,3))))
    expect_error(show(jc(x = c(TRUE, FALSE))))
    expect_error(show(jc(x = c("char1", "char2"))))
    
    call_list <- list(integer = 42L, numeric = .05, logical = TRUE, null = NULL,
                  character = "character", jscascade = jc(multiargs = ma(1, "chr")),
                  javascript = js("'jscharacter'"))
    
    j <- jc(.listData = call_list)
    
    expect_identical(j, do.call(jc, call_list))
    expect_identical(j, asJC(call_list))
    
    expect_false(all(names(j) %in% "null"), label = "NULL value should have been removed")
})


test_that("JSCascade to JS Conversion", {
    expect_js_identical <- function (jc1, jc2, ...) {
        js1 <- asJS(jc1)
        js2 <- asJS(jc2)
        expect_identical(js1, js2, ...)
    }
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


test_that("JSCascade Combination", {
    j <- jc(x = 1)
    k <- jc(y = jc(a = "chr"))
    expect_identical(c(j, k), jc(x = 1, y = jc(a = "chr")))
})
