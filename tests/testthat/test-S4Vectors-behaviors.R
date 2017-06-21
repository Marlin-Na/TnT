
# This test aims to monitor some special behaviors involving conversion of
# nested data frame in S4Vectors.
#
# It seems that currently S4Vectors does not really consider nested data.frame nor DataFrame
# a valid usage.

context("S4Vectors-behaviors")

suppressPackageStartupMessages({
    library(testthat)
    library(S4Vectors)
})


test_that("as.data.frame(DF) with nested df will have the nested df wrapped in AsIs class", {
    DF <- DataFrame(x = c(1:5))
    DF$nested <- data.frame(
        nested_a = runif(5),
        nested_b = paste0("aa", 1:5)
    )
    converted.df <- as.data.frame(DF)
    expect_identical(unclass(converted.df$nested), unclass(DF$nested))
    
    # Error is associated with printing method of the converted data frame
    expect_error(print(converted.df), "dims .* do not match the length of object")
    # Reason is that the column of nested data frame has an AsIs class
    expect_identical(class(converted.df$x), "integer")
    expect_identical(class(converted.df$nested), c("AsIs", "data.frame"))
    # Unwrap the AsIs class
    class(converted.df$nested) <- class(converted.df$nested)[class(converted.df$nested) != "AsIs"]
    # No error now
    capture_output(print(converted.df))
})

test_that("DF with nested DF can not be converted to data.frame", {
    DF <- DataFrame(x = c(1:5))
    DF$nested <- DataFrame(
        nested_a = runif(5),
        nested_b = paste0("aa", 1:5)
    )
    
    expect_error(as.data.frame(DF))
})

test_that("Error printing GRanges with a meta-column of nested df which has only one column", {
    suppressPackageStartupMessages(library(GenomicRanges))
    
    gr <- GRanges("chr21", IRanges(1:5, width = 1))
    
    gr$df <- data.frame(x = 1:5)
    expect_error(capture.output(show(gr)),
                 "number of rows of matrices must match")
    
    gr$col <- 1:5
    expect_warning(capture.output(show(gr)),
                   "row names were found from a short variable and have been discarded")
    
    gr$df <- data.frame(x = 1:5, y = 1:5)
    # Now the printing is correct
    capture.output(show(gr))
})
