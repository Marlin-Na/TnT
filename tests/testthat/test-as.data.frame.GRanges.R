context("as.data.frame.GRanges")

test_that("test util function .as.data.frame.GRanges", {
    gr <- GRanges("chr1", IRanges(1:4, 5:8))
    gr$tooltip <- as.data.frame(gr)
    df <- .as.data.frame.GRanges(gr)
    
    expect_identical(class(df), "data.frame")
    expect_identical(colnames(df), c("seqnames", "start", "end", "width", "strand", "tooltip"))
    expect_identical(class(df$tooltip), c("AsIs", "data.frame"))
    expect_identical(colnames(df$tooltip), c("seqnames", "start", "end", "width", "strand"))
})

