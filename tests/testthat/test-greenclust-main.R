context("greenclust (main function)")
library(greenclust)

# Test matrix
m <- matrix(c(1:15, rep(c(0, 20, 25), 5), 45:31), ncol=3)
colnames(m) <- c("yes", "no", "unknown")
rownames(m) <- state.name[seq(3, by=3, length.out=nrow(m))]


test_that("greenclust stops when x has dimensions less than 3x2", {
    expect_error(greenclust(matrix(c(4, 8), nrow=1)), "rows")
    expect_error(greenclust(matrix(c(4, 8), ncol=1)), "rows")
    expect_error(greenclust(matrix(1:4, ncol=2)), "rows")
    expect_error(greenclust(matrix(1:8, ncol=1)), "columns")
})

test_that("greenclust stops when x is not a matrix or data frame", {
    expect_error(greenclust(NULL), "non-null", fixed=TRUE)
    expect_error(greenclust(NA), "non-null", fixed=TRUE)
    expect_error(greenclust(matrix(c(1:3, NA, 5:7, NA, 9), ncol=3)),
                 "non-null", fixed=TRUE)
    expect_error(greenclust("abc"), "matrix")
    expect_error(greenclust(c("a", "b", "c"), "matrix"))
    expect_error(greenclust(c("a", "b", "c"), "matrix"))
    expect_error(greenclust(1:20), "matrix")
    expect_error(greenclust(list(a=matrix(1:6, ncol=2),
                                 b=matrix(1:12, ncol=3))), "matrix")
})

test_that("greenclust stops when x is non-numeric or has negative/non-finite values", {
    expect_error(greenclust(matrix(rep("foo", 20), ncol=4)), "numeric")
    expect_error(greenclust(matrix(c(1:3, NaN, 5:8, NaN), ncol=3)), "x must be")
    expect_error(greenclust(matrix(-10:9, ncol=4)), "negative")
    expect_error(greenclust(matrix(c(-Inf, 1:7, Inf), ncol=3)), "finite")
})

test_that("greenclust stops when x has negative row or column sums", {
    expect_error(greenclust(matrix(c(0:4, 0, 10:13), ncol=2)),
                 "all row totals must be greater than zero")
    expect_error(greenclust(matrix(c(rep(0, 5), 1:10), ncol=3)),
                 "all column totals must be greater than zero")
})

test_that("greenclust creates rownames when none are provided", {
    g <- greenclust(matrix(1:12, ncol=2))
    expect_equal(g$labels, as.character(1:6))
})

test_that("greenclust returns a valid greenclust/hclust object", {
    g <- greenclust(m)
    expect_is(g, "greenclust")
    expect_is(g, "hclust")
    expect_output(str(g), "List of 8")
    expect_output(str(g), "$ height     : num [1:", fixed=TRUE)
    expect_output(str(g), "$ order      : num [1:", fixed=TRUE)
    expect_output(str(g), "$ labels     : chr [1:", fixed=TRUE)
    expect_output(str(g), "$ call       : language greenclust(x = m)",
                  fixed=TRUE)
    expect_output(str(g), "$ dist.method: chr", fixed=TRUE)
    expect_output(str(g), "$ p.values   : num [1:", fixed=TRUE)
    expect_output(str(g), "$ tie        : logi [1:", fixed=TRUE)
    expect_equal(ncol(g$merge), 2)
    expect_equal(length(g$height), length(g$tie))
    expect_equal(length(g$order), length(g$labels))
    expect_equal(nrow(g$merge), length(g$p.values) + 1)
})

test_that("greenclust works when verbose=TRUE", {
    expect_output(greenclust(matrix(1:6, ncol=2), verbose=TRUE),
                  "Step: 1", fixed=TRUE)
    expect_output(greenclust(matrix(1:6, ncol=2), verbose=TRUE),
                  "Cluster 1")
})

test_that("greenclust gives expected results on a test matrix", {
    g <- greenclust(m)
    expect_equal(g$order, c(1, 4, 7, 10, 13, 2, 3, 5, 6, 8, 9, 11, 12, 14, 15))
    expect_equal(g$merge[1, ], c(-14, -15))
    expect_equal(g$merge[14, ], c(12, 13))
    expect_equal(sum(g$tie), 4)
    expect_equal(round(min(log(g$p.values))), -59)
    expect_equal(round(sum(g$height) * 100), 183)
})



