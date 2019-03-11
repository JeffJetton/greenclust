context("greencut")
library(greenclust)

# Test matrix (Guttman's "Worries" Data)
m <- matrix(c(128, 52, 81, 14, 12,
              118, 28, 32,  6,  7,
              218, 28, 97, 12, 14,
              11,  2,  4,  1,  1,
              104, 22, 61,  8,  5,
              117, 24, 70,  9,  7,
              42,  6, 20,  2,  0,
              48, 16,104, 14,  9),
            ncol=5, byrow=TRUE)
rownames(m) <- c("oth", "pol", "mil", "eco",
                 "enr", "sab", "mto", "per")
colnames(m) <- c("euam", "ifea", "asaf", "ifaa", "ifi")

# Test cluster object
g <- greenclust(m)

test_that("greencut stops when passed a non-greenclust object", {
    expect_error(greencut(NULL), "not a valid.*greenclust.*object")
    expect_error(greencut(NA), "not a valid.*greenclust.*object")
    expect_error(greencut(c("foo", "bar", "baz")), "not a valid.*greenclust.*object")
    expect_error(greencut(123.456), "not a valid.*greenclust.*object")
    expect_error(greencut(hclust(dist(m))), "not a valid.*greenclust.*object")
})

test_that("greencut returns a vector of correct length", {
    expect_equal(length(greencut(g)), length(g$order))
})

test_that("greencut returns names, r.squared, and p.value as attributes", {
    expect_equal(names(attributes(greencut(g))), c("names", "r.squared", "p.value"))
})

test_that("greencut returns correct number of clusters by default on test matrix", {
    expect_equal(max(greencut(g)), 4)
})

test_that("greencut returns specified clusters when k is given", {
    for (k in 1:length(g$order)) {
        expect_equal(max(greencut(g, k=k)), k, info=paste("Test value of k is", k))
    }
})

test_that("greencut returns specified clusters when h is given", {
    for (i in 1:length(g$height)) {
        exp.c <- length(g$order) - i + 1
        h <- g$height[i] * 0.95
        expect_equal(max(greencut(g, h=h)), exp.c,
                     info=paste0("Test value of h is ", h,
                                "; Expected clusters: ", exp.c))
    }
})
