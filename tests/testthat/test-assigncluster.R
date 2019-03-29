context("assign.cluster")
library(greenclust)

# Create a test clustering
grc <- greenclust(table(chickwts$feed,
                        ifelse(chickwts$weight < 200, "Y", "N")))
clusters <- greencut(grc)
# Vector of renamed observations (to force non-matches)
feed.types <- as.character(chickwts$feed)
feed.types[seq(1, length(feed.types), 12)] <- "newlevel1"
feed.types[seq(3, length(feed.types), 15)] <- "newlevel2"


test_that(paste("assign.cluster returns expected results on a test cluster",
                "when impute is left to default (FALSE)"), {
    feed.clustered <- assign.cluster(feed.types, clusters)
    tab <- table(names(feed.clustered), feed.clustered, useNA="always")

    expect_equal(dim(tab), c(9, 4))
    expect_equal(sum(tab[,1:3]), 60)
    expect_equal(as.numeric(tab[,1]), c(10, 0, 0, 10, 0, 0, 0, 10, 0))
    expect_equal(tab[2, 2], 8)
    expect_equal(tab[3, 3], 10)
    expect_equal(tab[7, 3], 12)
    expectation <- c(6, 5)
    names(expectation) <- c("newlevel1", "newlevel2")
    expect_equal(tab[5:6, 4], expectation)
})


test_that(paste("assign.cluster returns expected results on a test cluster",
                "when impute is TRUE"), {
    feed.clustered <- assign.cluster(feed.types, clusters, impute=TRUE)
    tab <- table(names(feed.clustered), feed.clustered, useNA="always")

    expect_equal(dim(tab), c(9, 4))
    expect_equal(sum(tab[,1:3]), 71)
    expect_equal(as.numeric(tab[,1]), c(10, 0, 0, 10, 6, 5, 0, 10, 0))
    expect_equal(tab[2, 2], 8)
    expect_equal(tab[3, 3], 10)
    expect_equal(tab[7, 3], 12)
    expect_equal(sum(tab[ ,4]), 0)
})


test_that("assign.cluster returns a factor", {
    expect_equal(is.factor(assign.cluster(feed.types, clusters)), TRUE)
})


test_that("assign.cluster stops when x is invalid", {
    clusters  <- 1:3
    names(clusters) <- c("november", "oscar", "papa")

    expect_error(assign.cluster(NULL, clusters),
                 "x must be a factor or character vector")
    expect_error(assign.cluster(rep(c(TRUE, FALSE), 10), clusters),
                 "x must be a factor or character vector")
    expect_error(assign.cluster(1:10, clusters),
                 "x must be a factor or character vector")
    expect_error(assign.cluster(c("a", "b", NA, "d"), clusters),
                 "x must not contain NAs")
})


test_that("assign.cluster stops when 'clusters' is invalid", {
    x <- c(rep("a", 5), rep("b", 5))
    badclusters  <- 1:3
    names(badclusters) <- c("tango", NA, "uniform")

    expect_error(assign.cluster(x, NULL),
                 "'clusters' must be a numeric vector", fixed=TRUE)
    expect_error(assign.cluster(x, c("a", "b", "c")),
                 "'clusters' must be a numeric vector", fixed=TRUE)
    expect_error(assign.cluster(x, 1:3),
                 "elements in 'clusters' must have names", fixed=TRUE)
    expect_error(assign.cluster(x, badclusters),
                 "NA or.*names not allowed for .clusters.")
})


test_that("assign.cluster gives no errors when x and clusters are valid", {
    clusters  <- 1:3
    names(clusters) <- c("quebec", "romeo", "sierra")

    expect_silent(assign.cluster(c(rep("quebec", 5), rep("sierra", 5)),
                                   clusters))
    expect_silent(assign.cluster(as.factor(rep("romeo", 10)), clusters))
})
