context("assign.cluster")
library(greenclust)


test_that("assign.cluster returns expected results on a test cluster", {
    # Create a test clustering
    grc <- greenclust(table(chickwts$feed,
                            ifelse(chickwts$weight < 200, "Y", "N")))
    # Rename the type of some original observations
    feed.types <- as.character(chickwts$feed)
    feed.types[seq(1, length(feed.types), 12)] <- "newlevel1"
    feed.types[seq(3, length(feed.types), 15)] <- "newlevel2"
    feed.clustered <- assign.cluster(feed.types, greencut(grc))
    # Summarize clustering/assignment results
    tab <- table(names(feed.clustered), feed.clustered)

    expect_equal(dim(tab), c(8, 3))
    expect_equal(sum(tab), 71)
    expect_equal(as.numeric(tab[,1]), c(10, 0, 0, 10, 6, 5, 0, 10))
    expect_equal(tab[2, 2], 8)
    expect_equal(tab[3, 3], 10)
    expect_equal(tab[7, 3], 12)
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

    expect_silent(assign.cluster(c("a", "b", "c", "a"), clusters))
    expect_silent(assign.cluster(as.factor(c("e", "f", "f")), clusters))
})
