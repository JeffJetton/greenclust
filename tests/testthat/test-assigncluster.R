context("assign.cluster")
library(greenclust)


test_that(paste("assign.cluster returns expected results on a test cluster"), {
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

# Return value should be numeric
