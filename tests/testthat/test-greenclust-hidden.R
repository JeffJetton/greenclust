context("greenclust (hidden functions)")
library(greenclust)

# Test matrix
m <- matrix(1:16, ncol=4)
colnames(m) <- paste0("Col", 1:4)
rownames(m) <- 1:nrow(m)


test_that(".combine.rows returns a matrix with one fewer row", {
    expect_equal(nrow(greenclust:::.combine.rows(m, 1, 2, 42)), nrow(m) - 1)
})

test_that(".combine.rows returns a matrix with same number of equally-named columns", {
    expect_equal(colnames(greenclust:::.combine.rows(m, 3, 4, 99)), colnames(m))
})

test_that(".combine.rows returns a matrix with a correctly-combined row", {
    combo <- greenclust:::.combine.rows(m, 2, 4, 86)
    expect_equal(sum(combo), sum(m))
    expect_equal(combo["86",], apply(m[c(2, 4), ], 2, sum))
})

test_that(".print.step returns correct cluster step information", {
    # Set one row name to a negative (representing unclustered)
    rownames(m)[3] <- -2
    expect_output(greenclust:::.print.step(m, c("Whiskey", "X-Ray", "Yankee", "Zulu"),
                                           99,TRUE, 0.424242, 0.8675309, 2.0),
                  "^Step.*99.*tie")
    expect_output(greenclust:::.print.step(m, c("Whiskey", "X-Ray", "Yankee", "Zulu"),
                                           99,TRUE, 0.424242, 0.8675309, 2.0),
                  "X-Ray", fixed=TRUE)
    expect_output(greenclust:::.print.step(m, c("Whiskey", "X-Ray", "Yankee", "Zulu"),
                                           99,TRUE, 0.424242, 0.8675309, 2.0),
                  "Chi-squared: 0.4", fixed=TRUE)
    expect_output(greenclust:::.print.step(m, c("Whiskey", "X-Ray", "Yankee", "Zulu"),
                                           99,TRUE, 0.424242, 0.8675309, 2.0),
                  "p-value: 0.8", fixed=TRUE)
    expect_output(greenclust:::.print.step(m, c("Whiskey", "X-Ray", "Yankee", "Zulu"),
                                           99,TRUE, 0.424242, 0.8675309, 2.0),
                  "R-squared: 0.2", fixed=TRUE)
})
