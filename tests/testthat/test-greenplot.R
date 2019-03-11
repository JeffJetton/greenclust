context("greenplot")
library(greenclust)


test_that("greenplot stops when passed an object without a p.values vector", {
    expect_error(greenplot(list(height=1:10)), "missing.*p.values")
})

test_that("greenplot stops when passed an object without a height vector", {
    expect_error(greenplot(list(p.values=11:20)), "missing.*height")
})

test_that("greenplot stops if type argument is invalid", {
    expect_error(greenplot(greenclust(matrix(7:20, ncol=2)), type="x"), "type must be")
})

test_that("greenplot runs without errors or messages when defaults are used", {
    expect_silent(greenplot(greenclust(matrix(7:20, ncol=2))))
})

test_that(paste("greenplot runs without errors or messages when greenplot",
                "arguments are passed"), {
    g <- greenclust(matrix(8:21, ncol=2))
    expect_silent(greenplot(g, type="p", main="Foo Bar Baz"))
    expect_silent(greenplot(g, bg="lightgreen", xlab="Foo"))
    expect_silent(greenplot(g, optim.col="orange", ylab="Bar"))
})

test_that(paste("greenplot runs without errors or messages when additional",
                "graphics parameters are passed"), {
    g <- greenclust(matrix(9:22, ncol=2))
    expect_silent(greenplot(g, pch=19, cex=2))
    expect_silent(greenplot(g, lty=3, lwd=5))
    expect_silent(greenplot(g, font=2, col.main="blue"))
})
