###############################################################################
#                                                                             #
#  greenplot                                                                  #
#                                                                             #
#  Part of the greenclust R package                                           #
#                                                                             #
#  Jeff Jetton                                                                #
#  March 2019                                                                 #
#                                                                             #
###############################################################################



#' Plot Statistics for a Greenclust Object
#'
#' Displays a connected scatterplot showing the r-squared values (x-axis) and
#' p-values (y-axis) at each clustering step of a \code{\link{greenclust}}
#' object. Points are labeled with their cutpoints, i.e., the number of
#' groups/clusters found at each step. The point with the lowest p-value
#' (typically the optimal cutpoint) is highlighted.
#' @param g an object of the type produced by \code{\link{greenclust}}
#' @param type 1-character string giving the type of plot desired: "p" for
#'   points, "l" for lines, and "b" (the default) for both points and lines.
#' @param bg a vector of background colors for open plot symbols. Also used
#'   for the line color if type is "b".
#' @param pch a vector of plotting characters or symbols: see
#'   \code{\link{points}}
#' @param cex a numerical vector giving the amount by which plotting
#'   characters and symbols should be scaled relative to the default. For
#'   this plot, the numeric labels on each point are always scaled to
#'   0.80 of this value.
#' @param optim.col color to use for highlighting the "optimal" cutpoint.
#' @param pos specifies the position of labels relative to their points:
#'   1 = below, 2 = left, 3 = above, and 4 = right.
#' @param main an overall title for the plot.
#' @param xlab a title for the x axis.
#' @param ylab a title for the y axis.
#' @param ... additional arguments to be passed to the plotting methods.
#' @references Greenacre, M.J. (1988) "Clustering the Rows and Columns of
#'   a Contingency Table," \emph{Journal of Classification 5}, 39-51.
#'   \doi{10.1007/BF01901670}
#' @seealso \code{\link{greenclust}}, \code{\link{greencut}},
#'     \code{\link{assign.cluster}}
#' @examples
#' # Combine Titanic passenger attributes into a single category
#' # and create a contingency table for the non-zero levels
#' tab <- t(as.data.frame(apply(Titanic, 4:1, FUN=sum)))
#' tab <- tab[apply(tab, 1, sum) > 0, ]
#'
#' grc <- greenclust(tab)
#' greenplot(grc)
#'
#'
#' # Plot using custom graphical parameters
#' greenplot(grc, type="p", bg="lightblue", optim.col="darkorange",
#'           pos=3, bty="n", cex.main=2, col.main="blue")
#' @export
greenplot <- function(g, type="b", bg="gray75", pch=21,
                      cex=1, optim.col="red", pos=2,
                      main="P-Value vs. R-Squared for Num. Clusters",
                      xlab="r-squared", ylab=NULL, ...) {

    if (is.null(g$p.values))
        stop("g is missing a 'p.values' vector")
    if (anyNA(g$p.values) || sum(is.nan(g$p.values)) != 0)
        stop("p.values vector cannot contain NAs")
    if (!is.numeric(g$p.values))
        stop("p.values vector must be numeric")
    if (is.null(g$height))
        stop("g is missing a 'height' vector")
    if (anyNA(g$height) || sum(is.nan(g$height)) != 0)
        stop("height vector cannot contain NAs")
    if (!is.numeric(g$height))
        stop("height vector must be numeric")
    if (!(type %in% c("b", "l", "p")))
        stop("type must be 'p' (points), 'l' (lines), or 'b' (both)")



    #Should error if any element in either the p.values or height vectors is NA (or NAN or non-numeric... maybe infinite?)

   # Also add a unit test for this

    # Add a small adjustment if any p-values are zero
    if (sum(g$p.values==0) > 1) {
        log.p <- log(g$p.values + 1e-15)
        if (is.null(ylab)) {
            ylab <- "log of (p-value + 1e-15)"
        }
    } else {
        log.p <- log(g$p.values)
        if (is.null(ylab)) {
            ylab <- "log of p-value"
        }
    }

    # Get r-squared from height vector
    r2 <- 1 - g$height
    # There's always one more height than p-value (the
    # final "1" height at the end). Remove it.
    r2 <- r2[-length(r2)]

    clust.num <- length(g$height):2
    optim.clust <- max(greencut(g))
    bg <- ifelse(clust.num==optim.clust, optim.col, bg)

    if (type=="b" || type=="l") {
        col <- ifelse(type=="b", bg, "black")
        graphics::plot(r2, log.p, type="l", col=col,
                       xlab=xlab, ylab=ylab,
                       main=main, ...)
    }
    if (type=="b") {
        graphics::points(r2, log.p, bg=bg, pch=pch, cex=cex, ...)
    }
    if (type=="p") {
        graphics::plot(r2, log.p, bg=bg, pch=pch, cex=cex,
                       xlab=xlab, ylab=ylab, main=main, ...)
    }

    graphics::text(r2, log.p, clust.num, pos=pos, cex=cex*0.8,
                   col=ifelse(clust.num==optim.clust, optim.col, 1),
                   ...)
}

