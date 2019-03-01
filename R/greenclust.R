# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Build and Reload Package:  'Cmd + Shift + B'
#   Check Package:             'Cmd + Shift + E'
#   Test Package:              'Cmd + Shift + T'


################################################################################


#' Row Clustering Using Greenacre's Method
#'
#' Iteratively collapses the rows of a table (typically a contingency table)
#' by selecting the pair of rows each time whose combination results in a
#' new table with the smallest loss of chi-squared.
#'
#' @param x a numeric matrix or data frame
#' @param correct a logical indicating whether to apply a continuity
#'   correction if and when the clustered table reaches a 2x2 dimension.
#' @param verbose if TRUE, prints the clustered table along with r-squared and
#'   p-value at each step
#' @return An object of class \code{greenclust} which is compatible with most
#'   most \code{\link{hclust}} object functions, such as \code{\link{plot}()} and
#'   \code{\link{rect.hclust}()}
#' @examples
#' # Combine Titanic passenger attributes into a single category
#' tab <- t(as.data.frame(apply(Titanic, 4:1, FUN=sum)))
#' # Remove rows with all zeros
#' tab <- tab[apply(tab, 1, sum) > 0, ]
#'
#' gc <- greenclust(tab)
#' plot(gc)
#'
#' # Plot r-squared and p-values for each potential cut point
#' greenplot(gc)
#'
#' # Get clusters at suggested cut point
#' clusters <- greencut(gc)
#' rect.hclust(gc, max(clusters))
#'
#' @export
#' @importFrom stats chisq.test as.dendrogram order.dendrogram
#' @importFrom utils flush.console
greenclust <- function(x, correct=FALSE, verbose=FALSE) {

    #TODO: Clean up verbose output: Have it display original row name
    #TODO: Move combine rows to an external "dot" function

    # Check for valid arguments
    if (is.na(x) || is.null(x) || !(is.matrix(x) || is.data.frame(x)))
        stop("x must be a non-null matrix or data frame")
    if (!is.numeric(x))
        stop("x must be numeric")
    if (any(x < 0) || anyNA(x))
        stop("all elements must be nonnegative and finite")
    if (nrow(x) < 3)
        stop("x should have at least 3 rows")
    if (ncol(x) < 2)
        stop("x should have at least 2 columns")
    if(sum(apply(x, 1, sum)==0) > 0)
        stop("all row totals must be greater than zero")
    if(sum(apply(x, 2, sum)==0) > 0)
        stop("all column totals must be greater than zero")


    # Nested function for collapsing categories
    combine.rows <- function(m, r1, r2, combo.name) {
        # Returns a matrix that sums rows r1 and r2 of matrix m
        # combo.name is the name to give the newly-combined row
        # For speed, no checks are done. Ensure that:
        #    * Index r1 is less than r2
        #    * Both are valid rows in range of m

        # Copy matrix without row r1, preserving dimensions
        # (i.e., not letting it become a vector)
        new.m <- m[-r1, , drop=FALSE]
        # Replace row r2 (which is now at row r2-1) with combination
        combrow <- r2 - 1
        new.m[combrow, ] <- apply(m[c(r1, r2), ], 2, sum)
        row.names(new.m)[combrow] <- combo.name
        return(new.m)
    }

    # Remember chi-squared for the initial, un-clustered matrix
    suppressWarnings(initial.chi <- chisq.test(x, correct=correct)$statistic)
    # Replace row names with "negative" row numbers, to help us build the merge matrix
    saved.names <- rownames(x)
    n <- nrow(x)
    rownames(x) <- -(1:n)
    # Initialize the merge matrix and the heights/p.value/tie vectors as empty vectors
    merge.matrix <- vector()
    heights <- vector()
    p.values <- vector()
    tie <- vector()

    # Main clustering loop. Iterates over each clustering decision step.
    for (cluster.number in 1:(n-1)) {

        # Figure out how many row combinations we'll try out this iteration
        trials <- choose(nrow(x), 2)
        # Track the best chi-square, as well as the p-value and rows combined
        # that go with that best chi-square trial
        best.chi <- -Inf
        best.p <- NA
        best.i <- NA
        best.j <- NA
        best.trial <- NA
        trial.num <- 1

        # Loop over each combination of two rows in current version of matrix
        for (i in 1:(nrow(x)-1)) {
            # Combine row i with every row higher than i...
            for (offset in 1:(nrow(x)-i)) {
                j <- i + offset
                trial.x <- combine.rows(x, i, j, cluster.number)
                # Calculate new chi-sq test results
                new.chi <- suppressWarnings(chisq.test(trial.x, correct=correct))
                # Best so far?
                if (new.chi$statistic > best.chi) {
                    best.chi <- new.chi$statistic
                    best.p <- new.chi$p.value
                    best.i <- i
                    best.j <- j
                    best.trial <- trial.num
                    tie.flag <- FALSE
                } else if (new.chi$statistic == best.chi) {
                    tie.flag <- TRUE
                }
                # On to the next trial...
                trial.num <- trial.num + 1
            }
        }

        # Add info on winning clustering to our tracking variables
        merge.matrix <- rbind(merge.matrix, as.numeric(rownames(x)[c(best.i, best.j)]))
        heights <- c(heights, (initial.chi - best.chi)/initial.chi)
        p.values <- c(p.values, best.p)
        tie <- c(tie, tie.flag)

        # Change our matrix to the winning combined row matrix
        if (best.trial==trials) {
            x <- trial.x
        } else {
            x <- combine.rows(x, best.i, best.j, cluster.number)
        }

        if (verbose && cluster.number < (n - 1)) {
            cat(paste("Step:", cluster.number,"\n"))
            print(x)
            cat(paste("\nChi-squared:",
                      round(best.chi, 2), "\n"))
            cat(paste("p-value:", signif(best.p, 4), "\n"))
            cat(paste("R-squared:",
                      round(best.chi/initial.chi, 4),
                      "\n\n\n"))
            flush.console()
        }
    }

    # Final height should always be 1
    heights[length(heights)] <- 1
    # Remove the height names given by the chisq.test function
    names(heights) <- NULL

    # Final p.value is not meaningful
    p.values <- p.values[-length(p.values)]

    # Pack everything up (except the order vector)
    gc <- structure(list(merge=merge.matrix,
                         height=heights,
                         order=vector(),
                         labels=saved.names,
                         call=match.call(),
                         dist.method="chi-squared",
                         p.values=p.values,
                         tie=tie),
                    class=c("hclust", "greenclust"))

    # Use dendrogram object to help create the order vector
    gc$order <- order.dendrogram(as.dendrogram(gc))

    return(gc)
}



#' @export
greencut <- function(g, k=NULL, h=NULL, r.squared=TRUE, p.value=TRUE) {


    # TODO: support getting passed k or h
    #       use either as value for cutree, but still return r-sq
    # If h or k passed and add.r2 = FALSE, basically functions as
    # cutree pass-through

    # Check validity specific to greenclust objects
    # (hclust validity will be checked by the cutree function)
    if (!inherits(g, "greenclust") || is.null(g$p.values))
        stop("not a valid 'greenclust' object")

    # Performs a cutree, automatically calculating the
    # optimal number of groups based on the cluster
    # with the chi.sq test having the lowest p.value
    min.indices <- which(g$p.values==min(g$p.values))
    # In case of ties, go with the highest index
    # (smallest number of clusters)
    min.i <- min.indices[length(min.indices)]
    # Convert index to cluster count
    c <- length(g$order) - min.i

    # Perform cut
    groups <- stats::cutree(g, c)

    # Calculate r-squared
    if (r.squared) {
        attr(groups, "r.squared") <- 1 - g$height[min.i]
    }

    # Add p.value
    if (p.value) {
        attr(groups, "p.value") <- g$p.values[min.i]
    }
    return(groups)
}



#' @export
greenplot <- function(g, type="b", bg="gray75", pch=21, cex=1,
                      optim.col="red", pos=2, xlab="r-squared", ylab=NULL,
                      main="P-Value vs. R-Squared for Num. Clusters", ...) {

    # TODO: Check valid arguments (for type, etc.)


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
    if (type=="b" || type=="p") {
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


