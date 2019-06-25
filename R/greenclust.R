###############################################################################
#                                                                             #
#  greenclut                                                                  #
#                                                                             #
#  Part of the greenclust R package                                           #
#                                                                             #
#  Jeff Jetton                                                                #
#  March 2019                                                                 #
#                                                                             #
###############################################################################



#########  "Hidden" functions used by greenclust()  ###########################

# Return a new matrix that sums rows r1 and r2 of matrix m
# combo.name is the name to give the newly-combined row
# For speed, no checks are done. Ensure that:
#    * Index r1 is less than r2
#    * Both are valid rows in range of m
.combine.rows <- function(m, r1, r2, combo.name) {
    # Copy matrix without row r1, preserving dimensions
    # (i.e., not letting it become a vector)
    new.m <- m[-r1, , drop=FALSE]
    # Replace row r2 (which is now at row r2-1) with combination
    combrow <- r2 - 1
    new.m[combrow, ] <- apply(m[c(r1, r2), ], 2, sum)
    row.names(new.m)[combrow] <- combo.name
    return(new.m)
}


# Display information about a particular clustering step
# Called by greenclust() when verbose==TRUE
#
#           x: final matrix at this step
#  orig.names: text to display when row name < 0
#           n: cluster step number
#    tie.flag: were there tied chi-squares at this step?
#       chisq: chi-squared statistic of this step's matrix
#           p: p-value of above chi-squared test
# initial.chi: chi-squared of original (unclustered) matrix,
#              used to calculate r-squared
#
.print.step <- function(x, orig.names, n, tie.flag, chisq, p, initial.chi) {
    # Translate negative row names to original text and append
    # non-negative row names with "Cluster "
    rnames <- rownames(x)
    name.is.neg <- as.numeric(rnames) < 0
    rnames[name.is.neg] <- orig.names[-as.numeric(rnames[name.is.neg])]
    rnames[!name.is.neg] <- paste("Cluster", rnames[!name.is.neg])
    rownames(x) <- rnames
    # Display step information
    cat(paste("Step:", n))
    if (tie.flag) {
        cat(" (tie)\n")
    } else {
        cat("\n")
    }
    print(x)
    cat(paste("\nChi-squared:", round(chisq, 2), "\n"))
    cat(paste("p-value:", signif(p, 4), "\n"))
    cat(paste("R-squared:", round(chisq/initial.chi, 4), "\n\n\n"))
    utils::flush.console()
}


###############################################################################



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
#'   \code{\link{hclust}} object functions, such as \code{\link{plot}()} and
#'   \code{\link{rect.hclust}()}. The height vector represents the proportion
#'   of chi-squared, relative to the original table, seen at each clustering
#'   step. The greenclust object also includes a vector for the chi-squared
#'   test p-value at each step and a boolean vector indicating whether the
#'   step had a tie for "winner".
#' @references Greenacre, M.J. (1988) "Clustering the Rows and Columns of
#'   a Contingency Table," \emph{Journal of Classification 5}, 39-51.
#'   \url{https://doi.org/10.1007/BF01901670}
#' @seealso \code{\link{greencut}}, \code{\link{greenplot}},
#'     \code{\link{assign.cluster}}
#' @examples
#' # Combine Titanic passenger attributes into a single category
#' tab <- t(as.data.frame(apply(Titanic, 4:1, FUN=sum)))
#' # Remove rows with all zeros
#' tab <- tab[apply(tab, 1, sum) > 0, ]
#'
#' # Perform clustering on contingency table
#' grc <- greenclust(tab)
#'
#' # Plot r-squared and p-values for each potential cut point
#' greenplot(grc)
#'
#' # Get clusters at suggested cut point
#' clusters <- greencut(grc)
#'
#' # Plot dendrogram with clusters marked
#' plot(grc)
#' rect.hclust(grc, max(clusters))
#'
#' @export
#' @importFrom stats chisq.test as.dendrogram order.dendrogram
greenclust <- function(x, correct=FALSE, verbose=FALSE) {

    # Check for valid arguments
    if (anyNA(x) || is.null(x) || !(is.matrix(x) || is.data.frame(x)))
        stop("x must be a non-null matrix or data frame")
    if (!is.numeric(x) || sum(is.nan(x)) != 0)
        stop("x must be numeric")
    if (any(x < 0) || sum(is.infinite(x)) != 0)
        stop("all elements must be nonnegative and finite")
    if (nrow(x) < 3)
        stop("x should have at least 3 rows")
    if (ncol(x) < 2)
        stop("x should have at least 2 columns")
    if(sum(apply(x, 1, sum)==0) > 0)
        stop("all row totals must be greater than zero")
    if(sum(apply(x, 2, sum)==0) > 0)
        stop("all column totals must be greater than zero")

    # If there are no row names, give them names
    if (is.null(rownames(x))) {
        rownames(x) <- 1:nrow(x)
    }

    # Remember chi-squared for the initial, un-clustered matrix
    suppressWarnings(initial.chi <- chisq.test(x, correct=correct)$statistic)
    # Replace row names with negative row numbers, for building merge matrix
    saved.names <- rownames(x)
    n <- nrow(x)
    rownames(x) <- -(1:n)
    # Initialize merge matrix and object vectors
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
                trial.x <- .combine.rows(x, i, j, cluster.number)
                # Calculate new chi-sq test results
                new.chi <- suppressWarnings(chisq.test(trial.x,
                                                       correct=correct))
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
        merge.matrix <- rbind(merge.matrix,
                              as.numeric(rownames(x)[c(best.i, best.j)]))
        heights <- c(heights, (initial.chi - best.chi)/initial.chi)
        p.values <- c(p.values, best.p)
        tie <- c(tie, tie.flag)

        # Change our matrix to the winning combined row matrix
        if (best.trial==trials) {
            x <- trial.x
        } else {
            x <- .combine.rows(x, best.i, best.j, cluster.number)
        }

        if (verbose && cluster.number < (n - 1)) {
            .print.step(x, saved.names, cluster.number, tie.flag,
                        best.chi, best.p, initial.chi)
        }
    }

    # Final height should always be 1 (for plotting)
    heights[length(heights)] <- 1
    # Remove the height names given by the chisq.test function
    names(heights) <- NULL

    # Final p.value is not meaningful
    p.values <- p.values[-length(p.values)]

    # Pack everything up (except the order vector)
    grc <- structure(list(merge=merge.matrix,
                          height=heights,
                          order=vector(),
                          labels=saved.names,
                          call=match.call(),
                          dist.method="chi-squared",
                          p.values=p.values,
                          tie=tie),
                     class=c("hclust", "greenclust"))

    # Use dendrogram object to help create the order vector
    grc$order <- order.dendrogram(as.dendrogram(grc))

    return(grc)
}

