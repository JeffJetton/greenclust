###############################################################################
#                                                                             #
#  greenclust                                                                 #
#                                                                             #
#  Part of the greenclust R package                                           #
#                                                                             #
#  Jeff Jetton                                                                #
#  March 2019 - Initial coding                                                #
#  January 2020 - version 1.1: Improved clustering implementation             #
#                              and a few bugfixes                             #
#                                                                             #
###############################################################################



#' Row Clustering Using Greenacre's Method
#'
#' Iteratively collapses the rows of a table (typically a contingency table)
#' by selecting the pair of rows each time whose combination creates the
#' smalled loss of chi-squared.
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
#'   \doi{10.1007/BF01901670}
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
#'             aggregate cutree pchisq
greenclust <- function(x, correct=FALSE, verbose=FALSE) {

    #######################################
    #   Validation Checks and Data Prep   #
    #######################################

    # Check for valid arguments
    if (anyNA(x) || is.null(x) || !(is.matrix(x) || is.data.frame(x)))
        stop("x must be a non-null matrix or data frame")
    # If x is a dataframe, convert to matrix
    if (is.data.frame(x)) {
        x = as.matrix(x)
    }
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

    # If elements in x are integer, convert to higher-precision double
    if (is.integer(x)) {
        storage.mode(x) <- "double"
    }

    # Make sure chi-squared is not already zero
    suppressWarnings(chi.result <- chisq.test(x, correct=correct))
    if(chi.result$statistic == 0)
        stop("x already has a chi-squared statistic of zero and cannot be clustered")

    # If there are no row names, give them names
    if (is.null(rownames(x))) {
        rownames(x) <- 1:nrow(x)
    }


    ################################
    #   Preliminary calculations   #
    ################################

    # Make a copy of the original matrix (used at the end if correct=TRUE).
    original.x <- x

    # Pre-calculate the column totals and overall table total
    # These will never change, so we just have to do this once
    column.totals <- apply(x, MARGIN=2, FUN=sum)
    table.total <- sum(column.totals)

    # Keep track of all currently "active" row indices. That is, the
    # rows that would still be part of the table at each iteration,
    # ignoring the rows that have already been combined into a new row,
    # and adding any newly-combined rows. This gives us a vector to cycle
    # through each time when calculating potential reductions in chi for
    # all the various active row combinations (and keeps us from having
    # to actually combine the rows into a new table each time).
    active.rows <- 1:nrow(x)

    # Store current degrees of freedom and initial row count
    df <- prod(dim(x) -  1)
    n <- nrow(x)

    # Store row chi values for every row in the table
    row.chis <- apply(x, MARGIN=1, FUN=.calculate.row.chi,
                      column.totals, table.total)

    # Sum of row chis is the overall chi-squared statistic
    # Remember this value for later r-squared ("height") calculations
    initial.chi <- sum(row.chis)
    running.chi <- initial.chi

    # Initialize the "merge matrix" (record of each merge step) and the vectors
    # that will be used to construct our "greenclust" object at the end
    merge.matrix <- vector()
    heights <- vector()
    p.values <- vector()
    tie <- vector()

    # For the merge matrix, we'll need to keep track of which rows are still
    # "original" (unmerged) and which are from a previous clustering step.
    # (See `hclust` docs for details on the merge component of the object.)
    merge.indices <- -(1:n)

    # Create a data.frame to store the chi "reduction" calculations, i.e.,
    # the amount by which the table's overall chi statistic would drop for
    # every potential two-row combination left to consider
    reductions <- data.frame(i=vector("integer"),
                             j=vector("integer"),
                             reduction=vector("numeric"))

    # Perform the initial reduction calculations for the entire table
    for (i in 1:(n-1)) {
        # Check combinations of row i with every row higher than i...
        for (j in (i+1):n) {
            # offset in 1:(n-i)
            # j = i + offset
            # If we combined these two rows, what row chi would result?
            combined.row.chi <- .calculate.row.chi(x[i, ] + x[j, ],
                                                   column.totals, table.total)
            # How much of a reduction is that, compared to the separate rows?
            separate.row.chi.total <- row.chis[i] + row.chis[j]
            chi.reduction <-  separate.row.chi.total - combined.row.chi
            # If this is negative, that's only due to floating-point error.
            # (It's not possible for a combination of rows to increase chi.)
            # Adjust to zero...
            if (chi.reduction < 0) {
                chi.reduction <- 0
            }
            # Store the results for this row combo
            reductions <- rbind(reductions,
                                data.frame(i=i, j=j, reduction=chi.reduction),
                                make.row.names=FALSE)
        }
    }


    ################################
    #   Clustering                 #
    ################################

    for (cluster.number in 1:(n-1)) {

        # Get the current-lowest chi reduction value
        lowest.reduction <- min(reductions$reduction)

        # Get the rows combinations that result in that reduction value
        winners <- reductions[reductions$reduction==lowest.reduction, ]

        # Do we have more than one with that lowest reduction?
        tie.flag <- nrow(reductions[reductions$reduction==lowest.reduction,
                                    ]) > 1
        if (tie.flag) {
            # Make sure the combos are sorted in row order before picking
            # the "winner" (this isn't really necessary, technically, but
            # it adds a nice consistency to the clustering)
            reductions <- reductions[order(reductions$i, reductions$j),]
        }
        # Winner is the first combo with the lowest reduction
        index.of.winning.reduction <- match(lowest.reduction,
                                            reductions$reduction)

        # Combining the "winner" combo...
        # We don't literally combine the two rows in our dataset, yielding a
        # new dataset with one fewer row (like version 1.0 did), since that
        # would throw off all of the row references in our pre-calcuated data.
        # Instead we replace the first of the two rows with the combo, then
        # update the chi value in row.chis for that row.
        # We don't delete the second row, but instead effectively ignore it
        # by deleting its reference from the active.rows vector.
        # Then we remove any row in reduction that references either of the
        # two combined indices and add new rows based on comparing the new,
        # freshly-clustered row with all the other ones.

        # Combine the winning rows and replace first winning row with result
        rows.to.combine <- c(reductions[index.of.winning.reduction, "i"],
                             reductions[index.of.winning.reduction, "j"])
        new.row <- x[rows.to.combine[1], ] + x[rows.to.combine[2], ]
        new.row.index <- rows.to.combine[1]
        x[new.row.index, ] <- new.row

        # Remove the index of second winning row from active.rows
        active.rows <- active.rows[active.rows != rows.to.combine[2]]

        # Update row.chis to reflect combined row
        new.row.chi <- .calculate.row.chi(new.row, column.totals, table.total)
        row.chis[new.row.index] <- new.row.chi



        # Reduce degrees of freedom
        df <- df - (ncol(x) - 1)

        # Remove reductions that refer to the two winning (pre-combined) rows
        # (A bit verbose, but slightly faster than using %in%, etc.)
        reductions <- reductions[reductions$i != rows.to.combine[1] &
                                     reductions$j != rows.to.combine[1] &
                                     reductions$i != rows.to.combine[2] &
                                     reductions$j != rows.to.combine[2], ]

        # Calculate chi reductions for the new row compared to each of
        # the other, older, active rows.
        other.rows <- active.rows[active.rows != new.row.index]
        for (comp.row.index in other.rows) {
            combined.row.chi <- .calculate.row.chi(new.row +
                                                       x[comp.row.index, ],
                                                   column.totals,
                                                   table.total)
            separate.row.chi.total <- new.row.chi + row.chis[comp.row.index]
            chi.reduction <-  separate.row.chi.total - combined.row.chi
            if (chi.reduction < 0) {
                chi.reduction <- 0
            }
            reductions <- rbind(reductions,
                                data.frame(i=new.row.index,
                                           j=comp.row.index,
                                           reduction=chi.reduction),
                                make.row.names=FALSE)
        }

        # Adjust the running chi-sq statistic and figure out its p-value
        if (correct && length(active.rows)==2 && ncol(x)==2) {
            # Use Yates's correction (only applies to a 2x2 matrix)
            temp.x <- x[active.rows, ]
            suppressWarnings(chi.result <- chisq.test(temp.x, correct=TRUE))
            running.chi <- chi.result$statistic
            current.p <- chi.result$p.value
        } else {
            # Just reduce the overall chi by the winning reduction amount
            # No need to calculate chi "from scratch"
            running.chi <- running.chi - lowest.reduction
            current.p <- pchisq(running.chi, df, lower.tail=FALSE)
        }

        # Merge matrix: Add info on winning clustering. Use the indices
        merge.matrix <- rbind(merge.matrix, merge.indices[rows.to.combine])
        # Update merge.indices to reflect the fact that the new row is
        # the result of a merge (clustering step)
        merge.indices[new.row.index] <- cluster.number

        # Add info to the other cluster-step-tracking variables
        heights <- c(heights, (initial.chi - running.chi)/initial.chi)
        p.values <- c(p.values, current.p)
        tie <- c(tie, tie.flag)

        # Print details (if we're in verbose mode)
        if (verbose && cluster.number < (n - 1)) {
            # The new clusters should be named if we're going to show them
            rownames(x)[new.row.index] <- paste("Cluster", cluster.number)
            .print.step(x[active.rows, ], cluster.number,
                        rownames(x)[rows.to.combine], tie.flag, running.chi,
                        p.values[cluster.number], initial.chi)
        }

    }  # End of cluster loop:  for (cluster.number in 1:(n-1)) {


    ################################
    #   Prepare Return Object      #
    ################################

    # Final height should always be 1 (for plotting)
    heights[length(heights)] <- 1

    # Final p.value is not meaningful
    p.values <- p.values[-length(p.values)]

    # Pack everything up (except the order vector)
    grc <- structure(list(merge=unname(merge.matrix),
                          height=heights,
                          order=vector(),
                          labels=rownames(original.x),
                          call=match.call(),
                          dist.method="chi-squared",
                          p.values=unname(p.values),
                          tie=tie),
                     class=c("hclust", "greenclust"))

    # Use R's built-in dendrogram object to help create the order vector
    grc$order <- order.dendrogram(as.dendrogram(grc))

    # That's it!
    return(grc)
}



#########  "Hidden" helper functions  #########################################

# Calculate a single row's contribution to total chi.
#
#             r: numeric vector of one row's cells
# column.totals: numeric vector of the table's column totals (assumed to be
#                the same length as r... this is not checked!)
#   table.total: total sum of entire table
#
.calculate.row.chi <- function(r, column.totals, table.total) {

    # Determine expected cell values by distributing the row total
    # in proportion to the column totals
    expected.values <- column.totals * sum(r) / table.total
    # Each cell's chi value is the squared difference between
    # expected and actual, standardized by dividing by expected
    cell.chis <- (expected.values - r)^2 / expected.values

    # And the row chi is just the sum of cell chis...
    return(sum(cell.chis))
}



# Display information about a particular clustering step
# Called by greenclust() when verbose==TRUE
#
#           x: matrix at this step (with row names)
#           n: cluster step number
# combo.names: character vector of the names of the two rows
#              that were combined at this step
#         tie: were there tied chi-squares at this step?
#       chisq: chi-squared statistic of this step's matrix
#           p: p-value of above chi-squared test
# initial.chi: chi-squared of original (unclustered) matrix,
#              used to calculate r-squared
#
.print.step <- function(x, n, combo.names, tie, chisq, p, initial.chi) {

    # Display step information
    cat(paste0("Step ", n, ":  "))
    cat(paste(combo.names, collapse=" + "))
    if (tie) {
        cat(" (tie)\n\n")
    } else {
        cat("\n\n")
    }

    # Show current table/matrix
    print(x)

    # Display stats
    cat(paste("\nChi-squared:", round(chisq, 2), "\n"))
    cat(paste("p-value:", signif(p, 4), "\n"))
    cat(paste("R-squared:", round(chisq/initial.chi, 4), "\n\n\n"))

    # Make sure it prints...
    utils::flush.console()
}





###############################################################################



# Legacy version of greenclust.
#
# Call using this syntax:  greenclust:::.greenclust.v1()
#
# Same function interface as the current version. Uses a far less-efficient
# clustering method, but has essentially the same output. Some tied and
# nearly-tied row combinations may cluster in a different order from the
# current (version 1.1) of the function.
#
# Maintained in case exact reproducability with version 1.0 is still needed,
# and to perfom speed comparisons with version 1.1.
#
#  *** This function is deprecated and may be removed in future versions ***
#
.greenclust.v1 <- function(x, correct=FALSE, verbose=FALSE) {

    # Check for valid arguments
    if (anyNA(x) || is.null(x) || !(is.matrix(x) || is.data.frame(x)))
        stop("x must be a non-null matrix or data frame")
    # If x is a dataframe, convert to matrix
    if (is.data.frame(x)) {
        x = as.matrix(x)
    }
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
            .print.step.v1(x, saved.names, cluster.number, tie.flag,
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


# Return a new matrix that sums rows r1 and r2 of matrix m
# combo.name is the name to give the newly-combined row
# For speed, no checks are done. Ensure that:
#    * Index r1 is less than r2
#    * Both are valid rows in range of m
#
# This is called by .greenclust.v1() and is deprecated as of v1.1
#
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
# This is called by .greenclust.v1() and is deprecated as of v1.1
#
.print.step.v1 <- function(x, orig.names, n, tie.flag, chisq, p, initial.chi) {
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
