###############################################################################
#                                                                             #
#  assign.cluster                                                             #
#                                                                             #
#  Part of the greenclust R package                                           #
#                                                                             #
#  Jeff Jetton                                                                #
#  March 2019                                                                 #
#                                                                             #
###############################################################################



#' Assign clusters to a new vector of categories
#'
#' Maps a vector of cluster numbers to another categorical vector,
#' yielding a new vector of matching cluster numbers. Useful for distributing
#' cluster numbers back out to the original observations in cases where the
#' clustering was performed on a table of unique levels rather than directly
#' on the observations (such as with \code{\link{greenclust}}).
#'
#' Any categories in \code{x} that do not exist in \code{names(clusters)}
#' are given a cluster of \code{NA}, or (if \code{impute} is \code{TRUE})
#' assigned the cluster number that is most-frequently used for the other
#' existing categories, with ties going to the lowest cluster number. If
#' there are no matching clusters for any of the categories in \code{x},
#' imputation will simply use the first cluster number in \code{clusters}.
#'
#' If there are duplicate names in \code{clusters}, the first occurrence
#' takes precedence.
#' @param x a factor or character vector representing a categorical variable
#' @param clusters a named numeric vector of cluster numbers, such as an
#'   object returned by \code{\link{greencut}} or \code{\link{cutree}}
#' @param impute a boolean controlling the behavior when a value in \code{x}
#'   is not found in \code{names(clusters)} (see Details).
#' @return A factor vector of the same length as \code{x}, representing
#'   assigned cluster numbers.
#' @seealso \code{\link{greenclust}}, \code{\link{greencut}},
#'     \code{\link{greenplot}}
#' @examples
#' # Cluster feed types based on number of "underweight" chicks
#' grc <- greenclust(table(chickwts$feed,
#'                         ifelse(chickwts$weight < 200, "Y", "N")))
#' # Assign clusters to each original observation
#' feed.clustered <- assign.cluster(chickwts$feed, greencut(grc))
#' table(chickwts$feed, feed.clustered)
#' @export
assign.cluster <- function(x, clusters, impute=FALSE) {

    # Check for valid arguments
    if (is.null(x) || !(is.factor(x) || is.character(x)))
        stop("x must be a factor or character vector")
    if (anyNA(x))
        stop("x must not contain NAs")
    if (is.null(clusters) || !is.numeric(clusters) ||
        sum(is.nan(clusters)) != 0)
        stop("'clusters' must be a numeric vector")
    if (anyNA(clusters))
        stop("'clusters' must not contain NAs")
    cnames <- names(clusters)
    if (is.null(cnames) || !is.character(cnames))
        stop("elements in 'clusters' must have names")
    if (anyNA(cnames) || "" %in% cnames)
        stop("NA or \"\" names not allowed for 'clusters'")

    # Note that match() will, by default, return the first match only
    newx <- clusters[match(x, names(clusters))]

    # Handle any non-matches
    if (anyNA(newx)) {

        # Whether imputing or not, we want to at least put the
        # original category back into the name
        na.vector <- is.na(newx)
        names(newx)[na.vector] <- x[na.vector]

        if (impute) {
            tab <- table(newx)
            # If there are zero matches, use the first cluster
            if (length(tab) == 0) {
                imp.clust <- clusters[1]
            } else {
               # Get most-frequently assigned cluster so far
               imp.clust <- tab[order(as.numeric(tab),
                                      as.numeric(names(tab)),
                                      decreasing=c(TRUE, FALSE))][1]
            }
           # Assign imputed cluster to all the NAs
           newx[na.vector] <- as.numeric(names(imp.clust))
        }
    }

    return(as.factor(newx))
}



