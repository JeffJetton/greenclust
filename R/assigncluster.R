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
#' Any categories in \code{x} that do not exist as names in \code{cluster}
#' are assigned the cluster number that is most-frequently assigned to the
#' other, existing categories (with ties going to the lowest cluster number).
#'
#' If there are duplicate names in \code{cluster}, the first occurrence
#' takes precedence.
#' @param x a factor or character vector representing a categorical variable
#' @param clusters a numeric vector representing cluster numbers, such as an
#'   object returned by \code{\link{greencut}} or \code{\link{cutree}}
#' @return A numeric vector of the same length as \code{x}, representing
#'   assigned cluster numbers.
#' @examples
#' # Cluster feed types based on number of "underweight" chicks
#' grc <- greenclust(table(chickwts$feed,
#'                         ifelse(chickwts$weight < 200, "Y", "N")))
#' # Assign clusters to each original observation
#' feed.clustered <- assign.cluster(chickwts$feed, greencut(grc))
#' table(chickwts$feed, feed.clustered)
#' @export
assign.cluster <- function(x, clusters) {
    # TODO:   Handle cases where x category not in clusters

    # Check for NA, and other invalid types

    # Note that match() will, by default, return the first match only
    newx <- clusters[match(x, names(clusters))]

    # Any categories in x that weren't names in clusters?
    if (anyNA(newx)) {
        # Get most-frequently assigned cluster so far
        tab <- table(newx)
        most.freq <- tab[order(as.numeric(tab), as.numeric(names(tab)),
                         decreasing=c(TRUE, FALSE))][1]
        # Assign that cluster to all the NAs
        na.vector <- is.na(newx)
        newx[na.vector] <- as.numeric(names(most.freq))
        names(newx)[na.vector] <- x[na.vector]
    }

    return(newx)
}



