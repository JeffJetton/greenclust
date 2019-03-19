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
#' yielding a new vector of matching cluster numbers.
#'
#' Useful for cases where clustering was performed on a table of unique levels
#' (as with \code{\link{greenclust}}), and the clusters need to be distributed
#' back out to the original observations. In this case, the \code{cluster}
#' vector will typically have a \code{names} attribute of corresponding
#' categories, and \code{src} will not be used.
#'
#' Also handy for assigning clusters to a test data set when the original
#' clustering was done on a training set. Here, the names of \code{cluster}
#' will represent original observations rather than category names, so the
#' \code{src} vector will be needed.
#'
#' Any categories in \code{x} that do not exist as either names in
#' \code{cluster} or elements in \code{src} are assigned the most frequent
#' cluster number (with ties going to the lowest value).
#' @param x a factor or character vector representing a categorical variable
#' @param clusters a numeric vector representing cluster numbers, such as an
#'   object returned by \code{\link{greenclust}} or \code{\link{hclust}}
#' @param src optional factor or character vector of categories to use for
#'   the cluster numbers in \code{cluster} if they do not already exist in
#'   the  \code{names} attribute of  \code{cluster}
#' @examples
#' # Example goes here
#'
#' @export
assign.cluster <- function(x, clusters) {
    # TODO:   Check for unique cluster names
    #         Handle cases where x category not in clusters
    #         At documentation above discussing these two features ^^^
    #         Accept an optional vector of source observations to
    #         get levels from (in cases where the train set was hclusted)
    #
    #              If categories are not unique, the most-frequently occuring cluster
    #              number for each category is mapped (minimum if ties).
    #            If a category in x is not found in clusters, it is assigned the most-popular
    #            cluster assigned by the existing categories (minimum if ties)
    if (length(unique(names(clusters))) != length(clusters)) {
        # Determine which cluster to use for each unique category

    }
    return(as.factor(clusters[match(x, names(clusters))]))
}
