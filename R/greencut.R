###############################################################################
#                                                                             #
#  greencut                                                                   #
#                                                                             #
#  Part of the greenclust R package                                           #
#                                                                             #
#  Jeff Jetton                                                                #
#  March 2019                                                                 #
#                                                                             #
###############################################################################



#' Cut a Greenclust Tree into Optimal Groups
#'
#' Cuts a \code{\link{greenclust}} tree at an automatically-determined number
#' of groups.
#'
#' The cut point is calculated by finding the number of groups/clusters that
#' results in a collapsed contingency table with the most-significant (lowest
#' p-value) chi-squared test. If there are ties, the smallest number of
#' groups wins.
#'
#' If a certain number of groups is required or a specific r-squared
#' (1 - height) threshold is targeted, values for either \code{k} or \code{h}
#' may be provided. (While the regular \code{\link{cutree}} function could
#' also be used in this circumstance, it may still be useful to have the
#' additional attributes that \code{greencut()} provides.)
#'
#' As with \code{cutree()}, \code{k} overrides \code{h} if both are given.
#'
#' @param g a tree as producted by \code{\link{greenclust}}
#' @param k an integer scalar with the desired number of groups
#' @param h numeric scalar with the desired height where the tree should be cut
#' @return \code{greencut} returns a vector of group memberships, with the
#'   resulting r-squared value and p-value as object attributes,
#'   accessable via \code{\link{attr}}.
#' @references Greenacre, M.J. (1988) "Clustering the Rows and Columns of
#'   a Contingency Table," \emph{Journal of Classification 5}, 39-51.
#'   \doi{10.1007/BF01901670}
#' @seealso \code{\link{greenclust}}, \code{\link{greenplot}},
#'     \code{\link{assign.cluster}}
#' @examples
#' # Combine Titanic passenger attributes into a single category
#' # and create a contingency table for the non-zero levels
#' tab <- t(as.data.frame(apply(Titanic, 4:1, FUN=sum)))
#' tab <- tab[apply(tab, 1, sum) > 0, ]
#'
#' grc <- greenclust(tab)
#' greencut(grc)
#'
#' plot(grc)
#' rect.hclust(grc, max(greencut(grc)),
#'             border=unique(greencut(grc))+1)
#' @export
greencut <- function(g, k=NULL, h=NULL) {

    # Check validity specific to greenclust objects. (The cutree function
    # used later will check for hclust validity.)
    if (!inherits(g, "greenclust") || is.null(g$p.values))
        stop("not a valid 'greenclust' object")

    # Determine cutpoint and group membership vector
    if (is.null(k)) {
        if (is.null(h)) {
            # k and h are both null: determine cutpoint from p-values
            min.indices <- which(g$p.values==min(g$p.values))
            # In case of ties, go with the highest index
            # (smallest number of clusters/groups)
            clust.index <- min.indices[length(min.indices)]
            # Convert index to number of clusters
            k <- length(g$p.values) - clust.index + 2
            # Perform cut at k
            groups <- stats::cutree(g, k)
        } else {
            # k is null but h is not: cut at specified height
            groups <- stats::cutree(g, h=h)
            # Convert number of clusters to index
            k <- max(groups)
            clust.index <- length(g$p.values) - k + 2
        }
    } else {
        # k is not null: cut at specified number of groups
        groups <- stats::cutree(g, k)
        # Convert number of clusters to index
        clust.index <- length(g$p.values) - k + 2
    }

    # Add attributes for r-squared and p-value at cutpoint
    attr(groups, "r.squared") <- 1 - g$height[clust.index]
    attr(groups, "p.value") <- g$p.values[clust.index]

    return(groups)
}

