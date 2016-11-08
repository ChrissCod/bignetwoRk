#' Graph Density
#'
#' @description Calculate the density of a graph.
#' @param g	The input network.
#' @details Computes the ratio of the number of edges and the number of possible edges.
#' @return A real constant.
#' @author Xu Dong, Nazrul Shaikh.
#' @examples \dontrun{
#' x <-  net.erdos.renyi.gnp(1000, 0.01)
#' metric.graph.density(x)}
#' @export

metric.graph.density <- function (g) {
  n <- length(g)
  density <- sum(lengths(g))/(n*(n-1))
  density
}

