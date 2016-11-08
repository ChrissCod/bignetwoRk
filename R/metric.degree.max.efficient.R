#' Efficient Maximal Degree
#'
#' @description Calculate the efficient maximal degree of a graph.
#' @param g	The input network.
#' @details The efficient maximal degree is the 90\\% quantile of the degrees of all nodes in graph g.
#' @return A real constant.
#' @author Xu Dong, Nazrul Shaikh.
#' @examples
#' \dontrun{x <-  net.erdos.renyi.gnp(1000, 0.01)
#' metric.degree.max.efficient(x)}
#' @export

metric.degree.max.efficient <- function(g){
  effective.max.degree <- unname(stats::quantile(lengths(g),0.9))
  effective.max.degree

}
