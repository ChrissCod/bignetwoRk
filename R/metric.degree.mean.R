#' Mean Degree
#'
#' @description Calculate the mean degree of a graph.
#' @param g	The input network.
#' @details The mean degree is the average value of the degrees of all nodes in graph g.
#' @return A real constant.
#' @author Xu Dong, Nazrul Shaikh.
#' @examples \dontrun{
#' x <-  net.erdos.renyi.gnp(1000, 0.01)
#' metric.degree.mean(x)
#' }
#' @export

metric.degree.mean <- function(g) {

  mean.degree <- mean(lengths(g))

  mean.degree

}
