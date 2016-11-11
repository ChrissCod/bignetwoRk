#' Median Eccentricity
#'
#' @description Calculate the (estimated) median eccentricity of a graph.
#' @param g	The input network.
#' @param p The sampling probability.
#' @details The median eccentricity is the median eccentricities of all nodes in graph \emph{g}. metric.distance.medianecc calculates the (estimated) median eccentricity of graph g with a justified error.
#' @return A real constant.
#' @author Xu Dong, Nazrul Shaikh.
#' @references West, Douglas Brent. Introduction to graph theory. Vol. 2. Upper Saddle River: Prentice hall, 2001.
#' @examples \dontrun{
#' x <-  net.erdos.renyi.gnp(1000, 0.01)
#' metric.distance.medianecc(x, 0.01)}
#' @export
#' @import igraph
#' @import parallel
#' @import doParallel
#' @import foreach

metric.distance.medianecc <- function(g,p){

  n <- length(g)

  ect <- function (i,n,g) {

    p <- sample(seq(n-1),1)

    xx <- eccentricity(g,p)

    return(xx)

  }

  cl <- makeCluster(detectCores())
  registerDoParallel(cl, cores = detectCores())
  clusterExport(cl=cl, varlist=c("eccentricity"))
  i<-NULL
  ECT <- foreach(i = 1:(round(n*p)), .combine=c) %dopar% ect(i,n,g)

  medianecc <- stats::median(ECT)

  stopCluster(cl)

  medianecc

}
