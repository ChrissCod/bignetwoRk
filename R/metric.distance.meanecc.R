#' Mean Eccentricity
#'

#' @description Calculate the mean eccentricity of a graph.
#' @param g	The input network.
#' @param p The confidence level probability.
#' @param ncores The number of cores
#' @details The mean eccentricities of all nodes in graph \emph{g}. Calculates the (estimated) mean eccentricity of graph \emph{g} with a justified error.
#' @return A real constant.
#' @author Xu Dong, Nazrul Shaikh.
#' @references West, Douglas Brent. Introduction to graph theory. Vol. 2. Upper Saddle River: Prentice Hall, 2001.
#' @examples \dontrun{
#' x <-  net.erdos.renyi.gnp(1000, 0.01)
#' metric.distance.meanecc(x, 0.01)}
#' @export
#' @import parallel
#' @import doParallel
#' @import foreach

metric.distance.meanecc <- function(g, p, ncores= detectCores() ){
  if (!ncores%%1==0){
    stop("Parameter ncores must be integer",call. = FALSE)}
  else{

    if (ncores > detectCores() | ncores < 2)  {
      stop("Parameter ncores is lower than 2 or exceed number of available cores",
           call. = FALSE)
    } else{
      n <- length(g)

      ect <- function (i, n, g) {

        p <- sample(seq(n-1),1)

        xx <- eccentricity(g, p)

        return(xx)

      }

      cl <- makeCluster(ncores)
      registerDoParallel(cl, cores = ncores)
      clusterExport(cl = cl, varlist=c("eccentricity"))
      i <- NULL
      ECT <- foreach(i = 1:(round(n*p)), .combine=c) %dopar% ect(i, n, g)

      meanecc <- mean(ECT)

      stopCluster(cl)

      meanecc

    }
  }
}
