#' Global Clustering Coefficient
#'

#' @description Calculate the global clustering coefficient of a graph.
#' @param Network The input network.
#' @param node.sample	The percentage of nodesto be selected - random sample (0,1).
#' @param triplet.sample The number of triplets to explore for each node - random triplets.
#' @details The global clustering coefficient measures the ratio of triples versus the total number of all possible triples in graph \emph{g}. \code{metric.cluster.global()} calculates the (estimated) global clustering coefficient of graph \emph{g} with a justified error.
#' @return A real constant.
#' @author Xu Dong, Nazrul Shaikh.
#' @references Wasserman, Stanley, and Katherine Faust. Social network analysis: Methods and applications. Vol. 8. Cambridge university press, 1994.
#' @examples \dontrun{
#' x <-  net.erdos.renyi.gnp(1000, 0.01)
#' metric.cluster.global(x, 0.001,2)}
#' @export
#' @import parallel
#' @import doParallel
#' @import foreach

metric.cluster.global <- function(Network,node.sample, triplet.sample){

  Cores <- detectCores()

  ##/Triplet counting vectorized function
  Vector.triplets <- function(Network,node,triplet.sample){

    Triplets <- function(Network,node){

      ##Sampling at random nodes-edges to count closed and connected triplets

      Neig.1 <- unlist(Network[node])
      #; Neig.2 <- unique(unlist(Network[Neig.1]))

      n1 <- node;  Neig.1[-which(Neig.1==node)]

      n2 <- sample(Neig.1,1)
      e2 <- unlist(Network[n2])

      n3 <- sample(e2,1);
      e3 <- unlist(Network[n3])

      ##Triplets - closed
      ed.1.2 <- 1
      ed.1.3 <- sum(n1==e3)+sum(n3==Neig.1)
      ed.2.3 <- sum(n2==e3)+sum(n3==e2)
      trip.clo <- ifelse(ed.1.2+ed.1.3+ed.2.3>2,1,0)

      ##Triplets - connected
      trip.con <- ifelse(ed.1.2+ed.1.3+ed.2.3>1,1,0)

      triplet <- array(c(trip.clo,trip.con))


      triplet
    }

    ##Vectorized triplet calculations by node
    do <- matrix(nrow=triplet.sample, ncol = 1,rep(node,triplet.sample))
    A <- t(apply(do,1,Triplets,Network=Network))
    A <- c(sum(A[,1]),sum(A[,2]))
    A
  }

  ##/Extraction function
  Extract <- function(nodes,pos,Network){
    result <- Network[[nodes]][pos]
    result
  }


  ##/CC calculations
  nodes <- sample(seq(length(Network)), round(length(Network)*node.sample))

  ##Raw inputs from each node
  cl <- makeCluster(Cores)
  registerDoParallel(cl, cores = Cores)
  CC <- parLapply(cl,nodes,Vector.triplets, Network=Network, triplet.sample = triplet.sample)

  ##Extraction
  elements <- seq(length(CC))
  Close <- parLapply(cl=cl,elements,Extract,pos=1,Network=CC)
  Conn <- parLapply(cl=cl,elements,Extract,pos=2,Network=CC)
  stopCluster(cl)

  ##CC results
  Close <- sum(unlist(Close))
  Conn <- sum(unlist(Conn))
  CC <- Close/Conn
  CC

}
