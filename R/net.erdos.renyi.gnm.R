#' Directed / Undirected Erdos-Renyi \eqn{G(n,m)} network using a fix edge size.
#'
#' @description Simulate a random network with \emph{n} nodes and \emph{m} edges, according to Erdos and Renyi (1959).
#' @param n Number of nodes of the network.
#' @param m Number of edges of the network.
#' @param ncores Number of cores, by default \code{detectCores()} from \code{parallel}.
#' @param d A logical value determining whether is a network directed (default) or indirected.
#' @details In this (simplest) random network, \emph{m} edges are formed at random among \emph{n} nodes.
#'When \code{d = TRUE} is a directed network.
#' @return A list containing the nodes of the network and their respective neighbors.
#' @author Xu Dong, Nazrul Shaihk.
#' @examples \dontrun{
#' x <- net.erdos.renyi.gnm(1000, 100) }
#' @import parallel
#' @import doParallel
#' @import foreach
#' @export
#' @references Erdos, P. and Renyi, A., On random graphs, Publicationes Mathematicae 6, 290-297 (1959).


net.erdos.renyi.gnm <- function(n, m, ncores = detectCores(), d = TRUE){
  if (!ncores%%1==0){
    stop("Parameter ncores must be integer",call. = FALSE)}
  else{

    if (ncores > detectCores() | ncores < 2)  {
      stop("Parameter ncores is lower than 2 or exceed number of available cores",
           call. = FALSE)
    }
    else{
      if (d == TRUE) {
        #neilist <- list()
        #neilist[n] <- list(NULL)

        pool <- sample( seq(n*(n-1)),m )

        cl <- makeCluster(ncores)
        registerDoParallel(cl, cores = ncores)

        edge.to.nei <- function(i){

          nei <- list()
          nei[n] <- list(NULL)

          for (j in seq(i,n,ncores)  ){

            raw.nei <- intersect(pool, seq((j-1)*(n-1)+1, j*(n-1)))%%(n-1)

            nei[[j]] <- c( raw.nei[raw.nei<j], raw.nei[raw.nei>=j]+1 )

          }

          nei

        }

        cfun <- function(a,b){
          cc <- mapply(c,a,b, SIMPLIFY=FALSE)
          cc
        }

        Network <- foreach(i = 1:ncores, .combine='cfun') %dopar% edge.to.nei(i)
        stopCluster(cl)
        Network

      }
      else{

        neilist <- list()
        neilist[n] <- list(NULL)

        pool <- sample( seq(n*(n-1)/2),m )

        for (i in 1:(n-1)) {

          neilist[[i]] <- intersect(pool,seq(  (n*(n-1)-(n-i-1)*(n-i))/2-(n-i-1) ,  (n*(n-1)-(n-i-1)*(n-i))/2   ))+i-((n-1)+(n-i+1))*(i-1)/2

        }

        cl <- makeCluster(ncores)   ##Make cluster of cores
        registerDoParallel(cl, cores = ncores)

        reverse.connect <- function(i){

          reverse.neilist <- list()
          reverse.neilist[n] <- list(NULL)

          for (j in seq(i,n,ncores)  ){

            for (k in neilist[[j]]){

              reverse.neilist[[k]] <- c(reverse.neilist[[k]],j)

            }

          }

          reverse.neilist

        }

        cfun <- function(a,b){
          cc <- mapply(c,a,b, SIMPLIFY=FALSE)
          cc
        }

        reverselist <- foreach(i = 1:ncores, .combine='cfun') %dopar% reverse.connect(i)

        Network <- mapply(c,neilist,reverselist, SIMPLIFY=FALSE)
        stopCluster(cl)
        Network



      }
    }
  }
  }
