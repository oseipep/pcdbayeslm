#' The paired comparison graph
#' 
#' This function build the paired comparison graph or use a given paired comparison
#' outcomes to build the graph (G,S).
#' 
#' %% ~~ If necessary, more details than the description above ~~
#' 
#' @param noitems The number of items for the paired comparison.
#' @param nocompars The number of comparisons among \code{noitems}.
#' @param scores The true scores or merits of \code{noitems}.
#' @param vars The constant variance of \code{scores}.
#' @param data The data containing the comparison outcomes; defalut is NULL.
#' @param datatype The type of data to analyze: "simulated" (the default) for simulated data,
#' "real" for a given real dataset.
#' @return A comparison graph with samples on edges.
#' @note %% ~~further notes~~
#' @author Prince P. Osei and Ori Davidov
#' @seealso %% ~~objects to See Also as \code{\link{help}}, ~~~
#' @references %% ~put references to the literature/web site here ~
#' @importFrom  stats rnorm
#' @examples
#' ## K = 3 # number of items
#' ## compars <- rep(3,3) # number of comparison (1,2,1,3,2,3)
#' ## scores <-  1:3 # the true scores of the items
#' ## tvar <- 1
#' ## compagraph(K,compars,scores,tvar,data=NULL)
#' @export
compagraph <-
function(noitems,nocompars,scores,vars,data=NULL,datatype="simulated")

{
  # Simulated data is FALSE
  if(datatype == "real"){
    # data: is available in the form of Yij
    if (is.null(data))
      stop('compagraph: data is missing',
           call.=FALSE)
    compars <- matrix(0,nrow=noitems,ncol=noitems)
    compars[lower.tri(compars)] <- -data
    pairCompars <- compars - t(compars)
    
    # The Y data required
    Ydata <- apply(pairCompars, 1, sum)
    
    # The square of the pairs
    sqYij <- sapply(pairCompars[upper.tri(pairCompars)], function(x) x^2)
  }
  
  if (datatype == "simulated"){
    if (is.null(vars))
      stop('compagraph: True variance is missing',
           call.=FALSE)
    pairCompars <- matrix(list(0),noitems,noitems)
    Ydata <- array(0,noitems)
    s <- 0
    for (i in 1:noitems) 
    {
      for (j in 1:noitems) 
      {
        if (j > i)
        {
          s <- s + 1
          pairCompars[[i,j]] <- rnorm(nocompars[s],scores[i]-scores[j],sd=sqrt(vars))
        }
        pairCompars[[j,i]] <- -pairCompars[[i,j]]
        Ydata[i] <- Ydata[i]+sum(pairCompars[[i,j]])
      }
    }
    sqYij <-sapply(pairCompars[upper.tri(pairCompars)],function(x) sum(x^2))
  }
  
  res <- list(Ydata=Ydata,pairwiseComp=pairCompars,SquareYij=sqYij)
  return(res)
}
