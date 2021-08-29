#' @importFrom  igraph graph delete_edges E gsize ecount is_connected
#' @export
incompletegraph <-
function(noitems,L=2,M=1)
{
  # n_ij to be assigned
  nofComp <- M*choose(noitems,2)
  nn <- choose(noitems,2)/L
  m <- nofComp/nn
  compars <- rep(m,choose(noitems,2))
  edmoderate <- vedges(noitems)
  #-------------------
  # create a complete graph
  cgraph <- graph(edges=vedges(noitems),n=noitems,directed = FALSE)
  
  # find total number of edges to be deleted:
  N <- choose(noitems,2)-ceiling(nn)
  # Keep track of ids sampled/deleted
  ssamp <- NULL
  t <- 0
  n <- ecount(cgraph)
  pp <- 1:n
  while(t < N) {
    ee <- sample(pp,size=1)
    tmp <- cgraph-E(cgraph)[ee]
    if(is_connected(tmp)){
      pp <- pp[-which(pp==ee)]
      ssamp <- c(ssamp,ee)
      t <- t +1
    }
    else{
      pp <- pp
      ssamp <- ssamp
      t <- t - 1
    }
  }
  mg <- delete_edges(cgraph,edges=ssamp)
  compars[ssamp] <- 0
  if(sum(compars)==nofComp) compars <- compars
  else{
    kk <- nofComp-sum(compars)
    ss <- which(compars!=0)
    id <- sample(ss,size=1)
    compars[id] <- m + kk
  }
  res <- list(ModerateGraph=mg,OriginalGraph=cgraph,IDedgeoff=ssamp,
              ComparsMod=compars,EdgesMod=edmoderate)
  return(res)
}
