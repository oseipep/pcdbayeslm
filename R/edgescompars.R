#' @export
edgescompars <-
function(noitems,m=1,topology="balanced")
{
  # initialize the edge list for item 1
  Emat <- cbind(rep(1,noitems-1),2:noitems)
  Compars <- NULL
  
  # loop over the rest of items
  for (i in 2:noitems) {
    if(i == noitems) Emat <- rbind(Emat)
    else{
      tmp <- cbind(rep(i,noitems-i),(i+1):noitems)
      Emat <- rbind(Emat,tmp)
    }
  }
  n <- nrow(Emat)
  if (topology == "balanced"){
    Compars <- c(Compars,rep(m,choose(noitems,2)))
  }
  else if(topology == "unbalanced"){
    for (ii in 1:n) {
      tmp <- ifelse(Emat[ii,2]-Emat[ii,1]==1,m,0)
      Compars <- c(Compars,tmp)
    }
  }
  #-----------
  res1 <- as.vector(apply(Emat, 1, function(x) x))
  res <- list(Edgeset=res1,Compars=Compars,MatEdges=Emat)
  return(res)
}
