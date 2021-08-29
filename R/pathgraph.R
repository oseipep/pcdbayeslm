#' @export
pathgraph <-
function(noitems)
{
  # total number of possible edges (Balanced)
  noBa <- choose(noitems,2)
  Bcompars <- rep(1,noBa)
  #--- number of edges for unbalanced
  noE <- noitems-1
  #-- number of comparison per edge present
  nij <- noBa/noE
  #--- randomly select a path
  ss <- sample(1:noBa,size=noE)
  #--- assign number of comparisons to the path sampled
  Bcompars[ss] <- nij
  Bcompars[-ss] <- 0
  #--- all possible edges ----
  Baedges <- vedges(noitems)
  
  res <- list(Edgeset=Baedges,Compars=Bcompars)
  return(res)
}
