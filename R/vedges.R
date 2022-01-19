#' @keywords internal
vedges <-
function(noitems)
{
  # initialize the edge list for item 1
  Emat <- cbind(rep(1,noitems-1),2:noitems)
  
  # loop over the rest of items
  for (i in 2:noitems) {
    if(i == noitems) Emat <- rbind(Emat)
    else{
      tmp <- cbind(rep(i,noitems-i),(i+1):noitems)
      Emat <- rbind(Emat,tmp)
    }
  }
  res <- as.vector(apply(Emat, 1, function(x) x))
  return(res)
}
