#' @keywords internal
funcrank <-
function(x)
{
  n <- length(x)
  rr <- rep(0,n)
  for(ii in 1:n) rr[ii] <- n - sum(ifelse((x[ii] > x),1,0))
  return(rr)
}
