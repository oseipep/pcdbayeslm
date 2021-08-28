#' Projection of non-singular prior onto the subspace of the Laplacian 
#' 
#' @param noitems The number of items for the paired comparison.
#' @param xmu The normal prior mean of \code{noitems}.
#' @param xvar The prior covariance matrix of \code{noitems}.
#' @return Projected mean and variance matrix of the prior distribution.
#' @note %% ~~further notes~~
#' @author Prince P. Osei and Ori Davidov
#' @seealso %% ~~objects to See Also as \code{\link{help}}, ~~~
#' @references %% ~put references to the literature/web site here ~
#' @examples
#' ## K = 6 # number of items
#' ## mu = rep(0,6) # the prior mean
#' ## xcov = diag(6) # the prior covariance matrix
#' ## projbayeslm(K,mu,xcov)
#' @export
projbayeslm <-
function(noitems,xmu,xvar)
{
  xmu <- as.matrix(xmu)
  IDmat <- diag(noitems)
  J <- ones(noitems)
  P <- IDmat-(J/noitems)
  projmean <- P%*%xmu
  projvar <- P%*%xvar%*%t(P)
  res <- list(meanVector=projmean,varMatrix=projvar)
  return(res)
}
