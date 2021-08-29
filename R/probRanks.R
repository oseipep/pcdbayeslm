#' Ranks samples from posterior distribution of
#' Bayes linear models for paired comparison data
#' 
#' @param samples The posterior draws obtained from \code{\link{pcdbayeslm}} and 
#' \code{\link{postdrawspcd}}
#' @param ascending A logical variable default to True for ascending order of the 
#' item labels.
#' @return Ranks permutations and ranking probabilities
#' %% @note %% ~~further notes~~
#' @author Prince P. Osei and Ori Davidov
#' %% @seealso %% ~~objects to See Also as \code{\link{help}}, ~~~
#' %% @references %% ~put references to the literature/web site here ~
#' @importFrom plyr ddply
#' @examples
#' ## May be simulated data??
#' @export
probRanks <-
function(samples,ascending=T)
{
  K <- ncol(samples) # number of items
  nsize <- nrow(samples) # total sample size
  # Merits labels:
  mlabels <- meritlabels(K,ascending = ascending)
  # Create a dataframe
  rank.ds <- as.data.frame(t(apply(samples,1, funcrank)))
  colnames(rank.ds) <- mlabels
  ranks <- ddply(rank.ds,.variables=mlabels,nrow)
  colnames(ranks)[K+1] <- "freq"
  
  # Create rank probability matrix
  prob.rankmat <- matrix(0,nrow=K,ncol=K)
  a <- nrow(prob.rankmat)
  j <- ncol(prob.rankmat)
  for (x in 1:a) {
    for (k in 1:j) {
      prob.rankmat[x,k] <- sum(ranks[which(ranks[,x]==k),"freq"])/nsize
    }
  }
  rownames(prob.rankmat) <- mlabels
  res <- list(RankPermutations=ranks,ProbRanks=prob.rankmat)
  return(res)
}
