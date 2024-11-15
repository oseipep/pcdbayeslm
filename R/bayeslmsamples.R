#' Draws from the posterior distribution for Bayesian linear models for 
#' paired comparison data
#' 
#' @param iter The number of posterior draws.
#' @param noitems The number of items for the paired comparison.
#' @param nocompars A vector of pairwise comparisons among \code{noitems}.
#' @param scores The true scores or merits of \code{noitems}
#' @param vars The true constant variance of \code{scores}; default is 1.
#' @param muprior The normal prior mean for \code{noitems}; default is a vector of 
#' zeros of length \code{noitems}.
#' @param varprior The prior covariance matrix of \code{noitems}; default is the 
#' identity matrix of dimension \code{noitems}.
#' @param Edges The edge set of \code{noitems} in the graph, default is the edgeset of 
#' complete graph of three items.
#' @param prior The type of prior: "conju" (default) for conjugate prior, 
#' "semi-conju" for semi-conjugate prior, "flat" for flat prior, and "ref" 
#' for reference prior
#' @param data The data containing the comparison outcomes; default is NULL.
#' @param datatype The type of data to analyze: "simulated" (the default) for simulated data,
#' "real" for a given real dataset.
#' @return Posterior draws from \code{\link{cpcbayeslm}}
#' %% @note %% ~~further notes~~
#' @author Prince P. Osei and Ori Davidov
#' %% @seealso %% ~~objects to See Also as \code{\link{help}}, ~~~
#' %% @references %% ~put references to the literature/web site here ~
#' @importFrom invgamma rinvgamma
#' @examples
#' ##  A graph of three items
#' ##  K <- 3 # number of items
#' ## Kedges <- c(1,2,1,3,2,3) # edge set
#' ## compars <- rep(3,3) # pairwise comparisons
#' ##  Tscores <- 1:3-mean(1:3) # The true score sum to zero
#' ## bayeslmsamples(100,K,compars,Tscores,Edges=Kedges, prior="conju", datatype="simulated")
#' @export
bayeslmsamples <-
function(iter,noitems,nocompars,scores,vars=1, muprior = zeros(noitems,1),
         varprior=vars*diag(noitems),Edges=c(1,2,1,3,2,3),
         prior=c("conju", "semi-conju", "flat", "ref"),
         data=NULL, datatype=c("simulated","real"))
{
  # initialize
  tmp <- cpcbayeslm(noitems=noitems,nocompars=nocompars,scores=scores,vars=vars,
                  xmu=muprior,xvar=varprior,Edges=Edges,prior=prior,
                  data=data,datatype=datatype)
  psamples <- rbind(c(tmp$Bayes$meanP))
  for (ii in 2:iter) {
    tmp <- cpcbayeslm(noitems=noitems,nocompars=nocompars,scores=scores,vars=vars,
                    xmu=muprior,xvar=varprior,Edges=Edges,prior=prior,
                    data=data,datatype=datatype)
    psamples <- rbind(psamples,c(tmp$Bayes$meanP))
  }
  return(psamples)
}
