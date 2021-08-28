#' Draws from the posterior distribution for Bayesian linear models for 
#' paired comparison data
#' 
#' @param iter The number of posterior draws.
#' @param Kitems The number of items for the paired comparison.
#' @param Kcompars A vector of pairwise comparisons among \code{Kitems}.
#' @param Kscore The true scores or merits of \code{Kitems}
#' @param Kvars The true constant variance of \code{Kscore}.
#' @param Kmuprior The normal prior mean for \code{Kitems}.
#' @param Kvarprior The prior covariance matrix of \code{Kitems}.
#' @param Kedges The edge set of \code{Kitems} in the graph.
#' @param prior The type of prior: "conju" (default) for conjugate prior, 
#' "semi-conju" for semi-conjugate prior, "flat" for flat prior, and "ref" 
#' for reference prior
#' @param data he data containing the comparison outcomes; defalut is NULL.
#' @param type The type of data to analyze: "simulated" (the default) for simulated data,
#' "real" for a given real dataset.
#' @return Posterior draws from \code{\link{bayeslm}}
#' @note %% ~~further notes~~
#' @author Prince P. Osei and Ori Davidov
#' @seealso %% ~~objects to See Also as \code{\link{help}}, ~~~
#' @references %% ~put references to the literature/web site here ~
#' @importFrom invgamma rinvgamma
#' @examples
#' 
#' @export
bayeslmsamples <-
function(iter,Kitems,Kcompars,Kscore,Kvars,Kmuprior,Kvarprior,Kedges,prior,data,type)
{
  # initialize
  tmp <- bayeslm(noitems=Kitems,nocompars=Kcompars,scores=Kscore,vars=Kvars,
                  xmu=Kmuprior,xvar=Kvarprior,Edges=Kedges,prior=prior,
                  data,datatype=type)
  psamples <- rbind(c(tmp$Bayes$meanP))
  for (ii in 2:iter) {
    tmp <- bayeslm(noitems=Kitems,nocompars=Kcompars,scores=Kscore,vars=Kvars,
                    xmu=Kmuprior,xvar=Kvarprior,Edges=Kedges,prior=prior,
                    data,datatype=type)
    psamples <- rbind(psamples,c(tmp$Bayes$meanP))
  }
  return(psamples)
}
