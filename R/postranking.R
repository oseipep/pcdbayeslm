#' Bayesian linear models for cardinal paired comparison data
#' 
#' This function performs a new posterior ranking methodology developed in the 
#' paper by Prince P. Osei and Ori David (2021) to account for the uncertainty
#' in the ranking of items.
#' 
#' %% ~~ If necessary, more details than the description above ~~
#' 
#' @param N Number of posterior samples.
#' @param noitems Number of items in the comparison graph
#' @param nocompars The number of comparisons among \code{noitems}.
#' @param scores The true scores or merits of \code{noitems}
#' @param vars The constant variance of \code{scores}.
#' @param xmu The normal prior mean of \code{noitems}.
#' @param xvar The prior covariance matrix of \code{noitems}.
#' @param a0 The prior shape of inverse gamma distribution.
#' @param b0 The prior scale of inverse gamma distribution.
#' @param Edges The edge set of \code{noitems} in the graph.
#' @param data The data containing the comparison outcomes; defalut is NULL.
#' @param datatype The type of data to analyze: "simulated" (the default) for simulated data,
#' "real" for a given real dataset.
#' @param prior The type of prior: "conju" (default) for conjugate prior, 
#' "semi-conju" for semi-conjugate prior, "flat" for flat prior, and "ref" for reference prior
#' @return A posterior rankings of posterior draws from \code{\link{bayeslm}}
#' @note %% ~~further notes~~
#' @author Prince P. Osei and Ori Davidov
#' @seealso %% ~~objects to See Also as \code{\link{help}}, ~~~
#' @references %% ~put references to the literature/web site here ~
#' @importFrom mvtnorm rmvnorm
#' @examples
#' ## May be a simulated dataset??
#' @export
postranking <-
function(N,noitems, nocompars,scores,vars,xmu,xvar,a0,b0,
           Edges,data,datatype,
           prior=c("conju","flat","ref")){
    pes <- bayeslm(noitems=noitems,nocompars=nocompars,scores=scores,
                    vars=vars,xmu=xmu,xvar=xvar,
                    a0=a0,b0=b0,Edges=Edges,data=data,
                    datatype=datatype,prior=prior)
    # Posterior mean and covariance
    pmean=pes$Bayes$meanPost
    pcovar <- pes$Bayes$varPost
    # Posterior shape and scale parameter for sigma^2:
    pshape <- pes$Bayes$InGparms["shape"]
    pscale <- pes$Bayes$InGparms["scale"]
    
    # Obtain samples from normal inverse gamma: posterior distribution
    if (prior == "conju" | prior == "ref"){
      sigma2 <- as.matrix(rinvgamma(N,shape=pshape,rate=pscale))
      ds.sample <- t(apply(sigma2,1, function(x) rmvnorm(1,mean=pmean,sigma=x*pcovar)))
    }
    if(prior == "flat"){
      ds.sample <- rmvnorm(N,mean=pmean,sigma=pcovar)
    }
    return(ds.sample)
  }
