#' Draws samples from the posterior distribution for paired comparison data
#' 
#' This function performs a new posterior ranking methodology developed in the 
#' paper by Prince P. Osei and Ori David (2021) to account for the uncertainty
#' in the ranking of items.
#' 
#' @param N Number of posterior samples.
#' @param noitems Number of items in the comparison graph
#' @param nocompars The number of comparisons among \code{noitems}.
#' @param scores The true scores or merits of \code{noitems}
#' @param vars The constant variance of \code{scores} default is 1.
#' @param xmu The normal prior mean of \code{noitems} default is a vector of zeros of length
#' \code{noitems}.
#' @param xvar The prior covariance matrix of \code{noitems} default is \code{vars} times 
#' the identity matrix dimension \code{noitems}.
#' @param a0 The prior shape of inverse gamma distribution default is 2
#' @param b0 The prior scale of inverse gamma distribution default is 1
#' @param Edges The edge set of \code{noitems} in the graph default is complete graph of 
#' three \code{noitems}.
#' @param data The data containing the comparison outcomes; defalut is NULL.
#' @param datatype The type of data to analyze: "simulated" (the default) for simulated data,
#' "real" for a given real dataset.
#' @param prior The type of prior: "conju" (default) for conjugate prior, 
#' "semi-conju" for semi-conjugate prior, "flat" for flat prior, and "ref" for reference prior
#' @return A posterior rankings of posterior draws from \code{\link{cpcbayeslm}}
#' %% @note %% ~~further notes~~
#' @author Prince P. Osei and Ori Davidov
#' %% @seealso %% ~~objects to See Also as \code{\link{help}}, ~~~
#' %% @references %% ~put references to the literature/web site here ~
#' @importFrom mvtnorm rmvnorm
#' @examples
#' ##  A graph of three items
#' ##  K <- 3 # number of items
#' ## edges <- c(1,2,1,3,2,3) # edge set
#' ## compars <- rep(3,3) # pairwise comparisons
#' ##  Tscores <- 1:3-mean(1:3) # The true score sum to zero
#' ##  N <- 100 # number of samples to draw
#' ## postdrawspcd(N,K,compars,Tscores,Edges=edges)
#' @export
postdrawspcd <-
function(N,noitems, nocompars,scores,vars=1,xmu=zeros(noitems,1),xvar=vars*diag(noitems),
         a0=2,b0=1,Edges=c(1,2,1,3,2,3),data=NULL,datatype="simulated",prior="conju"){
    pes <- cpcbayeslm(noitems=noitems,nocompars=nocompars,scores=scores,
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
    res <- list(drawspost=ds.sample)
    res$meanpost <- pmean
    res$covarpost <- pcovar
    res$shapepost <- pshape
    res$scalepost <- pscale
    class(res) <- "cpcbayeslm"
    res
  }
