#' Bayesian linear models for cardinal paired comparison data
#' 
#' This function performs Bayesian inference for cardinal paired comparison
#' data. The methodology allows for doing Bayesian analysis on restricted
#' parameter space
#' 
#' @param noitems The number of items for the paired comparison.
#' @param nocompars The number of comparisons among \code{noitems}.
#' @param scores The true scores or merits of \code{noitems}
#' @param vars The constant variance of \code{scores}; default is 1.
#' @param xmu The normal prior mean of \code{noitems}; default is zero vector of 
#' length \code{noitems}.
#' @param xvar The prior covariance matrix of \code{noitems}; default is identity matrix 
#' of dimension \code{noitems}.
#' @param a0 The prior shape of inverse gamma distribution; default is 2.
#' @param b0 The prior scale of inverse gamma distribution; default is 1.
#' @param Edges The edge set of \code{noitems} in the graph; default is edgeset of
#' three items.
#' @param data The data containing the comparison outcomes; defalut is NULL.
#' @param datatype The type of data to analyze: "simulated" (the default) for simulated data,
#' "real" for a given real dataset.
#' @param prior The type of prior: "conju" (default) for conjugate prior, 
#' "semi-conju" for semi-conjugate prior, "flat" for flat prior, and "ref" for reference prior
#' @param tol The tolerance value to control near zero eigen values; default is 1e-08.
#' @return The Bayesian posterior mean and variance together with the least squares estimates.
#' %% @note %% ~~further notes~~
#' @author Prince P. Osei and Ori Davidov
#' %% @seealso %% ~~objects to See Also as \code{\link{help}}, ~~~
#' @references Osei, P. P. and Davidov, O. (2022). Bayesian linear models for cardinal paired comparison data. Under revision.
#' @importFrom igraph graph laplacian_matrix
#' @importFrom MASS ginv
#' @importFrom stats var
#' @examples
#' K = 3 # number of items
#' paircompars <- rep(3,3) # number of pairwise comparisons
#' Tscores <- 3:1-mean(3:1) # true scores 
#' cpcbayeslm(K,paircompars,Tscores)
#' @export
cpcbayeslm <-
function(noitems,nocompars,scores,vars=1,xmu=zeros(noitems,1),xvar=vars*diag(noitems),
           a0=2,b0=1,Edges=c(1,2,1,3,2,3),data=NULL,datatype="simulated",
           prior="conju",tol=1e-08)
{
  # create the simple graph and its Laplacian
  gh <- graph(Edges,n=noitems,directed = FALSE)
  L <- laplacian_matrix(gh, weights = nocompars, sparse = FALSE)
  # total number of comparisons
  n <- sum(nocompars)
  
  # Source of data: simulated or real
  s.data <- compagraph(noitems=noitems,nocompars=nocompars,
                       scores=scores,vars=vars,data=data,
                       datatype=datatype)
  Ydata <- as.matrix(s.data$Ydata)
  
  # estimated true variance of the data:
  sigma2 <- ifelse(is.null(vars),var(data),vars)
  
  # the sum of squares Yij
  SSY <- s.data$SquareY
  Cgraph <- s.data$pairwiseComp
  
  # LSE/MLE:
  muhat <- ginv(L)%*%Ydata
  Vmatmuhat <- sigma2*ginv(L)
  
  if (prior == "conju") {
    # Conjugate NIG prior
    # project the normal RV
    p.RV <- projbayeslm(noitems,xmu,xvar)
    Mu.0 <- p.RV$meanV
    Sigma.0 <- p.RV$varM
      
    # Posterior mean and variance
    Sigma.1 <- ginv(L + ginv(Sigma.0))
    Mu.1 <- Sigma.1%*%(L%*%muhat + ginv(Sigma.0)%*%Mu.0)
      
    # Update shape and scale parameters of sigma^2
    shape.parm <- a0 + n/2
      
    # scale parameter
    b <- L%*%muhat+ginv(Sigma.0)%*%Mu.0
    mu0.q <- t(Mu.0)%*%ginv(Sigma.0)%*%Mu.0
    scale.parm <- b0 + (1/2)*(sum(SSY)+mu0.q-t(b)%*%Sigma.1%*%b)
    norm.const <- NULL
    mis.scale.parm <- NULL
  }
  else if (prior == "semi-conju") {
    p.RV <- projbayeslm(noitems,xmu,xvar)
    Mu.0 <- p.RV$meanV
    Sigma.0 <- p.RV$varM
      
    # posterior mean and variance
    Sigma.1 <- ginv((L/sigma2) + ginv(Sigma.0))
    Mu.1 <- Sigma.1%*%((L/sigma2)%*%muhat + ginv(Sigma.0)%*%Mu.0)
      
    #update shape and scale parameters of sigma^2
    shape.parm <- a0 + n/2
    scale.parm <- b0 + sum(SSY)/2
    # other additional parameters for the sigma^2
    b <- (L/sigma2)%*%muhat + ginv(Sigma.0)%*%Mu.0
    mis.scale.parm <- (t(b)%*%Sigma.1%*%b)/2
      
    # Normalizing constant: c(Sigma.1)
    # Note the eigen values can be complex
    lambdas <- Re(eigen(Sigma.1)$val)[which(Re(eigen(Sigma.1)$val) > tol)]
    norm.const <- sqrt(prod(2*pi*lambdas))
  }
  else if (prior == "flat"){
    # Flat prior:
    Mu.1 <- muhat
    Sigma.1 <- sigma2*ginv(L)
    shape.parm <- NULL
    scale.parm <- NULL
    norm.const <- NULL
    mis.scale.parm <- NULL
    Mu.0 <- NULL
    Sigma.0 <- NULL
  }
  else if(prior == "ref"){
    # Reference prior:
    Mu.1 <- muhat
    Sigma.1 <- ginv(L)
    
    #update shape and scale parameters
    shape.parm <- (n-1)/2
    scale.parm <- abs((sum(SSY)-t(Ydata)%*%ginv(L)%*%Ydata)/2)
    norm.const <- NULL
    mis.scale.parm <- NULL
    Mu.0 <- NULL
    Sigma.0 <- NULL
  }
  #--------------
  LSE <- list(Muhat=muhat,varMuhat=Vmatmuhat)
  Bayes <- list(meanPost=Mu.1,varPost=Sigma.1,
                InGparms=c(shape=shape.parm,scale=scale.parm,
                           normconst=norm.const,
                           otherconjuscale=mis.scale.parm))
  others <- list(LapMatrix=L,Ydata=Ydata,SquareDat=SSY,Cograph=Cgraph,
                 MuPPrior=Mu.0,VarPPrior=Sigma.0,DatagenVar=vars)
  
  res <- list(Bayes=Bayes,LSE=LSE,othercomputations=others)
  
  return(res)
}
