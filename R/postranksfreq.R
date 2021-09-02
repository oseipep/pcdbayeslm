#' @importFrom gtools combinations permutations
#' @export
postranksfreq <- 
  function(postdraws,n=3,v=1:n)
    {
    # v the vector or lables of the numbers for the permutations
   PMat <- permutations(n,n,v)
   colnames(PMat) <- colnames(postdraws)
   N <- nrow(PMat)
   Freq <- NULL
   for (i in 1:N){
     tmp <- sum(apply(postdraws,1,function(x) 
      ifelse(identical(as.numeric(x),as.numeric(PMat[i,]))==TRUE,1,0)))
     Freq <- c(Freq,tmp)
   }
  res <- cbind(PMat,Freq)
  res
}
#------------------------