#' @importFrom ggplot2 ggplot geom_bar theme ggtitle xlab ylab aes element_text
#' @importFrom forcats fct_reorder
#' @importFrom graphics par legend
#' @keywords internal
postprobsucra <- 
  function(x,lower.is.better=FALSE,plot=TRUE)
  {
  rank.probability <- x
  
  # Convert rank.probability to matrix
  MAT <- as.matrix(rank.probability)
  
  # Loop over treatments/items, for each treatment: calculate SUCRA
  a <- ncol(MAT)
  j <- nrow(MAT)
  names <- rownames(MAT)
  Treatment <- names
  
  SUCRA <- numeric()
  for (x in 1:j) {
    SUCRA[x] <- sum(cumsum(MAT[x,1:(a-1)]))/(a-1)
    
  }
  
  # If condition for lower.is.better
  if (lower.is.better == TRUE){
    SUCRA = numeric()
    for (x in 1:j) {
      SUCRA[x] <- 1-sum(cumsum(MAT[x,1:(a-1)]))/(a-1)
    }
  }
  
  # Make data.frame
  res <- data.frame(Treatment = Treatment, SUCRA = SUCRA)
  
  # Order
  res <- res[order(-res$SUCRA),]
  rownames(res) <- 1:j
  
  if (plot==TRUE){
    
    plot = ggplot(res, aes(x=fct_reorder(Treatment, -SUCRA), y=SUCRA)) +
      geom_bar(stat="identity",width=0.1,colour="lightgrey") +
      theme(axis.text.x = element_text(angle=45, color="black"),
            axis.text.y = element_text(color="black")) +
      ylab("posterior probability") 
      xlab("Treatment") 
      ggtitle("Ranks uncertainties")
    plot(plot)
    
  }
  
  rownames(res) <- res$Treatment
  res$Treatment <- NULL
  
  return(res)
}


