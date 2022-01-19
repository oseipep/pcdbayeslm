#' @keywords internal
meritlabels <-
function(noitems,ascending=TRUE)
{
  prefix <- "i"; 
  if (ascending) suffix <- 1:noitems
  else suffix <- noitems:1
  labels <- paste(prefix,suffix,sep="")
  return(labels)
}
