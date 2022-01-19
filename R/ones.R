#' @keywords internal
ones <-
function(nx = 1, ny = nx){
  
  if (is.null(nx) | is.null(ny))
    stop('ones: nx or ny is NULL',
         call.=FALSE)
  
  if (!is.numeric(nx) | !is.numeric(ny))
    stop('ones: nx or ny is not numeric',
         call.=FALSE)
  
  if (length(nx)!=1 | length(ny)!=1)
    stop('ones: nx or ny is not of length 1',
         call.=FALSE)
  
  if (nx<=0 | ny<=0)
    stop('ones: nx or ny is less or equal to 0',
         call.=FALSE)
  
  nx <- as.integer(nx)
  ny <- as.integer(ny)
  
  return(matrix(1, nrow=nx, ncol=ny))
  
}
