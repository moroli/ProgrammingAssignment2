## Functions that are used to invert a matrix and cache the result. Before any calculation,  
## checks the presence of the result in the cache in order to avoir recomputation.


## makeCacheMatrix creates a special "Matrix", which is a list containing a function to
##  1-set the value of the vector
##  2-get the value of the vector
##  3-set the value of the mean
##  4-get the value of the mean
## Check, prior to any calculation, if the matrix is invertible

makeCacheMatrix <- function(x = matrix()) {

  MatInvCache <- NULL
  
  if (ncol(x) != nrow(x)) {
      print("Non invertible matrix - must be squared")
      invisible(return)
  }
  else if(det(x)==0) {
      print("Non invertible matrix - determinant is nul ")
      invisible(return)
  }
  else {
      SetMat <- function(y) {
      x <<- y
      MatInvCache <<- NULL
      }
    
      GetMat <- function() x
      
      SetMatInv <- function(MatInv) MatInvCache <<- MatInv
      
      GetMatInv <- function() MatInvCache
      
      list(
          SetMAt = SetMat, 
          GetMat = GetMat,
          SetMatInv = SetMatInv,
          GetMatInv = GetMatInv)
  }
}

## Calculates the inverse of the matrix created with the above function. 
## Checks first to see if the inverse has already been calculated. 
## If so, it gets the mean from the cache and skips the computation. 
## Otherwise, it calculates the inverse of the matrix and sets the value 
## of the inverse matrix in the cache via the SetMatInv function.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  MatInv <- x$GetMatInv()
  if(!is.null(MatInv)) {
      message("getting cached data")
      return(MatInv)
  }
  
  data <- x$GetMat()
  
  MatInv <- solve(data, ...)
  x$SetMatInv(MatInv)
  MatInv
}
