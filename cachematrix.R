## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

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

## Write a short comment describing this function

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
