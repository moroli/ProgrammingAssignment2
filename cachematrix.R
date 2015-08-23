## makeCacheMatrix : Functions that are used to invert a matrix and cache the result. 
## Before any calculation, checks the presence of the result in the cache in order 
## to avoir recomputation.


## makeCacheMatrix creates a special "Matrix", which is a list containing a function to
## 1-set the value of the matrix - SetMat
## 2-get the value of the matrix - GetMat
## 3-set the inverse of the matrix - SetMatInv
## 4-get the inverse of the matrix - GetMatInv
## Check, prior to any calculation, if the matrix is invertible

makeCacheMatrix <- function(x = matrix()) {

  MatInvCache <- NULL
  
  if (ncol(x) != nrow(x)) { ##Checks if the matrix is non squared, if so ends the function
      print("Non invertible matrix - must be squared")
      invisible(return)
  }
  else if(det(x)==0) {      ##Checks if the determinant is nul, if so ends the function
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
      
      list( ##
          SetMAt = SetMat, 
          GetMat = GetMat,
          SetMatInv = SetMatInv,
          GetMatInv = GetMatInv)
  }
}

## cacheSolve : Calculates the inverse of the matrix created with the above function. 
## First, checks if the inverse has already been calculated. 
## If so, it gets the result from the cache and skips the computation. 
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

## Sample run

##> a <- makeCacheMatrix(matrix(c(3,2,1,6), nrow=2, ncol=2))
##> cacheSolve(a)
##       [,1]    [,2]
##[1,]  0.375 -0.0625
##[2,] -0.125  0.1875

##> a <- makeCacheMatrix(matrix(c(3,2,1,6,3,5,2,6,7), nrow=3, ncol=3))
##> cacheSolve(a)
##        [,1]       [,2]        [,3]
##[1,]  0.1475410  0.5245902 -0.49180328
##[2,]  0.1311475 -0.3114754  0.22950820
##[3,] -0.1147541  0.1475410  0.04918033

##> a <- makeCacheMatrix(matrix(c(3,2,1,6,3,5), nrow=3, ncol=2))
##[1] "Non invertible matrix - must be squared"
