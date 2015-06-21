## cached Matrix Inverse V1.0.0
## 
## Description: set of functions that compute the inverse of a
## square matrix on a computational cost-effective method.
## The matrix is assumed to be invertible.
##
## Example of use: 
##                  ## generate the matrix
##                  x = matrix(c(2,3,1,3,2,4,3,2,7),3,3)
##                  ## generate a cached matrix and store x
##                  u = makeCacheMatrix(x)
##                  ## calculate the inverse of x (stored in u)
##                  x_inv = cachesolve(u)
##  x_inv will contain the inverse of x.
##
## Author: Federico Allo Ron
## 
## These functions were implemented fot the coursera course: 
## R programming - The data science Specialization - Assignment 2


## makeCacheMatrix
# Description: creates a special "matrix" object that can cache its
#              inverse. The matrix will be stored as well as its 
#              inverse (after first calculation).
# Input arguments: x - the matrix which inverse is desired.
# Output : (cache matrix object) - a list object that Contains a set
#          of functions to handle the cache stored matrix x and its
#          inverse. Functions contained:
#             - set(y) : sets matrix y as the new stored matrix.
#                        Previous inverse calculations are erased.
#             - get()  : returns the stored matrix at the time the
#                        function is called.
#             - setsolve(z) : sets the inverse value as z.
#                          NOTE: if using this function incorrecty
#                          the inverse matrix stored may not be the
#                          inverse of the stored matrix ($get()).
#             - getsolve(z) : returns the inverse matrix stored.               
makeCacheMatrix <- function(x = matrix()) {
  
  # Initialize internal variables.
  m <- NULL
  
  set <- function(y) {
    # store y as the new matrix and erase the previous inverse.
    x <<- y
    m <<-NULL
  }
  get <- function() x
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## cacheSolve
# Description: computes the inverse of the special "matrix" returned
#              by makeCacheMatrix function.
#              If the inverse has already been calculated (and the 
#              matrix has not changed), the cachesolve retrieve the 
#              inverse from the cache.
# Input arguments: x - a cache matrix object. (see makecachematrix)
# Output: (matrix object) - the inverse of the matrix stored in x.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'  
  
  # Obtain the stored inverse in x.
  m <- x$getsolve()
  
  # If there is an inverse alread stored, return it.
  if(!is.null(m)) {
    message("getting cached data")
    
    # Exit the function returning the cached inverse.
    return(m)
  }
  
  # If not, calculate the inverse:
  message("solving inverse")
  
  # Get the stored original matrix.
  data <- x$get()
  
  # Calculate the inverse of the original matrix.
  m <- solve(data, ...)
  
  # Store the inverse in cache.
  x$setsolve(m)
  
  # Return the inverse.
  m
}
