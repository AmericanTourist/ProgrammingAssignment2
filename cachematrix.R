## created by AmericanTourist on 12 May 2016

## This cachematrix.R script is intended to supply 2 functions. The first function,
## makeCacheMatrix, takes in a matrix and creates a special matrix that has 
## 4 internal functions that are utilised by cacheSolve.
## 
## An example of the user input would be:
## a <- matrix(c(2,6,7,3),nrow=2,ncol=2)
## m <- makeCacheMatrix(a)
## s <- cacheSolve(m)
## 
## If the inverse does not exist, it should simply solve the inverse and store it.
## If the inverse has already been calculated, it retrieves it.
## 
## When the user prints s, for this example, it should return:
##             [,1]        [,2]
## [1,] -0.08333333  0.19444444
## [2,]  0.16666667 -0.05555556


makeCacheMatrix <- function(x = matrix()) {
  
  # Create the inverse matrix object as NULL
  x_inv <- NULL
  
  # Define the Set function
  set <- function(y) {
    
    # Take in the matrix and store it as the global variable x
    x <<- y
    
    # Ensure the global inverse matrix is NULL
    x_inv <<- NULL
    
  } 
  
  # Define the Get function which simply returns x
  get <- function() x
  
  # Define the setInverse function
  setInverse <- function(i) x_inv <<- i
  
  # Define the getInverse function which returns the inverse (previousl calculated)
  getInverse <- function() x_inv
  
  # Create a list of the functions
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
  
}


## This function takes in a matrix, checks if the cache has already calculated
## and stored the inverse matrix. If it has, it returns the inverse matrix. If 
## it hasn't, it calculates the inverse matrix and stores it in the cache for
## future queries.

cacheSolve <- function(x, ...) {
  
  # The getInverse function attempts to retrieve the inverse matrix. 
  x_inv <- x$getInverse()
  
  # It checks if the location is NULL, which means it hasn't been previously 
  # calculated.
  if(!is.null(x_inv)) {
    
    # If there is a stored inverse matrix, it prepares the user, and then returns it.
    message("getting cached data")
    return (x_inv)
    
  }
  
  # If the inverse has not been previoulsy calculated and stored in the cache, 
  # the input matrix is temporarily stored as an object 'data'.
  data <- x$get()
  
  # The inverse matrix is then calculated and stored as x_inv.
  x_inv <- solve(data,...)
  
  # This value is then stored in the cache.
  x$setInverse(x_inv)
  
  # The inverse is then printed out, signalling the end of the function.
  x_inv
}
