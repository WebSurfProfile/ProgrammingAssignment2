# This script is to pull the inverse of a matrix

# Make a special function that sets/gets the matrix data and its inverse.
makeCacheMatrix <- function(x = matrix()) {
  
#  display input matrix
#   message("Input Matrix")
#   print(x)

  # set matrix inverse to null
  inv_mat <- NULL
  
  # store matrix data into get function
  # this will run as soon as the get function is loaded
  get <- function() { x }
  
  # store matrix inverse data into getinv function
  # this will run as soon as the getinv function is loaded
  getinv <- function() { 
    print("trying to get inverse")
    inv_mat 
  }
  
  # set matrix data. Only runs if called
  set <- function(set_mat) {
    x <<- set_mat
    inv_mat <<- NULL
  }
  
  # set matrix inverse data. 
  setinv <- function(inv_mat_to_set) {
    inv_mat <<- inv_mat_to_set 
  }
  
  # return a list of all set get functions
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


cacheSolve <- function(x, ...) {

  ## Return a matrix that is the inverse of 'x'
  
  inv <- x$getinv()
  print("got value from getinv")
  print(inv)
  
  if( !is.null(inv) ) {
    print("getting cached data")
    return(inv)
  }
  
  # this runs if value is not already stored
  mat_data <- x$get()
  message("got the value from get function not from cache")
  
  #print the value of matrix
  print(mat_data)
  
  #store matrix values
  inv <- solve(mat_data, ...)
  
  x$setinv(inv)
  print("inverse of matrix is")
  inv
}


# The below code is to test the script
# test_mat = matrix (c(1,2,3,4),nrow=2,ncol = 2)  
# test_cache_matrix = makeCacheMatrix(test_mat)
# cacheSolve(test_cache_matrix)
# 
