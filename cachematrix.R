## Computing the inverse of a square matrix can be done with the solve function in R. 
## For example, if X is a square invertible matrix, then solve(X) returns its inverse.


## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(m = matrix()) {
  
  nc = list(
    m = m,
    get = function(x) nc[[x]],
    set = function(x) nc[[x]] <<- solve(m)
  )
  
  nc <- list2env(nc)
  class(nc) <- "InverseMatrix"
  return(nc)
  
}


## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  cache <- makeCacheMatrix(x)
  if(toString(cache$get(digest(x)))==""){
    cache$set(digest(x))
  }
  return(cache$get(digest(x)))
}
