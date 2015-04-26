# J. Petters, April 26 2015
# R Programming Course Spring 2015
# Week 2 assignment
# code modified from makeVector and cachemean functions given
#   as demo of lexical scoping for assignement

# makeCacheMatrix: This function creates a special "matrix" 
#   object that can cache its inverse.
# arguments: a matrix. This function assumes the matrix is invertible.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL               #declare placeholder for output matrix inverse
  set <- function(y) {    #sets the value of the input matrix and  
    x <<- y               #wipes the output matrix inverse (does both in
    m <<- NULL            #   parent frame)
  }
  get <- function() x     #gets the value of the input matrix
  #setinverse sets the value of the inverse matrix in parent 
  #   frame to output solve()
  setinverse <- function(inverse) m <<- inverse 
  #getinverse retreives the value of the inverse matrix of 
  #   x (should it already exist)
  getinverse <- function() m 
  #the output of makeCacheMatrix is a list of the four functions 
  #   defined therein, operating on matrix x
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

# cacheSolve: This function computes the inverse of the 
#   special "matrix" returned by makeCacheMatrix above. 
#   If the inverse has already been calculated (and the 
#   matrix has not changed), then the cachesolve should 
#   retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        
  # attempt to retrieve the inverse of matrix x
  m <- x$getinverse()
  # if the inverse of matrix x exists, retrieve its cached value from
  #    the parent frame and exit this function 
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  # if the inverse of matrix x has not already been computed,
  #   get matrix x, compute its inverse using solve(), set
  #   its value in the parent frame and exit this function
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
