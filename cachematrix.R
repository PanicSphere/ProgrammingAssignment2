## This function caches a matrix
## and its inverse in the funtion's environment
## (makeCacheMatrix is always the defining environment)
## or returns previously cached values

makeCacheMatrix <- function() {
  ## set matrix
  set <- function(y) {
    ## chache a matrix in in defining environment
    x <<- y
    ## clear matrix inverse in defining environment as matrix has (presumably) changed
    x_inv <<- NULL
  }
  ## get matrix
  get <- function() x
  ## set inverse matrix
  ## cache inverse matrix in defining environment
  setinverse <- function(inverse_matrix) {
    x_inv <<- inverse_matrix
  }
  ## get the inverse matrix
  ## return inverse matrix from defining environment
  getinverse <- function() x_inv
  ## enumerate available function methods
  list (set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Given a matrix, this function checks if cached inverse exists
## If cached inverse exists, returns cached inverse
## Else calculates, caches and returns inverse
cacheSolve <- function(x) {
  ## Create reference to caching function
  cache <- makeCacheMatrix()
  ## Recover currently cached inverse
  x_inv <- cache$getinverse()
  ## check if there is a valid cached inverse matrix
  if(is.matrix(x_inv)) {
    ## inverse matrix exists
    ## check if the matrix has changed
    if(identical(x , cache$get())){
      ## matrix is same and inverse exists
      ## return cached value and exit
      message("Using cached data.")
      return (x_inv)
    }
  }
  
  ## need to calculate new inverse
  
  ## either no cache or changed matrix so need to    
  ## cache matrix we are calculating inverse to
  ## if no inverse proably needed anyway so not worth checking
  cache$set(x)
  ## need to calculate new inverse
  ## and cache a copy
  x_inv <- solve(x)
  cache$setinverse(x_inv)
  ## return inverse matrix
  message("New inverse")
  x_inv
}
