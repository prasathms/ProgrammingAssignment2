## Coursera Project Assignment 2
## function makeCacheMatrix - set, get Matrix and Set & Get inverse matrix
## function cacheSolve - return a inverse Matrix, check with makeCacheMatrix for cache


makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) 
      {
        x <<- y
        m <<- NULL
      }
    
  get <- function() x
  setmatrixinverse <- function(solve) m <<- solve
  getmatrixinverse <- function() m
  
  list(set = set, get = get,
       setmatrixinverse = setmatrixinverse,
       getmatrixinverse = getmatrixinverse)
}



## cacheSolve(a)
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        ## example: Second time "CacheSolve(a)" function runs, it gets from "Cached Data"
        ## a <- makeCacheMatrix(matrix(1:4,nrow=2)) 
        ## cacheSolve(a)
        ## cacheSolve(a) 
  
      m <- x$getmatrixinverse()
      
      if(!is.null(m)) 
        {
          message("getting cached data")
          return(m)
      }
      
      data <- x$get()
      m <- solve(data, ...)
      x$setmatrixinverse(m)
      m
}
