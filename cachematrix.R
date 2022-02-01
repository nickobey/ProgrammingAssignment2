## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
# This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
      xinv <- NULL
      set <- function(y) { # set matrix
            x <<- y
            inv <<- NULL
      }
      get <- function() x # get matrix
      setinv <- function(inverse) xinv <<- inverse #set inverse
      getinv <- function() xinv #get inverse
      list(set = set, get = get, setinv = setinv, getinv = getinv) #input for cacheSolve
}


## Write a short comment describing this function

# This function computes the inverse of the special "matrix" returned by 
# `makeCacheMatrix` above. 

cacheSolve <- function(x, ...) {
      xinv <- x$getinv()
      if(!is.null(xinv)) {     
            message("getting cached data")
            return(xinv)
            #If the inverse has already been calculated (and 
            # the matrix has not changed), then `cacheSolve` should retrieve the inverse 
            # from the cache.
      }
      data <- x$get()
      xinv <- solve(data, ...)
      x$setinv(xinv)
      xinv
        ## Return a matrix that is the inverse of 'x'
        #solve(x)
}
