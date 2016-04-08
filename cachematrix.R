makeCacheMatrix <- function(x = matrix()) {
## Set the matrix
   n <- NULL 
   set <- function(y) {
      x <<- y
      n <<- NULL
   }

## Get the matrix
   get <- function() x 
## Set the inverse
   setinverse <- function(inverse) n <<- inverse

## Get the inverse
   getinverse <- function() n
## This list is used as the input to cacheSolve()
   list(set = set, get =get,
        setinverse = setinverse, 
        getinverse = getinverse)
}

## cacheSolve: This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated (and the 
## matrix has not changed), then the cachesolve should retrieve the inverse 
## from the cache.
cacheSolve <- function(x, ...) {
        n <- x$getinverse()
## if the inverse has already been calculated, just return the calculated inverse matrix
        if(!is.null(n)) {
                message("getting cached inverse matrix data")
                return(n)
        }
## otherwise, calculated the inverse matrix data and then return the calculated inverse matrix
        data <- x$get()
        n = solve(data, ...)
        
        x$setinverse(n)
        return(n)
}
