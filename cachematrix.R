## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## Take matrix as parameter and create a cache for
## his inverse
## Allow us to have control over the cache solution
## Cache is been reset if the initial matrix changes
makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
        set <- function(y) {
                x <<-y
                inverse <<-NULL
        }
        get <- function() x
        setinverse <- function(solve) inverse <<- solve
        getinverse <- function() inverse
        list(set=set,
             get=get,
             setinverse=setinverse,
             getinverse=getinverse
            )
}


## Work with function makeCacheMatrix
## Check if the inverse matrix have been calculated before or not
## If it not, then calculate the inverse matrix
## After that, store the inverse as cache of the previos function
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverse <- x$getinverse()
        if (!is.null(inverse)) {
                message("getting cached data")
                return(inverse)
        }
        data <- x$get()
        inverse <- solve(data, ...)
        x$setinverse(inverse)
        inverse
}
