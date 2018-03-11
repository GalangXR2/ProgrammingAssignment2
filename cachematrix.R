## the function cacheSolve is used to return the inverse of a matrix, calculated using R's
## "solve" function. it takes as its argument a special matrix object created using the
## makeCacheMatrix. makeCacheMatrix takes a matrix as its argument and returns a list of
## functions. together, the two functions offer a solution for efficiently returning the
## inverse of a matrix - if it were already calculated, the inversed matrix is retrieved from
## the cache (rather than being recalculated)

## the function makeCacheMatrix takes a matrix x and creates a special object - a list of
## functions - that can retrieve and define both the matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) i <<- solve
        getinverse <- function() i
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## the function cacheSolve takes a matrix x (created with the makeCacheMatrix function above) 
## and returns its inverse. if the inverse has already been calculated, it simply returns the 
## inversed matrix cached before, otherwise it calculates the inverse and returns it (while
## saving it to the cache)

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}
