## Put comments here that give an overall description of what your
## functions do

## Function to create a matrix object that can cache its inverse 
makeCacheMatrix <- function(x = matrix()) { # define the argument with the "matrix" selection
                # initialize inv as NULL
                inv <- NULL
                # define set function
                set <- function(y) {
                # assign a new value of matrix
                x <<- y
                inv <<- NULL # initialize t as NULL 
                }
                # define the function to get matrix x
                get <- function() x
                setinverse <- function(inver) inv <<- inver
                getinverse <- function() inv # gets the value of inv
                list(set = set, get = get,
                setinverse = setinverse,
                getinverse = getinverse)
}


## getting the cache data
cacheSolve <- function(x, ...) {
                
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()
        if(!is.null(inv)) { # verify if inv is a NULL
        message("getting cached data")
        return(inv) # return inverse value
        }
        data <- x$get()
        inv <- solve(data, ...) # calculates inverse value
        x$setinverse(inv)
        inv # return the inverse of 'x' (matrix)
}