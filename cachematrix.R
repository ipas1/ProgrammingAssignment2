# The functions in this file compute inverse of a matrix using caching

# makeCacheMatrix function creates cache matrix object
# with 4 functions in the list: set, get, setInverse and getInverse
# it takes parameter x of type matrix
# and returns a list with 4 list items - 4 functions listed above
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setInverse <- function(solve) m <<- solve
        getInverse <- function() m
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## Write a short comment describing this function
# cacheSolve function calculates inverse of a matrix
# it uses makeCacheMatrix function and expects input parameter x of the type
# created by makeCacheMatrix which is special matrix representation
# it returns inverse of the input matrix
# it takes inverse matrix from the cache if available, 
# and computes it and stores (in cache) if not
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getInverse()      # trying to get m from the cache
        if(!is.null(m)) {        # if there is object in the cache
                message("getting cached data")
                return(m)        # just return it
        }
        data <- x$get()          # in case no object in cache we compute inverse
        m <- solve(data, ...)    # by calling solve function
        x$setInverse(m)          # and save it in the cache
        m                        # finally return it
}
