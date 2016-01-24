## Matrix inversion calculation may have negative performance impact on the server where R is run
## caching the inverse matrix can helo to prevent recalculation of same matrix repeatedly
## Following two functions will get the matrix as input and check if the inverse is already calculated
## if it was calculated simply show the result from previous calculation. If not then calculate that again and cache it

## makeCacheMatrix function creates square matrix and it can hold on to inverse values

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) i <<- inverse
        getInverse <- function() i
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## cacheSolve function it will first check if the Inverse matrix cache already exist, if yes then it will show 
## a text of ""getting cached data" followed by the result
## if not cached before, then it will calculte the inverse and return the result 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getInverse()
        if (!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        m <- x$get()
        i <- solve(m, ...)
        x$setInverse(i)
        i
}
