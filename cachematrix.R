## The functions below enable a user to calculate the inverse of a matrix and store it in a cache

## This function takes a matrix as an input and creates a special matrix that enables caching.  It returns a list.

makeCacheMatrix <- function(mat = matrix()) {
        i <- matrix()
        set <- function(y) {
                mat <<- y
                i <<- matrix()
        }
        get <- function() mat
        setinv <- function(inv) i <<- inv
        getinv <- function() i
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)

}


## This function takes a special matrix created by the makeCacheMatrix function and calculates the inverse of the matrix
## contained therein.  If the inverse has already been calculated before, the function simply returns the answer from the
## cache.

cacheSolve <- function(mat_, ...) {
        i <- mat_$getinv()
        if(!all(is.na(i))) {
                message("getting cached data")
                return(i)
        }
        data_ <- mat_$get()
        i <- solve(data_, ...)
        mat_$setinv(i)
        i
}
