## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    ## check the matrix is invertible
    if(det(x) == 0) {
        message("The matrix is not invertible.")
    }
    ## set m is empty
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- null
    }
    get <- function() x
    setinv <- function(inv) m <<- inv
    getinv <- function() m
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    #Get matrix from cache
    m <- x$getinv()
    #If there is some matrix then don't calculte but just return from cache.
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    #Else, by using solve function calculate inverse matrix s
    data <- x$get()
    m <- solve(data,...)
    x$setinv(m)
    m
}
