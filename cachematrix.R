## makeCacheMatrix creates an object for manipulating objects in cache
## cacheSolve calculates the inverse of a matrix and records it in cache

## This function defines four functions for storing the matrix and its inverse 
## to cache
makeCacheMatrix <- function(x = matrix()) {
    # Create an object m
    m <- NULL
    set <- function(y) {
        # Assign y to x; this resets x
        x <<- y
        # Clear the value of m
        m <<-NULL
    }
    # This function will retrieve x from the parent environment
    get <- function() x
    # Assigns the value of the inverse to m
    setinv <- function(solve) m <<- solve
    # Retrieves the value of the inverse from m
    getinv <- function() m
    # This is the data which the function returns to the parent environment
    # The command gives a name to each function defined above, and returns them
    list(set=set, get=get, setinv=setinv, getinv=getinv)
}


## Calculates the inverse for a makeCacheMatrix object
## Requires an object of class makeCacheMatrix as input
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        # Check if the inverse already calculated by getting it from memory
        m <- x$getinv()
        # if it is, then print a message and return the cached value
        if (!is.null(m)) {
             message("getting cached matrix inverse")
             return (m)
        }
        # Get the matrix from the makeCacheMatrix object x
        data <- x$get()
        # Compute the inverse of x and write it into m
        m <- solve(data)
        # record the inverse into the makeCacheMatrix object x
        x$setinv(m)
        m
}
