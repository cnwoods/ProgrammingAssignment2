## For regression analysis, inversion of a matrix is a handy technique,
## but can be costly on memory.  These functions use scoping to assign
## a matrix inversion to cache, so it's not constantly recalculated.

## This function creates a matrix object which will hold the inverse 
## of a matrix in a cache, allowing it to be used by other functions.

makeCacheMatrix <- function(x = matrix()) {
      ## these assignments 'clean the slate' for the function.
      ## note that 'set' is a nested function so <<- is needed to
      ## ensure the values will be available to the makeCacheMatrix
      ## environment (since it encloses the 'set' environment)
      s <- NULL
    set <- function(y) {
      x <<- y
      s <<- NULL
    }
     ## these assignments are back out in the main environment, but
     ## again setinv nests a function within a function, hence <<-
    get <- function() x
    setinv <- function(solve) s <<- solve
    getinv <- function() s
     ## this returns the object that holds the inverse of matrix x
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## This function checks to see if a matrix has been inverted and, if
## it has, so long as the matrix is unchanged, returns the solution 
## from cache.  If not, then the inverse is calculated and returned.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        ## checks to see if there is a value in cache and displays
        ## a message if cached values are found, then returns them.
    s <- x$getinv()
    if(!is.null(s)) {
      message("getting cached data")
      return(s)
    }
        ## calculates the inverse of the matrix, and auto-prints it
    data <- x$get()
    s <- solve(data, ...)
    x$setinv(s)
    s
}
