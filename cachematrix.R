## First we create a list, say A, which stores the original matrix, 
## and its inverse (if already calculated) by this makeCacheMatrix function

## Then we use the cacheSolve function either to calculate the inverse 
## if it has not been calculated or to fetch the stored data from A.

makeCacheMatrix <- function(x = matrix()) {
        ## initialize i as NULL
        i <- NULL
        ## super-assign y to x and i to NULL, 
        ## as we use $set() in the console
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        ## return the assigned matrix
        get <- function() x
        ## set i to the inverse matrix (calculated by cacheSolve later) 
        setinverse <- function(inverse) i <<- inverse
        ## retrive stored inverse matrix from i
        getinverse <- function() i
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

cacheSolve <- function(x, ...) {
        ## initialize i as the matrix from the element 'getinverse' 
        ## of the input 'list', created by makeCacheMatrix function
        i <- x$getinverse()
        ## if i has been calculated before, then just retrive the result
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        ## if not calculated yet, then calculate it by solve() function
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}
