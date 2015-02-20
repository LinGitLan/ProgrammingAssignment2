## Below is a pair of functions that cache the inverse of a matrix.

## 1. makeCacheMatrix: This function creates a special "matrix" 
## object that can cache its inverse. We assume that the matrix 
## supplied is always invertible.

makeCacheMatrix <- function(x = matrix()) {
        
        i <- NULL
        set <- function(y) { # 'set' changes the value of the matrix on which to calculate the inverse. Whenever 'set' is called, the solution (i.e. the inverse) is "cleared" to ensure it will be recalculated.
             x <<- y
             i <<- NULL # Special assignment operator (<<-) updates
                           # the value of variable i (wchich was previously
                           # set to NULL)
        }
        
        get <- function() x
        setinverse <- function(solve) i <<- solve
        getinverse <- function() i
        list(set = set, get = get, # Statement list returns a list of four functions. 
             setinverse = setinverse,
             getinverse = getinverse) 
        
}


## 2. cacheSolve: This function computes the inverse of the special 
## "matrix" returned by makeCacheMatrix function. If the inverse has 
## already been calculated (and the matrix has not changed), then the 
## cachesolve should retrieve the inverse from the cache. 

cacheSolve <- function(x, ...) {
        
        # cacheSolve "detects" that the inverse has been "cleared" 
        # (when the set function in makeCacheMatrix function changed
        # the matrix), so the 'return' is not executed...
        
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        
        # ...rather the function continutes by getting the new matrix, 
        # calculating the inverse and then setting (caching) it:
        
        data <- x$get()         # reads the matrix/data from the makeCacheMatrix function
        i <- solve(data, ...)   # computes the inverse
        x$setinverse(i)         # sets the calculated inverse in the cache
        i
}
