## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        cinv = NULL
        set = function(y) {
                x <<- y
                ## use <<- to assign a value to an object in an environment
                ## different from the current environnment
                cinv <<- NULL 
                ## sets the value of cinv to NULL
        }
        get = function() x ## get the value of the vector
        setcinv = function(inverse) cinv <<- inverse ## set the value of the inverse
        getcinv = function () cinv ## get the value of the inverse
        list(set = set, get = get,
             setcinv = setcinv, 
             getcinv = getcinv)
}

  ## caches potentially time-consuming computations              
                

cacheSolve <- function(x, ...) {
        cinv = x$getcinv()
        
        if (!is.null(cinv)){
                message("getting cached data")
                return(cinv)
        } 
        
        ##otherwise, calculates the invers
        inv.data = x$get()
        cinv = solve(inv.data, ...)
        
        x$setcinv(cinv)
        cinv
        ## Return a matrix that is the inverse of 'x'
}



