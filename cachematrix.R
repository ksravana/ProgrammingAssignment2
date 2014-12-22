## Function calculates inverse of a matrix. If the inverse is already in memory, 
## it is returned. If not, inverse is computed from "solve" function", the value
## stored and then it is returned.

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    
    m<-NULL
    
    set<-function (y) {
        x <<- y
        m <<- NULL
    }
    
    get <- function() { x }
    setmatrix <- function(solve) { m <<- solve }
    getmatrix <- function() { m }
    list(set=set, get=get,
         setmatrix=setmatrix,
         getmatrix=getmatrix)

}


## This function calculates invesrse using "solve" function. If the inverse matrix
## is already in memory, it simply returns it. If not, the function computed the 
## inverse using the "solve" function and stores it in memory and then return it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

    m <- x$getmatrix()
    if(!is.null(m)) {
        message("Using cached data")
        return(m)
    }

    matrix <- x$get
    m <- solve(matrix, ...)
    x$setmatrix(m)

    m # return value just stored

}
