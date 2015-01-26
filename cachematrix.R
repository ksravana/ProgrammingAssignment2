## Function calculates inverse of a matrix. If the inverse is already in memory, 
## it is returned. If not, inverse is computed using "solve" function", its value
## stored and then returned.

makeCacheMatrix <- function (x = matrix()) {
    
    minv <-NULL             # m stores the inverse matrix, initialized here to NULL
    
    set <- function (y) {   # you can use this function to set the original matrix
        x <<- y
        minv <<- NULL       # be sure to wipe out any stored value of inverse when the original matrix is set
    }
    
    get <- function() { x } # x is the original matrix
    
    setinverse <- function (minv_given) { 
        minv <<- minv_given 
    }
    
    getinverse <- function() { minv }
    
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)

}


## This function calculates invesrse using "solve" function. If the inverse matrix
## is already in memory, it simply returns it. If not, the function computes the 
## inverse using the "solve" function, stores it in memory and then returns it.

cacheSolve <- function (x, ...) {
        ## Return a matrix that is the inverse of 'x'

    minv <- x$getinverse() #  get inverse matrix from the cached matrix object

    if(!is.null(minv)) {   # if minv is not null, i.e. it is valid, just return it
        message("Using cached data")
        return (minv)
    }

    # minv does not exist, compute the inverse, update cache 
    # and return the computed minv

    m <- x$get()            # get the original matrix
    minv <- solve (m, ...)  # solve for inverse
    x$setinverse (minv)     # set inverse in the cached matrix object

    minv                    # return the value just stored

}


