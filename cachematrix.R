## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## Creates an list with functions used in cacheSolve
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    
    set <- function (m) { 
        x <<- m
        inv <<- NULL
    }
    get <- function() x
    
    setInv <- function(inverse) inv <<- inverse
    
    getInv <- function() inv
    
    list (set = set, get = get, setInv = setInv, getInv = getInv)

}


## Write a short comment describing this function
## Calculates inverse matrix of x if not already previously cached

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getInv()
    if (!is.null(inv)) {
        message('getting cached inv')
        return(inv)
    }
    
    matrix <- x$get()
    inv <- solve(matrix, ...)
    x$setInv(inv)
    inv
}
