## Aim of this script file is to cache inverse of matrix. Functions will check if inverse of matrix
## is already calculated and if it is present in cache(value of matrix is not changed) then it will return value from cache and if not then it will calculate inverse
## of matrix. makeCacheMatrix will generate object matrix and cacheSolve will calculate inverse of matrix
## by checking the value in cache.
## Assumtion - It is assumed that matrix taken for calcluation is always square invertible matrix.
## Author - Ankur Thakkar


## makeCacheMatrix function will generate special matrix. It will have list of functions, normally they are
## getters and setters for matrix. Input should be sqaure invertible matrix.

makeCacheMatrix <- function(x = matrix()) {
    IM <- NULL #initializing Inverse matrix to NULL
    
    # Function setMatrix to set matrix
    setMatrix <- function(y) {  
        x <<- y
        IM <<- NULL
    }
    
    # Function getMatrix to get matrix
    getMatrix <- function() x
    
    # Function setInv to set inverse
    setInv <- function(I){
        IM <<- I
    }
    
    # Function getInv to get inverse
    getInv <- function() IM
    
    # generate function list
    list(set=setMatrix, get=getMatrix, setInv= setInv, getInv=getInv)
}


## cacheSolve function will calculate inverse of matrix taken from makeCacheMatrix. It will
## first check if inverse is already available in cache and if matrix is not changed then it will 
## returned same inverse matrix from cache. Thus saving cacluation time.

cacheSolve <- function(x, ...) {
    #getting Inverse from function
    IM <- x$getInv()
    
    # if inverse is alreadt present than dont calculate inverse of the matrix
    if(!is.null(IM)) {
        message("getting cached data")
        return(IM)
    }
    
    # else calculate inverse of the matrix
    m <- x$get()
    IM <- solve(m)
    x$setInv(IM)
    IM
}
