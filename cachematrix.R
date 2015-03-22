## Put comments here that give an overall description of what your
## functions do

## this creates a special "matrix" object with a format that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {

        i = NULL
        set <- function(y){
                x <<- y  
                i <<- NULL
        }
        get <- function()x
        setInverse <- function(inv){
                i <<- inv
        }
        getInverse <-function()i
        
        list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
}


## This function takes the special matrix and solves for its inverse if it doesn't exist and retrieves it if it does.

cacheSolve <- function(x,...) {
        ## Return a matrix that is the inverse of 'x'
        
        inverse <- x$getInverse()
        
        if (!is.null(inverse)){
                message("Getting Cached Data")
                return(inverse)
        }
        
        else{
        ##if no inverse is stored, calculate inverse here
                data <- x$get()
                inverse <- solve(data,...)
                x$setInverse(inverse)
                inverse
        
        }
        
}
