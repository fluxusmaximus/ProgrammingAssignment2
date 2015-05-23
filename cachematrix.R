## Put comments here that give an overall description of what your
## functions do

## this creates a special "matrix" object with a format that can cache its inverse

makeCacheMatrix <- function(x = matrix()){  
        
        matrix <- NULL #setup a default null matrix
        
        set <- function(y){
                if (nrow(y)!= ncol(y)){ 
                        stop("matrix is not invertible: Not square matrix")
                }
                if (det(y)==0){
                        stop("matrix is not invertible: determinant = 0")
                }
                
                
                x <<- y
                m <<- NULL 
        } #set function that caches the matrix 
        
        get <- function()x
        #get cached matrix
        
        setCacheSolve <- function(cacheMatrix) {
                m <<- cacheMatrix
        } #sets the solved cached matrix
        
        getCacheSolve <- function() m
        #gets the solved cached matrix
        
        list(set = set, get = get, 
             setCacheSolve = setCacheSolve,
             getCacheSolve = getCacheSolve)
        #this list sets a format for storing all the needed functions/variables 
}

cacheSolve <- function(x){
        
        
        m <- x$getCacheSolve()
        #brings up the solved cached matrix
        
        if (!is.null(m)){
                message("Getting cache matrix")
                return(m)
                #if matrix is not null, return
        }
        
        data <- data.matrix(x$get())
        #gets the item from the list, translates this into a numeric matrix format
        
        m <- solve(data) #this solves for inverse
        
        x$setCacheSolve(m) #sets the inverse matrix
        message("Caching solution, inverse matrix is")
        m  
}
