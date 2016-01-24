## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {

    
    i<- NULL
    set<-function(y) {
        x<<- y
        i<<- NULL
    }
    
    ##functions to get and set inverted matrix
    get<-function() x
    setInverse<- function(solve) i <<- solve
    getInverse<- function() i

    list(
        set=set,
        get=get,
        setInverse=setInverse,
        getInverse=getInverse)
}


## computes the inverse of matrix
## if the inverse has been calculated (and still the same matrix)
##then cached inverse is returned

cacheSolve <- function(c, ...) {
   ##c is a matrix
   i <- c$getInverse()
     ## does the matrix exist?
    if(!is.null(i)){
        return(i)
    }
    
    #no cached matrix, so need to invert matrix and cache
    MatrixtoInvert<- c$get()
    i<- solve(MatrixtoInvert)
    c$setInverse(i)
    i
}
