## pair of functions to calculate the inverse of a matrix,
## and to cache this value and return it if action is repeated

## function to cache the inverse of a matrix

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
