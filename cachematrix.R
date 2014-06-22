## These functions cache and make use of the inverse of a matrix.
## Since matrix inversion can take a long time to calculate,
## using cache can be helpful. 

## This function creates a matrix object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
        m<-NULL
        set<-function(y){
                x<<-y
                m<<-NULL
        }
        get<-function() x
        setmatrix<-function(solve) m<<- solve
        getmatrix<-function() m
        list(set=set, get=get,
             setmatrix=setmatrix,
             getmatrix=getmatrix)
}

## This function uses the matrix from makeCacheMatrix
## and computes its inverse. If the inverse has already been calculated
## and the matrix hasn't changed, then this function should 
## retrieve the inverse from the cache.
cacheSolve <- function(x = matrix(), ...) {
        ## Return a matrix that is the inverse of x.
        m<-x$getmatrix()
        if(!is.null(m)){
                message("getting cached data")
                return(m)
        }
        matrix<-x$get()
        m<-solve(matrix, ...)
        x$setmatrix(m)
        m
}
