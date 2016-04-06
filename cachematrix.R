## My functions cache the inverse of a matrix

## This function creates a list containing functions to:
## 1. set the matrix
## 2. get the matrix
## 3. set the inverse
## 4. get the inverse

makeCacheMatrix <- function(x = matrix()) {
    ivs<-NULL
    set<-function(y){
        x<<-y
        ivs<<-NULL
    }
    get<-function() x
    getInverse<-function(inverse) ivs<<-inverse
    list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)

}


## This function first tests whether the inverse was already cached
## If so, it gets the inverse from the cache 
## if not, it calculates the inverse of the data and sets the value in cache via the setInverse function

cacheSolve <- function(x, ...) {
    ivs<-x$getInverse()
    if (!is.null(ivs)) {
        message(" getting cached data")
        return(ivs)
    }
    mdata<-x$get()
    ivs<-solve(mdata,...)
    x$setInverse(ivs)
    return(ivs)
}
