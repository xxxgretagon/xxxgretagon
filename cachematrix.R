## This file is mainly for chaching the inverse of a matrix
## Two main functions are used to achieve it.

## makeCacheMatrix function creates a special "matrix" object 
## that can cache its inverse. 

makeCacheMatrix <- function(x = matrix()) {
  #set the inverse of x to NULL
  i<-NULL
  #define the function to initialize the special "matrix"
  set<-function(y){
    x<<-y
    i<<-NULL
  }
  #return the matrix
  get<-function() x
  #store the inverse of the matrix
  seti<-function(inverse) i <<- inverse
  #return the inverse of the matrix
  geti<-function() inverse
  #return the special "matrix"
  list(set=set,get=get,seti=seti,geti=geti)
}
## CacheSolve function This function computes the inverse of 
## the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## get the inverse value from special "matrix"
  i<-x$geti()
  ## check if the value is calculated, if calculated, get the cached data
  if(! is.null(i)){
    message("getting chached data")
    return(i)
  }
  #If not calculated, calculate it and store in the special "matrix"
  data <- x$get()
  i <- solve(data,...)
  x$seti(i)
  i
}