## "cachematrix.R": R function to compute the inverse of a square matrix and then CACHE it  
##  to potentially reduce computational time  especially when input matrix is large and dense.
##  How to use: 1.source the code, 2.input matrix, 3.call makeCacheMatrix, 4. call cacheSolve
##  Try: mat<-matrix(c(1,2,3,4),nrow=2,ncol=2); x<-makeCacheMatrix(mat); sol1<-cacheSolve(x)

## makeCacheMatrix: creates and returns a special matrix by setting and getting the matrix, and
## setting and getting the inverse of the matrix

makeCacheMatrix <- function(x=matrix()){
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


# cacheSolve: Function to compute the inverse of the special matrix created by makeCacheMatrix, 
#             it calls the function "solve" 


cacheSolve<-function(x, ...){
  m<-x$getinverse()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  data <-x$get()
  m<- solve(data, ...)
  x$setinverse(m)
  m
}
