## R function to compute the inverse of a square matrix and then CACHE it  
## to potentially reduce computational time  
         

## makeCacheMatrix: creates and returns a special matrix 
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

# cacheSolve: Function to compute the inverse of the matrix 

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
