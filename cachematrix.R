## this function creates a special matrix that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
 ##initialize m to NULL, which will hold the value of matrix inverse
   m <- NULL
## define the set function to assign new matrix value in parent envrionment
## if there is a new value, set m to NULL
  set <- function(y){
    x <<- y
    m <<- NULL
  }
##define the get function that will return the value of the matrix argument
  get <- function()x
## assign value of inverse in parent envrionment
  setinverse <- function(inverse) m <<- inverse
## get the value of inverse where it calls
  getinverse <- function()m
  list(set = set, get= get, setinverse = setinverse,getinverse = getinverse)
}


## this function computes the value of the matrix inverse returned by the makeCachematrix above

cacheSolve <- function(x, ...) {
  m<-x$getinverse()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  
  data = x$get()
  m = solve(data, ...)
  x$setinverse(m)
  return(m)
  
}
# Assignment3
