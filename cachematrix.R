#This code will return the inverse of a matrix
#Running the below 3 sample lines in console will return the interted matrix
#   originalmatrix<-matrix(c(1,2,3,4),2,2)
#   cachedmatrix <- makeCacheMatrix(originalmatrix)
#   cacheSolve(cachedmatrix)


makeCacheMatrix <- function(x = matrix()) {
  #This fuction will output a list and will be made available for cacheSolve for invertion 
  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) inverse <<- solve
  getinverse <- function() inverse
  list(set = set, get = get,setinverse = setinverse,getinverse = getinverse)

  
}



cacheSolve <- function(x, ...) {
    #x is the output of the makeCacheMatrix function seen above
    inverse <- x$getinverse()
    if(!is.null(inverse)) {
      #if it has been already calculated, then skip, else perform the invertion
      message("getting cached data")
      return(inverse)
    }
    data <- x$get()
    inverse <- solve(data, ...)
    x$setinverse(inverse)
    inverse #print out the inverted matrix
    
  
}




