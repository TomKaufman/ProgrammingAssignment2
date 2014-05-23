#Matrix inversion is usually a costly computation 
#and there may be some benefit to caching the inverse of 
#a matrix rather than computing it repeatedly
#This function creats a special "matrix" object that can cache its inverse


makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {  #Set the value of the matrix
    x <<- y
    m <<- NULL
  }
  get <- function() x      #Get the value of the matrix
  setinv <- function(solve) m <<- solve  #set the inverse of the matrix
  getinv <- function() m                #get the inverse of the matrix
  list(set = set, get = get,  #creating a list holding all the above functions
       setinv = setinv, 
       getinv = getinv)
}

#This function checks if the inversion of 
#the matrix calculation was made before and
#if so returns the cached data. if not it 
#calculates the inversion.

cacheinv <- function(x, ...) {
  m <- x$getinv()  #getting the inverse matrix 
  if(!is.null(m)) { #checking if there is a previously calculated solution
    message("getting cached data") # if there is a solution 
    return(m)                     #printing the massage and the result
    
  }
  data <- x$get() # if there is no solution, calculating the solution
  m <- solve(data, ...)
  x$setinv(m)
  m
}
