######
##
## This function creates a list of functions.
## The list includes:
## 1.  A function to set a matrix.
## 2.  A function to get the matrix.
## 3.  A function to set a solved (inverse) matrix.
##     While I programmed the functionality to set the value
##     of the inverse matrix, I don't know why you would
##     want to do this.
## 4.  A function to get the solved matrix.
## 
## Each time makeCacheMatrix$set() is called, the variable
## mySolvedMatrix (the inverse) is set to NULL.  This ensures
## each time cacheSolve() is called, it knows whether the
## current matrix has been solved or not.
## 
## NOTE: This function doesn't actually calculate the
##       inverse of the matrix.  That is done by the
##       function cacheSolve() witch then returns
##       the solved matrix.
##
######
makeCacheMatrix <- function(myData = matrix()) {
  
  ## Initially set our inverse matrix to NULL
  mySolvedMatrix <- NULL
  
  set <- function(y) {
    myData <<- y
    ## Each call sets our inverse matrix to NULL
    mySolvedMatrix <<- NULL
  }
  get <- function() myData
  setinverse <- function(solve) mySolvedMatrix <<- solve
  getinverse <- function() mySolvedMatrix
  
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}
######
## 
## This function does two things:
## 1.  Solves (inverse) a matrix.
## 2.  Returns a pre-solved matrix if it already exists
## 
## The function checks the data (a potential matrix) retruned
## by myData$getinverse().  If it's NULL then it calculates
## the inverse.  If it's not NULL, it returns the matrix.
## 
## NOTE:  This is the function that actually solves the
##        inverse of the matrix.
##
######
cacheSolve <- function(myData, ...) {
  
  mySolvedMatrix <- myData$getinverse()
  
  ## Check to see if our inverse matrix is NULL
  if(!is.null(mySolvedMatrix)) {
    ## If it is, return the cache
    message("getting cached data")
    return(mySolvedMatrix)
  }
  
  ## Else calculate and return the inverse
  data <- myData$get()
  mySolvedMatrix <- solve(data, ...)
  myData$setinverse(mySolvedMatrix)
  mySolvedMatrix
  ## Return a matrix that is the inverse of 'x'
}
