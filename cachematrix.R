##This function returns functions retrieving a matrix and it's inverse and for setting 
## a matrix and its inverse.  The function returns a list of arguments corresponding to the functions
## so they can be called by the $ index in the second function.

makeCacheMatrix <- function(matCache = matrix()) {
  #Create the setting functions:  
  setMatrix <- function(aMatrix){     #pass invertible matrix, store in parent environment, clear past values
    matCache <<- aMatrix
    invCache <<- NULL
  }
  
  setInverse <- function(inverse){    #pass matrix inverse as argument and store in the parent environment
    invCache <<- inverse
  }
  
  #Create the retrieving functions: 
  getMatrix <- function(){      #return the matrix value
    matCache
  }
  
  getInverse <- function(){     #return the value of the inverse
    invCache
  }
  
  #Create the setting functions:  
  setMatrix <- function(aMatrix){     #pass some invertible matrix and store in parent environment, clear past invCache values
    matCache <<- aMatrix
    invCache <<- NULL
  }
  
  setInverse <- function(inverse){    #pass matrix inverse as argument and store in the parent environment
    invCache <<- inverse
  }
  
  #Create the retrieving functions: 
  getMatrix <- function(){      #return the matrix value
    matCache
  }
  
  getInverse <- function(){     #return the value of the inverse
    invCache
  }
  
  
  #Create a list in order to use the function after execution:
  myFunctions <- list(getMatrix = getMatrix, getInverse = getInverse, setMatrix = setMatrix, setInverse = setInverse)
  

}


## This function first calls the getInverse function using $ to index and, if the cached inverse object is
## not empty, it retrieves the value from makeCacheMatrix(). If empty it calculates the inverse using solve()
## and returns the matrix inverse.

##  **NOTE**  Some results may be the scalar multiple of the inverse

cacheSolve <- function(matCache, ...) {
  #Call inverse retrieving function if it has a value stored:
  invCache <- matCache$getInverse()
  if (!is.null(invCache)){
    message('Getting cached data')
    return(invCache)
  }
  
  #If the value of invCache is empty, calculate it and return the value:
  data <- matCache$getMatrix()
  invCache <- solve(data, ...)
  matCache$setInverse(invCache)
  invCache
       
}
