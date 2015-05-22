## Assignment 2 (R Programming): Caching the Inverse of a Matrix

## makeCacheMatrix (x = matrix())
## This function creates a special matrix object that can cache its inverse.

## Paramaters of the function :  
##  - x , which is a square invertible matrix
## Function returns a list containing four functions as following: 
##	- setMatrix  : sets the matrix data
##	- getMatrix  : gets the matrix data
##	- setInverse : solves the matric inverse
##	- getInverse : gets the matric inverse

makeCacheMatrix <- function(x = matrix()) {

  ## create a NULL object
  mtx <- NULL
  
  ## four get and set functions described above.
  ## ------------------------------------------
  
  setMatrix <- function(y) { 
    
  ## <<- assignment operator cause a search to made through parent environments.
  ## this code puts incomming matrix in 'x' and clears earlier cached solution, if any
  ## setMatrix function is not currently being used in cacheSolve() or testFunction() 
       x <<- y
     mtx <<- NULL
  }
  
  getMatrix <- function() { x }
  
  setInverse <- function(slv) { mtx <<- slv }
                           
  getInverse <- function() { mtx }
  
  ## ------------------------------------------
  
  ## function return a list of functions  
  list( setMatrix  = setMatrix,   getMatrix = getMatrix, 
        setInverse = setInverse, getInverse = getInverse)
  
}

## cacheSolve <- function(x, ...)
## This function computes the inverse of the special "matrix" 
## returned by the makeCacheMatrix function above.

## Paramaters of function :  
##  - x, a cachable matrix object created in makeCacheMatrix function
## Function returns inverse of a matrix

cacheSolve <- function(x, ...) {
  
  ## try to obtain inverse of matrix already cached
    tempMatrix <- x$getInverse()
      
  ## if matrix inverse is not null, i.e. solution is cached, give message and return the matrix
  ## no need to go further 
  if(!is.null(tempMatrix)) {
      
      message("Getting cached inverse matrix")
      return(tempMatrix)
      
    }
  
  ## otherwise, get matrix data, solve it and return the inverted matrix
  ## solve() is a generic function, which can be used to get inverse of a square invertible matrix. 
   
    tempData <- x$getMatrix()
  
    message("Solving the inverse matrix")
  
    tempMatrix <- solve(tempData, ...)
  
  ## set the result in cache
    x$setInverse(tempMatrix)
  
  ## Return a matrix that is the inverse of 'x'
    tempMatrix
  
}

## testFunction()
## tests the functionality of above two functions: makeCacheMatrix and cacheSolve

testFunction <- function() {
  
  ## create a small square invertible matrix 
  mat <- matrix(1:4, nrow = 2, ncol = 2)
  
  ## for a bigger matrix, you can use something like: mat <- matrix(rnorm(100*100),100,100)
     
  ## solve the matrix two times, first run will solve the matrix using solve() function and cache the result
 
  message("First run without using cached object")
 
  test <- makeCacheMatrix(mat)
  print(cacheSolve(test))  
 
  ## when the same function is called second time, it uses the cache instead of solvinging it again.
  message("Second run using cached object")
 
  cacheSolve(test)
 
}
