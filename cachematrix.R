## Assignment 2 (R Programming): Caching the Inverse of a Matrix

## makeCacheMatrix (x = matrix())
## This function creates a special "matrix" object that can cache its inverse.

## Paramaters of the function :  
##  - x , which is a square invertible matrix

## Function returns a list of four functions as following: 
##
##	- setmatrix  : sets the matrix data
##	- getmatrix  : gets the matrix data
##	- setinverse : solves the matric inverse
##	- getinverse : gets the matric inverse

makeCacheMatrix <- function(x = matrix()) {

  m <- NULL
  
  ## Four get and set functions mentioned above.
  ## ------------------------------------------
  
  setmatrix <- function(y) { 
    x <<- y
    m <<- NULL
  }
  
  getmatrix <- function() x
  
  # solve() is a generic function, which can be used to get inverse of a square invertible matrix.
  
  setinverse <- function(solve) m <<- solve
  
  getinverse <- function() m
  
  ## ------------------------------------------
  
  ## function return a list of functions
  
  list(setmatrix  = setmatrix,   getmatrix = getmatrix, 
       setinverse = setinverse, getinverse = getinverse)
  
}

## cacheSolve <- function(x, ...)
## This function computes the inverse of the special "matrix" 
## returned by the makeCacheMatrix function above.

## Paramaters of function :  
##  - x, a cachable matrix object created in makeCacheMatrix function

## Function returns inverse of a matrix

cacheSolve <- function(x, ...) {
  
  ## Obtain the inverse of matrix
  
    m <- x$getinverse()
  
  ## if matrix inverse is not null, give message and return the matrix
    if(!is.null(m)) {
      
      message("Getting cached inverse matrix")
      return(m)
    }
  
  ## otherwise, get matrix data, solve it and return the inverted matrix
    
    data <- x$getmatrix()
  
    m <- solve(data, ...)
  
    x$setinverse(m)
  
    message("Solving the inverse matrix")
    
  ## Return a matrix that is the inverse of 'x'
    m
  
}

## testFunction()
## tests the functionality of above two functions: makeCacheMatrix and cacheSolve
testFunction <- function() {
  
  ## creates a small square invertible matrix 
   mat <- matrix(1:4, 2, 2)
   
    
 ## solve the matrix two times, first run will solve the matrix and cache the result
 
 ## creates the cacheable matrix object using solve() function
 
 message("First run without using cached object")
 
 test <- makeCacheMatrix(mat)
 print(cacheSolve(test))  
 
 ## when the same function is run second time, it uses the cache instead of solvinging it again.
 
  message("Second run using cached object")
 
  cacheSolve(test)
 
}
