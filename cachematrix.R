## R Programming Assignment #2
## by FONG KUM PIEW

## Contains functions:

## Function [1]: makeCacheMatrix(x= matrix)
## Argument: matrix object
## Although no checks are conducted because assignment assumes
## that all matrix supplied is always invertable,
## inverse can be calculated from square matrix (where nrow == ncol)
## Returns: None

## Function [2]: cacheSolve(x, ...)
## Argument: A makeCacheMatrix instance
## Returns: Inverse value of matrix

## makeCacheMatrix
## list of functions for setting / getting the value of a matrix
## and also setting / getting the value of the inverse matrix
## calculated and returned using cacheSolve function

makeCacheMatrix <- function(x = matrix()) {

  ##set inverse
  inverse <- NULL
  
  ## setting the value of matrix
  ## this functionis not very useful since
  ## matrix is already passed to function as a parameter
  setMatrix <- function(y){
      x<<- y
      inverse <<- NULL
  }
  
  ##getting the matrix
  getMatrix <- function(){
    x
  }
  
  ##setting the inverse value
  ## value is received from cacheSolve()
  setInverse <- function(inv)
  {
    inverse <<- inv
  }
  
  ##get the inverse value
  getInverse <- function()
  {
    inverse
  }
  
  
  list(setMatrix = setMatrix, getMatrix=getMatrix, setInverse=setInverse, getInverse=getInverse)
}


## cacheSolve 
## first check if the matrix has been cached in the memory
## if already cached, it will return the inverse from memory
## if not cached, it will calculate the inverse of matrix and
## return the value

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  ##loading matrix from memory
  inverse <- x$getInverse()
  
  ## checking if the matrix has been cached. 
  ## If so (i.e. if inverse is not null), 
  ## inverse value cached in memory will be returned 
  if (!is.null(inverse)){
      message("Setting value from cached data")
      return(inverse)
  }
  
  ## loading matrix from inverse value in x
  data <- x$getMatrix()

  ## calculating the inverse using solve()
  x$setInverse(solve(data,...))
  
  ## return and print the inverse matrix
  inverse
}
