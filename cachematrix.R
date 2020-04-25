# This script contains a couple of functions for performing efficient caching of matrix inverse. 

## It is for the week3 assignment of the R programming course by JHU@Coursera.

## This function defines functions for caching of the matrix and its inverse
## and returns a list of these functions. 

makeCacheMatrix <- function(x = matrix()){
  inv <- NULL
  set <- function(y){
    # this function sets the matrix
    x <<- y
    # set inverse to NULL
    inv <<- NULL
  }
  # get the matrix
  get <- function() x
  # set and get the inverse
  setinv <- function(invmat){
    inv <<- invmat    # cache the inverse
  } 
  getinv <- function() inv
  list(set = set, get = get, setinverse = setinv, getinverse = getinv)
}

## This function takes the cacheSolve object as 'x' and a matrix 'matr'
## on which the inverse should be computed. 
## It then determines if the inverse is computed and the matrix is the same.
## If yes, then it returns the cached inverse. Otherwise it computes the inverse.

cacheSolve <- function(x,matr,...){
  inv <- x$getinverse()
  cachedmatr <- x$get()
  if((!is.null(inv))&&(matr==cachedmatr)){
    message("getting cached data")
    return(inv)
  }
  else{
    message("calculating the inverse")
    # calculate the inverse
    inv <- solve(matr)
    # set the inverse cache
    #x$setinverse(inv)
    # set the matrix cache
    #x$set(matr)
    # returns the inverse 
    return(inv)
  }
}
