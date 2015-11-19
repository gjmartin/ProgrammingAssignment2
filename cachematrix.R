## Put comments here that give an overall description of what your
## functions do

## The goal here is to use solve() to find the inverse of a matrix and cache
## it using a free floating variable. 

## makeCacheMatrix creates a special matrix which can cache the values of the inverse. 
## cacheSolve checks to see if theinverse of the matrix exists, 
## if it does it returns the inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
  m<-NULL
  set<-function(y){
    x<<-y
    m<<-NULL
  }
  get<-function() x
  setmatrix<-function(solve) m<<- solve
  getmatrix<-function() m
  list(set=set, get=get,
       setmatrix=setmatrix,
       getmatrix=getmatrix)
}

## This function computes the inverse of the special "matrix" 
## returned by  makeCacheMatrix  above. If the inverse has 
## already been calculated (and the matrix has not changed), then 
## cacheSolve  should retrieve the inverse from the cache.

cacheSolve <- function(x=matrix(), ...) {
  m<-x$getmatrix()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  matrix<-x$get()
  m<-solve(matrix, ...)
  x$setmatrix(m)
  m
}
  ## Return a matrix that is the inverse of 'x'

## I used the following commands:

## z <- makeCacheMatrix()
## z$set(matrix(1:4,2,2))


