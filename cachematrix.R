
# caching the inverse of a matrix rather than computing it repeatedly using solve() function.
# assuming that the matrix supplied is always invertible.

# makeCacheMatrix function (x=matrix) creates a matrix object that can cache its inverse.

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


# cachesolve function computes the inverse of the matrix returned by makeCacheMatrix.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

    m<-x$getmatrix()
    if(!is.null(m)){
        message("getting cached inverse")
        return(m)
    }
    matrix<-x$get()
    m<-solve(matrix, ...)
    x$setmatrix(m)
  m
}