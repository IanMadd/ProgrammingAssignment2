## makeCacheMatrix and cacheSolve calculate the inverse of a matrix. If the 
## inverse of the matrix has aleady been calculated then cacheSolve will return 
## the already computed inverse.

## makeCacheMatrix creates a list of several elements. 

makeCacheMatrix <-function(x){
  mat <- NULL
  set <- function(y) {
    x <<- y
    mat <<- NULL
  }
  get <- function() x
  setinv <- function(solve) mat <<- solve
  getinv <- function() mat
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

## cacheSolve checks to see if an inverse matrix has already been calculated. 
## If it has then it returns the already calculated value. If it hasn't it 
## calculates the inverse of the matrix stored in the makeCacheMatrix function 
## and then prints and stores this value in the list. 

cacheSolve <- function(x, ...) {
 
  mat <- x$getinv()
  if(!is.null(mat)) {
    message("getting cached data")
    return(mat)
  }
  data <- x$get()
  mat <- solve(data, ...)
  x$setinv(mat)
  mat
}


