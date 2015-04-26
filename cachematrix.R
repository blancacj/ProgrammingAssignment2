
## The first function,makeCacheMatrix creates a special "matrix", which is really a list containing a function to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the inverse of the matrix
## 4. get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
## inv is used to save the inverse of a matrix
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <-function() x
  setinverse <-function(inverse) inv<<-inverse
  getinverse <-function() inv
  list(set=set,get=get,
       setinverse=setinverse,
       getinverse=getinverse)
}


## The second function, cacheSolve calculates the inverse of the special "matrix" created with the first function. 
## However, it first checks to see if the inverse has alreasy been calculated. If so, it gets the inverse from the 
## cache and skips the computation. Otherwise, it calculates the inverse of the data and sets the value of the 
## inverse int the cache iva the setinverse function 

cacheSolve <- function(x, ...) {
## check if the inverse of the matrix is already created
  inv<-x$getinverse()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  ## if the inverse of the matrix has not been calcualted, then calculate the inverse of the matrix
  data<-x$get()
  inv<-solve(data,...)
  x$setinverse(inv)
}
