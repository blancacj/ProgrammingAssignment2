### Introduction

This script is able to cache potentially time-consuming computations.
For example, we can cache the inverse of a matrix. 

### Example: Caching the inverse of a matrix


The first function, `makeCacheMatrix` creates a special "matrix", which is
really a list containing a function to

1.  set the value of the matrix
2.  get the value of the matrix
3.  set the value of the inverse
4.  get the value of the inverse

<!-- -->



The second function `cacheSolve` calculates the inverse of the special "matrix"
created with the above function. However, it first checks to see if the
inverse has already been calculated. If so, it `get` the inverse from the
cache and skips the computation. Otherwise, it calculates the inverse of
the data and sets the value of the inverse in the cache via the `setinverse`
function.



