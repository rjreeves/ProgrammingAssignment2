## If the contents of a matrix are not changing, it makes sense to cache the value of the transpose
## so that when needed again, it can be looked up in the cache rather than recomputed.

## This function creates a special matrix with functions to set/get a matrix and set/get the transposed-matrix   

makeCacheMatrix <- function(x = matrix()) {
  trn <- NULL
  set <- function(y) {
    x <<- y
    trn <<- NULL
  }
  get <- function() x
  settrn <- function(tranpose) trn <<- tranpose
  gettrn <- function() trn
  list(set = set, get = get,
       settrn = settrn,
       gettrn = gettrn)
}


## This function "calculates" transpose of a matrix and cache the result. 
## If already cached simply provide the cached result, avoidng a re-calc.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  trn<- x$gettrn()
  if (!is.null(trn)) {
      message("From the cache")     # Advise the cahce provided answer
      return(trn)
  }
  data<-x$get()
  trn<- t(data,...)
  x$settrn(trn)
  trn
}
