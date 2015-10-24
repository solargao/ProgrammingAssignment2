## cache the inverse of a matrix

## 'makeCacheMatrix'creates a special "matrix" object
## that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
     s <- NULL
     set <- function(y){
          x <<- y
          s <<- NULL
     }
     get <- function() x
     setsolve <- function(solve) s <<- solve
     getsolve <- function() s
     list(set = set, get = get,
          setsolve = setsolve,
          getsolve = getsolve)
}


## 'cacheSolve'computes the inverse of the special "matrix" 
##  returned by `makeCacheMatrix` above. If the inverse has 
##  already been calculated (and the matrix has not changed), 
##  then 'cacheSolve' should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		     s <- x$getsolve()
     if(!is.null(s)){
          message("getting cached data")
          return(s)
     }
     data <- x$get()
     dim_data = dim(data)
     if(is.null(dim_data)){
          message("not a matrix!")
     }else if(dim_data[1]!=dim_data[2] || !det(data)){
          message("input matrix is irreversible!")
     }else {
          s <- solve(data, ...)     
          x$setsolve(s)
          return(s)
     }
}
