
#The first function, makeCacheMatrix creates a special "matrix", which is really a list containing a function to

#1. set the value of the vector
#2. get the value of the vector
#3. set the value of the mean
#4. get the value of the mean

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
  
}

#The second function calculates the inverse of the special "matrix" created with the above function.

cacheSolve <- function(x, ...) {
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m
}


#That is an example to check above functions

a1 <- c(3, 2, 5) 
a2 <- c(2, 3, 2) 
a3 <- c(5, 2, 4)
A <- rbind(a1, a2, a3) 


a = makeCacheMatrix(A)
cacheSolve(a)

