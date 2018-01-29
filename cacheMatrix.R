## Function makeCacheMatreix creates a special "matrix", which is a list containing a functions to
## 1. set the value of the vector
## 2. get the value of the vector
## 3. set the value of the mean
## 4. get the value of the mean

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
  
}

## Function cacheSolve computes the inverse of the special "matrix" created by makeCacheMatrix above. 
## If the inverse has already been calculated, it is retrieved from cache
## else it is created and stored in cache for repeat use

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  mat <- x$get()
  
  ## The inversion is done by the R library function solve. 
  ## The argument passed in can only be a square matrix. Error results otherwise!
  
  inv <- solve(mat, ...) 
  x$setInverse(inv)
  inv
}


# As Tested out on RStudio Console

#> my_matrix <- makeCacheMatrix(matrix(1:4, 2, 2))
# The first time since it not already cached it does the inverse and caches it
#> cacheSolve(my_matrix)
#  [,1] [,2]
#  [1,]   -2  1.5
#  [2,]    1 -0.5

# The second time it fetches it from cache
# > cacheSolve(my_matrix)
# getting cached data
# [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5

# The inversion is done by the R library function solve. 
# The argument passed in can only be a square matrix. Error results otherwise!
# > my_matrix2 <- makeCacheMatrix(matrix(1:6, 3, 2))
# > cacheSolve(my_matrix2)
# Show Traceback
# Rerun with Debug
# Error in solve.default(mat, ...) : 'a' (3 x 2) must be square 
