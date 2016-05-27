## JLF - March 2016 
## Store in cache the Inverse of a matrix to make available later on


makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  inv <- NULL
  
  ## Define different methods to play with a matrix
  ## @x: a square invertible matrix
  ## return: a list containing functions to
  ##              1. set the matrix
  ##              2. get the matrix
  ##              3. set the inverse
  ##              4. get the inverse
  ##         this list is used as the input to cacheSolve()
  
  ## Store the matrix in the cache
  
  setMatrix <- function(matrix = matrix()){
    
    # use `<<-` to assign a value to an object in an environment 
    # different from the current environment. 
    
    x <<- matrix
    inverse <- NULL
  }
  
  ## Get the matrix from the cache
    
  getMatrix <- function() x
  
   ## Store the Inverse matrix in the cache
  
  setInverse <- function(inverseMatrix = matrix()){ 
    inverse <<- inverseMatrix
  } 
  
  ## Get the inverse matrix from the cache
  
  getInverse <- function() inverse
  
  ## Assign the method to the function
  
  list(get = getMatrix, set = setMatrix, getInverse = getInverse, setInverse = setInverse)
}


## checks if the given makeCacheMatrix object already has it's inverse computed.
## Otherwise, it calculates the inverse Matrix and caches it

cacheSolve <- function(x, ...) {
  ## @x: output of makeCacheMatrix()
  ## return: inverse of the original matrix input to makeCacheMatrix()
  
  inv = x$getInverse()
  
  if(is.null(inv))  {
    # inverse matrix not existing, calculates the inverse 
    print("Inverse matrix does not exists in cache. Computing...") 
    inv <- solve(x$get())
    # sets the value of the inverse in the cache via the setInverse function.
    x$setInverse(inv)
    print("Inverse matrix computed and cached.") 
    } 
  else {
    # get it from the cache and skips the computation.
    print("Inverse matrix from cache")
    }
  
  return(inv)
}

## Data to play with and to test
## matrixplay <- matrix(data = c(4,2,7,8), nrow = 2, ncol = 2)
## matrixplay2 <- makeCacheMatrix(matrixplay)
## Not cached
## cacheSolve(matrixplay2)
## cached 
## cacheSolve(matrixplay2)
## Result should be the identitiy matrix
## matrixplay  %*% cacheSolve(matrixplay2)

