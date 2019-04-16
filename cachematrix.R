## R Programming Week 3 Assignment: Lexical Scoping 


##Takes a matrix, creates an environment (stored in memory) that contains matrix and cache values, 
##along with four functions that are returned to a variable in a list format
##ex. MyMatrix <- makeCacheMatrix(MatrixA)
makeCacheMatrix <- function(x = matrix()) {
  
  #Preset cache value to NULL any time a new matrix is introduced
  i = NULL
  
  #Setter; used to change the matrix, which will cause the cache value to empty
  set<- function(y){
    x <<-y
    i <<- NULL
  }
  
  #Retrieves matrix value; utilized  in cacheSolve() when solving the matrix
  get <- function() x
  
  #Sets the cache value, i (from parent enviro.), to the calculated inverse matrix
  setinverse <- function(inv) i <<- inv #inv stands for inverse
  
  #Retrieves cache value
  getinverse <- function() i
  
  #Returns a list of four functions that will be assigned to a variable; ex MyMatrix
  invisible(list(get = get, set = set, setinverse = setinverse, getinverse = getinverse))
}


##Compute the inverse of the special matrix returned by makeCacheMatrix above.
##If inverse has been created already,then retrieve the inverese from the cache
cacheSolve <- function(x, ...) {
  
  #Retrieve the currently existing cache value
  i <- x$getinverse()
  
  # If cache value, i, is not NULL the we can just return a stored value
  if(!is.null(i)) {
    
    #Simply return the cached value, along with a print message to detect this process
    message("getting cached data")
    return(i)
  }
  
  #In the case the i = NULL, we retrieve the matrix value stored in MyMatrix
  data <- x$get()
  
  #We solve the matrix that was obtained via x$get 
  i <- solve(data, ...)
  
  #Send the solved matrix to MyMatrix, where it will become the new cache value
  #If we try and solve the same matrix, it won't be NULL anymore and will return this cache value
  x$setinverse(i)
  
  #Finally return the solved matrix
  i
}
