##  Function stores a matrix and cache's its inverse.
##  If there is no cached matrix, then inverse of a matrix is computed.

## A function creates a list containing a function to set and get a value of the matrix, set and get the value of inverse matrix. 

makeCacheMatrix <- function(x = matrix()) {
  mati <- NULL                            
  set <- function(y){
    x <<- y                               
    mati <<- NULL                         
  }                                     
  get <- function() x                   
  setmati <- function(solve) mati <<- solve
  getmati <- function() mati              
  list(set = set, get = get,              
       setmati = setmati,
       getmati = getmati)
}


## Returns a matrix that is inverse of x (created with the above function) - cached or calculated.

cacheSolve <- function(x, ...) {          
  mati <- x$getmati()                     
  if(!is.null(mati)) {                    
    message("getting cached data")
    return(mati)                          
  }
  data <-x$get()                          
  mati.calc <- solve(data,...)
  x$setmati(mati.calc)
  mati.calc                             
}
