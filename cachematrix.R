## These functions make a cache matrix and generates function to access it including computing its inverse
## if the inverse already exists, the cached value is retrieved; else it is generated and store in cache


## this function takes in a matrix and defines the functions used to get, set, 
## compute the inverse and store its value in a cache


makeCacheMatrix <- function(x = matrix()) 
{
  i <- NULL                               ## reset 
  set <- function(y) 
  {                                      ## set function definition
    x <<- y                              ##store matrix in global environment
    i <<- NULL                           ## reset global inverse matrix storage element
  }
  get <- function() x                    ## define function for getting the matrix element
  setinverse <- function(inv) i <<- inv   ## define function for computing the inverse
  getinverse <- function() i              ## define function for retrieving the inverse
  list(set = set, get = get,              ## define list variable that returns the functions defined
       setinverse = setinverse,
       getinverse = getinverse)
  

}



##this function checks if the computed inverse matrix was previously 
##computed and store in cache, if it is return it
## if not compute it and store in cache and return the value computede

cacheSolve <- function(x, ...) 
{
  
  i <- x$getinverse()               ## get the inverse vector variable
  if(!is.null(i)) 
  {                                 ## if null, compute and store in cache and return computed value, else retrieve the cached data and return
    message("getting cached data")
  return(i)
  
  }

 }
  data <- x$get()        ## get matrix and compute its inverse, return it and store it in the cache
  i <- solve(data, ...)  ## compute inverse
  x$setinverse(i)         ## store in cache
  i                       ## return inverse