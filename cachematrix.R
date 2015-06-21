## This function creates a special matrix and then calculates its inverse. 
##  If the inverse exists in the cache, then it gets the inverse of the matrix from the cache. 

## makeCacheMAtrix creates a special matrix with a getter, a setter and functions to assign and 
##get inverse functions for the special matrix.

makeCacheMatrix <- function(x = matrix()) {
  inv_m <- NULL
  #assigns the matrix to special matrix
  set <- function(y) {
    x <<- y
    inv_m <<- NULL
  }
  
  #defines the get,setInv and getInv functions. 
  get <- function() x 
  #get obtains the matrix
  setInv <- function(inv) inv_m <<- inv
  #setInv assigns inv to the special matrix's inverse
  getInv <- function() inv_m
  #getInv obtains special matrix's inverse
  #return the set,get,setInv and getInv functions to the object
  list(set = set, get = get, 
       setInv = setInv,
       getInv = getInv)
}


## CacheSolve checks to see if the special matrix has an inverse already. If so
## it return the cached inverse, otherwise it calculates an inverse, 
## assigns it and returns the inverse. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  m_inv <- x$getInv()
  #check if already in cache
  if(!is.null(m_inv)) {
    message("getting cached data")
    return(m_inv)
  }
  #calculate and assign inverse if not in cache
  data <- x$get()
  m_inv<-solve(data)
  x$setInv(m_inv)
  #returns the inverse
  m_inv
}
