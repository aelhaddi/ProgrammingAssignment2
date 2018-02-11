
#  --eh Feb 11 2018
#  Get the inverse of a square matrix from cache, if not available compute it and cache it
#
#

#  setter and getter functions to compute matrix inverse.
#  x must be a square matrix

makeCacheMatrix <- function(x = matrix()) {
  # Creates a square matrix and provides setters and getters for the matrix inverse.
  #
  # Args:
  #   x: the square matrix
  # Returns:
  #   a list to set a matrix, get the inverse of the matrix from cache and to unset the matrix
  #
  
  mat_inverse <- NULL
  
  setmat <- function(m_in) {
    #  First test if this is a square matrix before we use it.
    if (!is.null(m_in)) {
        dim1  <- dim(x)
        if (dim1[1] != dim1[2]) {
          mat
          stop("Matrix is not a square matrix")
        }
    }
        
    x <<- m_in

	# This will mark matrix changed and force it to recompute on a get.
    # Client must test for null mat and force recompute. 
    mat_inverse <<- NULL
  }
  
  
  getmat <- function() {
    x
  } 

  setminverse <- function(inv) {
    mat_inverse <<- inv
  }
  
  unsetmat <- function() {
    mat_inverse <<- NULL
  }
  
  getminverse <- function() {
    mat_inverse
  }
  
  list(setmat = setmat, getmat = getmat, setminverse = setminverse, getminverse = getminverse)
}



cacheSolve <- function(x, ...) {
  # Return a matrix that is the inverse of 'x'
  # Args: 
  #   x: an object created by makeCacheMatrix
  # Returns:
  #   The inverse of the matrix
  
  minv <- x$getminverse()
  if (!is.null(minv)) {
    message("Returning inverse from cache")
    return(minv)
  }
  
  #  Did we really makeCacheMatrix
  mat <- x$getmat()
  if (is.null(mat)) {
      stop("No matrix was set. You forgot to call setmat first.")
  }
  
  
  minv <- solve(mat, ...)
  
  #  cache the inverse matrix
  x$setminverse(minv)
  minv
}


testinverse <- function(size=100) {
# Test driver to generate random matrices, compute the cache and get the cache. Compute the gains from using the cache.
# Args:
#   size : row and column size. Limit size to 2000 (The compute will take for ever for size  > 3000)
# Return:
#   None.
  

	# Generate  new random matrix
    m <- matrix(rnorm(n=size*size), nrow=size, ncol=size)
    mc <- makeCacheMatrix(m)
    
    t1 <- as.double(sprintf("%.6f", as.numeric(Sys.time())*1000))

    #  New matrix, we will be forced to compute.
    mi <- cacheSolve(mc)

    t2 <- as.double(sprintf("%.6f", as.numeric(Sys.time())*1000))
    #t2 <- print(as.numeric(Sys.time())*1000, digits=15)
    dt1 = t2-t1
    out <- sprintf("Size: %6d x %6d Computing the inverse  took:  %.6f msec", size, size, dt1)
    print(out)
    
    t1 <- as.double(sprintf("%.6f", as.numeric(Sys.time())*1000))
    #t1 <- print(as.numeric(Sys.time())*1000, digits=15)

    #  This time we have the inverse in the cache. So we will simply return the cached value
    mi<- cacheSolve(mc)

    t2 <- as.double(sprintf("%.6f", as.numeric(Sys.time())*1000))
    #t2 <- print(as.numeric(Sys.time())*1000, digits=15)
    dt2 =  t2 - t1
    out <- sprintf("Size: %6d x %6d get inverse mat from cache in %.6f msec, gained %6f msec ", size, size, dt2, dt1-dt2)
    print(out)
    
}


#
# small matrix, small gain from the cache
#
testinverse(10)
testinverse(100)
testinverse(200)
testinverse(500)
#
# Large matrices show huge gains
#
testinverse(1000)
testinverse(2000)


