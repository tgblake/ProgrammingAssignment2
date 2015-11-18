## Put comments here that give an overall description of what your
## functions do.

# makeCacheMatrix stores a value of the inverse of an input matrix, and controls a
#   flag that determines whether there is a valid cached inverse or whether the 
#   inverse must be recalculated.
# cacheSolve returns the function to do the inverse, or null if there is a valid 
#   cached value.  cacheSolve uses makeCacheMatrix to determine which.

## Write a short comment describing this function
# Creates list of functions (to be called by x$func) to set and get input 
# matrix x and to set and get the inverse of x, and to reset flag m

makeCacheMatrix <- function(x = matrix()) {
      m <- NULL               # m is a flag: NULL if cache value is not valid.
      set <- function(y) {    # Function set changes value of input matrix and
         if (!identical(y, x)) {   # resets flag m IF matrix is not the same as
            x <<- y           # previous one.  The x and m affected are in the 
            m <<- NULL        # makeCacheMatrix scope.
         }
      }
      get <- function() x     # Function get retrieves the value of the input
                              # matrix for which the inverse will be returned.
      setinv <- function(func) m <<- func   # Function setinv assigns the function
                                            # (solve) to calculate the inverse.
      getinv <- function() m    # Function getinv returns the function (solve, ie.
                                # inverse) to be applied to the last-entered matrix.
      list(set = set, get = get,     # Returning the list of functions to be used to
           setinv = setinv,          # operate the makeCacheMatrix() function (to
           getinv = getinv)          # be called as x$set(), etc., after 
}                                    # x <- makeCacheMatrix(input) has been called.


## Write a short comment describing this function
## Return inverse of a matrix x; get cache value if input matrix has not been 
## changed by x$set() since cacheSolve was last called.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      m <- x$getinv()      # Assign to m the function from makeCacheMatrix().
      if(!is.null(m)) {    # If m is not null, there is valid cached data.
            message("getting cached data")
            return(m)      # Here, m is the function that calculates the inverse.
      }
      data <- x$get()      # Need new calculation.
      m <- solve(data)   
      x$setinv(m)          # Apply function 'solve' to calculate inverse.
      m                    # Return function to compute inverse: 
}                          #     cacheSolve(x) becomes solve(x).

testcode <- function() {
      # Test and time your code (per student Nawal El Bagdady):
      #source("cachematrix.R")
      #
      # generate matrix, and the inverse of the matrix.
      size <- 2000 # size of the matrix edge, don't make this too big
      mymatrix <- matrix(rnorm(size^2), nrow=size, ncol=size)
      mymatrix.inverse <- solve(mymatrix)
      #
      # now solve the matrix via the cache-method
      #
      Rprof()
      special.matrix   <- makeCacheMatrix(mymatrix)
      #
      # this should take long, since it's the first go
      start1 <- Sys.time ()
      special.solved.1 <- cacheSolve(special.matrix)
      end1 = Sys.time () - start1
      #
      # this should be lightning fast
      start2 <- Sys.time ()
      special.solved.2 <- cacheSolve(special.matrix)
      end2 = Sys.time () - start2
      #
      print("Check if all solved matrices are identical; should return TRUE:")
      print(identical(mymatrix.inverse, special.solved.1) & identical(mymatrix.inverse, special.solved.2))
      #
      print("The execution times for each approach:")
      print(end1)
      print(end2)
      
      ## Track what happens when you modify the contents of the matrix:
      #First create a new matrix:
      mymatrix2 <- matrix(rnorm(size^2), nrow=size, ncol=size)
      mymatrix2.inverse <- solve(mymatrix2)
      
      #Then modify the contents of the the special matrix using the 'set' function: 
      special.matrix$set(mymatrix2)
      
      #Now use cacheSolve. You should NOT see 'getting cached data' in your console because the matrix contents are now modified:
      special.solved.3 = cacheSolve(special.matrix)
      
      print("This should return FALSE:") # because now you have a new inverse value:
      print(identical(mymatrix.inverse, special.solved.3))
      print("This should return TRUE:")
      print(identical(mymatrix2.inverse, special.solved.3))
      
      print("You should now see 'getting cached data':")
      special.solved.4 = cacheSolve(special.matrix)
      
      print("Cached inverse == original inverse; This should return TRUE:")
      print(identical(mymatrix2.inverse, special.solved.4))
      
      print("Track what happens when you copy the input matrix:")
      #First copy the last input matrix:
      mymatrix5 <- mymatrix2
      
      #Then re-set the contents of the special matrix to the same thing:
      special.matrix$set(mymatrix5)
      #
      # this should be lightning fast again:
      start2 <- Sys.time ()
      special.solved.5 <- cacheSolve(special.matrix)
      end2 = Sys.time () - start2
      print(end2)
      
      Rprof(NULL)
}