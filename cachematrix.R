## The following functions create a special matrix object and calculate its inverse and cache the inverse of the matrix

## The first function makeCacheMatrix() creates a special matrix object and returns a list of functions that can be used to 
## set the the matrix, get the matrix, set the inverse of the matrix and get the inverse of the matrix 
## 


makeCacheMatrix <- function(m1 = matrix()) {
        
	inv <- NULL
        
	set <- function(m2) {
                
		m1 <<- m2
                
		inv <<- NULL
        
	}
        
	get <- function() m1
        
	setinv <- function(invrse)inv <<- invrse
        
	getinv <- function() inv        
	list(set = set, get = get,
 setinv = setinv,
 getinv = getinv)

}



## This function takes the matrix created by the makeCahceMatrix function and checks to see if the
## inverse of the matrix is already cached. if it is in the cache, it retrieves it and returns it.
## if it is not in the cache, it calculates the inverse of the matrix, caches it and returns the inverse.
## if the matrix cannot be inverted (e.g singular matrices), it returns an error message. 



cacheSolve <- function(m1) {
        
		inv <- m1$getinv()
        
		if(!is.null(inv)) {
                
			message("retrieved matrix inverse from cache")
                
			return(inv)
        
		}
        
		m2 <- m1$get()
        
		inv <- try(solve(m2), silent=T)

		# Check and see if solve() returned a matrix
		# solve will return error if matrix passed is a singular matrix which is not invertible 
        	if(class(inv) != "matrix")
		{
			message("matrix is not invertible")
                
			return(NULL)
   
		}