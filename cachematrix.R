##
## Package of functions that enhance a matrix to cache its own inverse.
##


##
## Make a wrapped matrix object that can cache its own inverse matrix.
##
## Parameters:
##   sourceMatrix - The source matrix whose inverse will be cached. Default is
##     empty.
##
## Returns:
##   A wrapped matrix object capable of caching its inverse matrix.
##   The wrapped matrix can be accessed via related functions like 'cacheSolve'.
##
makeCacheMatrix <- function(sourceMatrix = matrix()) {
    
    ##
    ## Parent environment
    ## 
    
    # The cached inverse matrix of the source matrix. Default is NULL.
    inverseMatrix <- NULL
    
    ##
    ## Functions
    ##
    
    # 
    # Set the source matrix, and hence invalidate its cached inverse.
    #
    # Parameters:
    #   sourceMatrix - New source matrix.
    # 
    # Returns:
    #   Nothing
    # 
    setSourceMatrix <- function(sourceMatrix) {
        
        # Store source matrix in parent environment.
        # NOTE: Same name can be used, since <<- operator is smart about its
        #  left-hand side vs. right-hand side.
        sourceMatrix <<- sourceMatrix
        
        # Invalidate cached inverse in parent environment.
        inverseMatrix <<- NULL
    }
    
    # 
    # Get the source matrix.
    #
    # Parameters:
    #   None
    # 
    # Returns:
    #   The source matrix of this wrapper object.
    # 
    getSourceMatrix <- function() sourceMatrix
    
    # 
    # Set the inverse matrix.
    #
    # Parameters:
    #   inverseMatrix - New inverse matrix. The caller must ensure that this
    #     inverse is consistent with the source matrix.
    # 
    # Returns:
    #   Nothing
    # 
    setInverseMatrix <- function(inverseMatrix) inverseMatrix <<- inverseMatrix
    
    # 
    # Get the inverse matrix, if already calculated.
    # If not already calculated, use the 'cacheSolve' function.
    #
    # Parameters:
    #   None
    # 
    # Returns:
    #   The inverse matrix wrapped in this object. Possibly NULL.
    # 
    getInverseMatrix <- function() inverseMatrix
    
    ##
    ## Return wrapped matrix object including accessor functions.
    ##
    
    list(setSourceMatrix = setSourceMatrix,
         getSourceMatrix = getSourceMatrix,
         setInverseMatrix = setInverseMatrix,
         getInverseMatrix = getInverseMatrix)
}


##
## Provide the inverse matrix of the given wrapped matrix object.
##
## Parameters:
##   cacheMatrix - A wrapped matrix made by 'makeCacheMatrix'.
##   ... - Additional parameters to pass to the inverse calculation ("solve").
##
## Returns:
##   A matrix that is the inverse of the source matrix in 'cacheMatrix'.
##   For performance, if the source matrix has not changed since the last
##   inverse calculation, the last calculated inverse is re-used.
##
cacheSolve <- function(cacheMatrix, ...) {
    
    # If cached in cacheMatrix, return the inverse directly.
    inverseMatrix <- cacheMatrix$getInverseMatrix()
    if(!is.null(inverseMatrix)) {
        message("getting cached inverse")
        return(inverseMatrix)
    }
    
    # The inverse is _not_ cached.
    # Calculate ("solve"), cache, and return the inverse of the source matrix.
    sourceMatrix <- cacheMatrix$getSourceMatrix()
    inverseMatrix <- solve(sourceMatrix, ...)
    cacheMatrix$setInverseMatrix(inverseMatrix)
    inverseMatrix
}
