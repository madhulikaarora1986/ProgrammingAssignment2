# This function creates a special "matrix" object that can cache its inverse

# Argument to this function is a matrix
makeCacheMatrix <- function(Input.Matrix = matrix())  
                  {
                    
                    # The first time this func is run for a given matrix input, then it will 
                    # initialize the output of the function to Null
                    Inverse.Matrix <- NULL
                    
                    # If you want to pass a matrix using a function call
                    set <- function(InputMat) 
                      {
                        Input.Matrix <<- InputMat  # Assigning the input variable
                        Inverse.Matrix <<- NULL    # Setting m is NULL in global env
                      }
                    
                    #get returns the inputted matrix to makeCacheMatrix
                    get <- function() Input.Matrix       
                    
                    # <<- used so that it can be accessed by getInverseMat function
                    setInverseMat <- function(Inversed) Inverse.Matrix <<- Inversed
                    
                    # No argument, func to read a value
                    getInverseMat <- function() Inverse.Matrix 
                    
                    # A list object is created that is an output of makeCacheMatrix function
                    # Elements of the list are function definition themselves
                    list( set=set
                         , get = get
                         , setInverseMat = setInverseMat
                         , getInverseMat = getInverseMat)
                  }


# This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
# If the inverse has already been calculated (and the matrix has not changed), 
# then the cachesolve should retrieve the inverse from the cache


#Input to this function is a list with function as objects whiz is the output of makeCacheMatrix function
cacheSolve <- function(Out.makeCacheMatrix)
                  {
                    # Whether inverse of matrix is in cache or not, we have to assign it to an object
                    Inverse.Matrix <- Out.makeCacheMatrix$getInverseMat() 
                                        
                    # If the inverse of Matrix exist already then if condition should be true
                    if(!is.null(Inverse.Matrix)) 
                      {
                            message("Getting cached inversed matrix")
                            return(Inverse.Matrix)
                      }
                    
                    # Get input matrix from global env and then assign it here locally
                    Input.Matrix <- Out.makeCacheMatrix$get()
                    
                    # Calculation inverse if not already done that and setting the output data object value
                    Inverse.Matrix <- solve(Input.Matrix)
                    message("Calculated first time and not cached!")
                    
                    # Also want to set the inverse mat in the global env for future calls which can refer cache
                    Out.makeCacheMatrix$setInverseMat(Inverse.Matrix)
                    
                    # Simply Outputting
                    Inverse.Matrix
                  }

# ## Lets test!!
# trace()
# exam1<-makeCacheMatrix(matrix(c(32,54,65,57),2,2))
# cacheSolve(exam1)

