#makeCacheMatrix make it possible to invert the orginal matrix
#without changing the same.

# Write a matrix as cached object to the parent invronment
makeCacheMatrix <- function(orginalMatrix = matrix()) {
  #define and init variable to store inversed matrix
  cachedInverseMat <- NULL
  #setter function to move the matrix parameter to 
  #parent environment and init cached inversed variable
  set <- function(mat){
    orginalMatrix <<- mat
    cachedInverseMat <<- NULL
  }
  #getter to return the orginal matrix 
  get<-function() orginalMatrix
  #setter to store the inversed matrix cached to the parent environment
  setInverseMat <- function(inverseMat) cachedInverseMat <<- inverseMat
  #getter to return the cached inversed matrix
  getInverseMat <- function() cachedInverseMat
  #add function stubs to list 
  list (set = set, 
        get = get,
        getInverseMat = getInverseMat,
        setInverseMat = setInverseMat)
}


# Calculate the inversed matrix in cached the parent environment
# and return the inversed matrix of 'x'
cacheSolve <- function(orginalMatrix, ...) {
  #set the cached variable for the inversed matrix with the memory place of
  #the parent environment
  cachedInverseMat <- orginalMatrix$getInverseMat()
  #do inverting only if a matrix is not defined
  if (!is.null(cachedInverseMat)){
    message("inversed matrix is calculated and will be returned")
    return (cachedInverseMat)
  }
  #if no cachedInverseMat is calculated then calculate it
  #get cached orginal matrix
  matData <- orginalMatrix$get()
  # calculate inversed matrix on data of cached orginal matrix
  cachedInverseMat <- solve(matData)
  #set the solved matrix result to 
  orginalMatrix$setInverseMat(cachedInverseMat)
  # display solved matrix 
  cachedInverseMat
}

#Solution example
#> orginalMatrix <- rbind(c(2,1),c(3,2))
#> orginalMatrix
#[,1] [,2]
#[1,]    2    1
#[2,]    3    2
#> fkt<-makeCachematrix(orginalMatrix)
#Error: could not find function "makeCachematrix"
#> fkt<-makeCacheMatrix(orginalMatrix)
#> cacheSolve(fkt)
#[,1] [,2]
#[1,]    2   -1
#[2,]   -3    2
#> solve(orginalMatrix)
#[,1] [,2]
#[1,]    2   -1
#[2,]   -3    2
#> 
