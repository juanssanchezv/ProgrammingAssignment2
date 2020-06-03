#--In this script there are two functions: makeCacheMatrix which four built-in
#--fuctions and returns them within a list to the parent enviroment. And the 
#--second funcion is cacheSolve which calculates the inverse of a matrix given
#--from the makeCacheMatrix function and stores it as cache-
#--makeCacheMatrix is a function that receives a parameter that is a Matrix
#--in this function there are 4 functions that are returned later in a list
makeCacheMatrix <- function(x = matrix()) 
  {
   #Initialization of the objects
    i <- NULL                           #this is null because we have not make the inverse of the matrix yet
   #First function, this initialize also the objects
    set <- function(y=matrix())         #this function receives the matrix and set i to null
    {
      x <<- y                           #the matrix x here is initialized
      i <<- NULL                        #this is null because we have not make the inverse of the matrix yet
    }
    get <- function() x                 #this function calls the matrix set in the previous function
    setinv <- function(inv) i <<- inv   #this function calls the inverse "i" of the matrix when it is already calculated by the solveCache function 
    getinv <- function() i              #this function calls the inverse matrix set in the previous function
   #The following list is created so the user can access the built in functions
    list(set=set,                       #the function set is named set
         get=get,                       #the function get is named get
         setinv=setinv,                 #the function setinv is named setinv
         getinv=getinv)                 #the function getinv is named getinv
  }


## Write a short comment describing this function
##--This function makes the actual calculation of the inverse of the matrix
##--and caches it so if the user want to know the inverse of the matrix again
##--the computer does not have to make the same calculation again
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i = x$getinv()                         ##calling the getinv function from makeCacheMatrix to get i
  if(!is.null(i))                        ##Checking if the inverse matrix has already been created
  {
    message("getting cached data")       ##if the inverse has been created it will print this message
    return(i)                            ##return the inverse matrix
  }
  data <- x$get()                        ##calls the get function to obtain the matrix that has to be calculated
  i <- solve(data,...)                   ##use the function solve to calculate the inverse matrix
  x$setinv(i)                            ##call the setinv function to store the value of the inverse matrix
  i                                      ##show the inverse matrix   
}
