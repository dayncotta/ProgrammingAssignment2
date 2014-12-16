## The 2 Main functions 'makeCacheMatrix' and 'cacheSolve' are used to: 
##   First store the matrix value and the  cached matrix inverse value 
##     in an object, along with the functions for fetching above values,
##   Secondly, to check for the computed inverse value of a matrix in cache,
##     or to compute it if absent in the cache, Respectively. 




## The funtcion makeCacheMatrix accepts a matrix 'x' as input 
## and creates a list object containing the functions:
##      set_matrix(y)  - sets the local(inside the function) value y,
##                       of the matrix, to the global(outside the function) 'x', 
##                       due to operator '<<-'.
##      get_matrix()   - returns the matrix value x.
##      set_inv(i)     - sets the local value i of the inverse,  
##                       to the global 'inv'. 
##      get_inv()      - returs the inverse value inv.
## which is then returned.


makeCacheMatrix <- function(x = matrix()) 
{
  
  inv <- NULL
  
  set_matrix <- function(y) 
  {
    x <<- y                    ## set the passed inverse value 'y' to global 
                               ## inverse variable 'inv' 
    
    inv <<- NULL               ## initialize the inverse value 'inv' to NULL
  }
  
  get_matrix <- function()
  {
    x                          ## return global matrix value of 'x'
  }
  
  set_inv <- function(i)
  {
    inv <<- i                  ## set the passed inverse value 'i' to global 
                               ## inverse variable 'inv' 
  }
  
  get_inv <- function()
  {
    inv                        ## return global inverse value of 'inv'
  }
  
  list(set_matrix = set_matrix, get_matrix = get_matrix, 
       set_inv = set_inv, get_inv = get_inv)  
  ## return list with set and get fuctions
}



## The function cacheSolve accepts a list object, as returned by the 
## function 'makeCacheMatrix'(which corresponds to the particular matrix 
## used to create it) and checks if the inverse for that 
## particular matrix has been computed before. If it has, 
## it returns the cached value, otherwise it computes the inverse,
## and returns the computed value.


cacheSolve <- function(x, ...)      ## accept list object 'x' corresponding to a  
{                                   ## particular matrix.
  
  i <- x$get_inv()                  ## fetch inverse value from object 'x' 
                                    ## and store in 'i'
  
  if(!is.null(i))                   ## check if cached inverse value exists
  {
    message("Fetching Value from Cache")
    
    if(is.matrix(i))
    {
      return(i)                     ##return cached inverse value 'i'
    }
    
    else
    {
      return(message(i))
    }
  }
  
  m <- x$get_matrix()               ## fetch matrix value from object 'x'
                                    ## and store in 'm'
  if(det(m)==0)
  {
    i<-"The matrix is not invertible"
    x$set_inv(i)
    return(message(i))
  }
  i <- solve(m)                     ## compute inverse for matrix 'm'
                                    ## and store in 'i'
  x$set_inv(i)                      ## store computed value 'i' in cache
  i                                 ## return computed inverse value 'i'
  
}
