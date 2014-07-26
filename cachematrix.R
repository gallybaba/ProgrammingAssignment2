## Put comments here that give an overall description of what your
## functions do
## makeCacheMatrix creates and returns a list 
## with a set of utility and access functions to
## get and set data and inverse of data
## cacheSolve checks if the inverse exists in cache
## if yes, it returns the inverse from cache otherwise
## it uses solve method to calculate inverse and then
## uses setinv method on makeCacheMatrix to set the inverse
## back to the list
## Write a short comment describing this function
## sample client / test code

####> source('cachematrix.R')
####> m <- matrix(1:4, 2, 2)
####> m1 <- matrix(2:7, 2, 2)
####> solve(m)
####[,1] [,2]
####[1,]   -2  1.5
####[2,]    1 -0.5
####> solve(m1)
####[,1] [,2]
####[1,] -2.5    2
####[2,]  1.5   -1
####> l <- makeCacheMatrix(m)
####> linv <- cacheSolve(l)
####> linv <- cacheSolve(l)
####getting from cache
####> l <- makeCacheMatrix(m1)
####> linv <- cacheSolve(l)
####> linv <- cacheSolve(l)
####getting from cache
####> l <- makeCacheMatrix(m)
####> l <- makeCacheMatrix(m)
####> linv <- cacheSolve(l)
####> linv <- cacheSolve(l)
####getting from cache
 
makeCacheMatrix <- function(x = matrix()) 
{
  i <- NULL
  
  set <- function(y)
  {
    x <<- y
    i <<- NULL
  }
  
  get <- function()
  {
    x
  }
  
  setinv <- function(inv)
  {
    i <<- inv
  }
  
  getinv <- function()
  {
    i
  }
  
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## Write a short comment describing this function
####> source('cachematrix.R')
####> m <- matrix(1:4, 2, 2)
####> m1 <- matrix(2:7, 2, 2)
####> solve(m)
####[,1] [,2]
####[1,]   -2  1.5
####[2,]    1 -0.5
####> solve(m1)
####[,1] [,2]
####[1,] -2.5    2
####[2,]  1.5   -1
####> l <- makeCacheMatrix(m)
####> linv <- cacheSolve(l)
####> linv <- cacheSolve(l)
####getting from cache
####> l <- makeCacheMatrix(m1)
####> linv <- cacheSolve(l)
####> linv <- cacheSolve(l)
####getting from cache
####> l <- makeCacheMatrix(m)
####> l <- makeCacheMatrix(m)
####> linv <- cacheSolve(l)
####> linv <- cacheSolve(l)
####getting from cache

cacheSolve <- function(x, ...) 
{
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv))
  {
    message("getting from cache")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinv(inv)
  inv
}
