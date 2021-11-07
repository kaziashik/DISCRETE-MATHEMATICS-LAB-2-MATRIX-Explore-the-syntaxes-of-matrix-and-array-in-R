A <- matrix(c (1,4,5,3,2,6,2,0,8,-4,7,-3),nrow=3,ncol=4)

B <- matrix(c (-1,12,3,8,6,3,7,4),nrow=4,ncol=2)

C <- matrix(c (5,-13,9,7,11,8,9,2,-7),nrow=3,ncol=3)

mplus <- function(x,y){
  
  r1=nrow(x)
  r2=nrow(y)
  c1=ncol(x)
  c2=ncol(y)
  
  if (r1!=r2 || c1!=c2){
    print('Sorry, your matrix size does not match')
    return()
  }
  z = matrix(0,nrow=r1,ncol=c1)
  for (i in 1:r1){
    for (j in 1:c1){
      z[i,j]=x[i,j]+y[i,j]
    }
  }
  return(z)
}

mMultiply <- function(x,y){
  
  r1=nrow(x)
  r2=nrow(y)
  c1=ncol(x)
  c2=ncol(y)
  if (r2 != c1){
    print('size does not match')   
    return()
  }
  z = matrix(0,nrow = r1, ncol = c2)
  
  if(r1 == r2 && c1 == c2) {                    # This is for matrices that have identical dimension
    for (i in 1:r1){      
      for (j in 1:c1){    
        z[i,j]=x[i,j] * y[i,j]
      }
    }
  } else if(c1 == r2) {                         # This is for C * A and A' * A
    for (i in 1:r1){
      for (j in 1:c2){
        for (k in 1:r2) {
          z[i,j] = z[i,j] + (x[i,k] * y[k,j])
        }
      }
    }
  }
  
  return(z)
}

transpose <- function(x) {                      # function for transpose
  r1 = nrow(x)
  c1 = ncol(x)
  
  z = matrix(0, nrow = c1, ncol = r1) 
  
  for (i in 1:nrow(x))
  {   
    for (j in 1:ncol(x))
    { 
      z[j, i] = x[i, j] 
    }
  }
  return(z)
}

cat("This is for A + A\n")
print(mplus(A,A))

cat("\nThis is for A + B\n")
print(mplus(A,B))

cat("\nThis is for C * C\n")
print(mMultiply(C,C))

cat("\nThis is for C * A\n")
print(mMultiply(C,A))

cat("\nThis is for A' * A\n")
print(mMultiply(transpose(A), A))
