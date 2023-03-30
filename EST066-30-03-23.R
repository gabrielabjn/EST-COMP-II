
# 30/03/2023

# Quadratic Distribution

# You've gotta create: pene, qene, rene and dene;

# Wiki: In probability theory and statistics, the U-quadratic distribution is a 
# continuous probability distribution defined by a unique convex quadratic function
#with lower limit a and upper limit b.

# In our case, a = 0 and b = 1;

# Suppose given values for x are between 0 and 1 (with 0 and 1 included);

# pdf --------------------------------------------------------------------------

dene <- function(x, a = 0, b = 1){
  
  B = (b + a)/2
  A = 12/(b-a)**3
  
  
  return(A*(x-B)**2) 
  
  
}

dene(0.5)

# cdf --------------------------------------------------------------------------

pene <- function(x, a = 0, b = 1){
  
  B = (b + a)/2
  A = 12/(b-a)**3
  
  return((A/3)*((x-B)**3+(B-a)**3))
  
  
}

pene(0.5)


# qene -------------------------------------------------------------------------

# ????????????????

# rene -------------------------------------------------------------------------

rene <- function(tam = 1, a = 0, b = 1){ # tam da amostra
  
  u = runif(tam)
  x = qene(u,n)
  return(x)
} # ???????????????????????/













