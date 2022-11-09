
g <- function(x)
{
  sqrt(10/(4+x))
}
p0 <- 1.5
steffensen <-  function(g, p0, tol = 1e-5, maxIter = 100)
{
  #step1
  i <- 1
  
  #step2
  p_seq <- c()
  while (i <= maxIter) {
    #step3
    p1 <- g(p0)
    p2 <- g(p1)
    p <- p0 - (p1 - p0)^2 / (p2 - 2*p1 + p0)
    p_seq[i] <- p
    
    #step4
    if(abs(p-p0) < tol)
    {
      cat(p)
      return(data.frame(p_seq))
    }
    
    #step5
    i <- i+1
    
    #step6
    p0 <- p
  }
  
  #step7
  return(p)
}
steffensen(g, p0)
