gini <- function(y) {
  y <- sort(y)
  
  n <- length(y)
  
  numer <- 2 * sum((1:n) * y)
  
  denom <- n * sum(y)
  
  return ((numer / denom) - (n + 1) / n)
}

random_split <- function(A, B) {
  # Calculate the total pot
  pot <- A + B
  
  # Generate a random share between 0 and the total pot
  share <- runif(1, 0, pot)
  
  # Return the two shares (first actor and second actor)
  return(c(share, pot - share))
}

