LCBD.comp <- function(x, sqrt.x=TRUE)
{
  ### Internal function
  centre <- function(D,n)
    # Centre a square matrix D by matrix algebra
    # mat.cen = (I - 11'/n) D (I - 11'/n)
  {
    One <- matrix(1,n,n)
    mat <- diag(n) - One/n
    mat.cen <- mat %*% D %*% mat
  }
  ###
  n <- nrow(as.matrix(x))
  if(sqrt.x) {
    # x = sqrt(x)
    SStotal <- sum(x)/n # eq. 8
    BDtotal <- SStotal/(n-1) # eq. 3
    G <- centre(as.matrix(-0.5*x), n) # Gower-centred matrix
  } else {
    SStotal <- sum(x^2)/n # eq. 8
    BDtotal <- SStotal/(n-1) # eq. 3
    G <- centre(as.matrix(-0.5*x^2), n) # Gower-centred matrix
  }
  LCBD <- diag(G)/SStotal # Legendre & De Caceres (2013), eq. 10b
  out <- list(SStotal_BDtotal=c(SStotal,BDtotal), LCBD=LCBD, D=x)
} 

# Arguments --
#
# x : D or beta diversity component matrix, class=dist.
# sqrt.x : Take sqrt() of components before computing LCBD.comp. Use
# sqrt.x=TRUE for the replacement and richness/abundance difference indices
# computed by beta.div.comp(), as well as for the corresponding D matrices.

