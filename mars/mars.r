#' Multivariate Adaptive Regression Splines (MARS)
#'
#' This function fits a Multivariate Adaptive Regression Splines (MARS) model to the provided data
#' using the specified formula. MARS is a non-parametric regression technique that models
#' complex relationships by fitting piecewise linear regressions. This implementation includes
#' a forward selection phase to add terms to the model and a backward elimination phase to prune
#' terms, optimizing model complexity and fit.
#'
#' @param formula A formula specifying the model to fit, with the response on the left of a `~`
#' and predictors on the right, separated by `+` symbols.
#' @param data A data frame containing the variables specified in the formula.
#' @param control A list of control parameters for the fitting process. If not specified,
#' defaults are provided by `mars.control()`.
#' 
#' @return An object of class `mars`, which includes the fitted model among other components:
#' \describe{
#'   \item{call}{The matched call to the `mars` function.}
#'   \item{formula}{The model formula.}
#'   \item{y}{The response variable from the provided data.}
#'   \item{B}{The matrix of basis functions used in the model.}
#'   \item{Bfuncs}{Details of the basis functions selected during the model fitting process.}
#'   \item{x_names}{Names of the predictor variables.}
#'   \item{fit}{An object of class `lm` representing the linear model fit to the selected terms.}
#' }
#' The `mars` object supports standard methods like `print`, `summary`, and `plot`.
#'
#' @details The MARS model fitting process includes both forward selection to add interaction
#' terms and basis functions, followed by backward elimination to remove less impactful terms.
#' This ensures the model captures complex interactions and nonlinearities in the data while
#' maintaining parsimony.
#'
#' @examples
#' \dontrun{
#' data(mtcars)
#' mars_fit <- mars(mpg ~ wt + hp, data = mtcars)
#' summary(mars_fit)
#' }
#'
#' @references
#' Friedman, J.H. (1991). "Multivariate Adaptive Regression Splines." The Annals of Statistics,
#' 19(1), 1-67. Provides the foundational methodology for MARS.
#'
#' @seealso \code{\link{mars.control}}, \code{\link{lm}}
#'
#' @export
#' @importFrom stats lm model.frame model.matrix
#' @importFrom Rcpp cppFunction
mars <- function(formula,data,control=mars.control()) {
  cc <- match.call() # save the call
  mf <- model.frame(formula,data)
  y <- model.response(mf)
  mt <- attr(mf, "terms")
  x <- model.matrix(mt, mf)[,-1,drop=FALSE]
  x_names <- colnames(x)
  # there is a validator here.
  
  if(is.null(control))
    control <- mars.control()
  
  fwd <- fwd_stepwise(y,x,control)
  bwd <- bwd_stepwise(fwd,control)  # If you opt to not get the bonus point, you can simply ignore this line.
  
  fit <- lm(y~.-1,data=data.frame(y=y,bwd$B)) # Note: I am just using a dummy example of lm() here; this needs to be changed to the correct MARS implementation (Hint: Fit a lm to all the terms from the forward/backward stepwise.)
  out <- c(list(call=cc,formula=formula,y=y,B=bwd$B,Bfuncs=bwd$Bfuncs,
                x_names=x_names),fit)
  
  class(out) <- c("mars",class(fit))
  out
}

fwd_stepwise <- function(y,x,control=mars.control()){
  N <- length(y) # sample size
  n <- ncol(x) # number of predictors
  
  # To do: Initialize B with your init_B() function and
  #        Bfuncs to be an empty list of length mc$Mmax+1
  B <- init_B(N,control$Mmax)
  Bfuncs <- vector(mode="list",length=control$Mmax+1)
  #---------------------------------------------------
  #
  VAL1 = control$Mmax/2
  for(i in 1:VAL1) {     # To do: Replace the M loop with a loop over pairs i
    
    M <- 2*i-1          # To do: Set the value of M from the value of i.
    
    if(control$trace) cat("M",M,"\n")
    
    lof_best <- Inf
    
    for(m in 1:M) { # choose a basis function to split
      
      # To do: Create an object "svars" to store the indices of variables that are not
      #        already in basis function m.
      svars = setdiff(1:n, Bfuncs[[m]][,"v"])
      
      if(control$trace) cat("M, m, svars", M, m, svars,"\n")
      
      for(v in svars){ # select a variable to split on
        
        tt <- split_points(x[,v],B[,m])
        
        for(t in tt) {
          
          #        Change the way of creating "Bnew" so that we do not remove the
          #        parent basis function, and we add pairs of child basis functions
          #        using the hinge function defined previously.
          Bnew <- data.frame(B[,(1:M)],
                             Btem1=B[,m]*h(x[,v],-1,t),
                             Btem2=B[,m]*h(x[,v],+1,t))
          
          
          gdat <- data.frame(y=y,Bnew)
          lof <- LOF(y~., gdat, control)
          if(lof < lof_best) {
            lof_best <- lof
            split_best <- c(m=m,v=v,t=t)
          }
        }
      }
    }
    mstar <- split_best["m"]; vstar <- split_best["v"]; tstar <- split_best["t"]
    
    # Update Bfuncs and B according to Algorithm 2 - forward stepwise in Friedman's paper.
    Bfuncs[[M+1]] <- rbind(Bfuncs[[mstar]], c(s=-1, vstar, tstar))
    Bfuncs[[M+2]] <- rbind(Bfuncs[[mstar]], c(s=+1, vstar, tstar))
    
    B[,M+1:2] <- cbind(B[,mstar]*h(x[,vstar], -1, tstar),B[,mstar]*h(x[,vstar], +1, tstar))
    
  } # end for loop over i
  
  #  Set the column names of B.
  colnames(B) <- paste0("B", (0:(ncol(B)-1)))
  
  #  Return the list 'list(y=y, B=B, Bfuncs=Bfuncs)'.
  return(list(y=y,B=B,Bfuncs=Bfuncs))
}

init_B <- function(N,Mmax) {
  B <- data.frame( matrix(NA,nrow=N,ncol=(Mmax+1)) )
  B[,1] <- 1 # first column for intercept: B0
  names(B) <- c("B0",paste0("B",1:Mmax))
  return(B)
}

bwd_stepwise <- function(fwd,control) {
  #fwd is a list with elements y, B and Bfuncs
  Mmax <- ncol(fwd$B)
  Jstar <- 2:Mmax
  Kstar <- Jstar
  dat <- data.frame(y=fwd$y, fwd$B)
  lofstar <- LOF(y~.-1,dat,control)
  for(M in Mmax:2) {
    
    b <- Inf
    L <- Kstar
    if(control$trace) cat("L:",L,"\n")
    for(m in L){
      K <- setdiff(L,m) # hint: use setdiff
      dat <- data.frame(y=fwd$y,fwd$B[,K])
      lof <- LOF(y~.,dat,control)
      if(control$trace) cat("M:K:lof",M,":",K,":",lof,"\n")
      if(lof < b) {
        b <- lof
        Kstar <- K
      }
      if(lof < lofstar) {
        lofstar <- lof
        Jstar <- K
      }
    }
    if(control$trace) cat("M:Jstar:lofstar",M,":",Jstar,":",lofstar,"\n")
  }
  Jstar <- c(1,Jstar)
  return(list(y=fwd$y,B=fwd$B[,Jstar],Bfuncs=fwd$Bfuncs[Jstar]))
}


LOF <- function(form,data,control) {
  ff <- lm(form,data)
  RSS <- sum(residuals(ff)^2)
  N <- nrow(data)
  M <- length(coef(ff))-1
  Ctilde <- sum(diag(hatvalues(ff))) + control$d*M
  return(RSS * N/(N-Ctilde)^2)
}


# h <- function(x,s,t) {
# ##
#   # if x>t, s=+1, this return max(0,x-t)
#   # if x<t, s=-1, this return max(0,t-x)
#   return(pmax(0, s*(x-t)))
# }


cppFunction('NumericVector hCpp(NumericVector x, int s, double t) {
  int n = x.size();
  NumericVector result(n);

  for(int i = 0; i < n; ++i) {
    if(s == 1) {
      result[i] = std::max(0.0, x[i] - t);
    } else if(s == -1) {
      result[i] = std::max(0.0, t - x[i]);
    }
  }
  return result;
}')




split_points <- function(xv,Bm) {
  #
  out <- sort(unique(xv[Bm>0]))
  return(out[-length(out)])
}

#------------------------------------------------------------------------
# constructor, validator and helper for class mars.control
new_mars.control <- function(control) {
  structure(control, class="mars.control")}


validate_mars.control <- function(control) {
  stopifnot(is.integer(control$Mmax),is.numeric(control$d),
            is.logical(control$trace))
  if(control$Mmax < 2) {
    warning("Mmax must be >= 2; Reset it to 2")
    control$Mmax <- 2}
  if(control$Mmax %% 2 > 0) {
    control$Mmax <- 2*ceiling(control$Mmax/2)
    warning("Mmax should be an even integer. Reset it to ",control$Mmax)}
  control
}

validate_mars.control <- function(control) {
  
}

#' Constructor for `mars.control` objects
#'
#' This function constructs a `mars.control` object that specifies
#' parameters used in the model fitting procedure.
#'
#' @param Mmax Maximum number of basis functions. Should be an even integer. Default value is 2.
#' @param d The coefficient in the penalty term of the generalized cross validation measure. Default is 3.
#' @param trace Should we print status information about the fitting? Default is `FALSE`
#'
#' @return a `mars.control` object
#' @export
#'
#' @examples mc <- mars.control(Mmax=10)
mars.control <- function(Mmax=2,d=3,trace=FALSE) {
  Mmax <- as.integer(Mmax)
  control <- list(Mmax=Mmax,d=d,trace=trace)
  control <- validate_mars.control(control)
  new_mars.control(control)
}

new_mars.control <- function(control) {
  structure(control, class="mars.control")}

validate_mars.control <- function(control) {
  stopifnot(is.integer(control$Mmax),is.numeric(control$d),
            is.logical(control$trace))
  if(control$Mmax < 2) {
    warning("Mmax must be >= 2; Reset it to 2")
    control$Mmax <- 2}
  if(control$Mmax %% 2 > 0) {
    control$Mmax <- 2*ceiling(control$Mmax/2)
    warning("Mmax should be an even integer. Reset it to ",control$Mmax)}
  control
}
