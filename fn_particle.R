# Create a particule
newParticule <- function(data, id = 0, w= 0, wnorm = 0, w_norm_x_prop = 0,idx = 0, entropy = 0, par = globalenv()){ 
  object <- new.env(parent = par)
  object$value = data
  object$id   = id
  object$w = w
  object$wnorm = wnorm
  object$w_norm_x_prop = w_norm_x_prop
  object$idx = idx
  object$entropy = entropy
  class(object)  <- "particule"
  return(object) 
} 
# build the particule genealogy

buildGenealogy <- function(part){
  gen <- NULL
  value = NULL
  w = NULL
  wnorm = NULL
  w_norm_x_prop = NULL
  idx = NULL
  entropy = NULL
  pr <- part
  while(is(parent.env(pr), "particule")) {
    gen <- c(pr$id, gen)
    value <- c(pr$value, value)
    w = c(pr$w, w)
    wnorm = c(pr$wnorm, wnorm)
    w_norm_x_prop = c(pr$w_norm_x_prop, w_norm_x_prop)
    idx = c(pr$idx, idx)
    entropy = c(pr$entropy, entropy)
    pr = parent.env(pr)
    
  }
  
  return(list(gen = gen, value = value, w = w, wnorm = wnorm, w_norm_x_prop = w_norm_x_prop, idx = idx, entropy = entropy))
}

# get the number of parents

getDepth <- function(part, verbose = FALSE) {
  pr <- part
  N <- 0
  while(is(parent.env(pr), "particule")) {
    N <- N + 1
    pr <- parent.env(pr)
    if (verbose) {print(paste0(">>> i = ", N))}
  }
  return(N)
}

buildSimu <- function(part){
  nx = length(part$value)
  nt = getDepth(part)
  simu  = matrix(numeric(length = nx * nt), nrow = nx, ncol = nt)
  pr <- part
  t  <- nt
  while(is(parent.env(pr), "particule")) {
    if(length(pr$value) == nx) {
      simu[, t] <- pr$value
      t <- t - 1
    }
    pr <- parent.env(pr)  
  }
  return(simu)
}

myfunc <- function(v1) {
  deparse(substitute(v1))
}
#' Particle Filter: Algo. 1 (Non conditional Simulation)
#' M_X : mean of the process X_t
#' P_X : variance of the proces X_t
#' M_0 : mean of the initial value X_0
#' P_0 : variance of the initial vlaue X_0
#' M_Y : mean of the noise on the observation Y_t
#' P_Y : vairance of the noise on the observation Y_t
#' t   : number of time steps
#' f   : function defining the Markov dynamic on X_t = f(X_{t-1}, t) + N(M_X, S_X) : f(x,t)
#' g   : function defining the obervations Y_t = g(X_{t}) + N(M_Y, S_Y) : g(x)
#' c_particulas: Parameters of quantity how many normalized weights are over c_particles