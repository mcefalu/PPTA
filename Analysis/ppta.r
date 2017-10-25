#########################
## PPTA implementation ##
#
# Y is the outcome
# A is the treatment
# X are the covariates
# M is the number of MCMC
# n.grid is number of bins for grid approach

# PPTA.design only considers design part of the procedure
PPTA.design <- function(A,X,M=1000){#},n.grid=10){
   # load required library
   library(MCMCpack)
   
   # some basic numbers
   n = length(A)
   k = ncol(as.matrix(X))
   
   # fit the PS model
   ps.mcmc = MCMClogit(A~X,burnin=1000,mcmc=M)

   ##############################
   ## for each mcmc, calculate ##
   # ps = propensity score
   # ppta = posterior predictive treatment assignment
   # S = indicator of inclusion in unconfounded subset (PPTA approach)
   # S.grid = indicator for grid approach
   ps = matrix(NA,n,M)
   ppta = matrix(NA,n,M)
   S = S.grid = matrix(NA,n,M)
   # could get rid of this loop and do everything with matrix functions
   for (i in 1:M){
      ##################
      ## PPTA based S ##
      ps[,i] = plogis(model.matrix(A~X)%*%ps.mcmc[i,])
      ppta[,i] = rbinom(n,1,ps[,i])
      S[,i] = ppta[,i]!=A
      
      ##################
      ## grid based S ##
   
      # create a ps grid
  #    ps.grid = quantile(ps[,i],seq(0,1,1/n.grid))
      
      # classifiy into a grid cell
  #    ps.class = cut(ps[,i],ps.grid,include.lowest=T)
      
      # find prob of treatment within cells
  #    probs = tapply(A,ps.class,mean)
      
      # calculate inclusion probs
  #    S.prob = probs[ps.class]
  #    S.prob = A*(1-S.prob) + (1-A)*S.prob
  #    S.grid[,i] = rbinom(n,1,S.prob)==1
   }
   out = list(ps.mcmc=ps.mcmc,ps=ps,ppta=ppta,S=S,A=A,M=M) #S.grid=S.grid,
   class(out) = "PPTA.design"
   return(out)
}

# once design is decided, analysis takes place
PPTA.analysis <- function(Y,out,Qburn=10,Q=10,outcome.dist="guassian"){
   # load required library
   library(MCMCpack)
   
   # check that out is of class PPTA.design
   if (class(out)!="PPTA.design"){
      stop("out is not a PPTA.design object")
   }
   
   # some basic numbers
   n = length(Y)
   M = out$M
   A = out$A
   S = out$S
#   S.grid = out$S.grid

   # fit the outcome model
   # check the type of model and fit accordingly
   seeds = abs(round(rnorm(M)*100000)+1)
   if (outcome.dist=="gaussian"){
      # out.mcmc = out.mcmc.grid = matrix(NA,M,3)
      out.mcmc = array(NA, c(Q,3,M)) # M design iterations, Q analysis iterations, 3 is outcome parameters,  = out.mcmc.grid
      out.freq  = matrix(NA,M,2) # Store the MLE and variance,  =out.freq.grid
      for (i in 1:M){
         out.mcmc[,,i] = MCMCregress(Y[S[,i]]~A[S[,i]],burnin=Qburn,mcmc=Q,seed=seeds[i])  
 #        out.mcmc.grid[,,i] = MCMCregress(Y[S.grid[,i]]~A[S.grid[,i]],burnin=Qburn,mcmc=Q,seed=seeds[i])  
         ## use variance MLE instead of Bayes
         freqmod = lm(Y[S[,i]]~A[S[,i]])
         out.freq[i,1] = freqmod$coefficients[2]
         out.freq[i,2] = vcov(freqmod)[2,2] #summary(freqmod)$cov.unscaled[2,2]*summary(freqmod)$sigma^2
#         freqmod.grid = lm(Y[S.grid[,i]]~A[S.grid[,i]])
#         out.freq.grid[i,1] = freqmod.grid$coefficients[2]
#         out.freq.grid[i,2] = vcov(freqmod.grid)[2,2] 
      }
   }
   if (outcome.dist=="binomial"){
     out.mcmc = array(NA, c(Q,2,M)) # M design iterations, Q analysis iterations, 2 is outcome parameters,  = out.mcmc.grid
     out.freq  = matrix(NA,M,2) # Store the MLE and variance
      for (i in 1:M){
        out.mcmc[,,i] = MCMClogit(Y[S[,i]]~A[S[,i]],burnin=Qburn,mcmc=Q,seed=seeds[i])  
        freqmod = glm(Y[S[,i]]~A[S[,i]], family = "binomial")
        out.freq[i,1] = freqmod$coefficients[2]
        out.freq[i,2] = vcov(freqmod)[2,2] #summary(freqmod)$cov.unscaled[2,2]*summary(freqmod)$sigma^2
      }
   }
   if (outcome.dist=="poisson"){
     out.mcmc = out.mcmc.grid = array(NA, c(Q,2,M)) # M design iterations, Q analysis iterations, 2 is outcome parameters
     out.freq  = matrix(NA,M,2) # Store the MLE and variance
      for (i in 1:M){
        out.mcmc[,,i] = MCMCpoisson(Y[S[,i]]~A[S[,i]],burnin=Qburn,mcmc=Q,seed=seeds[i])  
        freqmod = glm(Y[S[,i]]~A[S[,i]], family = "poisson")
        out.freq[i,1] = freqmod$coefficients[2]
        out.freq[i,2] = vcov(freqmod)[2,2] #summary(freqmod)$cov.unscaled[2,2]*summary(freqmod)$sigma^2
      }
   }
  
   out = list(mcmc=out.mcmc,MLE=out.freq) #mcmc.grid=out.mcmc.grid, ,MLE.grid=out.freq.grid
   class(out) = "PPTA.analysis"
   return(out)
}

###################################
## plot function for PPTA design ##
# 
# x is a PPTA.design object
# y is the covariate to evaluate balance on
plot.PPTA.design <- function(x,y,type="balance"){
   library(ggplot2)
   if (type=="balance"){
      m = nrow(x$ps.mcmc)
      bal.ppta = numeric(m)
      bal.grid = numeric(m)
      for (i in 1:m){
         index = x$S[,i]==1
         bal.ppta[i] = diff(tapply(y[index],x$A[index],mean))
         #index = x$S.grid[,i]==1
         #bal.grid[i] = diff(tapply(y[index],x$A[index],mean))
      }
      d = data.frame(Iteration=1:m,balance=bal.ppta,estimator="PPTA")
      #e = data.frame(Iteration=1:m,balance=bal.grid,estimator="Grid")
      #d = rbind(d,e)
      ggplot(data=d, aes(Iteration, balance)) + 
         geom_line() + facet_grid(estimator~.)
   }
}





