#############################################
## implementation of overlap weight method ##
##  from Li, Morgan, and Zaslavsky (2016)  ##
#
# Y is the outcome
# A is the treatment
# PS is the propensity score

ato = function(Y,A,PS){
  w = rep(NA, length(Y))
  w[A==0] = PS[A==0]
  w[A==1] = 1-PS[A==1]
  
  ato = sum( w*A*Y)/sum(w*A) - sum(w*(1-A)*Y)/(sum(w*(1-A)))
  msm <- svyglm(Y ~ A, design = svydesign(~ 1, weights = ~ w , data=data.frame(A=A,Y=Y,w=w)))
  
  return(list(ato = ato, w = w, CI=confint(msm)[2,]))
  
}