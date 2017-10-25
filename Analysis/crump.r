#####################################
## implementation of crumps method ##
#
# Y is the outcome
# A is the treatment
# PS is the propensity score

crump <- function(Y,A,PS){
   PS2 = PS*(1-PS)
   PStemp = PS*(PS<=.5) + (1-PS)*(PS>.5)
   PStemp = sort(PStemp)
   for (alpha in PStemp){
      alpha2=alpha*(1-alpha)
      if( (1/alpha2) <= (2*sum( (PS2>=alpha2)*1/PS2 ) / sum(PS2>=alpha2)) ){
         w =A*(PS2>=alpha2)/PS  + (1-A)*(PS2>=alpha2)/(1-PS)
         msm <- svyglm(Y ~ A, design = svydesign(~ 1, weights = ~ w , data=data.frame(A=A,Y=Y,w=w)))
         return(list(est=coef(msm)[2],CI=confint(msm)[2,], weight=w))
      }
   }
}