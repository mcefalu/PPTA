rm(list = ls())

## load packages used in the analysis
library(data.table)
library(maps)
library(stringr)
library(survey)
library(ggplot2)
library(plyr)
library(twang)

## load functions that implement PPTA, Crump, and ATO
source("ppta.r")
source("crump.r")
source("ato.r")

## load data
dat = fread("AllEGUs.csv") # -- Change File Path to Read In Data -- #
minyear = min(dat$Year)
minmonth = min(dat$Month[dat$Year==minyear])
maxyear = max(dat$Year)
maxmonth = max(dat$Month[dat$Year==maxyear])
nmonths = length(unique(dat$year_month))

## Define the treatment and outcome
dat[, Tx:=S_n_CR >0]
dat[, Outcome:= NOx..tons.] ## 0.55% have NOx tons == 0

## Analysis will be performed separately by year
## 'results' will save the point estimates and confidence intervals
## 'sample_sizes' will save the number of observations in each year of the analysis
years = 2002:2014
results = array(NA, c(5, 3, length(years)))
dimnames(results) = list(c("IPTW", "IPTWt50", "Crump", "OverlapWeight", "PPTA"), c("Est", "2.5%", "97.5%"), years)
sample_sizes = matrix(NA, length(years), 3)
dimnames(sample_sizes) = list(years, c("Untreated", "Treated", "Total"))

## loop over the years and perform the analyses
for (i in 1:length(years)){
   # - keep untransformed, then log the annual emissions later
   dat_annual = dat[Year==years[i], list(Outcome = sum(Outcome, na.rm = TRUE), 
                                     months_with_outcome = sum(!is.na(Outcome)),
                                     Txmonths = sum(Tx==TRUE),
                                     isCoal = Fuel.Type..Primary..x[1] == 'Coal',
                                     initialYear = Initial.Year.of.Operation[1],
                                     S_n_CR = (sum(S_n_CR) >= 6),  # if installed at least 6 months
                                     avgNOxControls = mean(NumNOxControls - (Tx==1), na.rm = TRUE), # Avg number of NOx controls OTHER THAN SCR/SNCR
                                     scrubber = (sum(AnySO2control) >=6), #if installed at least 6 months
                                     totOpTime = sum(Operating.Time),
                                     NOxemissions = sum(NOx..tons.),
                                     SO2emissions = sum(SO2..tons.),
                                     CO2emissions = sum(CO2..short.tons.),
                                     GrossLoad = sum(Gross.Load..MW.h.),
                                     HeatInput = sum(Heat.Input..MMBtu.),
                                     pctCapacity = mean(Heat.Input..MMBtu./Capacity), 
                                     Phase2 = Is.Phase2[1],
                                     EPA.Region = EPA.Region[1],
                                     Latitude = Facility.Latitude.x[1],
                                     Longitude = Facility.Longitude.x[1]),
                      by = c("uID") ]
   dim(dat_annual)

   # - Delete units with 0 annual emissions
   dat_annual = dat_annual[Outcome != 0, ]
   
   # - Check how many units have missing emissions
   with(dat_annual, table(months_with_outcome))
     
   # - Subset to units that have all 12 months of data
   dat_annual = dat_annual[months_with_outcome==12, ]
   dim(dat_annual)
     
   # - Check how many units had scrubbers for only part of the year
   with(dat_annual, table(Txmonths))
     
   # - Remove units that install scrubber mid year
   dat_annual = dat_annual[Txmonths %in% c(0,12)]
   dim(dat_annual)
     
   colMeans(is.na(dat_annual))
   # -- Remove observations with missing heat input or pctcapacity (important confounders)
   dat_annual = dat_annual[!is.na(HeatInput) & !is.na(pctCapacity), ]
   
   dat_annual[, Tx := (Txmonths == 12)]
   with(dat_annual, table(Tx))
   
   dim(dat_annual)
   
   # -------------------------------------------
   #         Analysis      
   # -------------------------------------------
   dat_annual[, Outcome:= log(Outcome)]
   n_grp = with(dat_annual, table(Tx))
   sample_sizes[i,c("Untreated", "Treated")] = n_grp
   sample_sizes[i, "Total"] = dim(dat_annual)[[1]]
   
   #-- Simple mean comparison
   with(dat_annual, t.test(Outcome~Tx))
   
   #-- Propensity score model
   dat_annual[, coal_no_scrubber := (isCoal==TRUE & scrubber==FALSE)]
   dat_annual[, coal_with_scrubber := (isCoal==TRUE & scrubber==TRUE)]
   psmod = glm(Tx ~  totOpTime + HeatInput + pctCapacity + Phase2 + avgNOxControls 
                + coal_no_scrubber + coal_with_scrubber + as.factor(EPA.Region),
                family = binomial, data = dat_annual)
   
   summary(psmod)
   dat_annual[, ps:= psmod$fitted]
   
   #-- Histogram of Propensity Score Overlap
   #############################################
   colors = c(rgb(0,0,1,.5), rgb(1,0,0,.5))
   par(mfrow=c(1,1))
   with(subset(dat_annual, Tx == FALSE), hist(ps, breaks=100, col=colors[1], main ="", xlab = "Estimated Propensity Scores", xlim=c(0,1)))
   with(subset(dat_annual, Tx == TRUE), hist(ps, breaks=100, col=colors[2], add = TRUE))
   legend("top", c("Untreated", "Treated"), fill = colors, bty="n")

   par(mfrow = c(2,1))
   with(subset(dat_annual, Tx == FALSE), hist(ps, xlim = c(0,1), breaks=50, main = paste(n_grp[1],"Untreated")))
   with(subset(dat_annual, Tx == TRUE), hist(ps, xlim = c(0,1), breaks=50,  main = paste(n_grp[2],"Treated")))
   dev.off()
   
   #-- Standard IPTW Estimate
   dat_annual[, W:= Tx/ps + (1-Tx)/(1-ps)]
   with(dat_annual, hist(W, breaks=50))
   summary(dat_annual$W)
   
   msm = svyglm(Outcome~Tx, family = "gaussian", design = svydesign(~ 1, weights = ~W, data=dat_annual))
   IPW = c(coef(msm)[2] , confint(msm)[2,])
   IPW

   #-- Balance diagnostics for IPTW
   bal.ipw = bal.table(dx.wts(dat_annual$ps , dat_annual , treat.var = "Tx" , 
          vars=c("totOpTime" , "HeatInput" , "pctCapacity" ,"Phase2", "avgNOxControls", "coal_no_scrubber" , "coal_with_scrubber"),
          estimand="ATE", x.as.weights=F))
   
   # set up object to hold balance results
   if (i==1){
      balance = numeric(length(years)*6*9)
      dim(balance) = c(length(years),9,6)
      dimnames(balance)[[2]] <- rownames(bal.ipw[[1]])
      dimnames(balance)[[3]] <- c("Unweighted","IPTW","IPTWt50","PPTA","OverlapWeight", "Crump")
   }
   
   balance[i,,"Unweighted"] = bal.ipw[[1]][,"std.eff.sz"]
   balance[i,,"IPTW"] = bal.ipw[[2]][,"std.eff.sz"]
   
   
   #-- IPTW with trucnated weights
   dat_annual[, Wt50 := ifelse(W>50,50,W)]
   with(dat_annual, table(Wt50 == 50)) 
   msmt50 = svyglm(Outcome~Tx, family = "gaussian", design = svydesign(~ 1, weights = ~Wt50, data=dat_annual))
   IPWt50 = c(coef(msmt50)[2] , confint(msmt50)[2,])
   IPWt50
   
   #-- Balance diagnostics for truncated IPTW
   balance[i,,"IPTWt50"] = bal.table(dx.wts(dat_annual$Wt50 , dat_annual , treat.var = "Tx" , 
                              vars=c("totOpTime" , "HeatInput" , "pctCapacity" ,"Phase2", "avgNOxControls", "coal_no_scrubber" , "coal_with_scrubber"),
                              estimand="ATE" , x.as.weights = T))[[2]][,"std.eff.sz"]
   
   #-- Crump Estimate
   crum = crump(Y=dat_annual$Outcome,A=dat_annual$Tx,PS=dat_annual$ps)
   crum[c("est","CI")]
   
   #-- Balance diagnostics for truncated Crump
   balance[i,,"Crump"] = bal.table(dx.wts(crum$w , dat_annual , treat.var = "Tx" , 
                                                   vars=c("totOpTime" , "HeatInput" , "pctCapacity" ,"Phase2", "avgNOxControls", "coal_no_scrubber" , "coal_with_scrubber"),
                                                   estimand="ATE" , x.as.weights = T))[[2]][,"std.eff.sz"]
   
   
   #-- ATO Estimate
   overlap_wt = ato(Y=dat_annual$Outcome,A=dat_annual$Tx,PS=dat_annual$ps)
   overlap_wt[c("ato","CI")]
   
   #-- Balance diagnostics for ATO
   balance[i,,"OverlapWeight"] = bal.table(dx.wts(overlap_wt$w , dat_annual , treat.var = "Tx" , 
                                                   vars=c("totOpTime" , "HeatInput" , "pctCapacity" ,"Phase2", "avgNOxControls", "coal_no_scrubber" , "coal_with_scrubber"),
                                                   estimand="ATE" , x.as.weights = T))[[2]][,"std.eff.sz"]
   
   
   #-- PPTA Estimate
   X = model.matrix(psmod)[, -1]
   out.design = PPTA.design(A=dat_annual$Tx,X=X,M=1000)
   out.analysis = PPTA.analysis(Y=dat_annual$Outcome,out=out.design,Qburn=100,Q=1000,outcome.dist="gaussian")
   
   cond.means = apply(out.analysis$mcmc[,2,], 2, mean) #M conditional means
   out.ppta = c(mean(cond.means),quantile(out.analysis$mcmc[,2,], c(0.025, 0.975)))
   
   #-- Balance diagnostics for PPTA
   if (T){
      S = out.design$S
      S[is.na(S)] = 0
      temp2 = apply(S,2,function(x) bal.table(dx.wts(1*x , dat_annual , treat.var = "Tx" ,
                                                        vars=c("totOpTime" , "HeatInput" , "pctCapacity" ,"Phase2", "avgNOxControls", "coal_no_scrubber" , "coal_with_scrubber"),
                                                        estimand="ATE" , x.as.weights = T))[[2]][,"std.eff.sz"] )
      balance[i,,"PPTA"] = rowMeans(temp2,na.rm=T)
   }
   
   results["IPTW",,i] = IPW
   results["IPTWt50",,i] = IPWt50
   results["Crump",,i] = c(crum$est,crum$CI)
   results["OverlapWeight",,i] = c(overlap_wt$ato,overlap_wt$CI)
   results["PPTA",,i] = out.ppta
   
   print(paste("Year", years[i], "completed."))
}

## Evaluate Balance
balance[,,"Unweighted"]

## reshape for easy use with ggplot -- crude reshape by looping could be replaced
bal = data.frame()
for (i in 1:dim(balance)[1]){
   for (j in 1:dim(balance)[2]){
      for (k in 1:dim(balance)[3]){
         bal = rbind( bal , data.frame(Year=years[i] , Balance=balance[i,j,k] , Estimator=dimnames(balance)[[3]][k] , Covariate=dimnames(balance)[[2]][j]))
      }
   }
}
setDT(bal)

## mean absolute standardized difference
ggplot(bal[,list(Balance=mean(abs(Balance))),by=.(Estimator,Year)]) + geom_line(aes(y=Balance, x=Year ,  color=Estimator , group=interaction(Estimator)))

## maximum absolute standardized difference
ggplot(bal[,list(Balance=max(abs(Balance))),by=.(Estimator,Year)]) + geom_line(aes(y=Balance, x=Year ,  color=Estimator , group=interaction(Estimator)))

## Compare sample sizes and treatment prev over time
sample_sizes
range(sample_sizes[, "Total"])
tx_prev = sample_sizes[, "Treated"]/sample_sizes[, "Total"]
tx_prev
range(tx_prev)

## plot of estimates and CIs over time
results_dat = adply(results, c(1,3))
names(results_dat) = c("Method", "year","Est", "Lower", "Upper")

p = ggplot(data=results_dat, aes(x=year, y=Est, group=Method, color=Method)) + geom_point() + geom_line()
p = p + geom_ribbon(aes(ymin = results_dat$Lower, ymax = results_dat$Upper, group=results_dat$Method), linetype=2, alpha=0.1)
p







