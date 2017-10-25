## -----             Define Categories of Coal Based on Sulfur Content        ------ ##

## -- Data comes from EIA_767: http://www.eia.gov/survey/form/eia_767/instructions_form.pdf:
## -- Sulfur Content and Ash Content, columns (d) and (e), (i) and (j), report content to nearest 0.01 percent for sulfur and the nearest 0.1 percent for ash. 

## --- From John Bachman on 8/28/2014: 
## ------ Looking up some numbers quickly on the net, I found this:  "Subbituminous Wyoming 
## ------ coal is only 0.35 percent sulfur by weight, while Kentucky coal is 1.59 percent sulfur.”  
## ------ I certainly recall Indiana, West Va coals in the 2% range, and that western coal number sounds right.

## --- From Barrett on 9/3/2014:
## ------ A Feb 1993 Report from EIA (Appendix B Table C1) creates categories: 
## ------ <=0.60 (Low Sulfur), 0.61 - 1.67 (Medium Sulfur), >=1.68 (High Sulfur)

CreateSulfurCategories = function(dat, sulfurvar){
  SulfurCatCuts = c(0, 0.60, 1.67, max(sulfurvar, na.rm = TRUE))
  dat$SulfurCat = as.numeric(cut(sulfurvar, breaks = SulfurCatCuts, right = TRUE, include.lowest = TRUE))
  print("Variable Created: SulfurCat, equal to 1,2,3 for Low,Medium,High Sulfur Coal, respectively.")
  
  dat[, LowScoal := SulfurCat==1]
  print("Variable Created: LowScoal, indicator that SulfurCat==1")
  dat[, MedScoal := SulfurCat==2]
  print("Variable Created: MedScoal, indicator that SulfurCat==2")
  dat[, HighScoal := SulfurCat==3]
  print("Variable Created: LowScoal, indicator that SulfurCat==3")

  return(dat)
}