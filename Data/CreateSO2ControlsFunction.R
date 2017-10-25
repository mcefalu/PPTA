### - This funciton takes a unit-level data set and creates several indicators denoting the presence/absence
### - of various SO2 control strategies.

SO2controltechnologies = function(dat){
  dat$DryLimeFGD = FALSE
  dat$DryLimeFGD[(dat$SO2.Scrub1 == "Dry Lime FGD") | 
                   (dat$SO2.Scrub2 %in% "Dry Lime FGD") |  
                   (dat$SO2.Scrub3 %in% "Dry Lime FGD") | 
                   (dat$SO2.Scrub4 %in% "Dry Lime FGD")] = TRUE
  print("Variable Created: DryLimeFGD")
  #with(dat, mean(DryLimeFGD))
  
  dat$DrySorbInj = FALSE
  dat$DrySorbInj[(dat$SO2.Scrub1 == "Dry Sorbent Injection") | 
                   (dat$SO2.Scrub2 %in% "Dry Sorbent Injection") |  
                   (dat$SO2.Scrub3 %in% "Dry Sorbent Injection") | 
                   (dat$SO2.Scrub4 %in% "Dry Sorbent Injection")] = TRUE
  print("Variable Created: DrySorbInj")
  #with(dat, mean(DrySorbInj))
  
  dat$DualAlk = FALSE
  dat$DualAlk[(dat$SO2.Scrub1 == "Dual Alkali") | 
                (dat$SO2.Scrub2 %in% "Dual Alkali") |  
                (dat$SO2.Scrub3 %in% "Dual Alkali") | 
                (dat$SO2.Scrub4 %in% "Dual Alkali")] = TRUE
  print("Variable Created: DualAlk")
  #with(dat, mean(DualAlk))
  
  dat$FluidizedBed = FALSE
  dat$FluidizedBed[(dat$SO2.Scrub1 == "Fluidized Bed Limestone Injection") | 
                     (dat$SO2.Scrub2 %in% "Fluidized Bed Limestone Injection") |  
                     (dat$SO2.Scrub3 %in% "Fluidized Bed Limestone Injection") | 
                     (dat$SO2.Scrub4 %in% "Fluidized Bed Limestone Injection")] = TRUE
  print("Variable Created: FluidizedBed")
  #with(dat, mean(FluidizedBed))

  
  dat$MagOx = FALSE
  dat$MagOx[(dat$SO2.Scrub1 == "Magnesium Oxide") | 
              (dat$SO2.Scrub2 %in% "Magnesium Oxide") |  
              (dat$SO2.Scrub3 %in% "Magnesium Oxide") | 
              (dat$SO2.Scrub4 %in% "Magnesium Oxide")] = TRUE
  print("Variable Created: MagOx")
  #with(dat, mean(MagOx))
  
  dat$SodiumBased = FALSE
  dat$SodiumBased[(dat$SO2.Scrub1 == "Sodium Based") | 
                    (dat$SO2.Scrub2 %in% "Sodium Based") |  
                    (dat$SO2.Scrub3 %in% "Sodium Based") | 
                    (dat$SO2.Scrub4 %in% "Sodium Based")] = TRUE
  print("Variable Created: SodiumBased")
  #with(dat, mean(SodiumBased))
  
  dat$WetLimeFGD = FALSE
  dat$WetLimeFGD[(dat$SO2.Scrub1 == "Wet Lime FGD") | 
                   (dat$SO2.Scrub2 %in% "Wet Lime FGD") |  
                   (dat$SO2.Scrub3 %in% "Wet Lime FGD") | 
                   (dat$SO2.Scrub4 %in% "Wet Lime FGD")] = TRUE
  print("Variable Created: WetLimeFGD")
  #with(dat, mean(WetLimeFGD))
  
  dat$WetLime = FALSE
  dat$WetLime[(dat$SO2.Scrub1 == "Wet Limestone") | 
                (dat$SO2.Scrub2 %in% "Wet Limestone") |  
                (dat$SO2.Scrub3 %in% "Wet Limestone") | 
                (dat$SO2.Scrub4 %in% "Wet Limestone")] = TRUE
  print("Variable Created: WetLime")
  #with(dat, mean(WetLime))
  
  dat$OtherSO2 = FALSE
  dat$OtherSO2[(dat$SO2.Scrub1 == "Other") | 
                 (dat$SO2.Scrub2 %in% "Other") |  
                 (dat$SO2.Scrub3 %in% "Other") | 
                 (dat$SO2.Scrub4 %in% "Other")] = TRUE
  print("Variable Created: OtherSO2")
  #with(dat, mean(OtherSO2))
  
  return(dat)
}