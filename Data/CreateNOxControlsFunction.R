### - This funciton takes a unit-level data set and creates several indicators denoting the presence/absence
### - of various Nox control strategies.

NOxcontroltechnologies = function(dat){
dat$SCR = FALSE
dat$SCR[dat$NOx.Scrub1 == "Selective Catalytic Reduction" | 
          dat$NOx.Scrub2 == "Selective Catalytic Reduction" |  
          dat$NOx.Scrub3 == "Selective Catalytic Reduction" |
          dat$NOx.Scrub4 == "Selective Catalytic Reduction"] = TRUE
print("Variable Created: SCR")

dat$SNCR = FALSE
dat$SNCR[dat$NOx.Scrub1 == "Selective Non-catalytic Reduction" | 
           dat$NOx.Scrub2 == "Selective Non-catalytic Reduction" |  
           dat$NOx.Scrub3 == "Selective Non-catalytic Reduction" |
           dat$NOx.Scrub4 == "Selective Non-catalytic Reduction"] = TRUE
print("Variable Created: SNCR")

dat$LowNOxBurner = FALSE
dat$LowNOxBurner[substr(dat$NOx.Scrub1, 1, 7) == "Low NOx" | 
                   substr(dat$NOx.Scrub2, 1, 7) == "Low NOx" | 
                   substr(dat$NOx.Scrub3, 1, 7) == "Low NOx" | 
                   substr(dat$NOx.Scrub4, 1, 7) == "Low NOx" ] = TRUE
print("Variable Created: LowNOxBurner")

dat$OverFire = FALSE
dat$OverFire[dat$NOx.Scrub1 == "Overfire Air" | 
               dat$NOx.Scrub2 == "Overfire Air" | 
               dat$NOx.Scrub3 == "Overfire Air" | 
               dat$NOx.Scrub4 == "Overfire Air" ] = TRUE
print("Variable Created: OverFire")

dat$Ammonia = FALSE
dat$Ammonia[dat$NOx.Scrub1 == "Ammonia Injection" | 
              dat$NOx.Scrub2 == "Ammonia Injection" | 
              dat$NOx.Scrub3 == "Ammonia Injection" | 
              dat$NOx.Scrub4 == "Ammonia Injection" ] = TRUE
print("Variable Created: Ammonia")

dat$CombustMod = FALSE
dat$CombustMod[dat$NOx.Scrub1 == "Combustion Modification/Fuel Reburning" | 
                 dat$NOx.Scrub2 == "Combustion Modification/Fuel Reburning" | 
                 dat$NOx.Scrub3 == "Combustion Modification/Fuel Reburning" | 
                 dat$NOx.Scrub4 == "Combustion Modification/Fuel Reburning" ] = TRUE
print("Variable Created: CombustMod")

dat$WaterInj = FALSE
dat$WaterInj[dat$NOx.Scrub1 == "Water Injection" | 
               dat$NOx.Scrub2 == "Water Injection" | 
               dat$NOx.Scrub3 == "Water Injection" | 
               dat$NOx.Scrub4 == "Water Injection" ] = TRUE
print("Variable Created: WaterInj")

dat$OtherNOx = FALSE
dat$OtherNOx[dat$NOx.Scrub1 == "Other" | 
               dat$NOx.Scrub2 == "Other" | 
               dat$NOx.Scrub3 == "Other" | 
               dat$NOx.Scrub4 == "Other" ] = TRUE
print("Variable Created: OtherNOx")

return(dat)
}
