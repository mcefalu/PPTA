library(data.table)
library(maps)
library(stringr)
source('CreateSO2ControlsFunction.R')
source('CreateNOxControlsFunction.R')

######## --------  Read in Data that is monthly emissions data merged with other stuff ------ #####
dat = fread("AMPD_Unit_with_Sulfur_Content_and_Regulations_with_Facility_Attributes.csv") # File located on dataverse
dat$uID = paste(dat$Facility.ID..ORISPL., dat$Unit.ID, sep = "_")
dat$FacID = dat$Facility.ID..ORISPL.
dat$Year = as.numeric(dat$Year)
dat$Month = as.numeric(dat$Month)
dat[, year_month := paste(Year, Month, sep="_")]
setkeyv(dat, c("uID", "Year", "Month"))
setorderv(dat, c("uID", "Year", "Month"))
dim(dat)
dat <- unique(dat)
dim(dat)


#       ----------- Define and Group SO2 and NOx Control Technologies ------------------       #
## -- Define SO2 Control Strategies at the Unit Level
dat = SO2controltechnologies(dat)

## ---  Group scrubbing technologies together
# ----- this grouping determined blessed by John Bachmann via email on 9/1/2015
dat$SO2scrub = FALSE
dat$SO2scrub = (apply(dat[, c("DryLimeFGD", "DrySorbInj", "DualAlk", "MagOx", 
                             "SodiumBased", "WetLimeFGD", "WetLime"), with=FALSE], 1, sum) >=1)
with(dat, mean(SO2scrub))

dat$NonScrubberSO2control = FALSE
dat$NonScrubberSO2control = (apply(dat[, c("OtherSO2", "FluidizedBed"), with=FALSE], 1, sum) >=1)
with(dat, mean(NonScrubberSO2control))

dat$AnySO2control = FALSE
dat$AnySO2control = (apply(dat[, c("SO2scrub", "NonScrubberSO2control"), with=FALSE], 1, sum) >=1)
with(dat, mean(AnySO2control))

dat$NumSO2Controls = 0
dat$NumSO2Controls = apply(dat[, c("DryLimeFGD", "DrySorbInj", "DualAlk", "MagOx", 
                                   "SodiumBased", "WetLimeFGD", "WetLime", "OtherSO2", "FluidizedBed"), with = FALSE], 1, sum, na.rm = TRUE)
with(dat, table(NumSO2Controls))


## -- Define NOx Control Strategies at the Unit Level
dat = NOxcontroltechnologies(dat)

## -- Group NOx Controls together

## --- Suggested grouping: (SCR, SNCR), (LowNOxBurner), (All others)
dat$S_n_CR = apply(dat[, c("SCR", "SNCR"), with = FALSE], 1, sum, na.rm = TRUE)
with(dat, mean(S_n_CR))

dat$OtherNOxControl = apply(dat[, c("OverFire", "Ammonia", "CombustMod", "WaterInj", "OtherNOx"), with = FALSE], 1, sum, na.rm = TRUE)
with(dat, mean(OtherNOxControl))

dat$NumNOxControls = 0
dat$NumNOxControls = apply(dat[, c("SCR", "SNCR", "LowNOxBurner", "OverFire", "Ammonia", "CombustMod", "WaterInj", "OtherNOx"), with = FALSE], 1, sum, na.rm = TRUE)
with(dat, table(NumNOxControls))

#       -----------     Define Efficiency Variables      ------------------       #
## -- Define Operating Capacity as the maximum number of hours per month times the Max Hourly Heat Input Rate
maxopp = with(dat, tapply(Operating.Time, year_month, max, na.rm = TRUE))## maxopp is the max total number of hours operating time for each of 12 months
### Max.Hourly.HI.Rate..MMBtu.hr. is the heat input capacity (MMBtu/hour)
for (i in 1:length(maxopp))
  dat[year_month == names(maxopp)[i], Capacity:= maxopp[i]*Max.Hourly.HI.Rate..MMBtu.hr.] ##MMBtu/month


## -- Percent Capacity is Heat Input / Capacity
dat$PctCapacity = dat$Heat.Input..MMBtu / dat$Capacity
#with(dat, mean(PctCapacity>1.5, na.rm = TRUE)) ## 0.1% capacity >1.5
dat$PctCapacity[dat$PctCapacity > 1.5] = NA ## > 150% capacity

## -- Heat Rate: The amount of energy used to generate one kWh (e.g., BTU/kWh)
dat$HeatRate = (dat$Heat.Input..MMBtu./dat$Gross.Load..MW.h)  ## MMbtu/MWh
dat$HeatRate[dat$HeatRate==Inf] = NA
# with(dat, mean(HeatRate>20, na.rm = TRUE))  ## 0.5% have >20
dat$HeatRate[dat$HeatRate > 20] = NA

### --- OUTUPUT Unit Level Data
write.csv(dat, file = "AllEGUs.csv")

