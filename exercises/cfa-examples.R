
library(dplyr)
library(lavaan)

################################################################################
## data import
################################################################################
# Read the dataset from the CSV file and convert -999 to NA
mydata <- read.csv("C:/Users/whowar/Documents/GitHub/bcorp-2025/exercises/mydata.csv", na.strings = "-999")

################################################################################
## Example 1 -- Fixed-Factor Method
################################################################################
m1 <- 'SUP =~ NA*SUP1 + SUP2 + SUP3
       SUP ~~ 1*SUP'

fit1 <- cfa(m1, data=mydata, std.lv=TRUE)
summary(fit1, standardized=TRUE, fit.measures=TRUE, rsquare=TRUE)


################################################################################
## Example 1.2 -- Marker-Variable Method (first variable)
################################################################################
mod42 <- 'Positive =~ Great + Cheerful + Happy'

fit42 <- cfa(mod42, sample.cov=dat, sample.nobs = nObs)

summary(fit42, standardized=T, fit.measures=T)

################################################################################
## Example 1.2.2 -- Marker-Variable Method (third variable)
################################################################################
mod44 <- 'Positive =~ NA*Great + NA*Cheerful + 1*Happy'

fit44 <- cfa(mod44, sample.cov=dat, sample.nobs = nObs)

summary(fit44, standardized=T, fit.measures=T)

################################################################################
## Example 1.3 -- Marker-Variable Method (second variable)
################################################################################
mod43 <- 'Positive =~ NA*Great + 1*Cheerful + Happy'

fit43 <- cfa(mod43, sample.cov=dat, sample.nobs = nObs)

summary(fit43, standardized=T, fit.measures=T)


################################################################################
## Example 1.4 -- Effects-Coding Method
################################################################################

mod45 <- 'Positive =~ L1*Great + L2*Cheerful + L3*Happy
					L1 == 3 - L2 - L3
					'

fit45 <- lavaan(mod45, sample.cov=dat, sample.nobs = nObs, std.lv=F, auto.var=T, auto.fix.first=F, auto.cov.lv.x=T, int.ov.free=T)

summary(fit45, standardized=T, fit.measures=T)
