# 1. Import libraries
library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggfortify)
library(survival)
library(survminer)
library(sf)
library(sp)
library(magrittr)
library(lubridate)
library(plyr)
library(raster)
library(ranger)

rm(list = ls())

setwd("C:/Users/perla/Desktop/Project _work")
shape <- read_sf(dsn = ".", layer = "padova_vignovo_stra_database5_CRS3263")

# Clean city names
shape$citta <- gsub('[^[:alnum:] ]','',shape$citta)
unique(shape$citta)
shape$citta[shape$citta == "Masera' Di Padova"] <- 'Masera Di Padova'
shape$citta[shape$citta == "MaserÃÆÃâÃâÃÂ di Padova"] <- 'Masera Di Padova'
shape$citta[shape$citta == "padova"] <- 'Padova'
shape$citta[shape$citta == "PAdova"] <- 'Padova'
shape$citta[shape$citta == "Sant'Angelo di Piove di Sacco"] <- 'Sant Angelo di Piove di Sacco'
shape$citta[shape$citta == "Ponte San NicolÃ?Æ?Ã?â??Ã?â??Ã?Â²"] <- 'Ponte San Nicola'
shape$citta[shape$citta == "Ponte San NicolÃÆÃÂÃâÃâÂÃâÃÂÃÆÃÂÃÂÃâÂÃÂÃâÃÂ"] <- 'Ponte San Nicola'

# Birthday  to Date format
birth <- sub(' .*', '', shape$data_nasci)
shape$birthday2 <- as.Date(parse_date_time(birth,"ymd"))
class(shape$birthday2)

# Begin of ED
begin <- sub('T.*', '', shape$data_prima)
shape$begin_symptoms <- as.Date(parse_date_time(begin,"ymd"))
class(shape$begin_symptoms)
shape <- shape %>% mutate(begin_symptoms = if_else(is.na(begin_symptoms),
                    ymd("2022-03-23"),begin_symptoms))


# Days from birth until first symptoms
shape$time_survival <- shape$begin_symptoms - shape$birthday2
shape$time_survival <- as.numeric(shape$time_survival)

# *************** Filtered DATABASE *******************
# Birthday  to Date format
shapeFilt <- filter(shape, birthYear >= 1985) # if not already loaded
birth <- sub(' .*', '', shapeFilt$data_nasci)
shapeFilt$birthday2 <- as.Date(parse_date_time(birth,"ymd"))
class(shapeFilt$birthday2)

table(shapeFilt$origin)

# Check if we have no NA's in survival time
sum(is.na(shapeFilt$time_survival))

# Cox regression (univariate)
# formula: is linear model with a survival object as the response variable.
# Survival object is created using the function Surv() as follow: Surv(time, event)



reg.cox1 <- coxph(Surv(time_survival, origin) ~ scale(NDVI), data = shapeFilt)
reg.cox1
summary(reg.cox1)


reg.cox2 <- coxph(Surv(time_survival, origin) ~ scale(NDVI) +
                    scale(GDP) +
                    scale(CO2) +
                    scale(birthYear) +
                    scale(dist_hosp) +
                    scale(elevation) +
                    as.factor(citta) +
                    scale(pop_mean),
                  data = shapeFilt)
reg.cox2
summary(reg.cox2)
table(shapeFilt$origin)
table(shapeFilt$citta)

reg.cox3 <- coxph(Surv(time_survival, origin) ~ scale(NDVI) +
                    scale(GDP) +
                    scale(CO2) +
#                    scale(pop_mean) +
                    scale(elevation) +
                    scale(dist_hosp),
                  data = shapeFilt)
reg.cox3
summary(reg.cox3)

autoplot(reg.cox3)

table(shapeFilt$origin)

# Power test
library(pwr)
pwr.t.test(d = 0.5, sig.level = 0.05, power = 0.80)

# Graphical representation of NDVI means among groups

ggplot(shapeFilt, aes(x = log(birthYear))) +
  geom_histogram(fill = "white", colour = "black", bins = 100) +
  facet_grid(origin~ .)

