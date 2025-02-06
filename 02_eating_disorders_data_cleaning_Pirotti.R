### Analysis eating disorders
### Perla Rivadeneyra
### Last Update: 10 AUGUST 2023

# 1. Import libraries
library(readr)
library(dplyr)
library(magrittr)
library(lubridate)
library(plyr)
library(terra)
library(sf)
library(sp)
library(tidyr)
library(ranger)
library(ggplot2)
library(ggfortify)
library(ggmap)

# 1. read from "01_getFromGEE"
data <- readRDS( "final.df.rds")

# 2. Birthday  to Date format
birth <- sub(' .*', '', data$data_nasci)
data$birthday <- as.Date(parse_date_time(birth,"ymd"))
class(data$birthday)

# 7. Extract Birth Year
birthYear <- as.numeric(substr(birth,0,4))
data$birthYear <- birthYear

# 8. Calculate nearest NDVI during childhood
data$YearsDiff <- data$yearNDVI - data$birthYear

data2 <- data #we create a copy with undeleted rows

# 10. We eliminate all negative differences, since they are NDVI's
# before the person was born
#  We eliminate all values > 12, according to a definition of
#  childhood age https://en.wikipedia.org/wiki/Child_development

data.f <- filter(data, YearsDiff > 0 , YearsDiff <= 12)

dd <- as.data.frame(table(data.f$yearNDVI, data.f$sensor))
names(dd)<- c("Year", "Sensor", "N")
dd$Year<- as.integer(as.character(dd$Year))


png("satAvailabilityCasesControls.png", width=4000, height=2500, res=300)
  p <- ggplot(dd,
              aes(x=Sensor, y=Year,
                  fill=Sensor, size=N )) +
    geom_point(alpha=0.2, shape=21) +
    scale_size(range = c(.1, 10), name="N. Cases") +
    theme_bw()
  print(p)

dev.off()

data.f.final <- data.f %>% dplyr::group_by(Tipo, codice, sensor) %>% dplyr::summarise(
  N.years = n(),
  NDVI.AVG.exposure.12years = mean(NDVI_max_mean),
  NDVI.SUM.exposure.12years = sum(NDVI_max_mean),
  lat = mean(lat),
  long = mean(long)

)

data.f.final.sf <- sf::st_as_sf(data.f.final, coords=c("long", "lat"), crs=4326 )

write_sf(data.f.final.sf, "padova_vignovo_stra_databasePiroti_CRS4326.gpkg")
writexl::write_xlsx(data.f.final, "padova_vignovo_stra_databasePiroti_CRS4326.xlsx")

data.f.final.codice <- data.f.final %>% dplyr::group_by(codice) %>% dplyr::summarize(  Tipo=paste(unique(Tipo), collapse=","),
                                                   NDVI.wAVG.exposure.12years = sum(NDVI.AVG.exposure.12years*N.years)/sum(N.years),
                                                   NDVI.wSUM.exposure.12years = sum(NDVI.SUM.exposure.12years*N.years)/sum(N.years)   )

writexl::write_xlsx(data.f.final.codice, "padova_vignovo_stra_databasePiroti_CRS4326_groupCodice.xlsx")

writexl::write_xlsx( list("sourceTable"=data.f, "groupbyCodice"=data.f.final.codice, "ungrouped"=data.f.final  ), "padova_vignovo_stra_databasePiroti_CRS4326_groupCodice.xlsx")

register_stadiamaps(key = "b74cc920-740f-4aac-9a81-816d80e0e997", write = TRUE)


bbox <- st_bbox(st_geometry(data.f.final.sf))
names(bbox)<-c("left","bottom","right","top")
ph_basemap <- get_stadiamap(bbox=bbox, zoom=9  )

data.f.final.sf$NDVI <-
  cut(data.f.final.sf$NDVI.AVG.exposure.12years, breaks = c(0.1, 0.2, 0.3, 0.4,
                                                            0.5, 0.6, 0.7,0.8))


png("map.png", width=2000, height=1500, res=300)

  p <- ggplot() + # ggmap(ph_basemap, alpha=0.2) +
    geom_sf(data=data.f.final.sf, aes( color=NDVI, fill=NDVI),
             inherit.aes = FALSE,
            shape=4, size=0.7) +
    scale_fill_viridis_d(option  = "turbo")+
    scale_color_viridis_d(option  = "turbo") +
    theme_bw()
  print(p)

dev.off()


#st_write(shape, "C:/Users/perla/Desktop/Project _work/padova_vignovo_stra_database5_CRS3263.shp",
#         append = FALSE)

# ADD date to begin of symptoms to calculate survival time
shapeSub <- subset(shape, select = c(origin, NDVI, GDP, CO2, dist_hosp))
class(shapeSub)

set.seed(222)
ind <- sample(2, nrow(shapeSub), replace = TRUE, prob = c(0.7, 0.3))
trainSub <- shapeSub[ind==1,]
testSub <- shapeSub[ind==2,]


originF <- as.data.frame(factor(shape$origin))
names(originF)[1] <- "origin"
originTest2 <- originF$origin
originTrain <- as.data.frame(originF[ind==1,])
names(originTrain)[1] <- "origin"
originTest <- as.data.frame(originF[ind==2,])
names(originTest)[1] <- "origin"


shapeSub2 <- subset(shape, select = c(NDVI, GDP, CO2, dist_hosp))
shapeSub2Train <- as.data.frame(shapeSub2[ind==1,])
shapeSub2Test <- as.data.frame(shapeSub2[ind==2,])
