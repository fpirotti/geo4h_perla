library(rgee)
library(dplyr)
library(ggplot2)

# ee_Initialize(quiet = T)
ee_Initialize(user = 'cirgeo' )

options(gargle_verbosity = "debug")
options(gargle_email= "cirgeo@unipd.it")
googledrive::drive_auth(email="cirgeo@unipd.it")


downloadGDrive <- function(task_vector, outpath="./perla.json"){
  bv <- ee$batch$Task$status(task_vector)

  gd_folder <- basename(bv[["destination_uris"]])
  gd_ExportOptions <- task_vector[["config"]][["fileExportOptions"]]
  gd_filename <- gd_ExportOptions[["driveDestination"]][["filenamePrefix"]]
  files_gd <- try(googledrive::drive_find(q = sprintf("'%s' in parents",
                                                      gd_folder), q = sprintf("name contains '%s'", gd_filename)),
                  silent = TRUE)
  if(file.exists(outpath)){

    user_input <- readline( sprintf("Are you sure you want to overwrite path %s? (y/n)  ", outpath ))
    if(user_input != 'y') stop('Exiting since you did not press y')
  }
  if(nrow(files_gd)>1){
    warning("%d files found, will only download the first one!",  )
  }
  googledrive::drive_download(file = files_gd[1,] , path =outpath, overwrite = TRUE)
}

getCentroids <- ee_utils_pyfunc(function(feature) {
  ee$feature$set({
    polyCent:feature.centroid()
  })
})

bufferPoly <- ee_utils_pyfunc(function(feature) {
  feature$buffer(bufferDistance)
})

join_filter <- ee$Filter$equals(
   leftField= 'system:index',
  rightField= 'system:index'
)

simpleJoin <- ee$Join$inner()




### GRAB THE DATA -----
padova <-
  ee$FeatureCollection("projects/ee-perlarivadeneyra/assets/Padova_Vigonovo_corrected_WGS89")


bufferDistance <- 1000L
cases <-
  ee$FeatureCollection("projects/ee-perlarivadeneyra/assets/cases_geocoded_with_ARCGIS_HERE")$map(bufferPoly)
controls <-
  ee$FeatureCollection("projects/ee-perlarivadeneyra/assets/controls_geocoded_with_ARCGIS_HERE")$map(bufferPoly)

## here we buffer 2 km to make sure to catch cloud and cloud shadows
## ... since we add to previous buffer, we have to get the new buffer distance
## from the margin of the buffer area
if(bufferDistance < 2000L){
  bufferDistance <- 2000L - bufferDistance
  cases4clouds <- cases$map(bufferPoly)
  controls4clouds  <-  controls$map(bufferPoly)
} else {
  cases4clouds <- cases
  controls4clouds  <-  controls
}


l1 <- ee$ImageCollection("LANDSAT/LM01/C02/T2")$filterBounds(padova)
l2 <- ee$ImageCollection("LANDSAT/LM02/C02/T2")$filterBounds(padova)
l3 <- ee$ImageCollection("LANDSAT/LM03/C02/T2")$filterBounds(padova)


## there seems to be no TIER 1 data
# l1 <- ee$ImageCollection("LANDSAT/LM01/C02/T1")$filterBounds(padova)
# l2 <- ee$ImageCollection("LANDSAT/LM02/C02/T1")$filterBounds(padova)
# l3 <- ee$ImageCollection("LANDSAT/LM03/C02/T1")$filterBounds(padova)
l5 <- ee$ImageCollection("LANDSAT/LT05/C02/T1_TOA")$filterBounds(padova)
l7 <- ee$ImageCollection("LANDSAT/LE07/C02/T1_TOA")$filterBounds(padova)
l8 <- ee$ImageCollection("LANDSAT/LC08/C02/T1_TOA")$filterBounds(padova)


### function to do things for the stack of yearly imagery

processYear <- ee_utils_pyfunc(function(year) {

  getNDVI  <-    ee_utils_pyfunc(function(image) {
    nirb <-  image$select(nir)
    redb <-  image$select(red)
    qa <-  image$select(qaBand)

    cloud <-
      qa$bitwiseAnd(bitwShiftL(1L, 5L))$Or(qa$bitwiseAnd(bitwShiftL(1L, 4L)))$Or(qa$bitwiseAnd(bitwShiftL(1L, 7L)))$Or(qa$bitwiseAnd(bitwShiftL(1L, 3L)))

    noRefl <- nirb$lt(1)$Or(redb$lt(1))

    fin <-
      ee$Image(
        nirb$subtract(redb)$divide(nirb$add(redb))$rename('NDVI')
      )

    fin <- fin$updateMask(fin$select("NDVI")$gt(0L))$updateMask(cloud$Not() )$updateMask(noRefl$Not() )

    return(fin)

  })

  ### MSS SENSOR RED AND NIR BAND DESIGNATION ----
  nir <- ee$String("B7")
  red <- ee$String("B5")
  qaBand <- ee$String("QA_PIXEL")

  date <- ee$Date(sprintf("%d-01-01", year) )

  l1.fd <-
    l1$filterDate(
      ee$Number(year)$format("%d")$cat('-03-21'),
      ee$Number(year)$format("%d")$cat('-10-21')
    )$map(getNDVI)$max()$set('system:time_start', date$millis())

  l2.fd <-
    l2$filterDate(
      ee$Number(year)$format("%d")$cat('-03-21'),
      ee$Number(year)$format("%d")$cat('-10-21')
    )$map(getNDVI)$max()$set('system:time_start', date$millis())

  l3.fd <-
    l3$filterDate(
      ee$Number(year)$format("%d")$cat('-03-21'),
      ee$Number(year)$format("%d")$cat('-10-21')
    )$map(getNDVI)$max()$set('system:time_start', date$millis())
#
#    ll.final <- l1.fd$merge(l2.fd)$merge(l3.fd)
#    ll.final

   nir <- ee$String("B4")
   red <- ee$String("B3")


   l5.fd <-
     l5$filterDate(
       ee$Number(year)$format("%d")$cat('-03-21'),
       ee$Number(year)$format("%d")$cat('-10-21')
     )$map(getNDVI)$max()$set('system:time_start', date$millis())


   l7.fd <-
     l7$filterDate(
       ee$Number(year)$format("%d")$cat('-03-21'),
       ee$Number(year)$format("%d")$cat('-10-21')
     )$map(getNDVI)$max()$set('system:time_start', date$millis())

   l8.fd <-
     l8$filterDate(
       ee$Number(year)$format("%d")$cat('-03-21'),
       ee$Number(year)$format("%d")$cat('-10-21')
     )$map(getNDVI)$max()$set('system:time_start', date$millis())



   ll.final <- ee$ImageCollection(list(l1.fd,l2.fd,l3.fd,
                                  l5.fd,l7.fd,l8.fd) )$max() #l1.fd$merge(l2.fd)#$merge(l3.fd)$merge(l5.fd)$merge(l7.fd)$merge(l8.fd)

   im <- ee$Image(ll.final)
   return()

   #$filter(ee$Filter$gt('stdDev', 0))$filter(ee$Filter$lt('Clouds', 1))

})



  from <- 1972
    to <- 1977
  # ndviStack <-  ee$ImageCollection(  ee$FeatureCollection( ee$List$sequence(from, to)$map(processYear)  )$flatten() )$toBands()
  ndviStack <-  ee$ImageCollection(   ee$List$sequence(from, to)$map(processYear)    )#$toBands()
  ndviStack$getInfo()
  ndviStack <-  ndviStack$rename( sprintf("NDVI%d", from:to) )


  ff <- ndviStack$reduceRegions(
    collection = controls, #$filter(ee$Filter$eq('codice', 'berval')),
    reducer = ee$Reducer$sum(),
    scale = 30L
  )

  ff$getInfo()

  ff <- fin$reduceRegions(
    collection = ff, # $filter(ee$Filter$eq('codice', 'berval')),
    reducer = ee$Reducer$count()$setOutputs( list("NDVIcells") ),
    scale = 30L
  )
  ff <- fin$select("NDVI")$reduceRegions(
    collection = ff,
    reducer = ee$Reducer$stdDev()$setOutputs( list("NDVIstdDev") ),
    scale = 30L
  )

  ff_clouds <-
    ee$Image(cloud)$addBands(ee$Image(1))$rename('CloudsNCellsWithClouds', "CloudsNTotCells")$reduceRegions(
      collection = controls4clouds,
      reducer = ee$Reducer$sum(),
      scale = 30L
    )

  ffJoined <- simpleJoin$apply(ff, ff_clouds, join_filter);

  ffJoined$map(ee_utils_pyfunc(function(ffIn) {

    newf <- ee$Feature(ffIn$get('primary'))$copyProperties(ffIn$get('secondary'), list('CloudsNCellsWithClouds', "CloudsNTotCells" ));
    newf <-
      newf$set(
        list(
          "Sensor" =  ee$Image(image)$id()$slice(2,4),
          "Timestamp" = ee$Image(image)$date()$format('YYYY-MM-dd HH:mm:ss'),
          "Path" = ee$Image(image)$get('WRS_PATH'),
          "Row" = ee$Image(image)$get('WRS_ROW')
        )
      )
    ee$Feature(newf)$setGeometry(NULL)
  }))


ndviStack$first()$getInfo()

task_vector <- ee_table_to_drive(
  collection = ndviStack,
  fileFormat = "GEO_JSON",
  timePrefix = TRUE,
  fileNamePrefix = "perla"
)
task_vector$start()
ee_monitoring(task_vector) # optional

downloadGDrive(task_vector)

all <- jsonlite::read_json("perla.json")

df <- na.omit( data.table::rbindlist(lapply(all$features, function(x) {

  x$properties
}), fill = TRUE) )

df$Sensor <- factor(sprintf("LS%s", df$Sensor))
df$Timestamp <- as.POSIXct(df$Timestamp)
df$Year <- lubridate::year(df$Timestamp)
df$Date <- as.Date(df$Timestamp)
df$codice <- as.factor(df$codice)
df$Row <- as.factor(df$Row)
df$Path <- as.factor(df$Path)
df$NDVI <- df$NDVIsum / df$NDVIcells

ggplot(df, aes(x=Year, fill=Sensor)) + geom_bar() + theme_bw()

df.filtered <- df %>% filter(CloudsNCellsWithClouds==0, NDVI < 1)

png("boxplot.png", width=3000, height=3000, res=300)
 p <- ggplot(df.filtered, aes(y=NDVI, x=Sensor, color=paste(Row,Path)) ) + geom_boxplot() + theme_bw()
 print(p)
dev.off()

png("lineplot.png", width=4000, height=2500, res=300)
  p <- ggplot(df.filtered, aes(x=Timestamp, y=NDVI, shape=Row,
                               group=Sensor, color=Sensor)) +
    geom_point(alpha=0.3) +
    theme_bw()
  print(p)
dev.off()



df.filtered.grouped <- df.filtered %>% dplyr::group_by(Sensor, Year, codice) %>% summarise(
  NDVI.mean = mean(NDVI),
  NDVI.sd = sd(NDVI),
  NDVI.num = n()
)
