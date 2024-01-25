library(rgee)
library(dplyr)
library(ggplot2)

# ee_Initialize(quiet = T)
ee_Initialize(user = 'cirgeo' )

options(gargle_verbosity = "debug")
options(gargle_email= "cirgeo@unipd.it")
googledrive::drive_auth(email="cirgeo@unipd.it")


from <- 1971L
to <- 2020L

downloadGDrive <- function(task_vector, outpath=NA){
  bv <- ee$batch$Task$status(task_vector)

  gd_folder <- basename(bv[["destination_uris"]])
  gd_ExportOptions <- task_vector[["config"]][["fileExportOptions"]]
  gd_filename <- gd_ExportOptions[["driveDestination"]][["filenamePrefix"]]

  if(is.na(outpath)) outpath<-gd_filename[[1]]

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
  outpath
}

reducers <- ee$Reducer$mean()$combine(
  reducer2 = ee$Reducer$stdDev(),
  sharedInputs = TRUE
)$combine(
  reducer2 = ee$Reducer$count(),
  sharedInputs = TRUE
)$combine(
  reducer2 = ee$Reducer$max(),
  sharedInputs = TRUE
)

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
# if(bufferDistance < 2000L){
#   bufferDistance <- 2000L - bufferDistance
#   cases4clouds <- cases$map(bufferPoly)
#   controls4clouds  <-  controls$map(bufferPoly)
# } else {
#   cases4clouds <- cases
#   controls4clouds  <-  controls
# }


l1 <- ee$ImageCollection("LANDSAT/LM01/C02/T2")$filterBounds(padova)
l2 <- ee$ImageCollection("LANDSAT/LM02/C02/T2")$filterBounds(padova)
l3 <- ee$ImageCollection("LANDSAT/LM03/C02/T2")$filterBounds(padova)

l5 <- ee$ImageCollection("LANDSAT/LT05/C02/T1_TOA")$filterBounds(padova)
l7 <- ee$ImageCollection("LANDSAT/LE07/C02/T1_TOA")$filterBounds(padova)
l8 <- ee$ImageCollection("LANDSAT/LC08/C02/T1_TOA")$filterBounds(padova)


### function to do things for the stack of yearly imagery

processLCollection <- ee_utils_pyfunc(function(landsatMissionCollection) {
  landsatMissionCollection <-  ee$ImageCollection(landsatMissionCollection)

  getNDVI  <-    ee_utils_pyfunc(function(image) {
    nirb <-  image$select(nir)
    redb <-  image$select(red)
    qa <-  image$select("QA_PIXEL")
    cloud <-
      qa$bitwiseAnd(bitwShiftL(1L, 5L))$Or(qa$bitwiseAnd(bitwShiftL(1L, 4L)))$Or(qa$bitwiseAnd(bitwShiftL(1L, 7L)))$Or(qa$bitwiseAnd(bitwShiftL(1L, 3L)))
    noRefl <- nirb$lt(1)$Or(redb$lt(1))
    fin <-  ee$Image(
        nirb$subtract(redb)$divide(nirb$add(redb))$rename('NDVI')
      )
    fin <- fin$updateMask(fin$select("NDVI")$gt(0L))$updateMask(cloud$Not() )#$updateMask(noRefl$Not() )
    return(fin)
  })

  processYear <- ee_utils_pyfunc( function(yeard){
    sensor <- ee$String("L")$cat(landsatMissionCollection$first()$id()$slice(2,4))
    ll <-landsatMissionCollection$filterDate(
      ee$Number(yeard)$format("%d")$cat('-03-21'),
      ee$Number(yeard)$add(1L)$format("%d")$cat('-10-21') )$filter(
        ee$Filter$calendarRange(4L, 10L, "month")
        )$map(getNDVI)$reduce(
          reducer= reducers
        )$set("system:time_start", ee$Date( ee$Number(yeard)$format("%d")$cat('-03-21') )$millis() )
    ll$set("nbands", ll$bandNames()$size() )$set("sensor", sensor)$set("year", yeard)$set("date", ee$Number(yeard)$format("%d")$cat('-03-21')  )
  } )
  ### MSS SENSOR RED AND NIR BAND DESIGNATION ----
  ### Convert to masked ndvi

  ### Aggregate per year and remove images with empty bands!!!!
  collColl <-ee$ImageCollection(
    ee$List$sequence(from, to)$map(processYear)
  )$filter( ee$Filter$gt("nbands", 0L) )

  l.fd  <- collColl$map( ee_utils_pyfunc( function(img){
    img$reduceRegions(
      reducer= reducers,
      collection = collection,
      scale = 30L
    )$map(ee_utils_pyfunc(function(ffIn) {
       newf <-
         ffIn$set(
          list(
            "year" = ee$Image(img)$get('year'),
            "sensor" = ee$Image(img)$get('sensor')
          )
        )
      ee$Feature(newf)$setGeometry(NULL)
    }))
  }))

  return(l.fd$flatten() )

})


  collection <- controls
  nir <- ee$String("B7")
  red <- ee$String("B5")
  ndviStack1 <- ee$FeatureCollection( ee$List(list(l1, l2, l3 ))$map(processLCollection) )$flatten()

  nir <- ee$String("B4")
  red <- ee$String("B3")
  ndviStack2 <- ee$FeatureCollection( ee$List(list(l5, l7, l8 ))$map(processLCollection) )$flatten()

  ndviStack <- ndviStack1$merge(ndviStack2)

  task_vector <- ee_table_to_drive(
    collection = ndviStack,
    fileFormat = "GEO_JSON",
    timePrefix = TRUE,
    fileNamePrefix = "controls"
  )
  task_vector$start()
  ee_monitoring(task_vector) # optional
  outpath.controls <- downloadGDrive(task_vector)




  collection <- cases
  nir <- ee$String("B7")
  red <- ee$String("B5")
  ndviStack1 <- ee$FeatureCollection( ee$List(list(l1, l2, l3 ))$map(processLCollection) )$flatten()

  nir <- ee$String("B4")
  red <- ee$String("B3")
  ndviStack2 <- ee$FeatureCollection( ee$List(list(l5, l7, l8 ))$map(processLCollection) )$flatten()

  ndviStack <- ndviStack1$merge(ndviStack2)

  task_vector2 <- ee_table_to_drive(
    collection = ndviStack,
    fileFormat = "GEO_JSON",
    timePrefix = TRUE,
    fileNamePrefix = "cases"
  )
  task_vector2$start()
  ee_monitoring(task_vector2) # optional
  outpath.cases <- downloadGDrive(task_vector2)

  dfoutput <- list(controls=outpath.controls, cases=outpath.cases)

  for(i in list(cases, controls))

  all <- jsonlite::read_json(outpath)

  df <- na.omit( data.table::rbindlist(lapply(all$features, function(x) {
    x$properties
  }), fill = TRUE) )

  df$sensor <- as.factor( df$sensor)
  df$codice <- as.factor(df$codice)

  wc<-grep("_count$", names(df))
  keep <- as.data.frame(df)[ ,  names(df)[wc[[1]]] ]
  df[, wc]<-NULL
  df$areaCovered <- keep / max(keep)




  table(as.integer(df$areaCovered*10))

table(df$year, df$sensor)

df.g <- df %>% group_by(year, sensor) %>% summarise()

ggplot(df %>% group_by(year, sensor), aes(x=year, y=sensor, color=NDVI_max_mean, group=codice)) +
  geom_point() + theme_bw()

ggplot(df , aes(y=NDVI_count_max, x=year, color=sensor, group=sensor)) +
  geom_point(position = "jitter", alpha=0.7 ) + ylab("N. pixels") + theme_bw()

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
