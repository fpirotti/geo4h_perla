library(rgee)
library(rgeeExtra)

bufferDistance <- 1000L

# ee_Initialize(quiet = T)
ee_Initialize(user = 'cirgeo' )

options(gargle_verbosity = "debug")
options(gargle_email= "cirgeo@unipd.it")
googledrive::drive_auth(email="cirgeo@unipd.it")


downloadGDrive <- function(task_vector, outpath="./perla.json"){
  gd_folder <- basename(ee$batch$Task$status(task_vector)[["destination_uris"]])
  gd_ExportOptions <- task_vector[["config"]][["fileExportOptions"]]
  gd_filename <- gd_ExportOptions[["driveDestination"]][["filenamePrefix"]]
  files_gd <- try(googledrive::drive_find(q = sprintf("'%s' in parents",
                                                      gd_folder), q = sprintf("name contains '%s'", gd_filename)),
                  silent = TRUE)
  if(file.exists(outpath)){

    user_input <- readline( sprintf("Are you sure you want to overwrite path %s? (y/n)  ", outpath ))
    if(user_input != 'y') stop('Exiting since you did not press y')
  }
  googledrive::drive_download(file = files_gd , path =outpath, overwrite = TRUE)

}

getCentroids <- ee_utils_pyfunc(function(feature) {
  ee$feature$set({
    polyCent:feature.centroid()
  })
})

bufferPoly <- ee_utils_pyfunc(function(feature) {
  feature$buffer(bufferDistance)
})


### GRAB THE DATA -----
padova <-
  ee$FeatureCollection("projects/ee-perlarivadeneyra/assets/Padova_Vigonovo_corrected_WGS89")
cases <-
  ee$FeatureCollection("projects/ee-perlarivadeneyra/assets/cases_geocoded_with_ARCGIS_HERE")$map(bufferPoly)
controls <-
  ee$FeatureCollection("projects/ee-perlarivadeneyra/assets/controls_geocoded_with_ARCGIS_HERE")$map(bufferPoly)

l1 <- ee$ImageCollection("LANDSAT/LM01/C02/T2")$filterBounds(padova)
l2 <- ee$ImageCollection("LANDSAT/LM02/C02/T2")$filterBounds(padova)
l3 <- ee$ImageCollection("LANDSAT/LM03/C02/T2")$filterBounds(padova)
## there seems to be no TIER 1 data
# l1 <- ee$ImageCollection("LANDSAT/LM01/C02/T1")$filterBounds(padova)
# l2 <- ee$ImageCollection("LANDSAT/LM02/C02/T1")$filterBounds(padova)
# l3 <- ee$ImageCollection("LANDSAT/LM03/C02/T1")$filterBounds(padova)
l5 <-
  ee$ImageCollection("LANDSAT/LT05/C02/T1_TOA")$filterBounds(padova)
l7 <-
  ee$ImageCollection("LANDSAT/LE07/C02/T1_TOA")$filterBounds(padova)
# l8 <- ee$ImageCollection("LANDSAT/LC08/C02/T1_L2")$filterBounds(padova)


### function to do things for the stack of yearly imagery

processYear <- ee_utils_pyfunc(function(year) {
  getNDVI  <-    ee_utils_pyfunc(function(image) {
    nirb <-  image$select(nir)
    redb <-  image$select(red)
    qa <-  image$select(qaBand)

    cloud <-
      qa$bitwiseAnd(bitwShiftL(1L, 5L))$Or(qa$bitwiseAnd(bitwShiftL(1L, 7L)))$Or(qa$bitwiseAnd(bitwShiftL(1L, 3L)))

    fin <-
      ee$Image(
        nirb$subtract(redb)$divide(nirb$add(redb))$addBands(ee$Image(1))$rename('NDVI', "Ncells")
      )

    fin <- fin$updateMask(fin$select("NDVI")$gt(0L))

    ff <- fin$reduceRegions(
      collection = controls,
      #$filter(ee$Filter$eq('codice', 'berval')),
      reducer = ee$Reducer$sum(),
      scale = 30L
    )

    ff <- fin$select("NDVI")$rename("stdDevNDVI")$reduceRegions(
      collection = ff,
      reducer = ee$Reducer$stdDev(),
      scale = 30L
    )
    ff <-
      ee$Image(cloud)$addBands(ee$Image(1))$rename('Clouds', "NCloudCells")$reduceRegions(
        collection = ff,
        reducer = ee$Reducer$sum(),
        scale = 30L
      )

    ff$map(ee_utils_pyfunc(function(ff) {
      ff <-
        ff$set(
          list(
            "Date" = ee$Image(image)$date()$format('YYYY-MM-dd'),
            "Timestamp" = ee$Image(image)$date()$format('YYYY-MM-dd HH:mm:ss'),
            "Path" = ee$Image(image)$get('WRS_PATH'),
            "Row" = ee$Image(image)$get('WRS_ROW')
          )
        )
    }))


  })

  ### MSS SENSOR RED AND NIR BAND DESIGNATION ----
  nir <- ee$String("B7")
  red <- ee$String("B5")
  qaBand <- ee$String("QA_PIXEL")


  l1.fd <-
    l1$filterDate(
      ee$Number(year)$format("%d")$cat('-03-21'),
      ee$Number(year)$format("%d")$cat('-10-21')
    )$map(getNDVI)$flatten()


  l1.fd$filter(ee$Filter$gt('stdDev', 0))$filter(ee$Filter$lt('Clouds', 1))

})



from <- 1972
to <- 1975
ndviStack <- ee$FeatureCollection( ee$List$sequence(from, to)$map(processYear) )$flatten()


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

df$Date <- as.Date(df$Date)
df$Timestamp <- as.POSIXct(df$Timestamp)
df$codice <- as.factor(df$codice)


names(all) <- sprintf("y%d", from:to)

all$features
dt.avail <- data.table::rbindlist(all, idcol = "Year")
