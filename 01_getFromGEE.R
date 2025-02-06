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
          reducer=  reducers
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
            "yearNDVI" = ee$Image(img)$get('year'),
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
  ndviStack2 <-   ee$ImageCollection( ee$List(list(l5, l7 ))$map(processLCollection) )#$flatten()

  nir <- ee$String("B5")
  red <- ee$String("B4")
  ndviStack3 <- ee$FeatureCollection( ee$List(list(l8 ))$map(processLCollection) )$flatten()

  ndviStack <- ndviStack1$merge(ndviStack2)$merge(ndviStack3)

  # task_vector <- ee_table_to_drive(
  #   collection = ndviStack,
  #   fileFormat = "GEO_JSON",
  #   timePrefix = TRUE,
  #   fileNamePrefix = "controls"
  # )

  print(ndviStack2$getInfo())

  task_vector <- ee_image_to_drive(
    image = ndviStack2$toBands()$toFloat(),
    description = "image",
    region = controls$geometry()$bounds(),
    scale = 30L
  )

  task_vector$start()
  ee_monitoring(task_vector) # optional
  outpath.controls <- downloadGDrive(task_vector)




  collection <- cases
  nir <- ee$String("B7")
  red <- ee$String("B5")
  ndviStack1 <- ee$FeatureCollection( ee$List(list(l1,l2,l3))$map(processLCollection) )$flatten()

  nir <- ee$String("B4")
  red <- ee$String("B3")
  ndviStack2 <- ee$FeatureCollection( ee$List(list(l5, l7  ))$map(processLCollection) )$flatten()

  nir <- ee$String("B5")
  red <- ee$String("B4")
  ndviStack3 <- ee$FeatureCollection( ee$List(list(l8      ))$map(processLCollection) )$flatten()

  ndviStackCases <- ndviStack1$merge(ndviStack2)$merge(ndviStack3)

  task_vectorCases <- ee_table_to_drive(
    collection = ndviStackCases,
    fileFormat = "GEO_JSON",
    timePrefix = TRUE,
    fileNamePrefix = "cases"
  )
  task_vectorCases$start()
  ee_monitoring(task_vectorCases) # optional
  outpath.cases <- downloadGDrive(task_vectorCases)


  outpath.controls <- "controls_2024_02_03_17_50_07"
  outpath.cases <- "cases_2024_02_03_17_52_30"

  dfoutput <- list(controls=outpath.controls, cases=outpath.cases)


  alldata <- list()

  for(i in names(dfoutput)){
    ff <- dfoutput[[i]]
    all <- jsonlite::read_json(ff)

    df <- na.omit( data.table::rbindlist(lapply(all$features, function(x) {
      x$properties
    }), fill = TRUE) )

    df$sensor <- as.factor( df$sensor)
    df$codice <- as.factor(df$codice)

    wc<-grep("_count$", names(df))
    keep <- as.data.frame(df)[ ,  names(df)[wc[[1]]] ]
    df[, wc]<-NULL
    df$areaCovered <- keep / max(keep)

    alldata[[i]] <- df %>% filter(NDVI_count_stdDev!=0)
    next
    ## NDVI_count_max = maximum number of non-masked pixels in area
    plot(density(df$NDVI_count_max, width=5),
         main="Number of aggregated yearly pixels in area",
         xlab="Max number of aggregated pixels in areas in 1 year aggregation.",
         )
    ## NDVI_count_max = maximum number of non-masked pixels in area
    plot(density(df$NDVI_count_max, width=5),
         main="Number of aggregated yearly pixels in area",
         xlab="Max number of aggregated pixels in areas in 1 year aggregation.",
    )

    }

  # names(alldata$controls)[[23]]<- "yearNDVI"
  # names(alldata$cases)[[28]]<- "yearNDVI"
 saveRDS(alldata, "NDVIdataAllYears.rds")
 alldata <- readRDS( "NDVIdataAllYears.rds")
 final.df<- as.data.frame(data.table::rbindlist(alldata, idcol = "Tipo", fill = TRUE))  %>%   filter(NDVI_count_max > 5  )
 final.df$Tipo <- as.factor(final.df$Tipo)

 table( final.df$sensor, final.df$year)

 df1 <- final.df  %>% filter(sensor %in% c("L07"), yearNDVI > 2012 )
 df2w <- which(final.df$sensor=="L08")
 df2 <- final.df  %>% filter(sensor %in% c("L08"), yearNDVI > 2012 )

 ## HISTOGRAM EQUALIZATION
 ## WE ASSUME THAT OVERLAPPING YEARS SHOULD HAVE SAME HISTOGRAM
 ## ACCROSS SENSORS
 l7 <- ecdf(df1$NDVI_max_mean)
 l7_inv <- function(x) {quantile(df1$NDVI_max_mean, x)}
 l8 <- ecdf(df2$NDVI_max_mean)
 l8t <- unname(l7_inv(l8(final.df[df2w, "NDVI_max_mean"] ) ) )
 final.df[df2w, "NDVI_max_mean"] <- l8t
 saveRDS( final.df, file = "final.df.rds")
 # l8te <- ecdf(l8t)

 ######### FINISHED -------------



  png("cdf.png", width=2000, height=2000, res=300)

    plot(df1$NDVI_max_mean, l7(df1$NDVI_max_mean), col="red", xlab="NDVI",
         main="Landsat 7 and 8 C.D.F. 2012-2018"  , pch='+')
    points(df2$NDVI_max_mean,  l8(df2$NDVI_max_mean), col="blue" , pch='x' )
    points(l8t, l8te(l8t), col="blue",  pch='.',    lty=2, cex=0.8 )
    legend(0.2, 0.2, legend=c("L7", "L8", "L8trans"),
           col=c("red", "blue", "blue"), lty=c(1,1,2), cex=0.8)

  dev.off()


 png("cluster.png", width=3000, height=2000, res=300)
   p <- ggplot(final.df, aes(y=NDVI_max_mean, x=as.factor(sensor) ) ) +
     geom_boxplot() + theme_bw()
   print(p)
 dev.off()

ggplot(df %>% group_by(year, sensor), aes(x=year, y=sensor, color=NDVI_max_mean, group=codice)) +
  geom_point() + theme_bw()

ggplot(df , aes(y=NDVI_count_max, x=year, color=sensor, group=sensor)) +
  geom_point(position = "jitter", alpha=0.7 ) + ylab("N. pixels") + theme_bw()

ggplot(df, aes(x=Year, fill=Sensor)) + geom_bar() + theme_bw()


png("boxplot.png", width=3000, height=1000, res=300)
 p <- ggplot(df, aes(y=NDVI_max_mean, x=codice) ) +
      geom_boxplot() + theme_bw()
 print(p)
dev.off()

png("lineplot.png", width=4000, height=2500, res=300)
  p <- ggplot(df.filtered, aes(x=Timestamp, y=NDVI, shape=Row,
                               group=Sensor, color=Sensor)) +
    geom_point(alpha=0.3) +
    theme_bw()
  print(p)
dev.off()



df.filtered.grouped <- final.df %>%
  dplyr::group_by(codice) %>%
  dplyr::summarise(
    NDVI.mean = mean(NDVI_max_mean),
    NDVI.sd = sd(NDVI_max_mean),
    NDVI.num = n()
  )

png("consistency.png", width=4000, height=2500, res=300)
  p <- ggplot(final.df %>% filter(codice=="aguval"),
              aes(x=yearNDVI, y=NDVI_max_mean,
                  fill=sensor, size=NDVI_count_max )) +
    geom_point(alpha=0.2, shape=21) +
    scale_size(range = c(.1, 10), name="Number") +
    theme_bw()
  print(p)
dev.off()


png("satAvailability.png", width=2000, height=2000, res=300)
dd<-as.data.frame(table(final.df$yearNDVI, final.df$sensor))
names(dd)<- c("Year", "Sensor", "N")
dd$Year<- as.integer(as.character(dd$Year))
p <- ggplot( dd,
            aes(x=Sensor, y=Year,
                fill=Sensor, width=(N/2500) )) +
  geom_tile(height=.3 ) +
  # scale_x_continuous(breaks=pretty(as.numeric(rev(df$Year)))) +
  # scale_size(range = c(.1, 10), name="Number") +
  theme_bw()
print(p)

dev.off()

