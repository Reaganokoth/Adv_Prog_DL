library(raster)
library(stringr)
library(dplyr)
library(sf)
library(devtools)


# Load a function to download worldclim data see the doccumentation on github.
source("/Users/rragankonywa/OneDrive/UniWurzburg/EAGLES/my_functions/getWC_data.R")
# here is the link to the same code in github: https://github.com/Reaganokoth/Myfunctions/blob/main/getWC_data.R


setwd("/Users/rragankonywa/OneDrive/UniWurzburg/EAGLES/Semester2/ADVANCED_PROGRAMMING/temp")


getWC_data(category = "HMWD", product = "tmin", period = c("1980-1989", "1990-1999", "2000-2009", "2010-2018"))

getWC_data(category = "HMWD", product = "tmax", period = c("1980-1989", "1990-1999", "2000-2009", "2010-2018"))



# Load the coordinate paoints and convert them to spatial points dataframe
# then repriject the points to the projection of the bioclim rasters.
csv <- read.csv("/Users/rragankonywa/Downloads/pruned_df_coords.csv")

# note that the easting and northing columns are in columns 1 and 2
csv_spatial <- SpatialPointsDataFrame(csv[,2:3],
                                      csv,
                                      proj4string = CRS("+proj=longlat"))
  
csv_spatial_projected <- spTransform(csv_spatial, crs(stack))


# load a function to get country boundaries as shapefile
#  change the projection to the projection of the spatialPointDataframe object
source("/Users/rragankonywa/OneDrive/UniWurzburg/EAGLES/my_functions/getAdminBoundary.R")
# here is the link to the same code in github: https://github.com/Reaganokoth/Myfunctions/blob/main/getAdminBoundary.R

Ethiopia <- getAdminBoundaries(path = "/Users/rragankonywa/OneDrive/UniWurzburg/EAGLES/Semester2/ADVANCED_PROGRAMMING", country = "Ethiopia", dissolve = T)
plot(Ethiopia)

crs(Ethiopia) <- crs(csv_spatial_projected)


# crop the points to retain points that fall within the extent of Ethiopia object
# this will be used for data extraction in the extract_temp function.
csv_spatial_ethipia <- crop(csv_spatial,Ethiopia)

crs(csv_spatial_ethipia) <- crs(stack)
# view on map
plot(stack[[1]])
plot(csv_spatial_ethipia,add =T)


# set the path to where each tmax and tmin folders are
path_tmin <- "/Volumes/ELEMENTS/Advance/tmin"
path_tmax <- "/Volumes/ELEMENTS/Advance/tmax"


# extract data using and save it in each period directory
#' load reshapeExtent, to read stack the raster files
#' This function ensures that the images are of the same extent and so does ton throw
#' non matching extent error when stacking multiple raster layers.
#' 
source("/Users/rragankonywa/OneDrive/UniWurzburg/EAGLES/my_functions/ReshapeExtent.R")
#you can also source the code from git hub from, : https://github.com/Reaganokoth/Myfunctions/blob/main/ReshapeExtent.R

extact_temp <- function(path, product){
  files <- list.dirs(path)[-1]
  pattern <-"(_)([0-9]){4}(-)([0-9]){2}" 
  pattern2 <-"(_)([0-9]){4}(-)([0-9]){4}"
  
  for(i in 1:length(files)){
    directory <- files[i]
    setwd(directory)
    images <- list.files(directory, ".tif")
    Rstack <- ReshapeExtent(images,directory)
    
    layerNames <- str_extract(images,pattern)%>%
      lapply(function(each){
        str_replace(each, "-", "_") }) %>% 
      unlist()
    
    names(Rstack) <- layerNames
    
    croped <-  crop(x = Rstack,Ethiopia)
    masked <- mask(x = croped, mask =Ethiopia)
    file_name <- str_extract(directory,pattern2) %>% 
      str_replace("-", "_")
    
    if (i==1){
      df <-  raster::extract(x = masked,y = csv_spatial_ethipia, df=T)
    } else{
      df_new <-  raster::extract(x = masked,y = csv_spatial_ethipia, df=T)
      df <- cbind(df,df_new[-1])
    }
  }
  dir.create(path = paste0(str_remove(path,product), product,"_combined"))
  combined_path <- paste0(str_remove(path,product), product,"_combined", "/")
  write.csv(df, paste(combined_path,"complete_",product,"_1980_2018", ".csv"))
  return(df)
}

tmin <- extact_temp(path = path_tmin, product = "tmin")
tmax <- extact_temp(path = path_tmax, product = "tmax")

head(tmax) 


