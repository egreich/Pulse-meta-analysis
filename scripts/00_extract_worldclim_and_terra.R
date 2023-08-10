# Extract WorldClim normals 1970-2020 for all sites
library(raster)
library(rgdal)
library(dplyr)

# WorldClim 2.1 data at 30s (~1 km) spatial resolution
# Means for 1970-2000 
# https://www.worldclim.org/data/worldclim21.html#google_vignette
# Citation: Fick, S.E. and R.J. Hijmans, 2017. 
# WorldClim 2: new 1km spatial resolution climate surfaces for global land areas. 
# International Journal of Climatology 37 (12): 4302-4315.


# Elevation raster, single not stacked
elev <- raster("data_raw/wc2.1_30s_elev/wc2.1_30s_elev.tif")

# Bioclimatic rasters, single not stacked
# BIO1 = Annual Mean Temperature
# BIO12 = Annual Precipitation

MAT <- raster("data_raw/wc2.1_30s_bio/wc2.1_30s_bio_1.tif")
MAP <- raster("data_raw/wc2.1_30s_bio/wc2.1_30s_bio_12.tif")

# # T in C
# tavg_fn <- list.files("data_raw/wc2.1_30s_tavg/",
#                       full.names = TRUE)
# tavg_stack <- stack(tavg_fn)
# str(tavg_stack)
# names(tavg_stack)
# 
# # Precip in mm
# prec_fn <- list.files("data_raw/wc2.1_30s_prec/",
#                       full.names = TRUE)
# prec_stack <- stack(prec_fn)
# names(prec_stack)
# 
# # Water vapor pressure in kPa
# vapr_fn <- list.files("data_raw/wc2.1_30s_vapr/",
#                       full.names = TRUE)
# vapr_stack <- stack(vapr_fn)
# names(vapr_stack)


# Load meta-analysis sites
latlons <- read.csv("./data_raw/Combined_study-source_info.csv", 
                        header = TRUE, skip = 1,
                        na.strings = "") %>%
  dplyr::select(Study.ID, Latitude, Longitude) %>%
  dplyr::filter(!is.na(Study.ID)) %>%
  rename(lat = Latitude,
          lon = Longitude) %>%
  relocate(Study.ID, lon, lat) %>% # need to give in x, y
  as_tibble() %>%
  tibble::column_to_rownames(var = "Study.ID")
  
# Extract 
elev.data <- extract(elev, latlons)
MAT.data <- extract(MAT, latlons)
MAP.data <- extract(MAP, latlons)
# tavg.data <- extract(tavg_stack, latlons)
# prec.data <- extract(prec_stack, latlons)
# vapr.data <- extract(vapr_stack, latlons)


# # Summarize to annual
# tavg <- rowMeans(tavg.data) # Mean across all months
# prec <- rowSums(prec.data) # Sum across all months
# 
# temp <- matrix(NA, nrow = nrow(vapr.data), ncol = 2)
# for(i in 1:nrow(vapr.data)) {
#   month <- which.max(vapr.data[i,])
#   vpd <- vapr.data[i, month]
#   temp[i, ] <- c(month, vpd)
# } # VPD of driest month

out.df <- data.frame(MAT = MAT.data,
                   MAP = MAP.data,
                   elev = elev.data)

out <- cbind.data.frame(latlons, out.df) %>%
  tibble::rownames_to_column(var = "Study.ID")

# Save out to data_clean/
write.csv(out, file = "data_clean/worldclim_vars.csv",
          row.names = FALSE)

#### Calculate aridity index using https://www.painblogr.org/2020-12-15-climate-change.html
### Uses data from:
### Abatzoglou JT, Dobrowski SZ, Parks SA, Hegewisch KC. 
### TerraClimate, a high-resolution global dataset of monthly climate and climatic water balance from 1958-2015. 
### Sci Data 5:170191, 2018. DOI: 10.1038/sdata.2017.191

#--- Download the files from the TerraClimate website ---#
# Precipitation
download.file(url = 'http://thredds.northwestknowledge.net:8080/thredds/fileServer/TERRACLIMATE_ALL/data/TerraClimate_ppt_2019.nc',
              destfile = './data_raw/ppt.nc')

# Evapotranspiration
download.file(url = 'http://thredds.northwestknowledge.net:8080/thredds/fileServer/TERRACLIMATE_ALL/data/TerraClimate_pet_2019.nc',
              destfile = './data_raw/pet.nc')

#--- Import the downloaded files ---#
# Precipitation
ppt <- stack(x = './data_raw/ppt.nc')

# Evapotranspiration
pet <- stack(x = './data_raw/pet.nc')


#--- Raster maths ---#
# Precipitation
ppt_mean <- calc(ppt, # RasterStack object
                 fun = mean, # Function to apply across the layers
                 na.rm = TRUE)

# Evapotranspiration
pet_mean <- calc(pet,
                 fun = mean, 
                 na.rm = TRUE)

#--- Set the extent ---#
# Cut off all values below 60 degrees South (removing Antarctica)
ext <- extent(c(xmin = -180, xmax = 180, 
                ymin = -60, ymax = 90))

#--- Crop ---#
# Precipitation
ppt_mean <- crop(x = ppt_mean, 
                 y = ext)

# Evapotranspiration
pet_mean <- crop(x = pet_mean, 
                 y = ext)

#--- Calculate aridity index ---#
# Precipitation (ppt) / Evapotranspiration (pet)
aridity_index <- overlay(x = ppt_mean, # Raster object 1
                         y = pet_mean, # Raster object 2
                         fun = function(x, y){return(x / y)}) # Function to apply

# Extract 
arid.data <- extract(aridity_index, latlons)

arid.out.df <- data.frame(aridity = arid.data)

arid.out <- cbind.data.frame(latlons, arid.out.df) %>%
  tibble::rownames_to_column(var = "Study.ID")

# Save out to data_clean/
write.csv(arid.out, file = "data_clean/aridity.csv",
          row.names = FALSE)


##### For mapping

#--- Convert raster to a matrix ---#
aridity_index_matrix <- rasterToPoints(aridity_index)

#--- Convert to the matrix to a dataframe ---#
aridity_index_df <- as.data.frame(aridity_index_matrix)

#--- Recode aridity index into categories --#
aridity_index_df <- aridity_index_df %>% 
  # Recode
  mutate(category = case_when(
    is.infinite(layer) ~ 'Humid',
    layer >= 0.65 ~ 'Humid',
    layer >= 0.5 & layer < 0.65 ~ 'Dry sub-humid',
    layer >= 0.2 & layer < 0.5 ~ 'Semi-arid',
    layer >= 0.05 & layer < 0.2 ~ 'Arid',
    layer < 0.05 ~ 'Hyper-arid'
  )) %>% 
  # Convert to ordered factor
  mutate(category = factor(category,
                           levels = c('Hyper-arid', 'Arid', 'Semi-arid',
                                      'Dry sub-humid', 'Humid'),
                           ordered = TRUE))

# Save out to data_raw/ to use for mapping later
write.csv(aridity_index_df, file = "data_raw/aridity_index_df.csv",
          row.names = FALSE)
# Save raster file
writeRaster(aridity_index, file = "data_raw/aridity_index.tif")



