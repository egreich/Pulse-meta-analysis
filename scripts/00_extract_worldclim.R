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
