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

tavg_fn <- list.files("data_raw/wc2.1_30s_tavg/",
                      full.names = TRUE)
tavg_stack <- stack(tavg_fn)
str(tavg_stack)
names(tavg_stack)


prec_fn <- list.files("data_raw/wc2.1_30s_prec/",
                      full.names = TRUE)
prec_stack <- stack(prec_fn)
names(prec_stack)

vapr_fn <- list.files("data_raw/wc2.1_30s_vapr/",
                      full.names = TRUE)
vapr_stack <- stack(vapr_fn)
names(vapr_stack)


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
  column_to_rownames(var = "Study.ID")
  
# Extract 
tavg.data <- extract(tavg_stack, latlons)
prec.data <- extract(prec_stack, latlons)
vapr.data <- extract(vapr_stack, latlons)


# Summarize to annual
tavg <- rowMeans(tavg.data) # Mean across all months
prec <- rowSums(prec.data) # Sum across all months

temp <- matrix(NA, nrow = nrow(vapr.data), ncol = 2)
for(i in 1:nrow(vapr.data)) {
  month <- which.max(vapr.data[i,])
  vpd <- vapr.data[i, month]
  temp[i, ] <- c(month, vpd)
} # VPD of driest month

out1 <- data.frame(MAT = tavg,
                   MAP = prec,
                   Dmax = temp[,2],
                   Dmax_month = temp[,1])

out <- cbind.data.frame(latlons, out1)


plot(out$MAT, out$MAP)
plot(out$Dmax, out$MAP)
