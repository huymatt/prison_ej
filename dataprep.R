####### Prison Justice App ##########################
####### Matthieu Huy, March 2023 ####################


#Load packages

library(shiny)
# library(raster) ## Replaced by terra
library(shinyWidgets)
library(shinycssloaders)
library(tidyverse)
library(sf)
library(janitor)
library(terra) ## Modern and faster raster processing package
library(leaflet)
library(leaflet.extras)
library(geojsonio)
library(rgdal)
library(spatialEco)
library(glue)
library(here)
library(stringr)
library(ggtext)
library(htmltools)
library(tmap)
library(usmap)
library(shinythemes)
library(rcartocolor)
library(readxl)
library(rsconnect)

select <- dplyr::select ## dplyr::select() was clashing with another function

######################## Read in data ##########################################

### State polygons from US Census Bureau
### applied sf::st_simplify() function in separate Rscript to reduce resolution and file size
state_sf <- read_sf(here("data/state_county/states_simple.shp"))

### County polygons from US Census Bureau
### applied sf::st_simplify() function in separate Rscript to reduce resolution and file size
county_sf <- read_sf(here("data/state_county/us_county_simple.shp"))


### Superfund data (pre-processed and loaded directly as sf objects)
superfund_noprison_1_sf <- read_sf(here("data/superfund_data/sites_withprisons_1.shp")) |>
  clean_names() |>
  filter(join_count == "0")
superfund_withprisons_1_sf <- read_sf(here("data/superfund_data/sites_withprisons_1.shp")) |>
  clean_names() |>
  filter(join_count == "1")

superfund_noprison_3_sf <- read_sf(here("data/superfund_data/sites_withprisons_3.shp")) |>
  clean_names() |>
  filter(join_count == "0")
superfund_withprisons_3_sf <- read_sf(here("data/superfund_data/sites_withprisons_3.shp")) |>
  clean_names() |>
  filter(join_count == "1")

### prison locations from US Bureau of Prisons
prison_boundaries_sf <- read_sf(here("data/Prison_Boundaries/Prison_Boundaries.shp")) |>
  clean_names() |>
  filter(type %in% c("COUNTY", "FEDERAL", "LOCAL", "STATE")) |>
  mutate(type = as.factor(type))

### convert all caps county names to match names in county_sf
prison_boundaries_sf$county <- str_to_title(prison_boundaries_sf$county)

### set crs for superfund buffers and prisons to same same county_sf
superfund_withprisons_3_sf <- st_transform(superfund_withprisons_3_sf, st_crs(county_sf))
superfund_withprisons_1_sf <- st_transform(superfund_withprisons_1_sf, st_crs(county_sf))
prison_boundaries_sf <- st_transform(prison_boundaries_sf, st_crs(county_sf))

### county heat data (current days above 100F from union of concerned scientists)
county_heat_100 <- readxl::read_excel(here("data/killer-heat-data-by-county.xlsx"),
                                      sheet = 3, skip = 2)

######### temperature climate projection rasters

### The following code reads all regional rasters using terra::rast(),
### stacks them into a multi-layer SpatRaster, and then computes the mean.
### The unnecessary step of converting to a raster::raster object is removed.

### temp rasters region 1 (ME, NH, VT, MA, RI, CT)
temp_1a <- rast(here("rasters/climate_scenarios/lasso_data_1/LOCA_rcp85_eparegion1_CMCC-CMS_r1i1p1_tas_F_2041_2070_1981_2010_Annual.tif"))
temp_1b <- rast(here("rasters/climate_scenarios/lasso_data_1/LOCA_rcp85_eparegion1_GFDL-ESM2G_r1i1p1_tas_F_2041_2070_1981_2010_Annual.tif"))
temp_1c <- rast(here("rasters/climate_scenarios/lasso_data_1/LOCA_rcp85_eparegion1_HadGEM2-CC_r1i1p1_tas_F_2041_2070_1981_2010_Annual.tif"))
temp_1d <- rast(here("rasters/climate_scenarios/lasso_data_1/LOCA_rcp85_eparegion1_inmcm4_r1i1p1_tas_F_2041_2070_1981_2010_Annual.tif"))
temp_1e <- rast(here("rasters/climate_scenarios/lasso_data_1/LOCA_rcp85_eparegion1_MIROC-ESM_r1i1p1_tas_F_2041_2070_1981_2010_Annual.tif"))
temp_1f <- rast(here("rasters/climate_scenarios/lasso_data_1/LOCA_rcp85_eparegion1_MIROC-ESM-CHEM_r1i1p1_tas_F_2041_2070_1981_2010_Annual.tif"))
temp1_stack <- c(temp_1a, temp_1b, temp_1c, temp_1d, temp_1e, temp_1f)
temp1_raster_avg <- mean(temp1_stack) # More direct than reduce() or app()

### temp rasters region 2 (NY, NJ)
temp_2a <- rast(here("rasters/climate_scenarios/lasso_data_2/LOCA_rcp85_eparegion2_CMCC-CMS_r1i1p1_pr_pctchg_2041_2070_1981_2010_Annual.tif"))
temp_2b <- rast(here("rasters/climate_scenarios/lasso_data_2/LOCA_rcp85_eparegion2_GFDL-ESM2M_r1i1p1_pr_pctchg_2041_2070_1981_2010_Annual.tif"))
temp_2c <- rast(here("rasters/climate_scenarios/lasso_data_2/LOCA_rcp85_eparegion2_GISS-E2-R_r2i1p1_pr_pctchg_2041_2070_1981_2010_Annual.tif"))
temp_2d <- rast(here("rasters/climate_scenarios/lasso_data_2/LOCA_rcp85_eparegion2_HadGEM2-CC_r1i1p1_pr_pctchg_2041_2070_1981_2010_Annual.tif"))
temp_2e <- rast(here("rasters/climate_scenarios/lasso_data_2/LOCA_rcp85_eparegion2_inmcm4_r1i1p1_pr_pctchg_2041_2070_1981_2010_Annual.tif"))
temp_2f <- rast(here("rasters/climate_scenarios/lasso_data_2/LOCA_rcp85_eparegion2_MIROC-ESM_r1i1p1_pr_pctchg_2041_2070_1981_2010_Annual.tif"))
temp_2g <- rast(here("rasters/climate_scenarios/lasso_data_2/LOCA_rcp85_eparegion2_MIROC-ESM-CHEM_r1i1p1_pr_pctchg_2041_2070_1981_2010_Annual.tif"))
temp_2h <- rast(here("rasters/climate_scenarios/lasso_data_2/LOCA_rcp85_eparegion2_MPI-ESM-MR_r1i1p1_pr_pctchg_2041_2070_1981_2010_Annual.tif"))
temp2_stack <- c(temp_2a, temp_2b, temp_2c, temp_2d, temp_2e, temp_2f, temp_2g, temp_2h)
temp2_raster_avg <- mean(temp2_stack)

### temp rasters region 3 (MA, MD, VA, WV, DC)
temp_3a <- rast(here("rasters/climate_scenarios/lasso_data_3/LOCA_rcp85_eparegion3_GFDL-CM3_r1i1p1_pr_pctchg_2041_2070_1981_2010_Annual.tif"))
temp_3b <- rast(here("rasters/climate_scenarios/lasso_data_3/LOCA_rcp85_eparegion3_HadGEM2-CC_r1i1p1_pr_pctchg_2041_2070_1981_2010_Annual.tif"))
temp_3c <- rast(here("rasters/climate_scenarios/lasso_data_3/LOCA_rcp85_eparegion3_HadGEM2-ES_r1i1p1_pr_pctchg_2041_2070_1981_2010_Annual.tif"))
temp_3d <- rast(here("rasters/climate_scenarios/lasso_data_3/LOCA_rcp85_eparegion3_inmcm4_r1i1p1_pr_pctchg_2041_2070_1981_2010_Annual.tif"))
temp_3e <- rast(here("rasters/climate_scenarios/lasso_data_3/LOCA_rcp85_eparegion3_IPSL-CM5A-LR_r1i1p1_pr_pctchg_2041_2070_1981_2010_Annual.tif"))
temp_3f <- rast(here("rasters/climate_scenarios/lasso_data_3/LOCA_rcp85_eparegion3_MIROC-ESM_r1i1p1_pr_pctchg_2041_2070_1981_2010_Annual.tif"))
temp_3g <- rast(here("rasters/climate_scenarios/lasso_data_3/LOCA_rcp85_eparegion3_MPI-ESM-MR_r1i1p1_pr_pctchg_2041_2070_1981_2010_Annual.tif"))
temp_3h <- rast(here("rasters/climate_scenarios/lasso_data_3/LOCA_rcp85_eparegion3_MRI-CGCM3_r1i1p1_pr_pctchg_2041_2070_1981_2010_Annual.tif"))
temp3_stack <- c(temp_3a, temp_3b, temp_3c, temp_3d, temp_3e, temp_3f, temp_3g, temp_3h)
temp3_raster_avg <- mean(temp3_stack)

### temp rasters region 4 (KY, TN, NC, SC, MS, AL, GA, FL)
temp_4a <- rast(here("rasters/climate_scenarios/lasso_data_4/LOCA_rcp85_eparegion4_FGOALS-g2_r1i1p1_tas_F_2041_2070_1981_2010_Annual.tif"))
temp_4b <- rast(here("rasters/climate_scenarios/lasso_data_4/LOCA_rcp85_eparegion4_GFDL-CM3_r1i1p1_tas_F_2041_2070_1981_2010_Annual.tif"))
temp_4c <- rast(here("rasters/climate_scenarios/lasso_data_4/LOCA_rcp85_eparegion4_HadGEM2-CC_r1i1p1_tas_F_2041_2070_1981_2010_Annual.tif"))
temp_4d <- rast(here("rasters/climate_scenarios/lasso_data_4/LOCA_rcp85_eparegion4_HadGEM2-ES_r1i1p1_tas_F_2041_2070_1981_2010_Annual.tif"))
temp_4e <- rast(here("rasters/climate_scenarios/lasso_data_4/LOCA_rcp85_eparegion4_inmcm4_r1i1p1_tas_F_2041_2070_1981_2010_Annual.tif"))
temp_4f <- rast(here("rasters/climate_scenarios/lasso_data_4/LOCA_rcp85_eparegion4_MPI-ESM-MR_r1i1p1_tas_F_2041_2070_1981_2010_Annual.tif"))
temp_4g <- rast(here("rasters/climate_scenarios/lasso_data_4/LOCA_rcp85_eparegion4_MRI-CGCM3_r1i1p1_tas_F_2041_2070_1981_2010_Annual.tif"))
temp4_stack <- c(temp_4a, temp_4b, temp_4c, temp_4d, temp_4e, temp_4f, temp_4g)
temp4_raster_avg <- mean(temp4_stack)

### temp rasters region 5 (MI, OH, IN, IL, WI, MN)
temp_5a <- rast(here("rasters/climate_scenarios/lasso_data_5/LOCA_rcp85_eparegion5_ACCESS1-0_r1i1p1_tas_F_2041_2070_1981_2010_Annual.tif"))
temp_5b <- rast(here("rasters/climate_scenarios/lasso_data_5/LOCA_rcp85_eparegion5_CMCC-CMS_r1i1p1_tas_F_2041_2070_1981_2010_Annual.tif"))
temp_5c <- rast(here("rasters/climate_scenarios/lasso_data_5/LOCA_rcp85_eparegion5_GFDL-CM3_r1i1p1_tas_F_2041_2070_1981_2010_Annual.tif"))
temp_5d <- rast(here("rasters/climate_scenarios/lasso_data_5/LOCA_rcp85_eparegion5_GFDL-ESM2M_r1i1p1_tas_F_2041_2070_1981_2010_Annual.tif"))
temp_5e <- rast(here("rasters/climate_scenarios/lasso_data_5/LOCA_rcp85_eparegion5_GISS-E2-R_r2i1p1_tas_F_2041_2070_1981_2010_Annual.tif"))
temp_5f <- rast(here("rasters/climate_scenarios/lasso_data_5/LOCA_rcp85_eparegion5_inmcm4_r1i1p1_tas_F_2041_2070_1981_2010_Annual.tif"))
temp_5g <- rast(here("rasters/climate_scenarios/lasso_data_5/LOCA_rcp85_eparegion5_IPSL-CM5A-MR_r1i1p1_tas_F_2041_2070_1981_2010_Annual.tif"))
temp_5h <- rast(here("rasters/climate_scenarios/lasso_data_5/LOCA_rcp85_eparegion5_MIROC-ESM_r1i1p1_tas_F_2041_2070_1981_2010_Annual.tif"))
temp_5i <- rast(here("rasters/climate_scenarios/lasso_data_5/LOCA_rcp85_eparegion5_MIROC-ESM-CHEM_r1i1p1_tas_F_2041_2070_1981_2010_Annual.tif"))
temp_5j <- rast(here("rasters/climate_scenarios/lasso_data_5/LOCA_rcp85_eparegion5_MRI-CGCM3_r1i1p1_tas_F_2041_2070_1981_2010_Annual.tif"))
temp5_stack <- c(temp_5a, temp_5b, temp_5c, temp_5d, temp_5e, temp_5f, temp_5g, temp_5h, temp_5i, temp_5j)
temp5_raster_avg <- mean(temp5_stack)

### temp rasters region 6 (AR, LA, NM, OK, TX)
temp_6a <- rast(here("rasters/climate_scenarios/lasso_data_6/LOCA_rcp85_eparegion6_ACCESS1-3_r1i1p1_tas_F_2041_2070_1981_2010_Annual.tif"))
temp_6b <- rast(here("rasters/climate_scenarios/lasso_data_6/LOCA_rcp85_eparegion6_HadGEM2-ES_r1i1p1_tas_F_2041_2070_1981_2010_Annual.tif"))
temp_6c <- rast(here("rasters/climate_scenarios/lasso_data_6/LOCA_rcp85_eparegion6_inmcm4_r1i1p1_tas_F_2041_2070_1981_2010_Annual.tif"))
temp_6d <- rast(here("rasters/climate_scenarios/lasso_data_6/LOCA_rcp85_eparegion6_IPSL-CM5A-MR_r1i1p1_tas_F_2041_2070_1981_2010_Annual.tif"))
temp_6e <- rast(here("rasters/climate_scenarios/lasso_data_6/LOCA_rcp85_eparegion6_MIROC-ESM-CHEM_r1i1p1_tas_F_2041_2070_1981_2010_Annual.tif"))
temp6_stack <- c(temp_6a, temp_6b, temp_6c, temp_6d, temp_6e)
temp6_raster_avg <- mean(temp6_stack)

### temp rasters region 7 (NE, IA, KS, MO)
temp_7a <- rast(here("rasters/climate_scenarios/lasso_data_7/LOCA_rcp85_eparegion7_ACCESS1-0_r1i1p1_tas_F_2041_2070_1981_2010_Annual.tif"))
temp_7b <- rast(here("rasters/climate_scenarios/lasso_data_7/LOCA_rcp85_eparegion7_EC-EARTH_r2i1p1_tas_F_2041_2070_1981_2010_Annual.tif"))
temp_7c <- rast(here("rasters/climate_scenarios/lasso_data_7/LOCA_rcp85_eparegion7_GFDL-CM3_r1i1p1_tas_F_2041_2070_1981_2010_Annual.tif"))
temp_7d <- rast(here("rasters/climate_scenarios/lasso_data_7/LOCA_rcp85_eparegion7_HadGEM2-ES_r1i1p1_tas_F_2041_2070_1981_2010_Annual.tif"))
temp_7e <- rast(here("rasters/climate_scenarios/lasso_data_7/LOCA_rcp85_eparegion7_inmcm4_r1i1p1_tas_F_2041_2070_1981_2010_Annual.tif"))
temp_7f <- rast(here("rasters/climate_scenarios/lasso_data_7/LOCA_rcp85_eparegion7_MIROC-ESM-CHEM_r1i1p1_tas_F_2041_2070_1981_2010_Annual.tif"))
temp_7g <- rast(here("rasters/climate_scenarios/lasso_data_7/LOCA_rcp85_eparegion7_MRI-CGCM3_r1i1p1_tas_F_2041_2070_1981_2010_Annual.tif"))
temp_7h <- rast(here("rasters/climate_scenarios/lasso_data_7/LOCA_rcp85_eparegion7_GFDL-ESM2M_r1i1p1_tas_F_2041_2070_1981_2010_Annual.tif"))
temp_7i <- rast(here("rasters/climate_scenarios/lasso_data_7/LOCA_rcp85_eparegion7_IPSL-CM5A-MR_r1i1p1_tas_F_2041_2070_1981_2010_Annual.tif"))
temp7_stack <- c(temp_7a, temp_7b, temp_7c, temp_7d, temp_7e, temp_7f, temp_7g, temp_7h, temp_7i)
temp7_raster_avg <- mean(temp7_stack)

### temp rasters region 8 (UT, CO, WY, SD, ND, MT)
temp_8a <- rast(here("rasters/climate_scenarios/lasso_data_8/LOCA_rcp85_eparegion8_ACCESS1-0_r1i1p1_tas_F_2041_2070_1981_2010_Annual.tif"))
temp_8b <- rast(here("rasters/climate_scenarios/lasso_data_8/LOCA_rcp85_eparegion8_EC-EARTH_r2i1p1_tas_F_2041_2070_1981_2010_Annual.tif"))
temp_8c <- rast(here("rasters/climate_scenarios/lasso_data_8/LOCA_rcp85_eparegion8_GFDL-CM3_r1i1p1_tas_F_2041_2070_1981_2010_Annual.tif"))
temp_8d <- rast(here("rasters/climate_scenarios/lasso_data_8/LOCA_rcp85_eparegion8_HadGEM2-AO_r1i1p1_tas_F_2041_2070_1981_2010_Annual.tif"))
temp_8e <- rast(here("rasters/climate_scenarios/lasso_data_8/LOCA_rcp85_eparegion8_inmcm4_r1i1p1_tas_F_2041_2070_1981_2010_Annual.tif"))
temp_8f <- rast(here("rasters/climate_scenarios/lasso_data_8/LOCA_rcp85_eparegion8_MIROC-ESM-CHEM_r1i1p1_tas_F_2041_2070_1981_2010_Annual.tif"))
temp_8g <- rast(here("rasters/climate_scenarios/lasso_data_8/LOCA_rcp85_eparegion8_MRI-CGCM3_r1i1p1_tas_F_2041_2070_1981_2010_Annual.tif"))
temp8_stack <- c(temp_8a, temp_8b, temp_8c, temp_8d, temp_8e, temp_8f, temp_8g)
temp8_raster_avg <- mean(temp8_stack)

### temp rasters region 9 (CA, NV, AZ)
temp_9a <- rast(here("rasters/climate_scenarios/lasso_data_9/LOCA_rcp85_eparegion9_CanESM2_r1i1p1_tas_F_2041_2070_1981_2010_Annual.tif"))
temp_9b <- rast(here("rasters/climate_scenarios/lasso_data_9/LOCA_rcp85_eparegion9_inmcm4_r1i1p1_tas_F_2041_2070_1981_2010_Annual.tif"))
temp_9c <- rast(here("rasters/climate_scenarios/lasso_data_9/LOCA_rcp85_eparegion9_MIROC-ESM_r1i1p1_tas_F_2041_2070_1981_2010_Annual.tif"))
temp_9d <- rast(here("rasters/climate_scenarios/lasso_data_9/LOCA_rcp85_eparegion9_MIROC-ESM-CHEM_r1i1p1_tas_F_2041_2070_1981_2010_Annual.tif"))
temp_9e <- rast(here("rasters/climate_scenarios/lasso_data_9/LOCA_rcp85_eparegion9_MIROC5_r1i1p1_tas_F_2041_2070_1981_2010_Annual.tif"))
temp_9f <- rast(here("rasters/climate_scenarios/lasso_data_9/LOCA_rcp85_eparegion9_MRI-CGCM3_r1i1p1_tas_F_2041_2070_1981_2010_Annual.tif"))
temp9_stack <- c(temp_9a, temp_9b, temp_9c, temp_9d, temp_9e, temp_9f)
temp9_raster_avg <- mean(temp9_stack)

### temp rasters region 10 (WA, OR, ID)
temp_10a <- rast(here("rasters/climate_scenarios/lasso_data_10/LOCA_rcp85_eparegion10_CanESM2_r1i1p1_tas_F_2041_2070_1981_2010_Annual.tif"))
temp_10b <- rast(here("rasters/climate_scenarios/lasso_data_10/LOCA_rcp85_eparegion10_CMCC-CM_r1i1p1_tas_F_2041_2070_1981_2010_Annual.tif"))
temp_10c <- rast(here("rasters/climate_scenarios/lasso_data_10/LOCA_rcp85_eparegion10_CMCC-CMS_r1i1p1_tas_F_2041_2070_1981_2010_Annual.tif"))
temp_10d <- rast(here("rasters/climate_scenarios/lasso_data_10/LOCA_rcp85_eparegion10_HadGEM2-AO_r1i1p1_tas_F_2041_2070_1981_2010_Annual.tif"))
temp_10e <- rast(here("rasters/climate_scenarios/lasso_data_10/LOCA_rcp85_eparegion10_HadGEM2-AO_r1i1p1_tas_F_2041_2070_1981_2010_Annual.tif"))
temp_10f <- rast(here("rasters/climate_scenarios/lasso_data_10/LOCA_rcp85_eparegion10_MIROC-ESM-CHEM_r1i1p1_tas_F_2041_2070_1981_2010_Annual.tif"))
temp_10g <- rast(here("rasters/climate_scenarios/lasso_data_10/LOCA_rcp85_eparegion10_MRI-CGCM3_r1i1p1_tas_F_2041_2070_1981_2010_Annual.tif"))
temp10_stack <- c(temp_10a, temp_10b, temp_10c, temp_10d, temp_10e, temp_10f, temp_10g)
temp10_raster_avg <- mean(temp10_stack)

### Use terra::merge to combine the regional SpatRasters into a single layer
temp_raster_merge <- terra::merge(temp1_raster_avg, temp2_raster_avg, temp3_raster_avg, temp4_raster_avg, temp5_raster_avg,
                                  temp6_raster_avg, temp7_raster_avg, temp8_raster_avg, temp9_raster_avg, temp10_raster_avg)

### Use terra::project for reprojection, providing the CRS as a WKT string from the sf object
temp_raster_merge <- terra::project(temp_raster_merge, st_crs(county_sf)$wkt)


### clean/prep data #########################################

state_names <- state_sf |>
  as.data.frame() |>
  select(statefp, state = name, state_abb = stusps)

county_state_names_df <- county_sf |>
  as.data.frame() |>
  select(statefp, countyfp, county = name, namelsad) |>
  mutate(namelsad = gsub("\\s", "", namelsad)) |>
  inner_join(state_names, by = c("statefp")) |>
  mutate(countyfips = paste(statefp, countyfp, sep = "")) |>
  mutate(state_county = paste(state_abb, namelsad, sep = "")) |>
  select(county, state, countyfips, state_county)

county_state_names_sf <- county_sf |>
  select(statefp, countyfp, county = name, namelsad) |>
  mutate(namelsad = gsub("\\s", "", namelsad)) |>
  inner_join(state_names, by = c("statefp")) |>
  mutate(countyfips = paste(statefp, countyfp, sep = "")) |>
  mutate(state_county = paste(state_abb, namelsad, sep = "")) |>
  select(county, state, countyfips, state_county)

prison_boundaries_sf <- prison_boundaries_sf |>
  select(-c("state", "county")) |>
  inner_join(county_state_names_df, by = c("countyfips"))

county_heat_100 <- county_heat_100 |>
  clean_names() |>
  select(state = x1, county = x2, historical, no_action_5) |>
  mutate(county = gsub("\\s", "", county)) |>
  mutate(state_county = paste(state, county, sep = ""))

county_heat_100_sf <- county_state_names_sf |>
  inner_join(county_heat_100, by = c("state_county")) |>
  select(county = county.x, state = state.x, historical, projected = no_action_5)


### ===================================================== ###
###       SAVE FINAL OBJECTS FOR THE SHINY APP            ###
### ===================================================== ###

# Create a directory for the pre-processed files if it doesn't exist
if (!dir.exists(here("data/preprocessed"))) {
  dir.create(here("data/preprocessed"))
}

# --- Save all the final sf (vector) objects as .rds files for fast loading ---
saveRDS(prison_boundaries_sf, here("data/preprocessed/prison_boundaries_sf.rds"))
saveRDS(superfund_noprison_1_sf, here("data/preprocessed/superfund_noprison_1_sf.rds"))
saveRDS(superfund_withprisons_1_sf, here("data/preprocessed/superfund_withprisons_1_sf.rds"))
saveRDS(superfund_noprison_3_sf, here("data/preprocessed/superfund_noprison_3_sf.rds"))
saveRDS(superfund_withprisons_3_sf, here("data/preprocessed/superfund_withprisons_3_sf.rds"))
saveRDS(county_state_names_sf, here("data/preprocessed/county_state_names_sf.rds"))
saveRDS(county_heat_100_sf, here("data/preprocessed/county_heat_100_sf.rds"))

# --- Save the final SpatRaster object as a GeoTIFF ---
terra::writeRaster(
  temp_raster_merge,
  here("data/preprocessed/final_temp_raster.tif"),
  overwrite = TRUE
)

print("All pre-processed files have been saved successfully!")
