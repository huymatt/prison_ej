####### Prison Justice App ##########################
####### Matthieu Huy, March 2023 ####################


#Load packages


library(shiny)
library(raster)
library(shinyWidgets)
library(shinycssloaders)
library(tidyverse)
library(sf)
library(janitor)
library(terra)
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

select <- dplyr::select ##dplyr::select() was clashing with another function

######################## Read in data ##########################################

### State polygons from US Census Bureau
### applied sf::st_simplify() function in separate Rscript to reduce resolution and file size

state_sf <- read_sf(here("data/state_county/states_simple.shp"))

### County polygons from US Census Bureau
### applied sf::st_simplify() function in separate Rscript to reduce resolution and file size
county_sf <- read_sf(here("data/state_county/us_county_simple.shp"))


### Superfund data
#superfund_csv <- read_csv(here("data/superfund_data/superfund_data_updated.csv")) |>
  #clean_names()

### convert superfund df to sf
#superfund_sf <- st_as_sf(superfund_csv,
                         #coords = c("longitude", "latitude"))

#st_crs(county_sf) ### EPSG 4296
#st_crs(superfund_sf) ### no crs

### set crs for superfund_sf to same same county_sf
#superfund_sf <- st_set_crs(superfund_sf, st_crs(county_sf))

#st_crs(superfund_sf) ### EPSG 4296

### superfund 1 mile and 3 mile buffers
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

### download all climate change models/scenarios suggested by EPA's LASSO Tool

### temp rasters region 1 (ME, NH, VT, MA, RI, CT)

temp_1a <- rast(here("rasters/climate_scenarios/lasso_data_1/LOCA_rcp85_eparegion1_CMCC-CMS_r1i1p1_tas_F_2041_2070_1981_2010_Annual.tif"))
temp_1b <- rast(here("rasters/climate_scenarios/lasso_data_1/LOCA_rcp85_eparegion1_GFDL-ESM2G_r1i1p1_tas_F_2041_2070_1981_2010_Annual.tif"))
temp_1c <- rast(here("rasters/climate_scenarios/lasso_data_1/LOCA_rcp85_eparegion1_HadGEM2-CC_r1i1p1_tas_F_2041_2070_1981_2010_Annual.tif"))
temp_1d <- rast(here("rasters/climate_scenarios/lasso_data_1/LOCA_rcp85_eparegion1_inmcm4_r1i1p1_tas_F_2041_2070_1981_2010_Annual.tif"))
temp_1e <- rast(here("rasters/climate_scenarios/lasso_data_1/LOCA_rcp85_eparegion1_MIROC-ESM_r1i1p1_tas_F_2041_2070_1981_2010_Annual.tif"))
temp_1f <- rast(here("rasters/climate_scenarios/lasso_data_1/LOCA_rcp85_eparegion1_MIROC-ESM-CHEM_r1i1p1_tas_F_2041_2070_1981_2010_Annual.tif"))

temp1_stack <- c(temp_1a, temp_1b, temp_1c, temp_1d, temp_1e, temp_1f) # Put all pr rasters in a stack
temp1_raster_avg <- reduce(temp1_stack, mean) #avg all climate models to generate 1 "mean" raster layer
temp1_raster_avg <- raster(temp1_raster_avg) ### convert to raster::raster layer

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
temp2_raster_avg <- app(temp2_stack, mean)
temp2_raster_avg <- raster(temp2_raster_avg) ### convert to raster::raster layer

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
temp3_raster_avg <- app(temp3_stack, mean)
temp3_raster_avg <- raster(temp3_raster_avg) ### convert to raster::raster layer

### temp rasters region 4 (KY, TN, NC, SC, MS, AL, GA, FL)

temp_4a <- rast(here("rasters/climate_scenarios/lasso_data_4/LOCA_rcp85_eparegion4_FGOALS-g2_r1i1p1_tas_F_2041_2070_1981_2010_Annual.tif"))
temp_4b <- rast(here("rasters/climate_scenarios/lasso_data_4/LOCA_rcp85_eparegion4_GFDL-CM3_r1i1p1_tas_F_2041_2070_1981_2010_Annual.tif"))
temp_4c <- rast(here("rasters/climate_scenarios/lasso_data_4/LOCA_rcp85_eparegion4_HadGEM2-CC_r1i1p1_tas_F_2041_2070_1981_2010_Annual.tif"))
temp_4d <- rast(here("rasters/climate_scenarios/lasso_data_4/LOCA_rcp85_eparegion4_HadGEM2-ES_r1i1p1_tas_F_2041_2070_1981_2010_Annual.tif"))
temp_4e <- rast(here("rasters/climate_scenarios/lasso_data_4/LOCA_rcp85_eparegion4_inmcm4_r1i1p1_tas_F_2041_2070_1981_2010_Annual.tif"))
temp_4f <- rast(here("rasters/climate_scenarios/lasso_data_4/LOCA_rcp85_eparegion4_MPI-ESM-MR_r1i1p1_tas_F_2041_2070_1981_2010_Annual.tif"))
temp_4g <- rast(here("rasters/climate_scenarios/lasso_data_4/LOCA_rcp85_eparegion4_MRI-CGCM3_r1i1p1_tas_F_2041_2070_1981_2010_Annual.tif"))

temp4_stack <- c(temp_4a, temp_4b, temp_4c, temp_4d, temp_4e, temp_4f, temp_4g)
temp4_raster_avg <- app(temp4_stack, mean)
temp4_raster_avg <- raster(temp4_raster_avg) ### convert to raster::raster layer

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
temp5_raster_avg <- app(temp5_stack, mean)
temp5_raster_avg <- raster(temp5_raster_avg) ### convert to raster::raster layer

### temp rasters region 6 (AR, LA, NM, OK, TX)

temp_6a <- rast(here("rasters/climate_scenarios/lasso_data_6/LOCA_rcp85_eparegion6_ACCESS1-3_r1i1p1_tas_F_2041_2070_1981_2010_Annual.tif"))
temp_6b <- rast(here("rasters/climate_scenarios/lasso_data_6/LOCA_rcp85_eparegion6_HadGEM2-ES_r1i1p1_tas_F_2041_2070_1981_2010_Annual.tif"))
temp_6c <- rast(here("rasters/climate_scenarios/lasso_data_6/LOCA_rcp85_eparegion6_inmcm4_r1i1p1_tas_F_2041_2070_1981_2010_Annual.tif"))
temp_6d <- rast(here("rasters/climate_scenarios/lasso_data_6/LOCA_rcp85_eparegion6_IPSL-CM5A-MR_r1i1p1_tas_F_2041_2070_1981_2010_Annual.tif"))
temp_6e <- rast(here("rasters/climate_scenarios/lasso_data_6/LOCA_rcp85_eparegion6_MIROC-ESM-CHEM_r1i1p1_tas_F_2041_2070_1981_2010_Annual.tif"))

temp6_stack <- c(temp_6a, temp_6b, temp_6c, temp_6d, temp_6e)
temp6_raster_avg <- app(temp6_stack, mean)
temp6_raster_avg <- raster(temp6_raster_avg) ### convert to raster::raster layer

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
temp7_raster_avg <- app(temp7_stack, mean)
temp7_raster_avg <- raster(temp7_raster_avg) ### convert to raster::raster layer

### temp rasters region 8 (UT, CO, WY, SD, ND, MT)

temp_8a <- rast(here("rasters/climate_scenarios/lasso_data_8/LOCA_rcp85_eparegion8_ACCESS1-0_r1i1p1_tas_F_2041_2070_1981_2010_Annual.tif"))
temp_8b <- rast(here("rasters/climate_scenarios/lasso_data_8/LOCA_rcp85_eparegion8_EC-EARTH_r2i1p1_tas_F_2041_2070_1981_2010_Annual.tif"))
temp_8c <- rast(here("rasters/climate_scenarios/lasso_data_8/LOCA_rcp85_eparegion8_GFDL-CM3_r1i1p1_tas_F_2041_2070_1981_2010_Annual.tif"))
temp_8d <- rast(here("rasters/climate_scenarios/lasso_data_8/LOCA_rcp85_eparegion8_HadGEM2-AO_r1i1p1_tas_F_2041_2070_1981_2010_Annual.tif"))
temp_8e <- rast(here("rasters/climate_scenarios/lasso_data_8/LOCA_rcp85_eparegion8_inmcm4_r1i1p1_tas_F_2041_2070_1981_2010_Annual.tif"))
temp_8f <- rast(here("rasters/climate_scenarios/lasso_data_8/LOCA_rcp85_eparegion8_MIROC-ESM-CHEM_r1i1p1_tas_F_2041_2070_1981_2010_Annual.tif"))
temp_8g <- rast(here("rasters/climate_scenarios/lasso_data_8/LOCA_rcp85_eparegion8_MRI-CGCM3_r1i1p1_tas_F_2041_2070_1981_2010_Annual.tif"))

temp8_stack <- c(temp_8a, temp_8b, temp_8c, temp_8d, temp_8e, temp_8f, temp_8g)
temp8_raster_avg <- app(temp8_stack, mean)
temp8_raster_avg <- raster(temp8_raster_avg) ### convert to raster::raster layer

### temp rasters region 9 (CA, NV, AZ)

temp_9a <- rast(here("rasters/climate_scenarios/lasso_data_9/LOCA_rcp85_eparegion9_CanESM2_r1i1p1_tas_F_2041_2070_1981_2010_Annual.tif"))
temp_9b <- rast(here("rasters/climate_scenarios/lasso_data_9/LOCA_rcp85_eparegion9_inmcm4_r1i1p1_tas_F_2041_2070_1981_2010_Annual.tif"))
temp_9c <- rast(here("rasters/climate_scenarios/lasso_data_9/LOCA_rcp85_eparegion9_MIROC-ESM_r1i1p1_tas_F_2041_2070_1981_2010_Annual.tif"))
temp_9d <- rast(here("rasters/climate_scenarios/lasso_data_9/LOCA_rcp85_eparegion9_MIROC-ESM-CHEM_r1i1p1_tas_F_2041_2070_1981_2010_Annual.tif"))
temp_9e <- rast(here("rasters/climate_scenarios/lasso_data_9/LOCA_rcp85_eparegion9_MIROC5_r1i1p1_tas_F_2041_2070_1981_2010_Annual.tif"))
temp_9f <- rast(here("rasters/climate_scenarios/lasso_data_9/LOCA_rcp85_eparegion9_MRI-CGCM3_r1i1p1_tas_F_2041_2070_1981_2010_Annual.tif"))

temp9_stack <- c(temp_9a, temp_9b, temp_9c, temp_9d, temp_9e, temp_9f)
temp9_raster_avg <- app(temp9_stack, mean)
temp9_raster_avg <- raster(temp9_raster_avg) ### convert to raster::raster layer

### temp rasters region 10 (WA, OR, ID)

temp_10a <- rast(here("rasters/climate_scenarios/lasso_data_10/LOCA_rcp85_eparegion10_CanESM2_r1i1p1_tas_F_2041_2070_1981_2010_Annual.tif"))
temp_10b <- rast(here("rasters/climate_scenarios/lasso_data_10/LOCA_rcp85_eparegion10_CMCC-CM_r1i1p1_tas_F_2041_2070_1981_2010_Annual.tif"))
temp_10c <- rast(here("rasters/climate_scenarios/lasso_data_10/LOCA_rcp85_eparegion10_CMCC-CMS_r1i1p1_tas_F_2041_2070_1981_2010_Annual.tif"))
temp_10d <- rast(here("rasters/climate_scenarios/lasso_data_10/LOCA_rcp85_eparegion10_HadGEM2-AO_r1i1p1_tas_F_2041_2070_1981_2010_Annual.tif"))
temp_10e <- rast(here("rasters/climate_scenarios/lasso_data_10/LOCA_rcp85_eparegion10_HadGEM2-AO_r1i1p1_tas_F_2041_2070_1981_2010_Annual.tif"))
temp_10f <- rast(here("rasters/climate_scenarios/lasso_data_10/LOCA_rcp85_eparegion10_MIROC-ESM-CHEM_r1i1p1_tas_F_2041_2070_1981_2010_Annual.tif"))
temp_10g <- rast(here("rasters/climate_scenarios/lasso_data_10/LOCA_rcp85_eparegion10_MRI-CGCM3_r1i1p1_tas_F_2041_2070_1981_2010_Annual.tif"))

temp10_stack <- c(temp_10a, temp_10b, temp_10c, temp_10d, temp_10e, temp_10f, temp_10g)
temp10_raster_avg <- app(temp10_stack, mean)
temp10_raster_avg <- raster(temp10_raster_avg) ### convert to raster::raster layer

temp_raster_merge <- merge(temp1_raster_avg, temp2_raster_avg, temp3_raster_avg, temp4_raster_avg, temp5_raster_avg,
                           temp6_raster_avg, temp7_raster_avg, temp8_raster_avg, temp9_raster_avg, temp10_raster_avg)

temp_raster_merge <- projectRaster(temp_raster_merge, crs = crs(county_sf))

### project() command didn't work well with SpatRaster object from 'terra' package, this achieves the same thing
#temp_raster_merge <- project(temp_raster_merge, crs = crs(county_sf))

### clean/prep data #########################################

state_names <- state_sf |>
  as.data.frame() |>
  select(statefp, state = name, state_abb = stusps)

county_state_names_df <- county_sf |>
  as.data.frame() |>
  select(statefp, countyfp, county = name, namelsad) |>
  inner_join(state_names, by = c("statefp")) |>
  mutate(countyfips = paste(statefp, countyfp, sep = "")) |>
  mutate(state_county = paste(state_abb, namelsad, sep = " ")) |>
  select(county, state, countyfips, state_county)

county_state_names_sf <- county_sf |>
  select(statefp, countyfp, county = name, namelsad) |>
  inner_join(state_names, by = c("statefp")) |>
  mutate(countyfips = paste(statefp, countyfp, sep = "")) |>
  mutate(state_county = paste(state_abb, namelsad, sep = " ")) |>
  select(county, state, countyfips, state_county)

prison_boundaries_sf <- prison_boundaries_sf |>
  select(-c("state", "county")) |>
  inner_join(county_state_names_df, by = c("countyfips"))

county_heat_100 <- county_heat_100 |>
  clean_names() |>
  select(state = x1, county = x2, historical, no_action_5) |>
  mutate(state_county = paste(state, county, sep = " "))

county_heat_100_sf <- county_state_names_sf |>
  inner_join(county_heat_100, by = c("state_county")) |>
  select(county = county.x, state = state.x, historical, projected = no_action_5)
  #filter(state %in% c("California", "Arizona", "Nevada", "New Mexico", "Texas",
                     # "Oklahoma", "Louisiana", "Arkansas")) ### same states included in raster


### UI ##########################################
ui <- shiny::navbarPage(theme = "shiny_theme.css",
  title = tags$div(
    "Intentional
    Indifference"),
    tags$head(
    tags$link(
      rel = "stylesheet", type = "text/css",
      href = "shiny_theme.css")
    ),
    tabPanel("Home",
             img(src = "prison_logo3.png", height = "100%", width = "100%%"),

             tags$div(
               h3(strong("Background")),
               p("Environmental justice is the belief that all people deserve safe and healthy conditions wherever they live, work, learn, pray, or play.
                 The environmental justice movement seeks to address historical inequities by including impacted individuals and communities in the decision-making process.
                 One catalyst for the environmental justice movement began", a("in 1982 in Warren County, NC,", href = 'https://www.nrdc.org/stories/environmental-justice-movement'),'when Black community members came together to protest the dumping of
                 hazardous waste in their rural community. The movement continues to grow due to the continued hard work of community organizers from predominantly BIPOC and
                 low-income communities.'),
               br(),
               p('Incarcerated people are one of the most vulnerable groups to environmental injustices, because they have little agency over their day-to-day movements and living conditions.
                 Additionally, they have few opportunities to seek improvements in their living environment. Incarcerated people have spoken out about issues like overcrowding, unsafe working conditions,
                 and lack of sanitation services, with both independent reports and peer-reviewed studies supporting these claims, challenging that these are issues of environmental injustice.
                 Reports have also documented the connection between carceral spaces and impacts to human health from exposure to', a('toxic chemicals', href = 'https://journals.sagepub.com/doi/abs/10.1177/14624745221114826'), 'and to', a('extreme heat events.', href = 'https://theintercept.com/series/climate-and-punishment/'),
                 'Similarly,', a('reports from across the U.S.', href = 'https://www.hrw.org/news/2005/09/21/new-orleans-prisoners-abandoned-floodwaters'), 'have identified incarcerated people left behind in the face of deadly natural disasters while other communities have been evacuated.'),
               br(),
               h3(strong("About this Site")),
               p('This RShiny app attempts a first-of-its-kind study mapping carceral facilities across the U.S. to determine how close they are to superfund sites,
                 as well as how likely they are to experience extreme heat events now, and in midcentury (years 2040-2070) using modeling data from', a('the Union of
                 Concerned Scientists', href = "https://www.ucsusa.org/resources/killer-heat-united-states-0"), 'and', a('the U.S. Environmental Protection Agency (EPA).', href = "https://lasso.epa.gov"), 'This interactive website builds upon tools like EnviroScreen by paying specific attention
                 to the environmental exposures of incarcerated people and providing context to users about the frequency, severity, and potential for harm.'),
               br(),
               p("We aspire to develop an open-access tool using R and ArcGIS that can be used by community members, policy makers, and researchers alike, to better understand the linkages
                 of harm between carceral communities and the marginalized communities they are often collocated with. This tool fills a critical education gap, while providing quantitative
                 backing to community advocates who are fighting for a more just and equitable system. It is our hope that this tool provides context to decision-makers, and will be used to
                 advocate for decarceration, improved emergency plans for carceral facilities, and increased climate resilience."),
               br(),
               br(),
               br(),
               h4(strong("Acknowledgement")),
               br(),
               p("We recognize that this information may be upsetting for individuals, particularly those that are or were housed,
                 or have loved ones that are or were housed within these facilities. This is a layered and challenging topic.
                 Please know that we are with you on this journey. If you are open to sharing, we welcome your feedback to improve this project."),
               br(),
               p("You may contact us at matthieu.huy@gmail.com or ebaker00@bren.ucsb.edu."),
            style = "padding: 20px 100px 40px 20px"),

tags$footer(
  h6(strong("Intentional Indifference: An Interactive Look at the Exposure of
            U.S. Carceral Facilities to Environmental Hazards")),
  h6(em("Created by Matthieu Huy. In partnership with Elijah Baker, and guidance from Dr. Summer Gray and Dr. David Pellow")),
  br(),
  style =
    "float:      center;
                text-align: left;
                bottom:     40px;
                width:      100%;
                height:     10px;
                color:      gray;
                padding:    30px 100px 40px 20px;
                z-index:    1;"
  )
), ### End Home

##### Superfund Maps Page #################################

tabPanel("Superfund Sites",

         tags$div(
           h3(strong("Background")),
           br(),
           p('From Baker E., Puig-Santana, A., Huy, M. P. "Assessing the risk and proximity of carceral facilities
               to superfund sites."', strong('(in preparation)')),
           p("In 1980 Congress established the Comprehensive Environmental Response,
             Compensation, and Liability Act (CERCLA), informally referred to as Superfund,
             to address the mismanagement of hazardous waste throughout the United States.
             The Superfund program provides the Environmental Protection Agency (EPA) with
             funding and the authority to identify and clean up Superfund sites through a
             remediation process. While the program has had successes, it also has flaws.
             There have been documented issues with getting a proposed site approved,
             and there are many sites with no clean-up date in sight."),
            br(),
            p("Modern life is dependent on many facilities and resources that can lead to the creation
              of a Superfund site. These include, but are not limited to: airports, power plants,
              hazardous landfills, waste incinerators, and mining and extractive activities.
              These are all examples of what land use planners refer to as “locally undesirable
              land uses” (LULUs).", a('Tara Opsal and Stephanie Malin', href = 'https://onlinelibrary.wiley.com/doi/10.1111/soin.12290'), "argue that prisons themselves are another form of
              LULU, as they are pushed out of wealthy communities and into poorer or more marginalized
              communities. Because Superfund sites are hazardous, they have often been utilized for
              siting LULUs, including prisons."),

           br(),

           tags$div(
             h3("Mapping Carceral Facilities' Exposure to Superfund Sites",
                style = "font-weight: bold")),
style = "padding: 20px 100px 40px 20px"
         ),

         sidebarPanel(
           tags$div(
             selectInput(
               inputId = "state",
               label = "Select a state:",
               choices = sort(unique(prison_boundaries_sf$state))
             ), #end selectinput
             selectInput(
               inputId = "county",
               label = "Select a county:",
               choices = NULL
             ),
             style = "color: black;"
           ), #end tags$div
           tags$div(
             p("These maps were created using data on active superfund sites from the", strong("EPA's Superfund National Priorities List (NPL)"),
               ", with state and county boundary shapefiles from", strong("U.S. Census Bureau.")),
             style = "color: black;"
           ),
         ), #end sidebarPanel

         mainPanel(

           withSpinner(
          tmapOutput("county_superfund_plot")),
           tags$div(
             br(),
             p("Although carceral facilities can contribute to environmental hazards, they are also built
             near or on undesirable lands, such as a Superfund site. Research on Superfund sites is
             challenging because of the fact that no two superfund sites are the same. In addition,
             carceral facilities located near or at a Superfund site can expose individuals with little
             to no agency to change their conditions to a myriad of hazards and toxins - resulting in
             environmental injustice. Currently, over 900 facilities fall within
             a 3-mile radius of a Superfund site,", a('with over 100 falling within a 1-mile radius of
             hazardous sites.', href = 'https://truthout.org/articles/america-s-toxic-prisons-the-environmental-injustices-of-mass-incarceration/')),
             br(),
             br(),
             br(),
             style = "padding: 10px 10px 0px 0px"
           )
         ), #end mainPanel

tags$footer(
  h6(strong("Intentional Indifference: An Interactive Look at the Exposure of
            U.S. Carceral Facilities to Environmental Hazards")),
  h6(em("Created by Matthieu Huy. In partnership with Elijah Baker, and guidance from Dr. Summer Gray and Dr. David Pellow")),
  br(),
        style =
          "float:      center;
                text-align: left;
                bottom:     40px;
                width:      100%;
                height:     10px;
                color:      gray;
                padding:    30px 100px 40px 20px;
                z-index:    1;"
         )
), #end Superfund


##### Heat Maps Page #################################

tabPanel("Heat Risk",

         tags$div(
           br(),
           h4(strong("“People tell us that they are suffering without access to breathable air.
                     Every summer we know this is coming and nothing changes.”"),
              style =
                "float:      center;
                text-align: center;"),
           p("- Julie Skarha, PhD in epidemiology at Brown University and author of
             'Heat-related mortality in U.S. state and private prisons: A case-crossover analysis'.",
             style =
               "float:      center;
                text-align: center;
                bottom:     40px;
                width:      100%;
                color:      gray;
                padding:    0px 0px 40px 20px;
                z-index:    1;"),
           h3(strong("Background")),
              br(),
              p("Extreme heat is the", a('leading cause of death', href = 'https://www.who.int/news-room/fact-sheets/detail/climate-change-heat-and-health#:~:text=Heat%20stress%20is%20the%20leading,a%20high%2Dcase%20fatality%20rate'),
                "from natural disasters globally, with extreme heat events in 2023", a('breaking records for heat-related deaths in the U.S.', href = 'https://www.wusf.org/health-news-florida/2024-06-02/ap-analysis-2023-record-us-heat-deaths-florida'),
                "Heat-related illness and deaths are not uncommon in U.S. prisons, jails, and detention centers.
                The likelihood and intensity of extreme heat events is increasing as a result of climate change,
                and with it, the risk to incarcerated people. However, these deaths are preventable."),

              p("Incarcerated people are uniquely vulnerable to heat-related illness for a multitude of reasons.
                The most pressing reason is that incarcerees lack the ability to alter their living conditions.
                They cannot open or close windows and cannot bring in air conditioning. In addition, people in prison are
                disproportionately diagnosed with diseases such as diabetes or hypertension that make individuals",
                a('particularly susceptible to harm from extreme heat.', href = 'https://grist.org/equity/new-study-people-dying-extreme-heat-in-prisons-us/?utm_campaign=Hot+News&utm_medium=email&_hsmi=248459899&_hsenc=p2ANqtz-_hs_Me2aJ16fjDL4QWqfYYLYqxauPN0cPn53EtXGqGdrmizF5SOAosR13LzqXTp-syv-1cFVsm04Zx3ULwnNFs3m8rDA&utm_content=248459899&utm_source=hs_email'),
                "Mental-illness is also prevalent among incarcerated people, and drugs used to treat certain mental illnesses",
                a('are known to cause heat-sensitivity.', href = 'https://www.prisonpolicy.org/blog/2019/06/18/air-conditioning/')),

              p(a('A study published in the peer-reviewed scientific journal PLOS ONE', href = 'https://journals.plos.org/plosone/article?id=10.1371/journal.pone.0281389'),
                'established a link between extreme heat and prison mortality. Researchers found that a temperature increase of 10-degrees above
                the average was associated with a significant increase in prison mortality, with deaths increasing 5.2% - or 6.7%
                among people diagnosed with heart disease. Although no publicly available national
                data exists to indicate prisons that lack air conditioning, the increase in mortality was much higher
                in the Northeast, at 21%, where prisons are less likely to have air conditioning or to be
                prepared to handle heatwaves. Shockingly,', a('Suicide rates increased by 22.8%', href = 'https://pubmed.ncbi.nlm.nih.gov/36857338/'), 'in the three days
                following an extreme heat event, as desperate prisoners reach a dangerous tipping point during these periods.'),
           br(),

           tags$div(
             h3("Mapping Carceral Facilities' Exposure Heat",
                style = "font-weight: bold")),
           style = "padding: 20px 100px 40px 20px"
         ),

         sidebarPanel(
           tags$div(
             selectInput(
               inputId = "state_heat",
               label = "Select a state:",
               choices = sort(unique(county_heat_100_sf$state))
             ), #end selectinput
             radioButtons("heat_map", label = "Choose Heat Map:",
                          choices = c("Historical",
                                      "Projected",
                                      "Temperature Change")),
             style = "color: black;"
           ), #end tags$div

           tags$div(

             p(strong("Historical"), ": This map displays the average number of days per year from 1971-2000 with a heat index
               above 100°F. Heat index is defined as the 'feels like' temperature resulting from the combination of temperature and humidity."),
             p(strong("Projected"), ": This map displays the projected average number of days per year with a heat index
               above 100°F in midcentury (2036-2065), with little to no action to curb current heat-trapping emissions growth."),
             p(strong("Temperature Change"), ": This map displays the projected increase in average daily temperatures
               by midcentury (2041-2070)."),
             br(),
             p("These maps were created using data on 'Days with a heat index above 100°F' from",
               strong("the Union of Concerned Scientists"), "as well as models of average temperature
               increase due climate change from the", strong("EPA's LASSO tool for climate change data.")),
             style = "color: black;"
           )
         ), #end sidebarPanel

         mainPanel(

           withSpinner(
             tmapOutput("county_heat_map")),
           tags$div(
             br(),
             p("Across the U.S, the number of days with a heat index above 100°F is expected to
               increase substantially by midcentury (years 2036-2065), with some areas that rarely experienced such heat in the past
               being subject to it regularly for the first time."),
             p("By combining multiple climate change models by the EPA, we predict an increase in average daily temperatures across the U.S. As
               this new reality comes to pass, extreme heat events will become more frequent as will heat-related
               deaths in prisons. Many carceral facilities are not equipped to protect inmates from prolonged exposure to heat currently.
               These facilities and more are woefully unprepared to address extreme heat events by mid-century predicted by our model."),
             p("This data highlights the need for prison reform and is a call to action for policymakers to 1) provide effective protections against extreme heat to incarcerated people
               and 2) reduce prison populations."),
             br(),
             br(),
             br(),
             br(),
             br(),
             br(),
             br(),
             style = "padding: 10px 10px 0px 0px"
           )
         ), #end mainPanel

         tags$footer(
           h6(strong("Intentional Indifference: An Interactive Look at the Exposure of
            U.S. Carceral Facilities to Environmental Hazards")),
           h6(em("Created by Matthieu Huy. In partnership with Elijah Baker, and guidance from Dr. Summer Gray and Dr. David Pellow")),
           br(),
        style =
          "float:      center;
                text-align: left;
                bottom:     40px;
                width:      100%;
                height:     10px;
                color:      gray;
                padding:    30px 100px 40px 20px;
                z-index:    1;"
         ) #end footer
), #end Heat Risk

##### Case Study #################################

tabPanel("Case Study",
         tags$div(
           h3(strong("Case Studies")),
           br(),
           p("The case studies shown on this page serve two purposes. 1) They highlight high-profile and egregious cases where incarcerated
             people are exposed to dangerous levels of pollutants or extreme heat in a way that puts their health at risk. 2) They exemplify how
             you - the user - can use this tool to conduct your own research by using the maps to find a facility of interest that overlaps with a Superfund site
             or is subject to extreme heat. Using the information from the tool on the site, you can then conduct your own case study on any facility."),
           tabsetPanel(
           tabPanel("Northwest Detention Center",

           h3(strong("The Northwest Detention Center, Tacoma Washington")),
           br(),
           img(src = "northwest_2.jpg", height = "100%", width = "100%%"),
           br(),
           br(),
           br(),
           p("Housing up to 1,575 immigrant men, women, and children, the Northwest Detention Center (NWDC) in Tacoma, Washington is one
             of the largest immigrant detention centers in the United States (NWIRP). It is owned and operated by a private company, the
             GEO Group, on behalf of US Immigration and Customs Enforcement (ICE). The facility opened in 2004 directly adjacent to the
             Commencement Bay Tideflats Superfund site following a problematic siting process that prioritized economic interest and over
             the health and safety of incarcerated people."),
           br(),
           style = "padding: 20px 100px 40px 20px",
           #end intro part of NWDC tab
         tabsetPanel(
           tabPanel("The Superfund Site",
           h3(strong("The Commencement Bay Tideflats Superfund Site")),
           br(),
           h4(strong("“Then the smells started coming. A strong petro chemical smell so strong that it permeats everything, gives us
                    headaches and makes my dog sneeze. There are times it is so bad I have to shut my windows or run home while on a
                    walk and try to protect myself indoors.”"),
              style =
                "float:      center;
                text-align: center;"),
           p("-Sara Wood, Northeast Tacoma Resident, expressing concerns during a public comment period discussing future industrial
            projects at the site (City of Tacoma 2017)",
            style =
              "float:      center;
                text-align: center;
                bottom:     40px;
                width:      100%;
                color:      gray;
                padding:    0px 0px 40px 20px;
                z-index:    1;"),
           img(src = "tacoma_superfund_epa_v2.png", height = "100%", width = "100%"),
           br(),
           br(),
           p("(Left) the Commencement Bay Tideflats Superfund Site and NWDC as shown in the interactive mapping tool.
             (Right) The Commencement Bay Tideflats Superfund Site as mapped by the USEPA in 2022. A white polygon showing the location of the NWDC has been added. The Green area
             labelled OU3 shows the Tacoma Tar Pits. Referencing the tar pits in their 2022 report, the EPA states they continue to work with
             responsible parties to monitor the clean up of sight and “may conduct an optimization study in the next couple
             of years“ (USEPA 2022).",
             style = "color:      gray;"),
           br(),
           br(),
           p("For thirty years, a coal-gasification plant leeched coal-tar wastes into the soil and groundwater of
             the Tacoma Tideflats. The area, which became known as the Tacoma Tar Pits, was included in the Commencement
             Bay Tideflats Superfund site (ID: WAD980726368) when the EPA designated it as a high priority toxic site in
             1983 due to high levels of volatile organic compounds and heavy metals in the groundwater and soil (U.S. Congress).
             The USEPA superfund contaminant list shows this area to be polluted with 27 different compounds, including antimony,
             arsenic, benzene, beryllium, cadmium, chromium VI, copper, lead, manganese, mercury, nickel, polychlorinated biphenyls,
             polycyclic aromatic hydrocarbons, selenium, silver, tetrachloroethylene, thallium, and zinc. These toxic compounds are
             found in soil, sediment, surface water, groundwater, and in the air on site (USEPA 2023). The quote above supports
             that these chemicals and toxic compounds are present in the air coming from this site. Being that residents of northeast Tacoma are not even
             living on the premises, it is likely that immigrant detainees at the Northwest Detention Center are suffering to a much
             greater degree"),
          br(),
          br(),
           ), #end Superfund Site tab
         tabPanel("Health Impacts",
         h3(strong("Health Impacts to Detainees")),
         br(),
         p(strong("Through breathing contaminated air, skin exposure, and water supplies, immigrants incarcerated at the NWDC can ingest the
             toxins present in the area which can lead to skin irritation, respiratory infections, developmental difficulties, cancers,
             organ failure, and early death (Pellow and Varzin 2019)."), "It is important to note that health problems caused by the environmental
             hazards at the NWDC are difficult to track and identify because detainees are constantly being deported or moved and are hard to
             track after they leave the facility. However, it is clear that this area should never be used to house human beings."),
         br(),
         ), #end Health tab
         tabPanel("Natural Disasters",
         h3(strong("Exposure to Natural Disasters")),
         br(),
         h4(strong("“In 2007, more than 900 pounds vaporized of poisonous chlorine gas leaked from the Pioneer Americas bleach plant at the Port of Tacoma,
                     sending at least 38 people to area hospitals. The lock-down detention center was just outside the evacuation zone designated for the
                     potentially deadly gas plume.”"),
            style =
              "float:      center;
                text-align: center;"),
         p("-Reported by the Tacoma News Tribune, this shows that emergency evacuation scenarios like those described above are not merely theoretical (Kamb 2012).",
           style =
             "float:      center;
                text-align: center;
                bottom:     40px;
                width:      100%;
                color:      gray;
                padding:    0px 0px 40px 20px;
                z-index:    1;"),
         p("The NWDC is built on a low-lying floodplain where soils are expected to liquify or shift dramatically during an earthquake, and
             the area would be completely wiped out in the event of a tsunami hitting the Puget Sound  (Pellow and Varzin 2019). Additionally,
             if nearby Mt. Rainier erupts, studies have shown that the area lies directly in the path of the volcanic mudslides that would follow (USGS 2016).
             The GEO group would have extremely limited time to evacuate almost 1500 detainees if any of these disasters occured, and yet has never released
             their emergency plans to the public (Pellow and Varzin 2019). They have reportedly ignored agreements and refused requests to provide local news
             outlets and city officials with copies of such plans when prompted to do so (Kamb 2012)."),
         br(),
         br(),
         ), # end Natural Disasters
         tabPanel("The Problematic Siting Process",
         h3(strong("The Problematic Siting Process of the Northwest Detention Center")),
         br(),
         p("Federal reports initially recommended the facility be built on a different site near the commercial port of Tacoma. A draft of the federal
           environmental impact statement in 2001 initially deemed the site where the NWDC ended up being built to be unfit because it contained excessive
           hazardous waste and was located adjacent to the Tacoma Tar Pits, posing “an unidentified risk” and “liability concerns” for the federal government (Kamb 2012).
           However, after pushback from port officials and local politicians, the initial recommendation was changed to express no preference between the two potential
           sites (Kamb 2012)."),
         br(),
         p(strong("“If we had to have it in Tacoma,” said Tacoma City Councilman Kevin Phelps during a 2012 interview, “I’d rather help try to encourage where it went, and
                  we didn’t want it on prime port property… Tacoma will make every possible effort to keep the INS from constructing a facility on this site” (Kamb 2012).")),
         br(),
         p("In December of 2001, the project was approved for construction at the site adjacent to the Tacoma Tar pits. The nearby Seattle Detention Center had reached maximum
           capacity by 1999, and this decision came at a time where there had been a significant increase in immigration to the U.S. and soon after the terrorist attacks of
           September 11, 2001, both of which contributed to sweeping changes in immigration policy and drove demand for such facilities. "),
         br(),
         br(),
         br(),
         ), #end siting process tab
         tabPanel("Resistance and Progress",
         br(),
         h3(strong("Resistance at the Northwest Detention Center")),
         br(),
         img(src = "nwdcprotest.png", height = "100%", width = "100%"),
         br(),
         br(),
         p("Demonstrators hold up signs while chanting in English and Spanish outside of the Northwest Detention Center protesting living conditions and opposing deportations on March 11, 2014.
           --Thomas Soerenes—The Tacoma News Tribune",
           style = "color:      gray;"),
         br(),
         br(),
         p("Prisoners have organized hunger strikes and other forms of resistance within the facility, including in 2014, 2017, and 2023 to protest their living conditions,
           citing concerns about abuse, food quality, and inadequate medical care, among other issues (Pellow and Vazin 2019). During a recent act of resistance in February of 2023,
           the grassroots group La Resistencia, which advocates “for the closing of the facility and an end to all detentions and deportations,” alleged in a news release Feb. 6
           that “tear gas” was used to control detainees after they asked another unit “to join in a protest against worsening conditions,” spreading beyond the intended target and
           reaching other parts of the facility. When prompted by local news outlets, ICE and the GEO group justified their actions and refused to share any written or video documentation
           related to the incident or the treatment of detainees exposed to chemicals, documentation that is required in federal standards about the use of force (Krell 2023). From outside
           the prison, organizations such as the Northwest Immigrant Rights Project, La Resistencia, and the Northwest Detention Center Resistance Coalition have fought to expose and
           improve conditions for detainees, provided them with legal resources, and staged protests."),
         br(),
         p("While the Northwest Detention Center continues to operate, these resistance efforts within and outside of the facility have led to much greater media attention on the living
           conditions at the center. Several lawsuits have been filed and new state legislation has been proposed to provide greater protections for detainees (Pellow and Varzin 2019).
           In 2023, the state of Washington passed a bill to create stronger health and safety protections for people detained in private prisons with the goal of bringing more state
           oversight into the Northwest Detention Center. However, the GEO group denied access to Washington state health and workplace inspectors twice in 2023 and is suing the state
           over the bill, arguing the policy is unconstitutional because it interferes with federal immigration enforcement (Deng 2024)."),
         br(),
         ) #end resistance tab
         ), # end NWDC sub tabs
         ), # end NWDC tab
         tabPanel("Angola Prison",
                  h3(strong("Louisiana State Penitentiary (Angola), West Feliciana Parish, Louisiana")),
                  br(),
                  img(src = "angola1.jpeg", height = "100%", width = "100%%"),
                  br(),
                  br(),
                  br(),
                  p("Notoriously known as Angola, after the former slave plantation that occupied the land, Louisiana State Penitentiary
                    is the nation’s largest adult maximum security prison, housing 6,300 prisoners and 1,800 staff. Louisiana has the highest
                    rate of incarceration in the world, with 1 of every 91 residents behind bars in 2021."),
             br(),
             style = "padding: 20px 100px 40px 20px",
             tabsetPanel(
               tabPanel("Extreme Heat Getting Worse",
                        br(),
                        h3(strong("Extreme Heat Expected to Get Worse in Louisiana")),
                        br(),
                        img(src = "louisiana3.png", height = "100%", width = "100%"),
                        br(),
                        br(),
                        p("(Left) Historical number of days per year above 100°F in Louisiana, and (Right) Expected number of days per year above 100°F by midcentury, as modeled by our tool",
           style = "color:      gray;"),
           br(),
           br(),
           p("According to climate models, and as seen in the map found under the Heat Risk tab, the state
             also experiences over 30 days per year above 100 degrees fahrenheit and is expected to experience 80 or more by midcentury."),
               ), # end heat subtab
           tabPanel("Legal Battles",
                    br(),
                    h3(strong("Legal Battles")),
                    br(),
                    p("In 2016, the state of Louisiana’s corrections department and attorney general’s
                      office spent over $1,067,000 in taxpayer money fighting a lawsuit filed on behalf
                      of three inmates with medical issues who claimed they were exposed to dangerous heat levels in their cells.")),
               tabPanel("Racial disparities and Labor practices",
               br(),
               h3(strong("Racial disparities and Labor practices")),
               br(),
               p("Many of those incarcerated at Angola, 74% of whom are Black, labor in farm fields for as
                 little as 2 cents an hour and report being placed in solitary confinement for being unwilling
                 or unable to perform this work, or if they are working too slowly.")), # end racial subtab
               tabPanel("Juveniles at Angola",
                        br(),
                        h3(strong("Juveniles housed in an adult prison")),
                        p("Recently, the prison which is only meant to house adults has started housing juveniles
                          “Since October 2022, 70-80 children, almost all Black boys, have been housed in the former
                          death row of Angola, where they were subjected to abusive conditions, including solitary confinement
                          for days—sometimes weeks— at a time, excessive force, and the routine use of handcuffs, restraints, and chemical agents.”"),
               ), # end kids subtab
         ), # end Angola subtabs
         ), # end Angola tab
         ),

         ), #end mainPanel

         tags$footer(
           h6(strong("Intentional Indifference: An Interactive Look at the Exposure of
            U.S. Carceral Facilities to Environmental Hazards")),
           h6(em("Created by Matthieu Huy. In partnership with Elijah Baker, and guidance from Dr. Summer Gray and Dr. David Pellow")),
           br(),
           style =
             "float:      center;
                text-align: left;
                bottom:     40px;
                width:      100%;
                height:     10px;
                color:      gray;
                padding:    30px 100px 40px 20px;
                z-index:    1;"
         )
         ), #end Case Study


##### References #################################

tabPanel("References",

           tags$div(h1("Acknowledgements", style = "font-weight: bold")),
           tags$div(
             br(),
             p("This project was intended to complement a working paper by Masters students at the Bren School of Environmental Science and Management:"),
             p(strong('Baker E., Puig-Santana, A., Kaveh, S., Lenihan, T. "Assessing the risk and proximity of carceral facilities
               to superfund sites." (in preparation).')),
             br(),
             p('Excerpts from this paper are included in the "Superfund Sites" page to provide context. Thank you to Elijah Baker and Alessandra Puig-Santana for their help.'),
             p('Thank you to Shayan Kaveh for his work in extracting and cleaning the
               data from HIFLD and the EPA on prison boundaries and superfund sites in preparation for this geospatial analysis.'),
             br()
           ),

           tags$div(h1("References", style = "font-weight: bold")),
           tags$div(
             br(),
             h3(strong("Home")),
             br(),
             p(a("“Climate and Punishment — Mass Incarceration in an Age of Environmental Crisis, an Investigation by The Intercept.”  (2022)",
                 href = 'https://theintercept.com/series/climate-and-punishment/ '),
               em('The Intercept'), '(accessed 25 Aug. 2022).'),
             br(),
             p(a(em("LASSO : Locating and Selecting Scenarios Online."),
                 href = "https://lasso.epa.gov"),
               "Environmental Protection Agency."),
             br(),
             p(a("“New Orleans: Prisoners Abandoned to Floodwaters.” (21 Sept. 2005)",
                 href = 'https://www.hrw.org/news/2005/09/21/new-orleans-prisoners-abandoned-floodwaters'),
               em('Human Rights Watch')),
             br(),
             p(a("Skelton, Renee, and Miller, Vernice. (August 2023)",
                 href = "https://www.nrdc.org/stories/environmental-justice-movement"),
               em("The Environmental Justice Movement")),
             br(),
             p(a("Toman, Elisa L. (July 2022).",
                 href = "https://doi.org/10.1177/14624745221114826"),
               '"Something in the Air: Toxic Pollution in and around U.S. Prisons."', em('Punishment & Society, SAGE Journals')),
             br(),
             h3(strong("Superfund Sites")),
             br(),
             p(a("Bernd, C., Loftus-Farren, Z., & Maureen Nandini Mitra. (1 June. 2017).",
                 href ="https://truthout.org/articles/america-s-toxic-prisons-the-environmental-injustices-of-mass-incarceration/"),
               '"America’s Toxic Prisons: The Environmental Injustices of Mass Incarceration."', em("Truthout.")),
             br(),
             p(a("Opsal, Tara, and Stephanie A. Malin. (May 2019).",
                 href = "https://onlinelibrary.wiley.com/doi/10.1111/soin.12290"),
               '"Prisons as LULUs: Understanding the Parallels between Prison Proliferation and Environmental Injustices."', em('Sociological Inquiry')),
             br(),
             p(a(em("Superfund: National Priorities List (NPL)."), "(updated 22 Feb. 2023)",
                 href = "https://www.epa.gov/superfund/superfund-national-priorities-list-npl"),
               "Environmental Protection Agency. (accessed 25 Aug. 2022)."),
             br(),
             h3(strong("Heat Risk")),
             br(),
             p(a("Bernd, C., Loftus-Farren, Z., & Maureen Nandini Mitra. (1 June. 2017).",
                 href ="https://truthout.org/articles/america-s-toxic-prisons-the-environmental-injustices-of-mass-incarceration/"),
               '"America’s Toxic Prisons: The Environmental Injustices of Mass Incarceration."', em("Truthout.")),
             br(),
             p(a("Brown, Allen. (1 Mar. 2023).  ",
                 href = "https://grist.org/equity/new-study-people-dying-extreme-heat-in-prisons-us/?utm_campaign=Hot+News&utm_medium=email&_hsmi=248459899&_hsenc=p2ANqtz-_hs_Me2aJ16fjDL4QWqfYYLYqxauPN0cPn53EtXGqGdrmizF5SOAosR13LzqXTp-syv-1cFVsm04Zx3ULwnNFs3m8rDA&utm_content=248459899&utm_source=hs_email"),
               '"Study: Extreme Heat Is Driving Deaths in US Prisons."', em("Grist.")),
             br(),
             p(a("Dahl, Kristina, Erika Spanger-Siegfried, Rachel Licker, Astrid Caldas, John Abatzoglou, Nicholas Mailloux, Rachel Cleetus, Shana Udvardy, Juan Declet-Barreto, and Pamela Worth. 2019.",
                 href =  "https://www.ucsusa.org/resources/killer-heat-united-states-0"),
               '"Killer Heat in the United States: Climate Choices and the Future of Dangerously Hot Days."', em("Cambridge, MA: Union of Concerned Scientists."), "91, 73–77."),
             br(),
             p(a(em("LASSO : Locating and Selecting Scenarios Online."),
                 href = "https://lasso.epa.gov"),
               "Environmental Protection Agency."),
             br(),
             p(a("Opsal, T., & Malin, S. A. (2020).",
                 href = "https://doi.org/10.1111/soin.12290"),
               '"Prisons as LULUs: Understanding the Parallels between Prison Proliferation and Environmental Injustices."', em("Sociological Inquiry,"), "90(3), 579–602."),
             br(),
             p(a("Skarha J, Spangler K, Dosa D, Rich JD, Savitz DA, Zanobetti A (2023).",
                 href = "https://doi.org/10.1371/journal.pone.0281389"),
               '"Heat-related mortality in U.S. state and private prisons: A case-crossover analysis."', em("PLOS ONE.")),
             br(),
             h3(strong("Maps")),
             br(),
             p(a(em("Prison Boundaries."), " (2020).",
                 href = "https://hifld-geoplatform.opendata.arcgis.com/datasets/geoplatform::prison-boundaries/about"),
               "Homeland Infrastructure Foundation Level Data (HIFLD)."),
             br(),
             p(a(em("State and County TIGER/Line Shapefiles."), " (2022).",
                 href = "https://www.census.gov/geographies/mapping-files/time-series/geo/tiger-line-file.2022.html#list-tab-790442341"),
               "U.S. Census Bureau."),
             br(),
             h3(strong("Case Study")),
             p(a("“Are We Cleaning Up? 10 Superfund Case Studies–Special Report” (June 1988)",
                 href = "https://www.princeton.edu/~ota/disk2/1988/8803/880301.PDF"),
               "U.S. Congress, Office of Technology Assessment."),
             br(),
             p(a("“Cleanups in Commencement Bay Nearshore/Tideflats Superfund Site” (May 2022)",
                 href = "https://semspub.epa.gov/work/10/100400472.pdf"),
               "Environmental Protection Agency Region 10."),
             br(),
             p(a("“Commencement Bay, Near Shore/Tide Flats Tacoma, Wa. Superfund Site Profile”",
                 href = "https://cumulis.epa.gov/supercpad/SiteProfiles/index.cfm?fuseaction=second.Healthenv&id=1000981"),
               "Environmental Protection Agency."),
             br(),
             p(a("Deng, Grace (January 2024)",
                 href = "https://washingtonstatestandard.com/2024/01/24/state-inspectors-denied-entry-to-privately-run-immigration-detention-center-in-tacoma/"),
               "“State inspectors denied entry to privately-run immigration detention center in Tacoma”", em("Washington State Standard.")),
             br(),
             p(a("Kamb, Lewis (2012)",
                 href = "https://www.thenewstribune.com/news/special-reports/article25860412.html"),
               "“A Rare Look inside Tacoma's Northwest Detention Center”", em("The News Tribune.")),
             br(),
             p(a("Krell, Lewis (2023)",
                 href = "https://www.thenewstribune.com/article272276968.html"),
               "“How are chemical agents used at the immigration detention center on the Tacoma Tideflats?”", em("The News Tribune.")),
             br(),
             p(a("Pellow, David, and Varzin, Jasmine (July 2019)",
                 href = "https://www.researchgate.net/publication/334607330_The_Intersection_of_Race_Immigration_Status_and_Environmental_Justice"),
               '"The Intersection of Race, Immigration Status, and Environmental Justice"'),
             br(),

             p(a("Tideflats Written Public Comments (September 2017)",
                 href = "https://cms.cityoftacoma.org/Planning/Tideflats/InterimRegulations/Tideflats%20Written%20Comments%20(thru%209-14-17).pdf"),
               "City of Tacoma Planning & Development Services Department."),
            br(),
            br(),
            br(),
            br(),
            br()
             ),

         tags$footer(
           h6(strong("Intentional Indifference: An Interactive Look at the Exposure of
            U.S. Carceral Facilities to Environmental Hazards")),
           h6(em("Created by Matthieu Huy. In partnership with Elijah Baker, and guidance from Dr. Summer Gray and Dr. David Pellow")),
           br(),
        style =
          "float:      center;
                text-align: left;
                bottom:     40px;
                width:      100%;
                height:     10px;
                color:      gray;
                padding:    30px 100px 40px 20px;
                z-index:    1;"
         ) #end footer
         ) # end refs page

) #end navbarPage

############# end ui ################################


# Define server logic required to draw a histogram
server <- function(input, output, session) {

  #Update the choices of county selectInput based on selected state
  observeEvent(input$state, {
    state_input <- input$state

    choices <- prison_boundaries_sf |>
      filter(state == state_input) |>
      pull(county)

    updateSelectInput(session, "county",
                      choices = sort(choices))
  })

  ### Superfund Sites reactive map ###

  tmap_mode("view")
  output$county_superfund_plot <- renderTmap({
    county_input <- input$county
    state_input <- input$state
    county_filtered <- county_state_names_sf |>
      filter(state == state_input) |>
      filter(county == county_input)
    county_prisons <- prison_boundaries_sf |>
      filter(state == state_input) |>
      filter(county == county_input)

    tm_shape(county_filtered) +
      tm_fill(col = "lightgrey",
              alpha = 0.3,
              id = "namelsad",
              popup.vars = c()) +
      tm_polygons() +
      tm_shape(superfund_noprison_3_sf) +
      tm_fill(col = "blue",
              alpha = 0.3,
              id = "site_name",
              popup.vars = c("site_name", "site_score", "status_2")) +
      tm_polygons("site_name") +
      tm_shape(superfund_withprisons_3_sf) +
      tm_fill(col = "red",
              alpha = 0.6,
              id = "site_name",
              popup.vars = c("site_name", "site_score", "status_2")) +
      tm_polygons("site_name") +
      tm_shape(superfund_noprison_1_sf) +
      tm_fill(col = "darkblue",
              alpha = 0.3,
              id = "site_name",
              popup.vars = c("site_name", "site_score", "status_2")) +
      tm_polygons("site_name") +
      tm_shape(superfund_withprisons_1_sf) +
      tm_fill(col = "darkred",
              alpha = 0.6,
              id = "site_name",
              popup.vars = c("site_name", "site_score", "status_2")) +
      tm_polygons() +
      tm_shape(county_prisons) +
      tm_dots(id = "name",
              col = "type",
              palette = c("grey90"),
              popup.vars = c("name", "address", "city", "county", "telephone",
                             "type", "status", "population", "capacity", "securelvl"),
              size = 0.035,
              legend.show = FALSE)
    })

  ### Heat Risk reactive map ###

  output$map <- renderPrint({ input$radio })

  tmap_mode("view")
  output$county_heat_map <- renderTmap({
    state_heat_input <- input$state_heat
    state_heat_filtered <- county_heat_100_sf |>
      filter(state == state_heat_input)
    state_prisons <- prison_boundaries_sf |>
      filter(state == state_heat_input)

    ###crop heat raster according to county
    temp_raster_cropped <- crop(temp_raster_merge, extent(state_heat_filtered))
    ###clip cropped raster according to county boundaries
    temp_raster_clipped <- mask(temp_raster_cropped, state_heat_filtered)

    if(input$heat_map == "Historical") {
      tm_shape(state_heat_filtered) +
        tm_fill("historical",
                title = "Number of days above 100°F",
                palette = "YlOrRd",
                alpha = 0.8,
                breaks = c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90),
                labels = c("0 - 10", "10 - 20", "20 - 30", "30 - 40", "40 - 50", "50 - 60", "60 - 70", "70 - 80", "80+")) +
        tm_polygons() +
        tm_shape(state_prisons) +
        tm_dots(id = "name",
                col = "type",
                palette = "gray90",
                popup.vars = c("name", "address", "city", "county", "telephone",
                               "type", "status", "population", "capacity", "securelvl"),
                size = 0.020,
                legend.show = FALSE)

    }

    else if(input$heat_map == "Projected") {
      tm_shape(state_heat_filtered) +
        tm_fill("projected",
                title = "Number of days above 100°F",
                palette = "YlOrRd",
                alpha = 0.8,
                breaks = c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90),
                labels = c("0 - 10", "10 - 20", "20 - 30", "30 - 40", "40 - 50", "50 - 60", "60 - 70", "70 - 80", "80+")) +
        tm_polygons() +
        tm_shape(state_prisons) +
        tm_dots(id = "name",
                col = "type",
                palette = c("gray90"),
                popup.vars = c("name", "address", "city", "county", "telephone",
                               "type", "status", "population", "capacity", "securelvl"),
                size = 0.02,
                legend.show = FALSE)
    }

    else if(input$heat_map == "Temperature Change") {
      tm_shape(temp_raster_clipped) +
        tm_raster(title = "Temperature increase (°F)",
                  palette = "YlOrRd",
                  alpha = 0.8,
                  breaks = c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9),
                  labels = c("0 - 1", "1 - 2", "2 - 3", "3 - 4", "4 - 5", "5 - 6", "6 - 7", "7 - 8", "8+"),
                  legend.show = TRUE) +
        tm_shape(state_prisons) +
        tm_dots(id = "name",
                col = "type",
                palette = c("gray90"),
                popup.vars = c("name", "address", "city", "county", "telephone",
                               "type", "status", "population", "capacity", "securelvl"),
                size = 0.020,
                legend.show = FALSE)
    }
  })
}


# Run the application
shinyApp(ui = ui, server = server)

# goals: load more rapidly
# terra package is faster than raster package for loading rasters, used in class
# Casey's repo "esm263" bren gis class, all sf and terra code

# st_simplify function -> simplify county outlines to reduce computational power

# compute the proportion of state's prisons that exceed a certain heat threshold

# slider input to have user decide the threshold
# if over x amount of days...

#checkbox group input
    #filter type %in% State, county....
    #can have multiple values (can select all, )

#look at increase in number of days above 100
# look at places where increase in number of days above 100 is statistically greater
# than the average increase across the country
# counties significantly higher than the average as most problematic


# how many prisons fall into red zones? Identify concentration of prisons falling within risk index
# Look at state policy on prisons
         # County levels
         # Case Studies would supplement maps nicely
         # textual data? identify most egregious cases where there were prisoner deaths
         # case study where animals (pigs) had A/C to keep them alive but not prisoners, prison sued?
         # How are we going mandate A/C?
         # Who has the power to change conditions in prisons? Question of Ownership?
         # Prison economy, (wildfire management in California) pick a state or two to dive into what the policies are
# social vulnerability index specific to incarcerated people vs how social vulnerability is conventionally studied
         # compounding factors, here's the data on heat but these other factors will also be making an impact
         # specific to incarcerated people
         # conventional social vulnerability factors are very different then prisoners
         # required to understand extreme heat map
# What data points to look for
# goal is to protect incarcerated people, but also to encourage abolishing prisons, reducing prison populations
# instead of making infrastructure more resilient
# tied to built environment, Hospitals a similar group?
# Lit review
# For Casey
     # integrate extreme heat events

# make points different colors based on State, Federal, or County prison
    #turn on and off different types of prison
    #only examine federal prisons...

    # Sarah Anderson?

    # goal this quarter: really finish up this tool, make it presentable to the public,
        # work on visuals (of the map and the website), work on text descriptions
        # identify audience, organizers who wouldb benefit from this tool, zoom with stakeholders
        # social and climate justice
        # tool to show "medical health concerns in this group, tool shows this prison could be in area of concern"
        # Fletcher scholarship for "research dexription" or RA-ship


#### superfund map
# prison points: different shapes based on ownership insteads of colors
  # impossible while using tmap_mode("view"), only works in "plot" mode, so would need to sacrifice interactivity
# add note explaining site_score for superfunds
# add superfund_csv data to geometry: name, score, status , site_epa_id, status2
# change colors based on status2
# prison points: different shapes based on ownership insteads of colors
