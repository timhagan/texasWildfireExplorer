##################
### INITIALIZE ###
##################

library(DescTools)
library(data.table)
library(tidycensus)
library(sf)
library(dplyr)
library(ggplot2)
library(ggridges)
library(hrbrthemes)
library(scales)
library(viridis)
library(plotly)
library(jsonlite)
library(tidycensus)
library(tigris)
library(jsonlite)
library(Hmisc)
library(caret)

### Choose your State ###
us_state_choice <- "TX" # Use two character abbreviation
uac_year <- 2020 # year for definition of UAC boundaries
test_year <- 2022 # all wildfires during and after this year will be held out of training

#####################
### Download Data ###
#####################

### Wildfire Data ### 
# CRS = 4326

# Historical Data
wildfire_locations       <- st_read("https://opendata.arcgis.com/datasets/d8fdb82b3931403db3ef47744c825fbf_0.geojson")
wildfire_locations_state <- wildfire_locations[wildfire_locations$POOState==paste0("US-",us_state_choice),]
rm(wildfire_locations); gc(); gc()

# Year to date data
wildfire_locations_yearToDate       <- st_read("https://opendata.arcgis.com/datasets/51192330d3f14664bd69b6faed0fdf05_0.geojson")
wildfire_locations_yearToDate_state <- wildfire_locations_yearToDate[wildfire_locations_yearToDate$POOState==paste0("US-",us_state_choice),]
rm(wildfire_locations_yearToDate); gc(); gc()

# Combine Wildfire Data
wildfire_locs <- unique(rbind(wildfire_locations_state, wildfire_locations_yearToDate_state))
rm(wildfire_locations_state, wildfire_locations_yearToDate_state); gc(); gc()

# Convert to sf object
wildfires_sf <- st_as_sf(wildfire_locs, crs = 4326)
rm(wildfire_locs); gc(); gc()

### Census Data ###
# register for a census API key at http://api.census.gov/data/key_signup.html

# Get population data
library(tidycensus)
state_population <- get_estimates(geography = "county",
                                  state = us_state_choice, 
                                  variables = "POP", 
                                  geometry = T)
state_population$GEOID <- as.integer(state_population$GEOID)
names(state_population) <- c("GEOID", "NAME", "census_var", "population", "geometry")
state_population$`Population (Thousands)` <- state_population$population/1000

# Get income data
state_income     <- get_acs(geography = "county", 
                            state = us_state_choice, 
                            variables = "B19013_001", 
                            geometry = T)
state_income$GEOID <- as.integer(state_income$GEOID)
names(state_income) <- c("GEOID", "NAME", "census_var", "median_income", "med_income_moe", "geometry")

# Get State Geometry
state_geometry <- get_acs(geography = "state", 
                          state = us_state_choice, 
                          variables = "B19013_001", # a variable is required to get state geometry
                          geometry = T)

# Get Urban Area Clusters (UACs)
uacs          <- urban_areas(year = uac_year)
uacs_state    <- uacs[grepl(paste0(", ", us_state_choice), uacs$NAME10, fixed = T),]
rm(uacs); gc(); gc()


########################################
### CLEANING and FEATURE ENGINEERING ###
########################################

### Align CRS ###
wildfires_sf <- st_as_sf(wildfires_sf)
wildfires_sf <- st_transform(wildfires_sf, st_crs(state_geometry))

### Calculate distance from each wildfire to nearest UAC
nearest_uac <- st_nearest_feature(wildfires_sf, uacs_state)
distance_to_nearest_uac <- st_distance(wildfires_sf, uacs_state[nearest_uac,],
                                       by_element = T)

wildfires_sf$distToNearestUAC <- distance_to_nearest_uac

wildfires_sf$distToNearestUACbin <- cut2(as.numeric(wildfires_sf$distToNearestUAC), g = 5)

### Filter Wildfires to those discovered on or after 2015 because the note from data  
### source indicates that data prior to the implementation of IRWIN (2014) are incomplete
wildfires_sf_dt <- data.table(wildfires_sf)[year(FireDiscoveryDateTime)>=2015,
                                            .(geometry, distToNearestUAC, distToNearestUACbin, IncidentName, 
                                              IncidentTypeCategory, IncidentTypeKind, 
                                              UniqueFireIdentifier, POOProtectingUnit, POOProtectingAgency,
                                              CalculatedAcres, ContainmentDateTime, ControlDateTime, 
                                              DailyAcres, DiscoveryAcres, EstimatedCostToDate, 
                                              FireCause, FireCauseGeneral, FireCauseSpecific, 
                                              FireDiscoveryDateTime, FireOutDateTime, GACC, 
                                              InitialLatitude, InitialLongitude, InitialResponseAcres, 
                                              InitialResponseDateTime, POOCity, POOCounty, POOFips, 
                                              POOLandownerCategory, POOLandownerKind, POOState, 
                                              PredominantFuelGroup, PredominantFuelModel, PrimaryFuelModel,
                                              SecondaryFuelModel, TotalIncidentPersonnel)]
rm(wildfires_sf)

# Fire Cause
wildfires_sf_dt$FireCause_Clean <- ifelse(wildfires_sf_dt$FireCause %in% c("", "Undetermined"),
                                          "Unknown",
                                          wildfires_sf_dt$FireCause)

wildfires_sf_dt$FireCause_Clean <- ifelse(is.na(wildfires_sf_dt$FireCause_Clean),
                                          "Unknown",
                                          wildfires_sf_dt$FireCause_Clean)

### Fuel Type
wildfires_sf_dt$PrimaryFuelModel_Clean <- ifelse(wildfires_sf_dt$PrimaryFuelModel %in% c(""),
                                                 "Missing",
                                                 wildfires_sf_dt$PrimaryFuelModel)

### Landowner Kind
wildfires_sf_dt$POOLandownerKind_Clean <- ifelse(wildfires_sf_dt$POOLandownerKind %in% c(""),
                                                 "Missing",
                                                 wildfires_sf_dt$POOLandownerKind)

### Dates
wildfires_sf_dt$discovery_month <- month(wildfires_sf_dt$FireDiscoveryDateTime)
wildfires_sf_dt$discovery_year  <- year(wildfires_sf_dt$FireDiscoveryDateTime)
wildfires_sf_dt$discovery_yday  <- yday(wildfires_sf_dt$FireDiscoveryDateTime)
wildfires_sf_dt$discovery_YM    <- substr(wildfires_sf_dt$FireDiscoveryDateTime,1,7)

wildfires_sf_dt$contain_month  <- month(wildfires_sf_dt$ContainmentDateTime)
wildfires_sf_dt$contain_year   <- year(wildfires_sf_dt$ContainmentDateTime)
wildfires_sf_dt$contain_yday   <- yday(wildfires_sf_dt$ContainmentDateTime)

wildfires_sf_dt$length_in_days <- round(as.integer(difftime(wildfires_sf_dt$ContainmentDateTime,
                                                            wildfires_sf_dt$FireDiscoveryDateTime,
                                                            units = "hours"))/24, 2)

wildfires_sf_dt$length_in_days <- ifelse(wildfires_sf_dt$length_in_days<0,
                                         NA,
                                         wildfires_sf_dt$length_in_days)

### Imputations
library(mice)
wildfires_sf_dt$X <- as.numeric(st_coordinates(st_as_sf(wildfires_sf_dt))[,1])
wildfires_sf_dt$Y <- as.numeric(st_coordinates(st_as_sf(wildfires_sf_dt))[,2])

# Predictive Mean Matching
imp_obj <- mice(data = data.table(wildfires_sf_dt)[,.(X, Y, DailyAcres, DiscoveryAcres, 
                                                      length_in_days, discovery_yday)])

complete_data <- complete(imp_obj, "broad")
rm(imp_obj); gc();gc()

wildfires_sf_dt$DailyAcres_imp       <- rowMeans(complete_data[grepl(pattern = "DailyAcres", names(complete_data))])
wildfires_sf_dt$DiscoveryAcres_imp   <- rowMeans(complete_data[grepl(pattern = "DiscoveryAcres", names(complete_data))])
wildfires_sf_dt$length_in_days_imp   <- rowMeans(complete_data[grepl(pattern = "length_in_days", names(complete_data))])
rm(complete_data); gc();gc()

### Further preparations for plotting
wildfires_sf_dt$FireInfo <- paste("<strong>", "Acres Burned:", "</strong>", wildfires_sf_dt$DailyAcres_imp, "<br>",
                                  "<strong>", "Discovery Date:", "</strong>", as.Date(wildfires_sf_dt$FireDiscoveryDateTime), "<br>",
                                  "<strong>", "FireCause:", "</strong>", wildfires_sf_dt$FireCause_Clean)

# wildfires_sf_ord_date$FireInfo <- paste("<strong>", "Acres Burned:", "</strong>", wildfires_sf_ord_date$`Daily Acres`, "<br>",
#                                         "<strong>", "Discovery Date:", "</strong>", as.Date(wildfires_sf_ord_date$`Fire Discovery Date`), "<br>",
#                                         "<strong>", "FireCause:", "</strong>", wildfires_sf_ord_date$`Fire Cause`)


### Merges
state_info <- st_join(state_income, 
                      state_population,
                      st_equals)

wildfires_sf_dt$POOFips <- as.integer(wildfires_sf_dt$POOFips)
# wildfires_sf_inc <- merge(wildfires_sf_dt, st_drop_geometry(state_income),
#                           by.x = "POOFips",
#                           by.y = "GEOID")
# rm(wildfires_sf_dt)

wildfires_sf_pop <- merge(wildfires_sf_dt, st_drop_geometry(state_info), 
                          by.x = "POOFips",
                          by.y = "GEOID.x")

### Order by date for model training
wildfires_sf_ord_date <- wildfires_sf_pop[order(wildfires_sf_pop$FireDiscoveryDateTime, decreasing = T),]
rm(wildfires_sf_pop)

# Using log transform fire size for visualization purposes
wildfires_sf_ord_date$DailyAcres_ws <- Winsorize(wildfires_sf_ord_date$DailyAcres_imp, minval = 1, 
                                                 maxval = max(wildfires_sf_ord_date$DailyAcres_imp))
wildfires_sf_ord_date$DailyAcres_log <- log(wildfires_sf_ord_date$DailyAcres_ws)

wildfires_sf_ord_date$FireCause_Natural <- ifelse(wildfires_sf_ord_date$FireCause_Clean=="Natural",
                                                  1,0)
wildfires_sf_ord_date$FireCause_Human <- ifelse(wildfires_sf_ord_date$FireCause_Clean=="Human",
                                                1,0)


wildfires_sf_ord_date$POOLandownerKind_Clean <- ifelse(is.na(wildfires_sf_ord_date$POOLandownerKind_Clean),
                                                       "Unknown",
                                                       wildfires_sf_ord_date$POOLandownerKind_Clean)

wildfires_sf_ord_date$POOLandownerKind_Federal <- ifelse(wildfires_sf_ord_date$POOLandownerKind_Clean=="Federal",
                                                         1,0)
wildfires_sf_ord_date$POOLandownerKind_Other <- ifelse(wildfires_sf_ord_date$POOLandownerKind_Clean=="Other",
                                                       1,0)
wildfires_sf_ord_date$POOLandownerKind_Private <- ifelse(wildfires_sf_ord_date$POOLandownerKind_Clean=="Private",
                                                         1,0)


### Train and Test Split
out_of_time_train_idx <- wildfires_sf_ord_date$discovery_year < test_year
wildfires_sf_ord_date$trainingData <- wildfires_sf_ord_date$discovery_year < test_year
train_data <- wildfires_sf_ord_date[out_of_time_train_idx,]
test_data <- wildfires_sf_ord_date[!out_of_time_train_idx,]

###  RF
wf.rf.mod.log <- train(DailyAcres_log ~ X + Y + distToNearestUAC + 
                         median_income + population +
                         FireCause_Natural + FireCause_Human + 
                         discovery_year + discovery_yday +
                         POOLandownerKind_Federal + POOLandownerKind_Other + POOLandownerKind_Private,
                       data = train_data,
                       method = "rf",
                       trControl = trainControl(method = "timeslice",
                                                initialWindow = aggregate(train_data$POOFips,
                                                                          list(train_data$discovery_year),
                                                                          length)$x[1],
                                                horizon = max(aggregate(train_data$POOFips,
                                                                        list(train_data$discovery_year),
                                                                        length)$x),
                                                skip = round(min(aggregate(train_data$POOFips,
                                                                           list(train_data$discovery_year),
                                                                           length)$x)/4), 
                                                fixedWindow = T),
                       tuneGrid = expand.grid(mtry = seq(2,4, by = 1)))
rm(train_data, test_data)

wildfires_sf_ord_date$rf.log.predictions <- predict(wf.rf.mod.log, 
                                                    newdata = wildfires_sf_ord_date)

library(randomForest)
varImpPlot(wf.rf.mod.log$finalModel)

### final preparations ###
# state_info <- st_join(state_income, 
#                       state_population,
#                       st_equals)

wildfires_sf_ord_date <- st_as_sf(wildfires_sf_ord_date)

names(wildfires_sf_ord_date)[names(wildfires_sf_ord_date)=="DailyAcres_imp"] <- "Daily Acres"
names(wildfires_sf_ord_date)[names(wildfires_sf_ord_date)=="FireCause_Clean"] <- "Fire Cause"

names(state_info)[names(state_info)=="NAME.x"] <- "County Name"
names(state_info)[names(state_info)=="population"] <- "Population"

wildfires_sf_ord_date$`Fire Discovery Date` <- as.Date(wildfires_sf_ord_date$FireDiscoveryDateTime)

wildfires_sf_ord_date$populationBin <- cut2(wildfires_sf_ord_date$population, g = 4)

### transform crs for leaflet
wildfires_sf_ord_date$`Fire Cause` <- as.factor(wildfires_sf_ord_date$`Fire Cause`)

### sunburst prep
cut_points <- cut2(state_info$median_income, g = 4, onlycuts = T)
state_info$income_level <- cut(state_info$median_income,
                               breaks = cut_points,
                               labels = c("Bot 25%", "25%-50%", "50-75%", "Top 25%"),
                               include.lowest = T)
wildfires_sf_ord_date$income_level <- cut(wildfires_sf_ord_date$median_income,
                                          breaks = cut_points,
                                          labels = c("Bot 25%", "25%-50%", "50-75%", "Top 25%"),
                                          include.lowest = T)


num_fires_in_county <- aggregate(wildfires_sf_ord_date$NAME.x, by = list(wildfires_sf_ord_date$NAME.x), FUN = length)
names(num_fires_in_county) <- c("NAME.x", "Count of Fires")
state_info1 <- merge(state_info, num_fires_in_county, 
                     by.x = "County Name", 
                     by.y = "NAME.x", 
                     all.x = T)
state_info1$`Count of Fires` <- ifelse(is.na(state_info1$`Count of Fires`),
                                       0,
                                       state_info1$`Count of Fires`)

### ridges prep
cut_points <- c(0, 5, 10, 50, 150, 500, 1000)
wildfires_sf_ord_date$rf.log.predictions.bin <- cut2(exp(wildfires_sf_ord_date$rf.log.predictions), 
                                                     cuts = cut_points)
wildfires_sf_ord_date$highlighted <- F

wildfires_sf_ord_date <- st_transform(wildfires_sf_ord_date, crs = '+proj=longlat +datum=WGS84')
state_info1 <- st_transform(state_info1, crs = '+proj=longlat +datum=WGS84')

state_info1$lightgrey <- "lightgrey"
wildfires_sf_ord_date$lightgrey <- "lightgrey"

saveRDS(wildfires_sf_ord_date, "wildfire_data.rds")
saveRDS(state_info1, "state_data.rds")

