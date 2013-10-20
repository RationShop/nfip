# graphics using googleVis 

library(stringr)
library(googleVis)

# process policies and claims data from NFIP to get data for each county

# FEMA NFIP policies and claims stats from 
# (Policy & Claim Statistics for Flood Insurance)
# http://www.fema.gov/policy-claim-statistics-flood-insurance/policy-claim-statistics-flood-insurance/policy-claim-13

source("analyze_policies.R")
policies <- Fn_Analyze_Policies("nfip_policies_raw.txt", 
                                "output_policies_all.txt",
                                "output_policies_county.txt")
# convert state names to upper case, consistent with claims data
policies$state <- toupper(policies$state)

source("analyze_claims.R")
claims <- Fn_Analyze_Claims("nfip_claims_raw.txt",
                            "output_claims_all.txt",
                            "output_claims_county.txt")

# combine county data on policies and claims
all_data <- merge(policies, claims, by = c("state", "county"), all = TRUE)
# convert state and county names to be consistent with ggplot2
all_data$state <- tolower(all_data$state)
all_data$county <- tolower(all_data$county)
# remove " county" and " parish" from county names
all_data$county <- gsub(" county", "", all_data$county)
all_data$county <- gsub(" parish", "", all_data$county)

library(ggplot2)

county_df <- map_data("county")
names(county_df) <- c("long", "lat", "group", "order", "state", "county")
state_df <- map_data("state")

choropleth <- merge(county_df, all_data, by = c("state", "county"))
choropleth <- choropleth[order(choropleth$order), ]
choropleth <- na.omit(choropleth)
# Discretise rate to use with Brewer colour scheme - many options here
choropleth$graph <- cut(choropleth$policies, 
                        breaks = c(1, 30, 100, 300, 1000, 10000, 400000))

ggplot(data = choropleth) + 
  geom_polygon(aes(long, lat, group = group, fill = graph)) + 
  geom_path(data = state_df, aes(x = long, y = lat, group = group), 
            fill = NA, na.rm = TRUE) +
  scale_fill_brewer(palette = "Accent")

# # plot
# chart_1 <- gvisGeoChart(data = all_data, 
#                         locationvar = "state",
#                         colorvar = "policies", 
#                         options = list(region = "US", 
#                                        displayMode = "regions", 
#                                        resolution = "provinces",
#                                        enableRegionInteractivity = TRUE,
#                                        width = 600, 
#                                        height = 400),
#                         chartid = "abc")
# 
# plot(chart_1)
