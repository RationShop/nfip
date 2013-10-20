# analyze policies and claims data from NFIP for each county
# FEMA NFIP policies and claims stats from 
# (Policy & Claim Statistics for Flood Insurance)
# http://www.fema.gov/policy-claim-statistics-flood-insurance/policy-claim-statistics-flood-insurance/policy-claim-13

# required libraries
library(stringr)
library(ggplot2)

# required functions
source("analyze_policies.R") # function to preprocess policies data
source("analyze_claims.R") # function to preprocess claims data

# policies data
policies <- Fn_Analyze_Policies() 

# claims data
claims <- Fn_Analyze_Claims()  

# combine county data on policies and claims
all_data <- merge(policies, claims, by = c("state", "county"), all = TRUE)
# convert state and county names to be consistent with ggplot2
all_data$state <- tolower(all_data$state)
all_data$county <- tolower(all_data$county)
# remove " county" and " parish" from county names
all_data$county <- gsub(" county", "", all_data$county)
all_data$county <- gsub(" parish", "", all_data$county)

# geo referencing info on counties and states
geo_county <- map_data("county")
names(geo_county) <- c("long", "lat", "group", "order", "state", "county")
geo_state <- map_data("state")

# data for graphics
gfx_data <- merge(geo_county, all_data, by = c("state", "county"))
gfx_data <- gfx_data[order(gfx_data$order), ]
# discretise variables of interest
gfx_data$policies_gfx <- cut(gfx_data$policies, 
                             breaks = c(1, 30, 100, 300, 1000, 10000, 400000),
                             labels = c("1 - 30", "30 - 100", "100 - 300", 
                                        "300 - 1k", "1k - 10k", "10k - 400k"))
gfx_data$payments_gfx <- cut(gfx_data$total_pay/10^6, 
                             breaks = c(0, 0.05, 0.40, 1.7, 6.3, 50, 7300),
                             labels = c("0 - 50k", "50k - 400k", "400k - 1.7M", 
                                        "1.7M - 6.3M", "6.3M - 50M", "50M - 7.3B"))

# plot policies
plot_map <- ggplot(data = gfx_data) + 
  geom_polygon(aes(long, lat, group = group, fill = policies_gfx)) + 
  geom_path(data = geo_state, 
            aes(x = long, y = lat, group = group), 
            fill = NA, 
            na.rm = TRUE) +
  labs(list(title = "NFIP Policies Per County", x = NULL, y = NULL)) +
  guides(fill = guide_legend(title = "Policies Per County")) +
  scale_fill_brewer(palette = "Accent")
print(plot_map)

# plot payments
plot_map <- ggplot(data = gfx_data) + 
  geom_polygon(aes(long, lat, group = group, fill = payments_gfx)) + 
  geom_path(data = geo_state, 
            aes(x = long, y = lat, group = group), 
            fill = NA, 
            na.rm = TRUE) +
  labs(list(title = "NFIP Payments Per County (USD)", x = NULL, y = NULL)) +
  guides(fill = guide_legend(title = "Payments Per County (USD)")) +
  scale_fill_brewer(palette = "Accent")
print(plot_map)

