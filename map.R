locations <- read.csv('data/locationdata.csv')

library(ggplot2)
library(maps)
library(dplyr)
library(shadowtext)

states_map <- map_data("state")

locations$State <- tolower(locations$location)

unique_states_map <- unique(states_map$region)
unique_states_location <- unique(locations$State)

# Print and compare to see any mismatches
print(setdiff(unique_states_location, unique_states_map))
print(setdiff(unique_states_map, unique_states_location))

map_data <- states_map %>%
  left_join(locations, by = c("region" = "State"))

# Define ecoregion colors
ecoregion_colors <- c("plains" = "lightgreen", "east" = "lightblue", 
                      "west" = "lightcoral", "desert" = "khaki")

coordinates <- data.frame(
  State = c(
    "Minnesota", "Pennsylvania", "Arizona", "California", "Colorado", 
    "Connecticut", "Florida", "Georgia", "Louisiana", "Maine", 
    "Maryland", "Massachusetts", "Montana", "Nebraska", "New Mexico",
    "New York", "Oregon", "South Carolina", "South Dakota", "Texas",
    "Vermont", "Virginia", "Washington", "Wisconsin"
  ),
  Latitude = c(
    46.2808, 40.8781, 34.2744, 37.1842, 38.9972, 
    41.6220, 28.6306, 32.6414, 31.0689, 45.3694, 
    39.0550, 42.2597, 47.0528, 41.5378, 34.4072,
    42.9539, 43.9336, 33.9169, 44.4442, 31.4758,
    44.0686, 37.5214, 47.3825, 44.6242
  ),
  Longitude = c(
    -94.3053, -77.7997, -111.6603, -119.4697, -105.5478, 
    -72.7272, -82.4497, -83.4425, -91.9967, -69.2428, 
    -76.7908, -71.8083, -109.6333, -99.7950, -106.1125,
    -75.5267, -120.5583, -80.8964, -100.2264, -99.3311, 
    -72.6658, -78.8536, -120.4472, -89.9942
  )
)

known_states <- coordinates$State

all_states <- c(
  "Minnesota", "Pennsylvania", "Arizona", "California", "Colorado", 
  "Connecticut", "Florida", "Georgia", "Louisiana", "Maine", 
  "Maryland", "Massachusetts", "Montana", "Nebraska", "New Mexico",
  "New York", "Oregon", "South Carolina", "South Dakota", "Texas",
  "Vermont", "Virginia", "Washington", "Wisconsin",
  "Alabama", "Arkansas", "Delaware", "District of Columbia", "Idaho",
  "Illinois", "Indiana", "Iowa", "Kansas", "Kentucky",
  "Michigan", "Mississippi", "Missouri", "Nevada", "New Hampshire",
  "New Jersey", "North Carolina", "North Dakota", "Ohio", "Oklahoma",
  "Rhode Island", "Tennessee", "Utah", "West Virginia", "Wyoming"
)

coordinates_complete$State <- tolower(coordinates_complete$State)

missing_states <- setdiff(all_states, known_states)

missing_data <- data.frame(
  State = missing_states,
  Latitude = NA,
  Longitude = NA
)

write.csv(coordinates, "coordinates.csv", row.names = FALSE)

map <- ggplot(data = map_data) +
  geom_polygon(aes(x = long, y = lat, group = group, fill = ecoregion),
               color = "black", size = 0.2) +
  scale_fill_manual(values = ecoregion_colors) +
  geom_shadowtext(data = locations,
            aes(x = Longitude, y = Latitude, label = n),
            color = "black", bg.colour = 'white', size = 3, inherit.aes = FALSE, 
            fontface = "bold") +
  coord_fixed(1.3) +
  theme_minimal() +
  labs(fill = "Ecoregion", x = 'Longitude', y = 'Latitude')

ggsave("map.png", plot = map, width = 10, height = 8, dpi = 300)
