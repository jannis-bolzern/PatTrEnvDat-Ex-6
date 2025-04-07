library("tidyverse")
library("sf")

# Import wild boar data and convert to sf object
wildschwein_BE <- read_delim("wildschwein_BE_2056.csv", ",") |>
  st_as_sf(coords = c("E", "N"), crs = 2056, remove = FALSE)

##Task 1: Import and visualize spatial data

# Import the vector dataset
fanel <- read_sf("Feldaufnahmen_Fanel.gpkg")

# View the column names and first few rows
names(fanel)
head(fanel)

# Get a summary of the data
summary(fanel)

# Check geometry type
st_geometry_type(fanel)

# Check data types
sapply(st_drop_geometry(fanel), class)

# Check coordinate reference system
st_crs(fanel)

##Task 2: Annotate Trajectories from Vector Data

# Convert DatetimeUTC to proper datetime format
wildschwein_BE$DatetimeUTC <- as.POSIXct(wildschwein_BE$DatetimeUTC, tz = "UTC")

# Extract month from datetime
wildschwein_BE$Month <- format(wildschwein_BE$DatetimeUTC, "%m")

# Filter for May (05) and June (06)
wildschwein_summer <- wildschwein_BE |>
  filter(Month %in% c("05", "06"))

# Verify the filtering
table(wildschwein_summer$Month)

# Quick plot to visualize overlap
plot(st_geometry(fanel), main = "Study Area with Wild Boar Locations")
plot(st_geometry(wildschwein_summer), add = TRUE, col = "red", pch = 20, cex = 0.5)

# Perform spatial join
wildschwein_annotated <- st_join(wildschwein_summer, fanel)

# Examine the results
head(wildschwein_annotated)
names(wildschwein_annotated)

# Check how many points got annotated with crop data
table(!is.na(wildschwein_annotated$Frucht))

# Count observations per crop type
crop_counts <- wildschwein_annotated |>
  st_drop_geometry() |>
  count(Frucht) |>
  arrange(desc(n))

# View the results
print(crop_counts)

# Visualize the top crops
ggplot(crop_counts, aes(x = reorder(Frucht, -n), y = n)) +
  geom_bar(stat = "identity") +
  labs(x = "Crop Type", y = "Number of Observations") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Extract hour from datetime
wildschwein_annotated$Hour <- format(wildschwein_annotated$DatetimeUTC, "%H")

# Calculate percentage per crop per hour
hourly_crop_dist <- wildschwein_annotated |>
  st_drop_geometry() |>
  group_by(Hour, Frucht) |>
  summarise(n = n()) |>
  mutate(percent = n / sum(n) * 100) |>
  filter(!is.na(Frucht))  # Remove NA values

# View results
head(hourly_crop_dist)

