library("tidyverse")
library("sf")
library("tmap")
library("terra")

# Import wild boar data and convert to sf object
wildschwein_BE <- read_delim("wildschwein_BE_2056.csv", ",") |>
  st_as_sf(coords = c("E", "N"), crs = 2056, remove = FALSE)


# Task 1: Import and visualize spatial data -------------------------------


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


# Task 2: Annotate Trajectories from Vector Data --------------------------


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


# Task 3: Explore Annotated Trajectories ----------------------------------


# Filter to only the three main animals and top crops
wildschwein_individuals <- wildschwein_annotated |>
  filter(TierName %in% c("Sabi", "Ruth", "Rosa")) |>
  filter(!is.na(Frucht)) |>  # Remove points without crop info
  mutate(
    Hour = as.numeric(format(DatetimeUTC, "%H")),
    Month = format(DatetimeUTC, "%B"),
    Month = factor(Month, levels = c("May", "June")),
    TierName = factor(TierName, levels = c("Sabi", "Ruth", "Rosa"))
  )

# Get top 5 crops for each animal
top_crops_per_animal <- wildschwein_individuals |>
  st_drop_geometry() |>
  group_by(TierName, Frucht) |>
  summarise(n = n()) |>
  group_by(TierName) |>
  top_n(5, n) |>
  pull(Frucht) |>
  unique()

# Plot activity patterns by hour for each animal
ggplot(wildschwein_individuals |> filter(Frucht %in% top_crops_per_animal), 
       aes(x = Hour, fill = Frucht)) +
  geom_histogram(binwidth = 1, position = "stack") +
  facet_wrap(~TierName, ncol = 1, scales = "free_y") +
  labs(title = "Hourly Activity Patterns by Animal and Crop Type",
       x = "Hour of Day",
       y = "Number of Observations",
       fill = "Crop Type") +
  scale_x_continuous(breaks = seq(0, 23, by = 3)) +
  theme_minimal() +
  theme(legend.position = "bottom")

# Create individual maps for each animal
# Visualization with top 20 crops and individual animals
tm_shape(fanel) +
  tm_polygons("Frucht", palette = "Set3", title = "Crop Type") +
  tm_shape(wildschwein_individuals) +
  tm_dots(size = 0.1, col = "TierName", 
          palette = c(Sabi = "red", Ruth = "blue", Rosa = "green")) +
  tm_facets(by = "TierName", ncol = 1) +
  tm_layout(legend.outside = TRUE)

# Create bar plot of crop preferences by animal
ggplot(wildschwein_individuals |> filter(Frucht %in% top_crops_per_animal), 
       aes(x = Frucht, fill = TierName)) +
  geom_bar(position = "dodge") +
  labs(title = "Crop Preferences by Individual Wild Boar",
       x = "Crop Type",
       y = "Number of Observations") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_brewer(palette = "Set2", name = "Animal") +
  facet_wrap(~Month, ncol = 1)

# Create circular plots for each animal
ggplot(wildschwein_individuals |> filter(Frucht %in% top_crops_per_animal), 
       aes(x = Hour, fill = Frucht)) +
  geom_histogram(binwidth = 1, alpha = 0.8) +
  coord_polar() +
  scale_x_continuous(limits = c(0, 24), breaks = seq(0, 24, by = 6)) +
  labs(title = "Daily Activity Patterns by Individual",
       x = "Hour of Day",
       y = "Count") +
  facet_grid(Frucht~TierName) +
  theme_minimal() +
  theme(legend.position = "none")


# Task 4: Import and Visualize Vegetation Height --------------------------

# Import the vegetation height raster
vegetation_height <- rast("vegetationshoehe_LFI.tif")

# Check basic raster properties
print(vegetation_height)
st_crs(vegetation_height)
res(vegetation_height)
minmax(vegetation_height)

# Basic plot with color gradient
plot(vegetation_height, 
     main = "Vegetation Height (m)",
     axes = TRUE)

tm_shape(vegetation_height) +
  tm_raster(title = "Vegetation Height (m)",
            palette = "YlGn",
            alpha = 0.7) +
  tm_shape(wildschwein_BE) +
  tm_dots(size = 0.05, col = "red", alpha = 0.3) +
  tm_layout(
    main.title = "Wild Boar Locations vs. Vegetation Height",
    main.title.position = "center",
    main.title.size = 1.1,
    legend.outside = TRUE,
    legend.outside.position = "right"
  )


# Task 5: Annotate Trajectories with Vegetation Height --------------------


# Check CRS of both datasets
st_crs(wildschwein_BE)  # Check wild boar CRS
crs(vegetation_height)  # Check vegetation raster CRS

# Not matching, transforming the points:
wildschwein_BE <- st_transform(wildschwein_BE, crs(vegetation_height))

# Convert sf points to terra's vect format
wildschwein_vect <- vect(wildschwein_BE)

# Extract values using terra's extract
vegetation_annotations <- terra::extract(
  x = vegetation_height,       # SpatRaster object
  y = wildschwein_vect,        # SpatVector points
  method = "bilinear",         # Interpolation method
  ID = FALSE                   # Don't include feature IDs
)

# Rename the extracted column
names(vegetation_annotations) <- "vegetation_height"

# Combine with original data
wildschwein_veg <- cbind(
  st_drop_geometry(wildschwein_BE),  # Keep all original columns
  vegetation_height = vegetation_annotations$vegetation_height,
  geometry = st_geometry(wildschwein_BE)  # Add geometry back
) |> 
  st_as_sf()

# Check results
head(wildschwein_veg)

# Count NA values (points outside raster coverage)
sum(is.na(wildschwein_veg$vegetation_height))

# Remove points with NA vegetation values
wildschwein_veg <- wildschwein_veg |> 
  filter(!is.na(vegetation_height))

# Visualize Distribution
ggplot(wildschwein_veg, aes(x = vegetation_height)) +
  geom_histogram(bins = 30, fill = "darkgreen") +
  labs(title = "Distribution of Vegetation Height at Wild Boar Locations",
       x = "Vegetation Height (m)",
       y = "Count")

# Add hour column
wildschwein_veg <- wildschwein_veg |>
  mutate(Hour = as.numeric(format(DatetimeUTC, "%H")))

# Plot
ggplot(wildschwein_veg, aes(x = Hour, y = vegetation_height)) +
  geom_point(alpha = 0.1, color = "darkgreen") +
  geom_smooth(method = "loess", color = "red", se = FALSE) +  # se=FALSE disables CI
  labs(title = "Vegetation Height Usage by Hour",
       y = "Vegetation Height (m)") +
  scale_x_continuous(breaks = 0:23)

# Filter to our three animals
wildschwein_veg_filtered <- wildschwein_veg |>
  filter(TierName %in% c("Sabi", "Ruth", "Rosa"))

# Violin plot comparison
ggplot(wildschwein_veg_filtered, aes(x = TierName, y = vegetation_height)) +
  geom_violin(fill = "lightgreen") +
  geom_boxplot(width = 0.1, fill = "white") +
  labs(title = "Vegetation Height Preferences by Animal",
       y = "Vegetation Height (m)")
