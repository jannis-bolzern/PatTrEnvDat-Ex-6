library("tidyverse")
library("sf")


wildschwein_BE <- read_delim("wildschwein_BE_2056.csv", ",") |>
  st_as_sf(coords = c("E", "N"), crs = 2056, remove = FALSE)

