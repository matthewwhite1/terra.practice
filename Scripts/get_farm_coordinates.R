farms <- read.csv("Data_Clean/farms.csv")
farms_coords <- tidygeocoder::geocode(farms, street = farm, state = state)
