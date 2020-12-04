## global

library(readr)
library(dplyr)
library(geojsonio)
library(ggplot2)
library(sp)


review.bins <-c(8.5,9,9.2,9.4,9.6,9.8,10)
review.palatte= c('#fee8c8','#fdbb84','#e34a33')
price.palatte= c('#fde0dd','#fa9fb5','#c51b8a')
price.bins <-c(0,75,100,150,200,1000)

listing=read_csv("listing_cleaned.csv")
neighGeoJson <- geojson_read("neighbourhoods.geojson", what = "sp")


# Remove duplicate entry from neighGeoJson table
neighGeoJson=neighGeoJson[which(!duplicated(neighGeoJson$neighbourhood)), ]
#Convert the price to numeric
listing$price<- as.numeric(gsub(",", "",substring(listing$price,2)))

#Define the roomtypes
room_type <- c("Entire home/apt", "Private room", "Shared room")

#Prepare the geoson data
NeighReviews <- listing %>% group_by(neighbourhood_cleansed) %>% summarise(avg_loc_review = mean(review_scores_location, na.rm = TRUE))
colnames(NeighReviews) <- c("neighbourhood","avg_review")

NeighPriveAvg <- listing %>% group_by(neighbourhood_cleansed) %>% summarise(avg_price = mean(price, na.rm = TRUE))
colnames(NeighPriveAvg) <- c("neighbourhood","avg_price")
temp <- merge(NeighPriveAvg, NeighReviews, by="neighbourhood")

airbnb_neigh <- sp::merge(neighGeoJson ,temp, by="neighbourhood",all.=FALSE)
