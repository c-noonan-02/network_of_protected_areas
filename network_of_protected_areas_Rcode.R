# clear environment
rm(list=ls())

# load required packages
library(tidyverse)
library(maps)
library(vegan)
library(sf)
library(betapart)


# 1. LOAD AND FORMAT THE DATA

# import the distributions.csv data file
bird_data <- read.csv("Data/distributions.csv")
# visualise data
head(bird_data)
str(bird_data)
unique(bird_data$period)
# remove all lines where the resolution is not 10
bird_data <- bird_data[which(bird_data$resolution==10),]

# import the grid_square_coordinates_lookup.csv data file
coordinate_data <- read.csv("Data/grid_square_coordinates_lookup.csv")
# visualise data
head(coordinate_data)
str(coordinate_data)
# remove all lines where the resolution is not 10
coordinate_data <- coordinate_data[which(coordinate_data$resolution==10),]


# 2. SELECT THE DATA ON THE MAIN ISLAND ONLY
# the r package maps allows you to access spatial polygons corresponding to different countries and regions

# visualise the UK
map(database="world",regions="UK")

# superimpose the coordinate data onto this
points(coordinate_data$long,coordinate_data$lat)

# we can store the polygons defining the UK
UK <- map(database="world",regions="UK", fill=TRUE)
# we can then convert them to a simple feature
UK_sf <- st_as_sf(UK)
UK_sf <- st_cast(UK_sf, "POLYGON") #ignore the warning

# we can visualise this again
plot(st_geometry(UK_sf))
points(coordinate_data)

# now we can keep grid cells with at least one corner on the main island. 
# first create a sf data frame with these coordinates
coordinate_sf <- st_as_sf(coordinate_data[,1:2],coords = c("long", "lat"), crs = st_crs(UK_sf))
# then we can isolate the points intersecting the different polygons representing
# the UK. Only keep the points intersecting the 15th polygon, which corresponds
# to the main island
coordinate_data_island_ind <- st_intersects(UK_sf,coordinate_sf)[[15]]
coordinate_data_island <- coordinate_data[coordinate_data_island_ind,]

# visualise again
plot(st_geometry(UK_sf))
points(coordinate_data_island)

