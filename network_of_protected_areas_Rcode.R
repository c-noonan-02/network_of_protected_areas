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


