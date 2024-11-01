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
bird_data <- bird_data[which(bird_data$resolution==10), ]

# import the grid_square_coordinates_lookup.csv data file
coordinate_data <- read.csv("Data/grid_square_coordinates_lookup.csv")
# visualise data
head(coordinate_data)
str(coordinate_data)
# remove all lines where the resolution is not 10
coordinate_data <- coordinate_data[which(coordinate_data$resolution==10), ]


# 2. SELECT THE DATA ON THE MAIN ISLAND ONLY
# the r package maps allows you to access spatial polygons corresponding to different countries and regions

# visualise the UK
map(database="world", regions="UK")

# superimpose the coordinate data onto this
points(coordinate_data$long, coordinate_data$lat)

# we can store the polygons defining the UK
UK <- map(database="world", regions="UK", fill=TRUE)
# we can then convert them to a simple feature
UK_sf <- st_as_sf(UK)
UK_sf <- st_cast(UK_sf, "POLYGON") #ignore the warning

# we can visualise this again
plot(st_geometry(UK_sf))
points(coordinate_data)

# now we can keep grid cells with at least one corner on the main island. 
# first create a sf data frame with these coordinates
coordinate_sf <- st_as_sf(coordinate_data[, 1:2], coords = c("long", "lat"), crs = st_crs(UK_sf))
# then we can isolate the points intersecting the different polygons representing
# the UK. Only keep the points intersecting the 15th polygon, which corresponds
# to the main island
coordinate_data_island_ind <- st_intersects(UK_sf, coordinate_sf)[[15]]
coordinate_data_island <- coordinate_data[coordinate_data_island_ind, ]

# visualise again
plot(st_geometry(UK_sf))
points(coordinate_data_island)


# 3. SELECT DATA FOR THE MOST RECENT PERIOD AND FORMAT IT INTO A SITE-BY-SPECIES MATRIX

# Since the data set covers a large period (1968-1972, 1988-1991 and 2008-2011)
# we can choose to select the most recent time frame. 

# The data also covers two seasons - April-July and Nov-Feb - we can save these
# in seperate dataframes.

# select bird data for the desired period
bird_recent_data <- bird_data[which(bird_data$period=="2008-11" | bird_data$period=="2007/08-10/11"), c("period", "speccode", "grid")]

# check dimensions and data
dim(coordinate_data_island)
coordinate_data_island <- coordinate_data_island[which(coordinate_data_island$grid %in% bird_recent_data$grid), ]
dim(coordinate_data_island)
bird_recent_data <- bird_recent_data[which(bird_recent_data$grid %in% coordinate_data_island$grid), ]
head(bird_recent_data)

# save the winter and summer subsets
bird_summer_data <- bird_recent_data[which(bird_recent_data$period=="2008-11"), c("speccode", "grid")]
bird_winter_data <- bird_recent_data[which(bird_recent_data$period=="2007/08-10/11"), c("speccode", "grid")]

# we then need to transform these data frames into site-by-species data frames
# we can use tidyverse like last practical

# add a column with the values to populate the site-by-species data frame
bird_summer_data$presence <- 1 
# create site-by-species data frame with NAs
bird_summer_data_new <- bird_summer_data %>% 
 pivot_wider(names_from=speccode, values_from=c(presence)) 
# create values to replace the NAs
list0 <- as.list(rep(0, ncol(bird_summer_data_new))) 
names(list0) <- names(bird_summer_data_new)
# replace the NAs by 0’s
bird_summer_data_new <- as.data.frame(bird_summer_data_new %>% replace_na(list0)) 
# change row names
row.names(bird_summer_data_new) <- bird_summer_data_new$grid
# remove the first column with site names
bird_summer_data_new <- bird_summer_data_new[, -1] 
# sort by grid cell names
bird_summer_data_new <- bird_summer_data_new[order(row.names(bird_summer_data_new)), ] 

# add a column with the values to populate the site-by-species data frame
bird_winter_data$presence <- 1
# create site-by-species data frame with NAs
bird_winter_data_new <- bird_winter_data %>% 
 pivot_wider(names_from=speccode, values_from=c(presence))
# create values to replace the NAs
list0 <- as.list(rep(0, ncol(bird_winter_data_new)))
names(list0) <- names(bird_winter_data_new)
# replace the NAs by 0’s
bird_winter_data_new <- as.data.frame(bird_winter_data_new %>% replace_na(list0))
# change row names
row.names(bird_winter_data_new) <- bird_winter_data_new$grid
# remove the first column with site names
bird_winter_data_new <- bird_winter_data_new[, -1]
# sort by grid cell names
bird_winter_data_new <- bird_winter_data_new[order(row.names(bird_winter_data_new)), ]

# We can then confirm that there are no empty cells, nor any species occurring
# in no cell:
which(colSums(bird_summer_data_new)==0)
which(rowSums(bird_summer_data_new)==0)
which(colSums(bird_winter_data_new)==0)
which(rowSums(bird_winter_data_new)==0)


# 4. COMPUTE GENERAL PATTERNS

# compute gamma diversity
summer_gamma_div <- ncol(bird_summer_data_new)
winter_gamma_div <- ncol(bird_winter_data_new)

# compute the alpha diversity
summer_alpha_div <- mean(rowSums(bird_summer_data_new))
winter_alpha_div <- mean(rowSums(bird_winter_data_new))

# plot the distribution of alpha diversity values using hist()
par(mfrow=c(1, 2))
hist(rowSums(bird_summer_data_new), main="Summer", xlab="Richness")
hist(rowSums(bird_winter_data_new), main="Winter", xlab="Richness")
par(mfrow=c(1, 1))

# use functions specaccum() for the observed SAC, and poolaccum() for the Chao2
# estimation from the vegan package
summer_SAC <- specaccum(bird_summer_data_new)
summer_estim <- poolaccum(bird_summer_data_new)
winter_SAC <- specaccum(bird_winter_data_new)
winter_estim <- poolaccum(bird_winter_data_new)

# visualise the outputs
par(mfrow=c(1, 2))
plot(summer_SAC$richness, pch=1, lty=1, lwd=2, type="b", col="blue", ylim=c(0, max(rowMeans(summer_estim$chao))), ylab="Richness", main="Summer")
points(3:nrow(bird_summer_data_new), rowMeans(summer_estim$chao), pch=2, lty=2, lwd=2, type="b", col="lightblue")
plot(winter_SAC$richness, pch=1, lty=1, lwd=2, type="b", col="blue", ylim=c(0, max(rowMeans(winter_estim$chao))), ylab="Richness", main="Winter")
points(3:nrow(bird_winter_data_new), rowMeans(winter_estim$chao), pch=2, lty=2, lwd=2, type="b", col="lightblue")
par(mfrow=c(1, 1))

# does it make sense to use the Chao2 estimator this time?
# our grid cells are too large for this to be useful
# chao thinks empty sections of cells means missing data - no just because of
# the large sizes - thinks zero = do not know

# re-plot only with useful parameter (alpha diversity/richness)
par(mfrow=c(1, 2))
plot(summer_SAC$richness, pch=1, lty=1, lwd=2, type="b", col="blue", ylim=c(0, 400), ylab="Richness", main="Summer")
plot(winter_SAC$richness, pch=1, lty=1, lwd=2, type="b", col="blue", ylim=c(0, 400), ylab="Richness", main="Winter")
par(mfrow=c(1, 1))

# use the beta.pair() function to calculate beta diversity
summer_beta_div <- beta.pair(bird_summer_data_new)
winter_beta_div <- beta.pair(bird_winter_data_new)
# compute the average simpsons dissimilarity
mean(summer_beta_div$beta.sim)
mean(winter_beta_div$beta.sim)
# compute the average Sorensen dissimilarity
mean(summer_beta_div$beta.sor)
mean(winter_beta_div$beta.sor)

# plot their distributions
par(mfrow=c(1, 2))
hist(summer_beta_div$beta.sim, main="Summer, Simpsons Dissimilarity")
hist(winter_beta_div$beta.sim, main="Winter, Simpsons Dissimilarity")
hist(summer_beta_div$beta.sor, main="Summer, Sorensens Dissimilarity")
hist(winter_beta_div$beta.sor, main="Winter, Sorensens Dissimilarity")
par(mfrow=c(1, 1))

# occupancy-frequency distributions
# compute the number of grid cells in which they occur, using the colSums() function
summer_freq <- colSums(bird_summer_data_new)/nrow(bird_summer_data_new)
winter_freq <- colSums(bird_winter_data_new)/nrow(bird_winter_data_new)
# plot this histograms of these
par(mfrow=c(1, 2))
hist(summer_freq, main="Summer", xlab="Occupancy", ylim=c(0,250), breaks = seq(0, 1, 0.1))
hist(winter_freq, main="Winter", xlab="Occupancy", ylim=c(0,250), breaks = seq(0, 1, 0.1))
par(mfrow=c(1, 1))

# What do these patterns tell you about bird species diversity in winter versus summer?

# the Gamma diversity is higher in the winter than in the summer
# the Simpsons Dissimilarity is higher in summer, but slightly more variable in summer too
# the Sorensens Dissimilarity is higher in winter, but slightly more variable in winter too
# the occupancy-frequency distributions show...?






