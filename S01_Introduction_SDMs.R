####################################################
### Session 01: 
####################################################

#-----------------------------------------------------------------------------------------
# 01. Correlation measures between body height and weight
#-----------------------------------------------------------------------------------------

# load data
hw <- read.csv("data/weight-height.csv")
head(hw)
summary(hw)

# convert american units into european
library(measurements)
hw2 <- data.frame(Gender=hw$Gender, Weight = conv_unit(hw$Weight, "lbs", "kg"),
                  Height = conv_unit(hw$Height, "inch", "cm"))

plot(hw2$Weight, hw2$Height)

# show boxplot on weight and height distribution
library(dplyr)
dplyr::sample_n(hw2, 10)

summary(filter(hw2, Gender=="Female"))
summary(filter(hw2, Gender=="Male"))

boxplot(filter(hw2, Gender=="Female")$Weight, filter(hw2, Gender=="Male")$Weight, notch = T)
boxplot(filter(hw2, Gender=="Female")$Height, filter(hw2, Gender=="Male")$Height, notch = T)

# check for normal distribution using the shapiro test
# low values because of both gender in the specified input data
hw3 <- sample_n(hw2, 5000)
shapiro.test(hw3$Weight)
shapiro.test(hw3$Height)

plot(density(hw3$Weight))
plot(density(hw3$Height))

# plot by gender
plot(density(filter(hw2, Gender=="Female")$Weight), col="red")
plot(density(filter(hw2, Gender=="Male")$Weight), col="blue")

# run shapiro test again with gender split
# -> now a normal distribution should be visible
shapiro.test(filter(hw3, Gender=="Female")$Weight)
shapiro.test(filter(hw3, Gender=="Male")$Weight)

# create dataset with only males
hw2.male <- filter(hw2, Gender=="Male")
summary(hw2.male)

# check for linear regression between height and weight
hw.lm <- lm(formula = Weight ~ Height, data = hw2.male)
summary(hw.lm)

hw.new <- data.frame(name=)

# create prediction model
hw.lm.p <- predict(objct = hw.lm, new = hw.new)
pred.weight <- data.frame(hw.new$name, weight.pred=hw.lm.p)
pred.weight

#-------------------------------------------------------------------------
# 02. EAGLEs distribution model
#-------------------------------------------------------------------------

library(rgdal)
library(raster)

# read data
occ <- readOGR("data/occurence.gpkg")
class(occ)
summary(occ)
plot(occ)

bui <- readOGR("data/campus_buildings.gpkg")
summary(bui)
plot(bui)

# show occurence data on top of building layer
plot(occ[occ$students == 1, ], col="blue", pch = 16, add=T)
plot(occ[occ$students == 0, ], col="red", pch = 16, add=T)

# rasterize buildings and compute distances to them
r <- raster(bui, ncols = 100, nrows = 100)
rr.0 <- rasterize(bui, r, progress = "text")
plot(rr.0)

rr.0.d <- distance(rr.0)
plot(rr.0.d)

# create spatial distribution model
library(sdm)
preds <- rr.0.d
d <- sdmData(formula = students ~ layer, train = occ, predictors = preds)
d

m1 <- sdm(students~., data = d, methods = c("glm"))

p1 <- predict(m1, newdata = preds, filename = 'sdm_preds_1.grd', overview = T, overwrite = T)
plot(p1)

# get sdms for each building
rr <- rasterize(bui, r, progress = "text", field = "id")
plot(rr)

# extract three buildings
rr.1 <- rr == 1
rr.1[rr.1 == 0] <- NA
plot(rr.1)
rr.2 <- rr == 2
rr.2[rr.2 == 0] <- NA
plot(rr.2)
rr.3 <- rr == 3
rr.3[rr.3 == 0] <- NA
plot(rr.3)

# calculate distances
rr.1.d <- distance(rr.1)
plot(rr.1.d)

rr.2.d <- distance(rr.2)
plot(rr.2.d)

rr.3.d <- distance(rr.3)
plot(rr.3.d)

# steck 'em up
preds <- stack(rr.1.d, rr.2.d, rr.3.d)

# subset data for better time prediction
occ.10h <- occ[occ$time == 10, ]
occ.13h <- occ[occ$time == 13, ]
occ.22h <- occ[occ$time == 22, ]

d.10h <- sdmData(formula = students~layer.1+layer.2+layer.3, train = occ.10h, predictors = preds)
d.13h <- sdmData(formula = students~layer.1+layer.2+layer.3, train = occ.13h, predictors = preds)
d.22h <- sdmData(formula = students~layer.1+layer.2+layer.3, train = occ.22h, predictors = preds)

m.10h <- sdm(students~.,data = d.10h, methods = c("glm"))
m.13h <- sdm(students~.,data = d.13h, methods = c("glm"))
m.22h <- sdm(students~.,data = d.22h, methods = c("glm"))

p.10h <- predict(m.10h, newdata=preds)
p.13h <- predict(m.13h, newdata=preds)
p.22h <- predict(m.22h, newdata=preds)

p.time <- stack(p.10h, p.13h, p.22h)

# plot it as rgb
plot(p.time)

plotRGB(p.time, 1,3,5, stretch ="lin")
plot(bui, add = T)
