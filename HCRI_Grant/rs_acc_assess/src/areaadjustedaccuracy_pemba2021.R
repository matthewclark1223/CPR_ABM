##Adapted from https://blogs.fu-berlin.de/reseda/area-adjusted-accuracies/

# area adjusted accuracy assessment 
library(raster)
library(xtable)
library(sf)

# import classification image and train and validation shapefiiles
setwd("~/BSU/HES/pemba/abm/data/")
pemba <- st_read("pemmy.shp")
img.class <- mask(raster("pemmyLC2021updateSAR.tif"), pemba)
val2021 <- st_read('points2021stratSAR.shp')

val2021$map <- extract(img.class, val2021)
# create regular accuracy matrix 
confmat <- table(as.factor(val2021$classifica), as.factor(val2021$Class))
#confmat <- table(as.factor(val2021$map), as.factor(val2021$Class))
confmat ## 99 CLOUDS one each 5 and 6

confmat = confmat[,1:8]## 99 CLOUDS one each 5 and 6

#Mangrove - 0
#High Forest - 1
#Rotational Ag - 2
#Urban - 3
#Bare Land - 4
#Coral rag - 5
#Agroforestry/Other woody veg - 6
#Water - 7

# get number of pixels per class and convert in km²
imgVal <- na.omit(as.factor(getValues(img.class)))
nclass <- length(unique(val2021$classifica))
## class labels,
classes <- seq(0,7,1)

## Hardcoded this to avoid weirdness associated with classes 0-6 then reassigned...
#maparea <- sapply(1:nclass, function(x) sum(imgVal == x))
maparea <- sapply(classes, function(x) sum(imgVal == x))
#maparea <- maparea * res(img.class)[1] ^ 2 / 1000000
maparea <- maparea * 20 ^ 2 / 1000000

# set confidence interval
conf <- 1.96

# total  map area
A <- sum(maparea)
# proportion of area mapped as class i
W_i <- maparea / A
# number of reference points per class
n_i <- rowSums(confmat) 

# population error matrix (Eq.4)
p <- W_i * confmat / n_i
p[is.na(p)] <- 0
p[is.infinite(p)] <- 0

# area estimation
p_area <- colSums(p) * A

# area estimation confidence interval (Eq.10)
p_area_CI <- conf * A * sqrt(colSums((W_i * p - p ^ 2) / (n_i - 1))) 

# overall accuracy (Eq.1)
OA <- sum(diag(p))
# producers accuracy (Eq.2)
PA <- diag(p) / colSums(p)

## swap NaNs for zeroes
PA[is.na(PA)] <- 0

# users accuracy (Eq.3)
UA <- diag(p) / rowSums(p)

## swap NaNs for zeroes
UA[is.na(UA)] <- 0

# overall accuracy confidence interval (Eq.5)
OA_CI <- conf * sqrt(sum(W_i ^ 2 * UA * (1 - UA) / (n_i - 1)))
# user accuracy confidence interval (Eq.6)
UA_CI <- conf * sqrt(UA * (1 - UA) / (n_i - 1)) 
# producer accuracy confidence interval (Eq.7)
N_j <- sapply(1:nclass, function(x) sum(maparea / n_i * confmat[ , x]) )
tmp <- sapply(1:nclass, function(x) sum(maparea[-x] ^ 2 * confmat[-x, x] / n_i[-x] * ( 1 - confmat[-x, x] / n_i[-x]) / (n_i[-x] - 1)) )

PA_CI <- conf * sqrt(1 / N_j ^ 2 * (maparea ^ 2 * ( 1 - PA ) ^ 2 * UA * (1 - UA) / (n_i - 1) + PA ^ 2 * tmp))
## swap NaNs for zeroes
PA_CI[is.na(PA_CI)] <- 0

# gather results
result <- matrix(c(p_area, p_area_CI, PA * 100, PA_CI * 100, UA * 100, UA_CI * 100, c(OA * 100, rep(NA, nclass-1)), c(OA_CI * 100, rep(NA, nclass-1))), nrow = nclass)
result <- round(result, digits = 2) 
#rownames(result) <- levels(as.factor(train$classes))
rownames(result) <- c( 'Mangrove', 'High Forest', 'Agriculture', 'Urban', 'Bare', 'Coral rag', 'Other woody veg', 'Water') 
colnames(result) <- c("km²", "km²±", "PA", "PA±", "UA", "UA±", "OA", "OA±")
class(result) <- "table"
result

xresult = xtable(result)

print(xresult, file = '../output/areaacc2021.html')
print(xresult, file = '../output/areaacc2021.txt')

## confmat
rownames(confmat) <- c( 'Mangrove', 'High Forest', 'Agriculture', 'Urban', 'Bare', 'Coral rag', 'Other woody veg', 'Water') 
colnames(confmat) <- c( 'Mangrove', 'High Forest', 'Agriculture', 'Urban', 'Bare', 'Coral rag', 'Other woody veg', 'Water') 
confmat

xconfmat = xtable(confmat)
print(xconfmat, file = '../output/confmat2021.html')
print(xconfmat, file = '../output/confmat2021.txt')