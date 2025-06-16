Sys.setenv(JAVA_HOME = "/Library/Java/JavaVirtualMachines/temurin-24.jdk/Contents/Home")
Sys.getenv("JAVA_HOME")  # should now return the path
library(rJava)           
.jinit()  # initializes the JVM
install.packages("climate4R.UDG")
library(remotes)
remotes::install_github("SantanderMetGroup/climate4R.UDG")
remotes::install_github("SantanderMetGroup/loadeR.java")
remotes::install_github("SantanderMetGroup/loadeR")
# remotes::install_github("SantanderMetGroup/climate4R.value")
remotes::install_github("SantanderMetGroup/VALUE", dependencies = TRUE)

#-------------------------------------------------------------------------#

#https://github.com/SantanderMetGroup/notebooks/blob/v0.1.4/2019_downscaleR_GMD.html

library(downscaleR)
library(transformeR)
library(visualizeR)
library(loadeR)
library(climate4R.UDG)
library(climate4R.datasets)
library(VALUE)
library(readr)
require(climate4R.value)
require(magrittr)

# Load example data
data("PRUDENCEregions", package = "visualizeR")
names(PRUDENCEregions)

# Extract bounding box for Iberia
bb <- PRUDENCEregions["IP"]@bbox
lonLim <- bb[1,]
latLim <- bb[2,]

# Create account at https://www.meteo.unican.es/udg-tap/signup
# Load predictors
loginUDG(username = "", password = "")

var.list <- c("psl",      # Sea level pressure
              "tas",      # Near-surface air temperature (usually 2m)
              "ta@500",   # Air temperature at 500 hPa
              "ta@700",   # Air temperature at 700 hPa
              "ta@850",   # Air temperature at 850 hPa
              "hus@500",  # Specific humidity at 500 hPa
              "hus@850",  # Specific humidity at 850 hPa
              "z@500")    # Geopotential height at 500 hPa

grid.list <- lapply(var.list, function(x) {
  loadGridData(dataset = "ECMWF_ERA-Interim-ESD",
               var = x,
               lonLim = lonLim,
               latLim = latLim,
               years = 1979:2008)
}
)

saveRDS(grid.list, file = "grid_list_ERA_Interim.rds")


# Stack grids (sets of predictors) into a single block 
x <- makeMultiGrid(grid.list)
saveRDS(x, file = "multi_grid_ERA_Interim.rds")
grid.list <- readRDS("grid_list_ERA_Interim.rds")
x <- readRDS("multi_grid_ERA_Interim.rds")


# Load predictands
value <- file.path(find.package("VALUE"), "example_datasets", "VALUE_ECA_86_v2.zip")
y <- loadStationData(dataset = value,
                     lonLim = lonLim,
                     latLim = latLim,
                     var = "precip",
                     years = 1979:2008) %>% binaryGrid(condition = "GE",
                                                       threshold = 1,
                                                       partial = TRUE)
y_bin <- binaryGrid(y, condition = "GE", threshold = 1)

# The fold list specifies the years composing each of the 5 subsamples for 
# 5-fold cross-validation, following the VALUE experimental setup:
# https://www.value-cost.eu/validation
folds <- list(1979:1984, 1985:1990, 1991:1996, 1997:2002, 2003:2008)

# All the predictor variables previously loaded in Section 2.1 are 
# considered for all methods:
(vars <- getVarNames(x))

# Try with method M1
spatial.pars.M1 <- list(which.combine = vars,
                        v.exp = .95,
                        rot = FALSE)

# As no other type of predictors (global and/or local) are used in the M1 
# configuration, the defaults values (NULL) assumed by downscaleCV are applied.
M1cv.bin <- downscaleCV(x = x, y = y_bin, method = "GLM",
                        family = binomial(link = "logit"),
                        folds = folds,
                        prepareData.args = list(global.vars = NULL,
                                                local.predictors = NULL,
                                                spatial.predictors = spatial.pars.M1,
                                                combined.only = TRUE))

# example the binary output is retained, applying the function subsetGrid along 
# the 'var' (variable) dimension:
M1cv.bin <- subsetGrid(M1cv.bin, var = "bin")

# Note that the log link function can’t deal with zeroes in the data for fitting 
# a rain amount model. Here, a minimum threshold of 1 mm precipitation 
# (condition = "GE", i.e., Greater or Equal) is retained for GLM training of 
# precipitation amount, following the VALUE criterion:
M1cv.cont <- downscaleCV(x = x, y = y, method = "GLM",
                         family = Gamma(link = "log"),
                         condition = "GE", threshold = 1,
                         folds = folds,
                         prepareData.args = list(global.vars = NULL,
                                                 local.predictors = NULL,
                                                 spatial.predictors = spatial.pars.M1,
                                                 combined.only = TRUE))

# The continuous and binary predictions are now multiplied, so the precipitation 
# frequency is adjusted and the final precipitation predictions are obtained:
M1cv <- gridArithmetics(M1cv.bin, M1cv.cont, operator = "*")

# Aggregation
aggr.pars <- list(FUN = "sum", na.rm = TRUE)
## Monthly accumulated (sum) aggregation of predictions and observations:
pred.M1 <- aggregateGrid(M1cv, aggr.m = aggr.pars)
obs <- aggregateGrid(y, aggr.m = aggr.pars)

# Plotting
temporalPlot(pred.M1, obs, 
             xyplot.custom = list(xlab = "",
                                  ylab = "Precip. (mm/month)",
                                  scales = list(cex = 1.2,
                                                x = list(rot = 0))))

#-----------------------------------------------------#

# Test for temperature 

# Load predictands
# I found their data here: 
# https://github.com/SantanderMetGroup/VALUE/tree/master/inst/example_datasets
value <- file.path(find.package("VALUE"), "example_datasets", 
                   "VALUE_ECA_12_Germany_multivar_v2.zip")

dataInventory(value)

lonLim <- c(5, 15)
latLim <- c(47, 55)

y <- loadStationData(dataset = value,
                     lonLim = lonLim,
                     latLim = latLim,
                     var = "tmean",
                     years = 1979:2008) %>% binaryGrid(condition = "LE",
                                                       threshold = 30,
                                                       partial = TRUE)

y_bin <- binaryGrid(y, condition = "LE", threshold = 30)

# The fold list specifies the years composing each of the 5 subsamples for 
# 5-fold cross-validation, following the VALUE experimental setup:
# https://www.value-cost.eu/validation
folds <- list(1979:1984, 1985:1990, 1991:1996, 1997:2002, 2003:2008)

# All the predictor variables previously loaded in Section 2.1 are 
# considered for all methods:
(vars <- getVarNames(x))

# Try with method M1
spatial.pars.M1 <- list(which.combine = vars,
                        v.exp = .95,
                        rot = FALSE)

# As no other type of predictors (global and/or local) are used in the M1 
# configuration, the defaults values (NULL) assumed by downscaleCV are applied.
M1cv.bin <- downscaleCV(x = x, y = y_bin, method = "GLM",
                        family = binomial(link = "logit"),
                        folds = folds,
                        prepareData.args = list(global.vars = NULL,
                                                local.predictors = NULL,
                                                spatial.predictors = spatial.pars.M1,
                                                combined.only = TRUE))

# example the binary output is retained, applying the function subsetGrid along 
# the 'var' (variable) dimension:
M1cv.bin <- subsetGrid(M1cv.bin, var = "bin")

# Note that the log link function can’t deal with zeroes in the data for fitting 
# a rain amount model. Here, a minimum threshold of 1 mm precipitation 
# (condition = "GE", i.e., Greater or Equal) is retained for GLM training of 
# precipitation amount, following the VALUE criterion:
M1cv.cont <- downscaleCV(x = x, y = y, method = "GLM",
                         family = Gamma(link = "log"),
                         condition = "GE", threshold = 1,
                         folds = folds,
                         prepareData.args = list(global.vars = NULL,
                                                 local.predictors = NULL,
                                                 spatial.predictors = spatial.pars.M1,
                                                 combined.only = TRUE))

# The continuous and binary predictions are now multiplied, so the precipitation 
# frequency is adjusted and the final precipitation predictions are obtained:
M1cv <- gridArithmetics(M1cv.bin, M1cv.cont, operator = "*")

# Aggregation
aggr.pars <- list(FUN = "sum", na.rm = TRUE)
## Monthly accumulated (sum) aggregation of predictions and observations:
pred.M1 <- aggregateGrid(M1cv, aggr.m = aggr.pars)
obs <- aggregateGrid(y, aggr.m = aggr.pars)

# Plotting
temporalPlot(pred.M1, obs, 
             xyplot.custom = list(xlab = "",
                                  ylab = "T_mean. (degreeC/day)",
                                  scales = list(cex = 1.2,
                                                x = list(rot = 0))))

#-----------------------------------------------------------------------------#

# Try for tmin
value <- file.path(find.package("VALUE"), "example_datasets", 
                   "VALUE_ECA_12_Germany_multivar_v2.zip")

dataInventory(value)

lonLim <- c(5, 15)
latLim <- c(47, 55)

y <- loadStationData(dataset = value,
                     lonLim = lonLim,
                     latLim = latLim,
                     var = "tmin",
                     years = 1979:2008) %>% binaryGrid(condition = "LE",
                                                       threshold = 30,
                                                       partial = TRUE)

y_bin <- binaryGrid(y, condition = "LE", threshold = 30)

# The fold list specifies the years composing each of the 5 subsamples for 
# 5-fold cross-validation, following the VALUE experimental setup:
# https://www.value-cost.eu/validation
folds <- list(1979:1984, 1985:1990, 1991:1996, 1997:2002, 2003:2008)

# All the predictor variables previously loaded in Section 2.1 are 
# considered for all methods:
(vars <- getVarNames(x))

# Try with method M1
spatial.pars.M1 <- list(which.combine = vars,
                        v.exp = .95,
                        rot = FALSE)

# As no other type of predictors (global and/or local) are used in the M1 
# configuration, the defaults values (NULL) assumed by downscaleCV are applied.
M1cv.bin <- downscaleCV(x = x, y = y_bin, method = "GLM",
                        family = binomial(link = "logit"),
                        folds = folds,
                        prepareData.args = list(global.vars = NULL,
                                                local.predictors = NULL,
                                                spatial.predictors = spatial.pars.M1,
                                                combined.only = TRUE))

# example the binary output is retained, applying the function subsetGrid along 
# the 'var' (variable) dimension:
M1cv.bin <- subsetGrid(M1cv.bin, var = "bin")

# Note that the log link function can’t deal with zeroes in the data for fitting 
# a rain amount model. Here, a minimum threshold of 1 mm precipitation 
# (condition = "GE", i.e., Greater or Equal) is retained for GLM training of 
# precipitation amount, following the VALUE criterion:
M1cv.cont <- downscaleCV(x = x, y = y, method = "GLM",
                         family = Gamma(link = "log"),
                         condition = "GE", threshold = 1,
                         folds = folds,
                         prepareData.args = list(global.vars = NULL,
                                                 local.predictors = NULL,
                                                 spatial.predictors = spatial.pars.M1,
                                                 combined.only = TRUE))

# The continuous and binary predictions are now multiplied, so the precipitation 
# frequency is adjusted and the final precipitation predictions are obtained:
M1cv <- gridArithmetics(M1cv.bin, M1cv.cont, operator = "*")

# Aggregation
aggr.pars <- list(FUN = "sum", na.rm = TRUE)
## Monthly accumulated (sum) aggregation of predictions and observations:
pred.M1_min <- aggregateGrid(M1cv, aggr.m = aggr.pars)
obs <- aggregateGrid(y, aggr.m = aggr.pars)

# Plotting
temporalPlot(pred.M1_min, obs, 
             xyplot.custom = list(xlab = "",
                                  ylab = "T_min. (degree/day)",
                                  scales = list(cex = 1.2,
                                                x = list(rot = 0))))

#-----------------------------------------------------------------------------#

# Try for tmax

value <- file.path(find.package("VALUE"), "example_datasets", 
                   "VALUE_ECA_12_Germany_multivar_v2.zip")

dataInventory(value)

lonLim <- c(5, 15)
latLim <- c(47, 55)

y <- loadStationData(dataset = value,
                     lonLim = lonLim,
                     latLim = latLim,
                     var = "tmax",
                     years = 1979:2008) %>% binaryGrid(condition = "LE",
                                                       threshold = 50,
                                                       partial = TRUE)

y_bin <- binaryGrid(y, condition = "LE", threshold = 50)

# The fold list specifies the years composing each of the 5 subsamples for 
# 5-fold cross-validation, following the VALUE experimental setup:
# https://www.value-cost.eu/validation
folds <- list(1979:1984, 1985:1990, 1991:1996, 1997:2002, 2003:2008)

# All the predictor variables previously loaded in Section 2.1 are 
# considered for all methods:
(vars <- getVarNames(x))

# Try with method M1
spatial.pars.M1 <- list(which.combine = vars,
                        v.exp = .95,
                        rot = FALSE)

# As no other type of predictors (global and/or local) are used in the M1 
# configuration, the defaults values (NULL) assumed by downscaleCV are applied.
M1cv.bin <- downscaleCV(x = x, y = y_bin, method = "GLM",
                        family = binomial(link = "logit"),
                        folds = folds,
                        prepareData.args = list(global.vars = NULL,
                                                local.predictors = NULL,
                                                spatial.predictors = spatial.pars.M1,
                                                combined.only = TRUE))

# example the binary output is retained, applying the function subsetGrid along 
# the 'var' (variable) dimension:
M1cv.bin <- subsetGrid(M1cv.bin, var = "bin")

# Note that the log link function can’t deal with zeroes in the data for fitting 
# a rain amount model. Here, a minimum threshold of 1 mm precipitation 
# (condition = "GE", i.e., Greater or Equal) is retained for GLM training of 
# precipitation amount, following the VALUE criterion:
M1cv.cont <- downscaleCV(x = x, y = y, method = "GLM",
                         family = Gamma(link = "log"),
                         condition = "GE", threshold = 1,
                         folds = folds,
                         prepareData.args = list(global.vars = NULL,
                                                 local.predictors = NULL,
                                                 spatial.predictors = spatial.pars.M1,
                                                 combined.only = TRUE))

# The continuous and binary predictions are now multiplied, so the precipitation 
# frequency is adjusted and the final precipitation predictions are obtained:
M1cv <- gridArithmetics(M1cv.bin, M1cv.cont, operator = "*")

# Aggregation
aggr.pars <- list(FUN = "sum", na.rm = TRUE)
## Monthly accumulated (sum) aggregation of predictions and observations:
pred.M1_max <- aggregateGrid(M1cv, aggr.m = aggr.pars)
obs <- aggregateGrid(y, aggr.m = aggr.pars)

# Plotting
temporalPlot(pred.M1_max, obs, 
             xyplot.custom = list(xlab = "",
                                  ylab = "T_max. (degreeC/day)",
                                  scales = list(cex = 1.2,
                                                x = list(rot = 0))))

# Try combining tmin, tmax and tmean
pred.M1 <- aggregateGrid(M1cv, aggr.m = aggr.pars)
pred.M1_min <- aggregateGrid(M1cv, aggr.m = aggr.pars)
pred.M1_max <- aggregateGrid(M1cv, aggr.m = aggr.pars)
pred.M1_combined <- aggregateGrid(c(pred.M1_min, pred.M1, pred.M1_max, aggr.m = aggr.pars))
# Plotting
temporalPlot(pred.M1_combined, obs, 
             xyplot.custom = list(xlab = "",
                                  ylab = "T_min.mean.max. (degreeC/day)",
                                  scales = list(cex = 1.2,
                                                x = list(rot = 0))))

#-----------------------------------------------------------------------------#

# Try their pan-European experiment for temperature
lonLim <- c(-10,32)
latLim <- c(36,72)

vars <- c("psl","tas","ta@500","ta@700","ta@850","hus@500","hus@850","z@500")
dataset <- "ECMWF_ERA-Interim-ESD"
# grid.list <- lapply(vars, function(x) {
#   loadGridData(dataset = dataset,
#                var = x,
#                lonLim = lonLim,
#                latLim = latLim,
#                years = 1979:2008)
# })
# x.eur <- makeMultiGrid(grid.list)

# Call saved gridlist
x.eur <- readRDS("multi_grid_ERA_Interim.rds")
value <- file.path(find.package("VALUE"), "example_datasets", 
                   "VALUE_ECA_86_v2.zip")
y <- loadStationData(dataset = value,
                     var = "tmean",
                     years = 1979:2008) %>% binaryGrid(condition = "LE",
                                                       threshold = 50,
                                                       partial = TRUE)
y_bin <- binaryGrid(y, condition = "LE", threshold = 50)

# The following code prepares a map displaying the predictor set reference grid
# and the predictand locations:
coords.x <- get2DmatCoordinates(x.eur)
names(coords.x) <- c("x","y")
grid_clim <- climatology(subsetDimension(x.eur, dimension = "var", indices = 1))
spatialPlot(grid_clim,at = seq(-2, 2, 0.1), set.min = 4, set.max = 8, 
            backdrop.theme = "countries", 
            main = "Predictand locations and predictor grid",
            sp.layout = list(list(sp::SpatialPoints(coords.x), 
                                  first = FALSE, 
                                  col = "grey80", pch = 3),
                             list(sp::SpatialPoints(getCoordinates(y)), 
                                  first = FALSE, 
                                  col = "red", pch = 22)
            ),
            colorkey = FALSE)



data("PRUDENCEregions", package = "visualizeR")
areas <- PRUDENCEregions
refcoords <- get2DmatCoordinates(x.eur)
grid_clim <- climatology(subsetDimension(x.eur, dimension = "var", indices = 1))
ind <- sapply(1:length(PRUDENCEregions), FUN = function(z) {
  which(y$Metadata$PRUDENCEregion == names(PRUDENCEregions)[z])
})
# Color palette for the regions
reg.colors <- c("blue", "gold", "green", "cyan", "navyblue",
                "darkgreen", "red", "violet")

# Point layer displaying stations by colors
stations <- lapply(1:length(PRUDENCEregions), function(i) {
  list(sp::SpatialPoints(getCoordinates(y)[ind[[i]],]),
       first = FALSE, col = reg.colors[i], pch = 15)
})
# Vector layer delimiting subregions, by colors
subregions <- lapply(1:length(PRUDENCEregions), function(i) {
  list(PRUDENCEregions[i], col = reg.colors[i], lwd = 1.5)
})
sp.layout <- c(subregions, stations)
# Other graphical parameters passed to spatialPlot:
sp.layout[[length(sp.layout) + 1]] <- list('sp.text',
                                           sp::coordinates(PRUDENCEregions),
                                           txt = names(PRUDENCEregions),
                                           cex = 1.5)
sp.layout[[length(sp.layout) + 1]] <- list(sp::SpatialPoints(refcoords),
                                           first = FALSE, col = "grey60",
                                           pch = 3, cex = .5, lwd = .5)

# Make spatial plot after tuning parameters for map
spatialPlot(grid_clim,at = seq(-2, 2, 0.1), 
            backdrop.theme = "coastline", 
            sp.layout = sp.layout, colorkey = FALSE)

# the folds used in the VALUE Project are defined for cross-validation:
folds <- list(1979:1984, 1985:1990, 1991:1996, 1997:2002, 2003:2008)

# Spatial Methods M1 and M6 (VALUE methods GLM-DET and ANALOG)
config.M1.M6 <- list(which.combine = vars,
                     v.exp = .95,
                     rot = FALSE)

#Cross-validation is undertaken iteratively for each PRUDENCE region sepparately. 
# Therefore, a for loop is introduced that iteratively subsets predictor and 
# predictand sets across PRUDENCE regions.
n <- names(PRUDENCEregions)
n_regions <- length(n)
areas <- PRUDENCEregions
M1cv <- M6cv <- list()

# check coordinates to fix error: Error in subsetSpatial(grid, lonLim, latLim, outside) : 
#Subset longitude boundaries outside the current grid extent: (6,14)
range(getCoordinates(x.eur)$x)
range(getCoordinates(x.eur)$y) 

for (i in 1:n_regions) {
  print(paste("Region:", n[i]))
  print(areas[n[i]]@bbox)
}

## Well out of bound, I remember I loaded the saved data 
## So, let's load the x.eur again to see if it works (larger lon/latlim)

loginUDG(username = "", password = "") # add username and password
lonLim <- c(-10,32)
latLim <- c(36,72)
vars <- c("psl","tas","ta@500","ta@700","ta@850","hus@500","hus@850","z@500")
dataset <- "ECMWF_ERA-Interim-ESD"
grid.list <- lapply(vars, function(x) {
  loadGridData(dataset = dataset,
               var = x,
               lonLim = lonLim,
               latLim = latLim,
               years = 1979:2008)
}) ## can't run now coz service temporarily unavailable
x.eur <- makeMultiGrid(grid.list)


for (i in 1:n_regions) {
  y1reg <- subsetDimension(y, dimension = "loc", indices = ind[[i]])
  x1reg <- subsetGrid(x.eur, lonLim = areas[n[i]]@bbox[1,],
                      latLim = areas[n[i]]@bbox[2,])
  # M6
  M6cv[[i]] <- downscaleCV(x1reg, y1reg,
                           folds = folds,
                           scaleGrid.args = list(type = "standardize"),
                           method = "analogs", n.analogs = 1,
                           prepareData.args = list(spatial.predictors = config.M1.M6))
  # M1
  y1reg_bin <- binaryGrid(y1reg, condition = "LE", threshold = 50)
  M1cv.bin <- downscaleCV(x1reg, y1reg_bin,
                          folds = folds,
                          scaleGrid.args = list(type = "standardize"),
                          method = "GLM", family = binomial(link = "logit"),
                          prepareData.args = list(spatial.predictors = config.M1.M6)) %>% 
    subsetGrid(var = "bin")
  
  M1cv.amo <- downscaleCV(x1reg, y1reg,
                          folds = folds,
                          scaleGrid.args = list(type = "standardize"),
                          method = "GLM", family = Gamma(link = "log"), 
                          condition = "LE", threshold = 50,
                          prepareData.args = list(spatial.predictors = config.M1.M6))
  M1cv[[i]] <- gridArithmetics(M1cv.bin, M1cv.amo, operator = "*")
}








