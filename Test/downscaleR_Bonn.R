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






#------------------------------------------------------------------------------#

# call x from saved RDS
x.eur <- readRDS("multi_grid_ERA_Interim.rds")

# Set coordinates for Bonn
lonLim <- c(3,8.6)
latLim <- c(50,60)

# Load github file
value <- file.path(find.package("VALUE"), "example_datasets", 
                   "VALUE_ECA_12_Germany_multivar_v2.zip")
 

y <- loadStationData(dataset = value,
                     lonLim = lonLim,
                     latLim = latLim,
                     var = "tmean",
                     years = 1979:2008) %>% binaryGrid(condition = "LE",
                                                       threshold = 50,
                                                       partial = TRUE)

# Error
# check value
dataInventory(dataset = value) # Bonn is within their coverage, i think?
# Try adjusting lonlim and latlim to higher values and it worked

y_bin <- binaryGrid(y, condition = "LE", threshold = 50)

# The following code prepares a map displaying the predictor set reference 
# grid and the predictand locations:
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


# I can't spatial plot for Bonn using their codes, maybe coz they covered
# the whole pan-european region

# folds used in the VALUE Project are defined for cross-validation:
folds <- list(1979:1984, 1985:1990, 1991:1996, 1997:2002, 2003:2008)

# Spatial Methods M1 and M6 (VALUE methods GLM-DET and ANALOG)
config.M1.M6 <- list(which.combine = vars,
                     v.exp = .95,
                     rot = FALSE)

# I think I need to download x.eur dataset again setting lon and latlim for Bonn
# But the dataset seems to cover pan-european region broadly and stations are not there
# specifically for Bonn coordinates

#-------------------------------------------------------------------------------#

# Try with ncdf4
library(ncdf4)
nc <- nc_open("DWD_Bonn_forecast_Jan_2024.nc")
print(nc) # var is t2m
names(nc$var)      # list of variable names
names(nc$dim)      # list of dimension names

# Set var
var <- "t2m"
# Try using the netCDF file from EMWCF
grid.list <- lapply(var, function(x) {
  loadGridData(dataset = "DWD_Bonn_forecast_Jan_2024.nc",
               var = x,
               lonLim = lonLim,
               latLim = latLim,
               years = NULL)
}
) # the loadGridData is not reading my nc file

## I think the problem here is the file type. The var in the code accepts a single
## file in txt format. I need to extract the timestep and t2m in txt file
## The rest of their codes (obs values) are for PRUDENCE regions between 
## 1979:2008. So, I need to adjust the codes and data for obs values as well. 

## This may not be a good source for me to downscale for cherry blossom?????

# Stack grids (sets of predictors) into a single block 
x <- makeMultiGrid(grid.list)

