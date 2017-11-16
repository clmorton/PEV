#Load Packages
library(spdep)
library(rgdal)
library(maptools)
library(RColorBrewer)
library(tmap)

#Load Data
setwd("~/GitHub/PEV")
SpatDat <- readOGR("Shapefiles","LAD_DEC_2012_GB_BGC_Linked")
summary(SpatDat)

#Choropleth Map of Private Plug-in Electric Vehicles (per '000 cars)
col.ramp <- brewer.pal(n = 6, name = "OrRd")
spplot(SpatDat,'PPEV1k', col = "transparent", main = "Private Plug-in Electric Vehicles (per '000 cars)")

tm_shape(SpatDat) +
  tm_polygons("PPEV1k", style="quantile", title="Private Plug-in Electric Vehicles (per '000 cars)", n = 10)
tmap_mode("view")
last_map()

#Create Spatial Weights
continuity.nb <- poly2nb(SpatDat, queen = TRUE)
plot(SpatDat, border = "grey")
plot(continuity.nb, coordinates(SpatDat), add = TRUE, col = "blue")
continuity.listw <- nb2listw(continuity.nb)
summary(continuity.listw)

#Autocorrelation Analysis
moran.test(SpatDat$PPEV1k,continuity.listw, randomisation=FALSE, alternative="two.sided")
moran.plot(SpatDat$PPEV1k, continuity.listw)
LMIresult <- localmoran(SpatDat$PPEV1k, continuity.listw)
LMImap <- spCbind(SpatDat, as.data.frame(LMIresult))
spplot(LMImap, "Z.Ii")

#Spatial log-normal Regression Models

#Spatial Lag Model
mod7SLM <- lagsarlm(log(PPEV1k) ~ log(MedianAge) + log(Level4) + log(SelfEmp) + log(MedianY) +
                        log(PopDens) + log(SemiD) + log(MeanRes) + log(OneCar) + log(CarDrive) + log(PHEV1k) + TotCPoint, 
                    data = SpatDat, continuity.listw)
summary(mod7SLM)

#Spatial Error Model
mod7SEM <- errorsarlm(log(PPEV1k) ~ log(MedianAge) + log(Level4) + log(SelfEmp) + log(MedianY) +
                        log(PopDens) + log(SemiD) + log(MeanRes) + log(OneCar) + log(CarDrive) + log(PHEV1k) + TotCPoint, 
                      data = SpatDat, continuity.listw)
summary(mod7SEM)

#Spatial Durbin Model
mod7SDM <- lagsarlm(log(PPEV1k) ~ log(MedianAge) + log(Level4) + log(SelfEmp) + log(MedianY) +
                      log(PopDens) + log(SemiD) + log(MeanRes) + log(OneCar) + log(CarDrive) + log(PHEV1k) + TotCPoint, 
                    data = SpatDat, continuity.listw, type = "mixed")
summary(mod7SDM)
W <- as(continuity.listw, "CsparseMatrix")
trMatc <- trW(W, type = "mult")
summary(impacts(mod7SDM, tr=trMatc, R=200), zstats=TRUE, short = TRUE)

#Spatial Durbin Error Model
mod7SDEM <- errorsarlm(log(PPEV1k) ~ log(MedianAge) + log(Level4) + log(SelfEmp) + log(MedianY) +
                         log(PopDens) + log(SemiD) + log(MeanRes) + log(OneCar) + log(CarDrive) + log(PHEV1k) + TotCPoint, 
                      data = SpatDat, continuity.listw, etype = "emixed")
summary(mod7SDEM)
summary(impacts(mod7SDEM))

#Simultaneous Autoregressive Model
mod7SAR <- spautolm(log(PPEV1k) ~ log(MedianAge) + log(Level4) + log(SelfEmp) + log(MedianY) +
                      log(PopDens) + log(SemiD) + log(MeanRes) + log(OneCar) + log(CarDrive) + log(PHEV1k) + TotCPoint, 
                    data = SpatDat, listw = continuity.listw, family = "SAR", method = "eigen")
summary(mod7SAR)
mod7resSAR <- MCMCsamp(mod7SAR, mcmc = 5000, burnin = 500, listw = continuity.listw)
summary(mod7resSAR)

#Conditional Autoregressive Model
mod7CAR <- spautolm(log(PPEV1k) ~ log(MedianAge) + log(Level4) + log(SelfEmp) + log(MedianY) +
                      log(PopDens) + log(SemiD) + log(MeanRes) + log(OneCar) + log(CarDrive) + log(PHEV1k) + TotCPoint, 
                    data = SpatDat, listw = continuity.listw, family = "CAR", method = "eigen")
summary(mod7CAR)
mod7resCAR <- MCMCsamp(mod7CAR, mcmc = 5000, burnin = 500, listw = continuity.listw)
summary(mod7resCAR)
