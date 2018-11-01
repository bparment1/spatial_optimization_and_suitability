#Converting Thermal to Brightness temperature
#https://www.gis-blog.com/calculation-of-land-surface-temperature-lst-from-landsat-8-using-r/


#1) Convert DN to Top of the atmosphere reflectance (TOA)
#Values from Metafile
RADIANCE_MULT_BAND_10 <- 3.3420E-04
RADIANCE_MULT_BAND_11 <- 3.3420E-04

RADIANCE_ADD_BAND_10 <- 0.10000
RADIANCE_ADD_BAND_11 <- 0.10000

#Load raster package and load band 10 & 11 into R (navigate to your image directory first)
library(raster)
band_10 <- raster("band10.tif") #change image name accordingly
band_11 <- raster("band11.tif") #change image name accordingly

#Calculate TOA from DN:
toa_band10 <- calc(band_10, fun=function(x){RADIANCE_MULT_BAND_10 * x + RADIANCE_ADD_BAND_10})
toa_band11 <- calc(band_11, fun=function(x){RADIANCE_MULT_BAND_11 * x + RADIANCE_ADD_BAND_11})

#2) Conversion to Brightness temp (sensor brightness)
#Values from Metafile
K1_CONSTANT_BAND_10 <- 774.8853
K1_CONSTANT_BAND_11 <- 480.8883
K2_CONSTANT_BAND_10 <- 1321.0789
K2_CONSTANT_BAND_11 <- 1201.1442

#Calculate LST in Kelvin for Band 10 and Band 11
temp10_kelvin <- calc(toa_band10, fun=function(x){K2_CONSTANT_BAND_10/log(K1_CONSTANT_BAND_10/x + 1)})
temp11_kelvin <- calc(toa_band11, fun=function(x){K2_CONSTANT_BAND_11/log(K1_CONSTANT_BAND_11/x + 1)})

#Convert Kelvin to Celsius for Band 10 and 11
temp10_celsius <- calc(temp10_kelvin, fun=function(x){x - 273.15})
temp11_celsius <- calc(temp11_kelvin, fun=function(x){x - 273.15})

#Export raster images
writeRaster(temp10_celsius, "temp10_c.tif")
writeRaster(temp11_celsius, "temp11_c.tif")


#it seems that you calculated Brightness Temperature.Land surface temperature would be calculated with emissivity and other constants:
#  LST= Brightness Temp / 1 + Wevelenghth (10 Micron) * ( Brightness Temp / P) * Ln (e)
#P= h*c/s = 14380
#h= Plank constant
#c= velocity of light
#s = Boltzmann constant
#e= emissivity
#e=0.004*Pv+0.986
#Pv=proportion of vegetation= (NDVI-NDVImin / NDVImax-NDVImin)^2
#LST calculation by NDVI Threshold Method (Sobrino et al, 2014)