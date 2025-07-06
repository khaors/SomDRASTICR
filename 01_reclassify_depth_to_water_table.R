#
# Script to reclassify depth to water table
#
library(here)
library(terra)
library(sf)
library(stars)
library(gstat)
#
# Define paths
#
base_path = here()
data_path = paste0(base_path, '/data/')
output_path = paste0(base_path, '/output/')
#
# Read raster
#
fname1 = paste0(data_path, 'Qal_dem.tif')
dem.rst = rast(fname1)
#
#
#
fname2 = paste0(data_path, 'Qal_handdug.csv')
Qal.handdug.df = read.csv(fname2, h = T)
#
#
#
depth.sf = st_as_sf(Qal.handdug.df, coords=c('Este', 'Norte'), crs = st_crs(3116))
prof.gamma = variogram(Prof_seco_~1,depth.sf)
prof.model = fit.variogram(prof.gamma, vgm(2, "Sph", 4000, 1))
plot(prof.gamma, model = prof.model, main = 'Depth to water table')
#
dem.star = st_as_stars(dem.rst)
depth.kr = krige(Prof_seco_~1, depth.sf, dem.star, model=prof.model, nmin=4, 
                 nmax=8, maxdist=20000)
plot(depth.kr['var1.var'])
depth.kr.pred.rst = rast(depth.kr['var1.pred'])
depth.kr.var.rst = rast(depth.kr['var1.var'])
#
#
#
m <- c(-1, 1.5, 10,
       1.5, 4.5, 9,
       4.5, 9, 7, 
       9, 15, 5, 
       15, 23, 3, 
       23, 30.5, 2, 
       30.5, 1e4, 1)
rclmat <- matrix(m, ncol=3, byrow=TRUE)
depth.kr.pred.reclass.rst = classify(depth.kr.pred.rst, rclmat)
fname3 = paste0(output_path, 'depth_reclassified.tif')
writeRaster(depth.kr.pred.reclass.rst, fname3)
fname4 = paste0(output_path, 'depth_variance.tif')
writeRaster(depth.kr.var.rst, fname4)
