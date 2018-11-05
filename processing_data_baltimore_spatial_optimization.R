############### Urban green infrastructure project ########## 
## SESYNC Research Support
##
## Processing data for spatial optimization for urban green infrastructure for the Baltimore area. 
##
##
## DATE CREATED: 10/29/2018
## DATE MODIFIED: 11/01/2018
## AUTHORS: Benoit Parmentier, Tijana Jovanovic  
## Version: 1
## PROJECT: Urban green infrastructure
## ISSUE: 
## TO DO:
##
## COMMIT: testing gdwal_warp
##

## Very good reference:
#Land Cover data : https://chesapeakeconservancy.org/conservation-innovation-center/high-resolution-data/land-cover-data-project-2/
#DEM data:  https://imap.maryland.gov/Pages/lidar-dem-download-files.aspx 

###################################################
#

###### Library used

library(sp)
library(raster)
library(rgdal)
require(rgeos)
library(BMS) #contains hex2bin and bin2hex
library(bitops)
require(RCurl)
require(stringr)
require(XML)
library(lubridate)
library(sf)

####### Functions used in this script and sourced from other files

generate_raster_dataType_table <- function(){
  #Goal: this function generate a table (data.frame) with data types
  # and valid value range used in the raster package R. The corresponding
  # data type in the GDAL library is provided to allow matching when using
  # GDAL commands.
  
  # Note that we are using the specific data types for tif.
  # The following links provide more information:
  #https://www.gdal.org/frmt_gtiff.html
  #urrently band types of Byte, UInt16, Int16, UInt32, Int32, Float32, 
  #Float64, CInt16, CInt32, CFloat32 and CFloat64 are supported for reading and writing.
  
  ######### Start scripts ################
  
  vals <- c("LOG1S",NA,	FALSE,TRUE, 
            "INT1S","Byte",	-127,	127,
            "INT1U",NA,0, 255,
            "INT2S","Int16",	"-32,767","32,767",
            "INT2U","UInt16",	0,	"65,534",
            "INT4S","int32",	"-2,147,483,647",	"2,147,483,647",
            "INT4U","UInt32",	0,	"4,294,967,296",
            "FLT4S","Float32",	"-3.4e+38",	"3.4e+38",
            "FLT8S","Float64",	"-1.7e+308",	"1.7e+308")
  
  dataType_table <- matrix(vals,nrow=9,ncol=4,byrow=T)
  
  dataType_table <-data.frame(dataType_table)
  
  names(dataType_table) <- c("r_type","gdal_type","min","max")
  ### bug error, columns have become factor: changed this here
  dataType_table <- data.frame(lapply(dataType_table, as.character), stringsAsFactors=FALSE)
  
  #class(dataType_table$gdal_type)
  
  return(dataType_table)
}

create_dir_fun <- function(outDir,out_suffix=NULL){
  #if out_suffix is not null then append out_suffix string
  if(!is.null(out_suffix)){
    out_name <- paste("output_",out_suffix,sep="")
    outDir <- file.path(outDir,out_name)
  }
  #create if does not exists
  if(!file.exists(outDir)){
    dir.create(outDir)
  }
  return(outDir)
}

#Used to load RData object saved within the functions produced.
load_obj <- function(f){
  env <- new.env()
  nm <- load(f, env)[1]
  env[[nm]]
}

################### Start script ###################

#Benoit setup
script_path <- "/nfs/bparmentier-data/Data/projects/soilsesfeedback-data/scripts"
#modeling_functions <- "bayes_logistic_model_functions_10242018.R"
#source(file.path(script_path,modeling_functions))

#########cd ###################################################################
#####  Parameters and argument set up ########### 

#ARGS 1
in_dir <- "/nfs/bparmentier-data/Data/projects/urban_green_planning/GAstart"
#Z:\Data\Baltimore\Hydrology\GAstart
#in_dir <- "/nfs/tjovanovic-data/Data/Baltimore/Hydrology/GAstart"
#ARGS 2
out_dir <- "/nfs/bparmentier-data/Data/projects/urban_green_planning/outputs"
#ARGS 3:
create_out_dir_param=TRUE #create a new ouput dir if TRUE
#ARGS 7
out_suffix <-"processing_data_11012018" #output suffix for the files and ouptut folder
#ARGS 8
num_cores <- 2 # number of cores
file_format <- ".tif"

dem_baltimore_filename <- "DEM_BaltArea_1m.tif"
landcover_baltimore_filename <- "landCover_area1m.tif"
reg_outline_filename <- "watersheds8digit.shp"

################# START SCRIPT ###############################

######### PART 0: Set up the output dir ################

options(scipen=999)

#set up the working directory
#Create output directory

if(is.null(out_dir)){
  out_dir <- in_dir #output will be created in the input dir
  
}
#out_dir <- in_dir #output will be created in the input dir

out_suffix_s <- out_suffix #can modify name of output suffix
if(create_out_dir_param==TRUE){
  out_dir <- create_dir_fun(out_dir,out_suffix)
  setwd(out_dir)
}else{
  setwd(out_dir) #use previoulsy defined directory
}

#######################################
### PART 1: Read in DATA #######

r_dem <- raster(file.path(in_dir,dem_baltimore_filename))
r_lc <- raster(file.path(in_dir,landcover_baltimore_filename))
reg_sf <- read_sf(file.path(in_dir,reg_outline_filename))

plot(reg_sf$geometry,add=T)
reg_sf

reg_sf_dem <- st_transform(reg_sf,projection(r_dem))
plot(r_dem)
plot(reg_sf_dem$geometry,add=T)

#########################################
### PART 2: Crop to region of interest #######

reg_sp_dem <- as(reg_sf_dem,"Spatial")
r_dem_crop <- crop(r_dem,reg_sp_dem)

dem_crop_baltimore_filename <- paste0("r_dem_crop_",out_suffix,file_format)
writeRaster(r_dem_crop,filename = file.path(out_dir,dem_crop_baltimore_filename))

reg_sf_lc <- st_transform(reg_sf,projection(r_lc))

reg_sp_lc <- as(reg_sf_lc,"Spatial")
r_lc_crop <- crop(r_lc,reg_sp_lc)
plot(r_lc)
plot(r_lc_crop)
plot(reg_sf_lc$geometry,add=T)

lc_crop_baltimore_filename <- paste0("r_lc_crop_",out_suffix,file_format)
writeRaster(r_lc_crop,filename = file.path(out_dir,lc_crop_baltimore_filename))

#########################
### PART 3: reproject raster to Maryland State plane #######

#local md projection:
crs_reg <- "+proj=lcc +lat_1=39.45 +lat_2=38.3 +lat_0=37.66666666666666 +lon_0=-77 +x_0=400000 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs" 
dataType_table <- generate_raster_dataType_table()

#r_dem_md <- projectRaster(r_dem_crop,res=c(1,1),crs=crs_reg)
#writeRaster(r_dem_md,"dem_md.tif")

#r_lc_md <- projectRaster(r_lc_crop,r_dem_md)

#### Use gdal:reproject dem first 

data_type_str <- dataType(r_dem)
NAvalue(r_dem)
dataType_selected <- dataType_table$r_type==data_type_str
data_type_table_selected <- dataType_table[dataType_selected,]
data_type_table_selected

dem_md_baltimore <- file.path(out_dir,paste0("r_dem_crop_","md_",out_suffix,file_format))
src_dataset <- dem_crop_baltimore_filename 
dst_dataset <- dem_md_baltimore
  
output_type <- data_type_table_selected$gdal_type
NA_flag_val_str <- data_type_table_selected$min
CRS_reg <- crs_reg

#gdalwarp -t_srs '+proj=utm +zone=11 +datum=WGS84' -overwrite raw_spot.tif utm11.tif

cmd_str = paste0("gdalwarp",
                 " -ot ",output_type,
                 " -srcnodata ",NA_flag_val_str,
                 " -t_srs ", paste0("'",CRS_reg,"'"),
                 " -dstnodata ",NA_flag_val_str,
                 " -overwrite",
                 " ",src_dataset, 
                 " ",dst_dataset)             

system(cmd_str)

r_dem_md <- raster(dem_md_baltimore)
plot(r_dem_md)

#### Use gdal:reproject lc

data_type_str <- dataType(r_lc)
NAvalue(r_lc)
dataType_selected <- dataType_table$r_type==data_type_str
data_type_table_selected <- dataType_table[dataType_selected,]
data_type_table_selected

src_dataset <- lc_crop_baltimore_filename 
dst_dataset <- file.path(out_dir,paste0("r_lc_crop_","md_",out_suffix,file_format))

output_type <- data_type_table_selected$gdal_type
NA_flag_val_str <- data_type_table_selected$min
CRS_reg <- crs_reg

#gdalwarp -t_srs '+proj=utm +zone=11 +datum=WGS84' -overwrite raw_spot.tif utm11.tif

#gdalsrsinfo -o wkt other.tif > target.wkt
cmd_str <- paste0("gdalsrsinfo ",
                  "-o wkt ", 
                  dem_md_baltimore,
                  " > ", "target.wkt")
out <- system(cmd_str)
res_val <- res(r_dem_md)
ncol(r_dem_md)
ts_val <- rev(dim(r_dem_md)[1:2])
extent_val <- extent(r_dem_md)
#> extent(r_dem_md)
#class       : Extent 
#xmin        : 408279.5 
#xmax        : 452098.3 
#ymin        : 168605.6 
#ymax        : 201700 

#" -te <x_min> <y_min> <x_max> <y_max>",
extent_gdal <- extent_val[c(1,3,2,4)]

#-tr xres yres:
#  set output file resolution (in target georeferenced units)
#-tap:
#  (GDAL >= 1.8.0) (target aligned pixels) align the coordinates of the extent of the output file to the values of the -tr, 
#such that the aligned extent includes the minimum extent.
#-ts width height:
#  set output file size in pixels and lines. 
#If width or height is set to 0, the other dimension will be guessed from the computed resolution. Note that -ts cannot be used with -tr

interpolation_method <- "near"
## Need to change data type!!!
cmd_str <- paste0("gdalwarp",
                 " -ot ",output_type,
                 " -srcnodata ",NA_flag_val_str,
                 " -r ", interpolation_method,
                 " -t_srs ", "target.wkt",
                 " -te ", paste0(extent_gdal,collapse=" "),
                 #" -tr ", paste0(res_val,collapse = " "),
                 #" -tap ",
                 " -ts ", paste0(ts_val,collapse = " "),
                 " -dstnodata ",NA_flag_val_str,
                 " -overwrite",
                 " ",src_dataset, 
                 " ",dst_dataset)      

#$ gdalwarp -te <x_min> <y_min> <x_max> <y_max> input.bil clipped_output.bil

system(cmd_str)

r_lc_md <- raster(dst_dataset)
extent(r_lc_md)
extent(r_dem_md)
res(r_lc_md)
res(r_dem_md)
dim(r_dem_md)
dim(r_lc_md)
#https://gis.stackexchange.com/questions/239013/reprojecting-a-raster-to-match-another-raster-using-gdal


################################### End of script #########################################
