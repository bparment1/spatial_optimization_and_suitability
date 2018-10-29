############### Urban green infrastructure project ########## 
## SESYNC Research Support
##
## Processing data for spatial optimization for urban green infrastructure for the Baltimore area. 
##
##
## DATE CREATED: 10/29/2018
## DATE MODIFIED: 10/29/2018
## AUTHORS: Benoit Parmentier, Tijana Jovanovic  
## Version: 1
## PROJECT: Urban green infrastructure
## ISSUE: 
## TO DO:
##
## COMMIT: initial commit
##

## Very good reference:

###################################################
#

###### Library used

library(MASS)
library(lme4)
library(rstanarm)
library("bayesplot")
library("ggplot2")
library("loo")
library("parallel")

####### Functions used in this script and sourced from other files

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
modeling_functions <- "bayes_logistic_model_functions_10242018.R"
source(file.path(script_path,modeling_functions))

#########cd ###################################################################
#####  Parameters and argument set up ########### 

#ARGS 1
in_dir <- "/nfs/bparmentier-data/Data/projects/soilsesfeedback-data/data"
#ARGS 2
out_dir <- "/nfs/bparmentier-data/Data/projects/soilsesfeedback-data/outputs"
#ARGS 3:
create_out_dir_param=TRUE #create a new ouput dir if TRUE
#ARGS 7
out_suffix <-"_10232018" #output suffix for the files and ouptut folder
#ARGS 8
num_cores <- 2 # number of cores

in_filename <- "NRCS_FSAMergeDataset_w_PDSI2_7_28_18.csv"
model_type <- "bayes_stan"
y_var_name <- "Concern_DryDrought"

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

dataDR <- read.csv(file.path(in_dir,in_filename), 
                   header = TRUE)

dataDR$y_var <- dataDR[[y_var_name]]

dataDR$stdiv <- factor(dataDR$stdiv)
dataDR$Agency <- factor(dataDR$Agency)
dataDR$y_var <- factor(dataDR$y_var)


#Z:\Data\Baltimore\Hydrology\GAstart
in_dir <- "/nfs/tjovanovic-data/Data/Baltimore/Hydrology/GAstart"