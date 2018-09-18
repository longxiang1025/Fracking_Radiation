library(mgcv)
library(ggmap)
library(raster)
library(lubridate)
library(knitr)
library(dplyr)
library(grid)
library(gridExtra)
library(tidyverse)
library(reshape2)
library(MASS)
#+ load-data,message=F,echo=F
study_period<-interval(ymd("2001-01-01"),ymd("2017-12-31"))
prod_period<-interval(ymd("2001-01-01"),ymd("2018-09-10"))
prjstring<-"+proj=aea +lat_1=20 +lat_2=60 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=WGS84 +units=m +no_defs "
geoprjstring<-"+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
load( here::here("data","RadNet_And_Gas_50B.Rdata"))
names(rad_frac_data)[2]<-"beta"
RadNet_City_List<-read_csv(here::here("data","Processed-RadNet-Beta-citylist.csv"))
load(here::here("data","Basic_Geodata","Boundaries.RData"))
rad_frac_data$City<-as.factor(rad_frac_data$City)
load(here::here("data","All_Gas_Wells_Active_After_2007.RData"))
load( here::here("data","Fort_Worth_Data.Rdata"))
#+ summary of the Fort Worth City Radiation Data
summary(city_rad)
