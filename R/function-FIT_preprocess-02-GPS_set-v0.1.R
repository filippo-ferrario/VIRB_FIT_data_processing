#===============================================================================
# Name   : Funtion GPS data
# Author : Filippo Ferrario 
# Date   : 29/04/2018 15:48:14
# Version: v0.1
# Aim    : function to arrange gps data from FIT file in a dataset
# URL    :
#===============================================================================

#===============================
# Function
#===============================


gps_set<- function(fit) {
                 T0<-T0_extract(fit)             
                 gps<- fit %>%  filter(.,Message=='gps_metadata', Type=='Data')
                 
                 nrows<-nrow(gps)
                 gps.melt<-as.data.frame( matrix(ncol=9,nrow=0))

                 names(gps.melt) <-c('Message','timestamp','sys_stamp','sys_stamp_ms','utc_timestamp_OR','lat_semicircles', 'lon_semicircles','lat','lon')

                 # identify which coloumns store the timestamps and coordinates data
                 C_timestamp   <-names(gps)[which(gps[1,]=='timestamp')+1]
                 C_timestamp_ms<-names(gps)[which(gps[1,]=='timestamp_ms')+1]
                 C_lat         <-names(gps)[which(gps[1,]=='position_lat')+1]
                 C_lon         <-names(gps)[which(gps[1,]=='position_long')+1]  
                 C_utc         <-names(gps)[which(gps[1,]=='utc_timestamp')+1]      
                 # extract data
                 for (k in 1:nrows){
                   # timestamp component since ORIGIN (i.e. referred to T0)
                   Sys_stamp    <-as.numeric(gps[k,C_timestamp])
                   Sys_stamp_ms <-as.numeric(gps[k,C_timestamp_ms])/1000  #Must be divided by 1000 to converti it in seconds
                   gps.melt[k,'timestamp']<- T0+Sys_stamp+Sys_stamp_ms
                   gps.melt[k,'sys_stamp']<- gps[k,C_timestamp]
                   gps.melt[k,'sys_stamp_ms']<- gps[k,C_timestamp_ms] 
                   
                   ## info on how to convert COUNTS to Lat long deg is available at:
                   ## https://forums.garmin.com/forum/developers/device-communications/garmin-device-interface-sdk/51683-
                   # lat
                   gps.melt[k,'lat_semicircles']<- as.numeric(gps[k, C_lat])
                   gps.melt[k,'lat']<- gps.melt[k,'lat_semicircles']/(2^31/180)
                   # lon
                   gps.melt[k,'lon_semicircles']<- as.numeric(gps[k,C_lon])
                   gps.melt[k,'lon']<- gps.melt[k,'lon_semicircles']/(2^31/180)
                   # message as in FIT
                   gps.melt[k,'Message'] <-gps[k,'Message']
                   gps.melt[k,'utc_timestamp_OR'] <-gps[k,C_utc]
                 }           
                 
                 return(gps.melt)
}


#===============================
# bench
#===============================

#fit<-fit2


#gps_fit<-gps_set(fit2)



