#===============================================================================
# Name   : Funtion Time Zero
# Author : Filippo Ferrario
# Date   : 29/04/2018 15:42:18
# Version: v0.1
# Aim    : Extract Time 0 from FIT file
# URL    : 
#===============================================================================



#################### 
# function  
#################### 

T0_extract<-function(fit){
 

             timecor<- fit %>% filter(.,Message=='timestamp_correlation', Type=='Data' )
             Tstamp       <-as.numeric(timecor[1,which(timecor[1,]=='timestamp')+1])
             Tstamp_ms    <-as.numeric(timecor[1,which(timecor[1,]=='timestamp_ms')+1] )/1000 # Must be divided by 1000 to converti it in seconds
             Sys_stamp    <-as.numeric(timecor[1,which(timecor[1,]=='system_timestamp')+1])
             Sys_stamp_ms <-as.numeric(timecor[1,which(timecor[1,]=='system_timestamp_ms')+1])/1000  #Must be divided by 1000 to converti it in seconds

             T0<- as.numeric(Tstamp +Tstamp_ms - Sys_stamp-Sys_stamp_ms)
             as.POSIXct(T0, origin='1989-12-31 00:00:00', tz='GMT')
             return(T0)
             }


####################
# bench
####################

#fit<-fit2
