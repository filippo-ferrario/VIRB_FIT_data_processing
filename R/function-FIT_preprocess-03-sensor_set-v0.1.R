#===============================================================================
# Name   : R script template
# Author : Filippo Ferrario 
# Date   : 29/04/2018 16:11:06
# Version: v0.1
# Aim    : Function to process the FIT file and obtain data in a spreadsheet-like dataframe (i.e. one value per row)
#          line2frame internal function is used to arrange the values of each variable stored in a single line in a temporary dataset. 
# URL    :
#===============================================================================

#===============================
# Functions
#===============================



sensor_set<-function(fit){


#+++++++++++++++++++++++
# internal functions
#+++++++++++++++++++++++

################ BENCH
#              dataset_line=sensor_data[k,]
#              res_names=names(sensor_melt)
#              y=1
#              set_3_d_sens_cal <- fit %>%  filter(.,Message=='three_d_sensor_calibration', Type=='Data')  # a dataframe contaninin the values of the Message=='three_d_sensor_calibration'   # set_3_d_sens_cal[,1:25]
#              output_frame  <-initial
#################
#


# define a function to arrange the values of each variable stored in a single line in a temporary dataset.
line2frame<-function (dataset_line, output_frame, C_data, set_3_d_sens_cal )   {  #,C_timestamp  ,C_timestamp_ms, C_sample_time_offset  # 06/04/2018 18:14:32 delated "messg"

                     # initialize the dataframe where to store the values extracted by each row. output_frame must be an initialized empty data.frame
                     line_temp<-output_frame
                     # initialize temporary dataframe
                     line_temp2<-line_temp

                     # timestamp component since ORIGIN (i.e. referred to T0)
                     Sys_stamp    <-as.numeric(dataset_line[1,C_timestamp])
                     Sys_stamp_off_ms<- as.integer( strsplit(dataset_line[1, C_sample_time_offset], split='\\|')[[1]] ) /1000
                     Sys_stamp_ms <-as.numeric(dataset_line[1,C_timestamp_ms])/1000  #Must be divided by 1000 to converti it in seconds
                     ndata<-length(Sys_stamp_off_ms)
                     line_temp2[1:ndata,'timestamp']<- T0+Sys_stamp+Sys_stamp_off_ms+Sys_stamp_ms
                     line_temp2[1:ndata,'sys_stamp_s']<- Sys_stamp
                     line_temp2[1:ndata,'sys_stamp_ms']<-Sys_stamp_ms
                     line_temp2[1:ndata,'sys_off_ms']<- Sys_stamp_off_ms

                     # each variable
                     for (y in 1: length(C_data)){
                      line_temp2[1:ndata,'variable']<- dataset_line[1,C_data[y]-1]
                      line_temp2[1:ndata,'unit']<- dataset_line[1,C_data[y]+1]
                      line_temp2[,'values']<- as.integer( strsplit(dataset_line[1, C_data[y]], split='\\|')[[1]] )
                      line_temp2[,'sample_id']<- 1:ndata
                      line_temp<-rbind (line_temp,line_temp2)
                     }
                     return(line_temp)

            }

#+++++++++++++++++++++++
# body
#+++++++++++++++++++++++

############ bench ###########
#             fit<-fit2
#             fit<-poolSX_1
#             i<-4
#             k=1
##############################


             # extract Time 0
             T0<-T0_extract(fit)
             # define data to be extracted

             {# 29/04/2018 17:00:40
             # apparently barometer data are not always recorded. So I need to  define the 'messg' elements based in the data really recorded. use regular expression to find which are recorded.
             Idata<-grep(unique(fit$Message),pattern='_data' )
             sens_recorded<-unique(fit$Message)[Idata] # 29/04/2018 17:22:19 use the length of this vector to define 'i'  in the loop next. I assume only the barometer data are optional.
             messg<-c('accelerometer_data','gyroscope_data','magnetometer_data','barometer_data')   # IN THIS ORDER TO MAINTAIN THE NUMERICAL CODE ASSIGNED BY PROFILE.XLSX (+1)
             }

             # initialize the result dataset
             initial<-as.data.frame( matrix(ncol=10,nrow=0))
             names(initial)<-c('sample_id','Message','utc_datetime','timestamp','sys_stamp_s','sys_stamp_ms','sys_off_ms','variable','unit', 'values')
             sensor_melt<-initial


             # parse sensors data per sensor type
             #for (i in 1:length(messg)){
             for (i in 1:length(sens_recorded)){ # This has been modify on 29/04/2018 17:19:55 to limit the loop only to the sensors that have been actually used (mainly exclude barometer if not present, assuming is the only sensor optional )

                 sensor_data<- fit %>%  filter(.,Message==messg[i], Type=='Data')

                 # identify which coloumns store the timestamps and data values for each message type
                 C_timestamp   <-names(sensor_data)[which(sensor_data[1,]=='timestamp')+1]
                 C_timestamp_ms<-names(sensor_data)[which(sensor_data[1,]=='timestamp_ms')+1]
                 C_sample_time_offset<-names(sensor_data)[which(sensor_data[1,]=='sample_time_offset')+1]
                 if (messg[i]=='barometer_data'){
                 C_data<-which(sensor_data[1,]=='baro_pres')+1
                 } else{
                     C_data_x<- grep(sensor_data[1,],pattern= '_x')+1
                     C_data_y<- grep(sensor_data[1,],pattern= '_y')+1
                     C_data_z<- grep(sensor_data[1,],pattern= '_z')+1
                     C_data<-c(C_data_x,C_data_y,C_data_z)              
                     }
                     
                 # initialize the dataframe where to store the values extracted by all the rows
#                 sensor_temp<-as.data.frame( matrix(ncol=9,nrow=0))
#                 names(sensor_temp) <- names(sensor_melt)
                 sensor_temp<-initial
                 # extraxct data per line                
                 
                 for (k in 1: nrow(sensor_data))  {
                    
                    temp_res<-line2frame(dataset_line=sensor_data[k,], output_frame=initial, C_data) #,C_timestamp  ,C_timestamp_ms, C_sample_time_offset       #06/04/2018 18:14:54 deleted "messg"
                    if (k>1){
                       temp_res$sample_id<- max(sensor_temp$sample_id)+temp_res$sample_id   # the sample_id is unique for each sampling time for each line, i.e. 3 axex values taken at the same time have the same id
                       }
                    sensor_temp<-rbind(sensor_temp,temp_res)        
                 }
                 sensor_temp$Message<-messg[i]
                 sensor_temp$utc_datetime<- as.POSIXct(sensor_temp$timestamp,origin='1989-12-31 00:00:00', tz='GMT')
                 
                 sensor_melt<-rbind(sensor_melt,sensor_temp)                 
             }
             
             return(sensor_melt)
     }      
  

