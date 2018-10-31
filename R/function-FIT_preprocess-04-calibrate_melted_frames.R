#===============================================================================
# Name   : R script template
# Author : Filippo Ferrario 
# Date   : 29/04/2018 16:18:02
# Version: v1
# Aim    : Calibrate and convert the sensor values from the COUNT format to physically meaningfull units
# URL    : 
#===============================================================================


#===============================
# BENCH VALUES
#===============================

# dataset_melted=examp#sensor_temp  
# set_3_d_sens_cal <- fit %>%  filter(.,Message=='three_d_sensor_calibration')  # a dataframe contaninin the values of the Message=='three_d_sensor_calibration'   # set_3_d_sens_cal[,1:25]   
#               
# messg<-c('accelerometer_data','gyroscope_data','magnetometer_data','barometer_data')   # IN THIS ORDER TO MAINTAIN THE NUMERICAL CODE ASSIGNED BY     
# m=2
# t=1
# s=1
             
  
             

#===============================
# function
#===============================




calibrate_melted_frames<-function(dataset_melted, set_3_d_sens_cal, messg){

          dataset_melted$cal_values<-dataset_melted$cal_unit<-NA
          dataset_melted$row_id<-1:nrow(dataset_melted)
          
           ## identify which coloumns store the calibration values
                     definitions<-set_3_d_sens_cal[set_3_d_sens_cal$Type=='Definition',]
                     C_calibration_factor<-which(definitions=='calibration_factor')+1
                     C_offset_cal        <-which(definitions=='offset_cal')+1
                     C_orientation_matrix<-which(definitions=='orientation_matrix')+1
                     C_level_shift       <-which(definitions=='level_shift')+1
                     C_calibration_divisor<-which(definitions=='calibration_divisor')+1
                     C_unit             <-which(definitions=='calibration_factor')+2
                     C_sen_type         <-which(definitions=='sensor_type')+1
                     C_tsmp             <-which(definitions=='timestamp')+1

           for (m in 1:3){
                     

                     # prepare calibration data per sensor
                     sens_type<-m-1
                     index<- set_3_d_sens_cal[C_sen_type]==sens_type                 
                     sens_cal<-set_3_d_sens_cal[index &set_3_d_sens_cal$Type=='Data',]  # sens_cal[,1:25]
                     cal.points<-sens_cal[,C_tsmp]
                     
                    
                     
                     for (t in 1:length(cal.points)){
                     
                     ## extract values
                     I1<- sens_cal[,C_tsmp]==cal.points[t]
                       calibration_factor<- as.numeric(sens_cal[I1, C_calibration_factor] )
                       level_shift       <- as.numeric(sens_cal[I1, C_level_shift] )
                       calibration_divisor<- as.numeric(sens_cal[I1, C_calibration_divisor] )
                       new_unit             <-  sens_cal[I1, C_unit] 
                       offset_cal<- matrix(ncol=1,nrow=3, as.numeric( strsplit(sens_cal[I1, C_offset_cal], split='\\|')[[1]] ) )
                       orientation_matrix<-matrix(ncol=3,nrow=3, byrow=T, as.numeric( strsplit(sens_cal[I1, C_orientation_matrix], split='\\|')[[1]] ) )
                     
                        # index to select rows per sensor per calibration time
                        ncal<-length(cal.points)
                        if (cal.points[t]<cal.points[ncal]){               
                      #  I2<-with(dataset_melted, which(Message==messg[m] & sys_stamp_s>=cal.points[t] & sys_stamp_s<cal.points[t+1]))
                         v_row_id<- dataset_melted %>% filter(Message==messg[m] ,sys_stamp_s>=cal.points[t] & sys_stamp_s<cal.points[t+1])  %>% pull(.,row_id)
                        } else {
                      #    I2<-with(dataset_melted, which(Message==messg[m] & sys_stamp_s>cal.points[t]))                      
                           v_row_id<- dataset_melted %>% filter(Message==messg[m],sys_stamp_s>=cal.points[t])  %>% pull(.,row_id) #%>% unique(.)
                           }
                            
                      if (length( v_row_id)>0 ) {
                      # vector specifing the sample_id (i.e. a sample has X Y and Z values taken at the same time)
                      sample_ID<- dataset_melted %>% filter(.,row_id%in% v_row_id ) %>% pull(.,sample_id) %>% unique(.)
                      
                      # 
                        for (s in 1 : length(sample_ID)){
                        

                       #    val<- dataset_melted %>% filter(.,row_id %in%v_row_id & sample_id==sample_ID[s]) %>% pull(.,values)
#                           dataset_melted<- dataset_melted %>% mutate(.,
#                                               cal_values=replace(cal_values, row_id %in%v_row_id & sample_id==sample_ID[s], as.vector(orientation_matrix %*% (val-level_shift-offset_cal) * calibration_factor)/  calibration_divisor),
#                                               cal_unit=replace(cal_unit, row_id %in%v_row_id & sample_id==sample_ID[s],new_unit))   
#                                               
                  #                          
                            I3<-which(dataset_melted$row_id %in%v_row_id & dataset_melted$sample_id==sample_ID[s])
                            val<-dataset_melted[I3,]$values
                            dataset_melted[I3,]$cal_values<- as.vector((orientation_matrix %*% (val-level_shift-offset_cal) * calibration_factor)/  calibration_divisor)
                            dataset_melted[I3,]$cal_unit<-new_unit
                         }
                         }
                      
                     
                     }
           }
                     
          dataset_melted<- select(dataset_melted,-row_id  )       
          
          return(dataset_melted)           
}



