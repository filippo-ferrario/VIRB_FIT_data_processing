
#===================================
#
# Load packages
#
#===================================



require(tidyverse)


#===================================
#
# Load data
#
#===================================


data <- read.csv("sample_data/2018-03-21-12-41-57.csv") %>% 
  select(-X) %>% 
  mutate(rowID=row.names(.))

str(data)

names(data)[1]<-'Type'

# Melt the dataframe in one long dataset 
#-------------------------------------------------
data_long <- data %>%
  gather(key,val,-Type,-Local.Number,-Message,-rowID) %>% #str(.) #tail(.)
  mutate(
    measure = unlist(lapply(strsplit(key,split='\\.'),'[[',1)), 
    instance = unlist(lapply(strsplit(key,split='\\.'),'[[',2))
  ) %>% 
  select(-key) %>% 
  spread(measure, val) %>% 
  filter(!is.na(Value)) %>% 
  filter(Value!='') %>% 
  arrange(as.numeric(rowID),as.numeric(instance)) %>% 
  mutate(Value = if_else(is.na(Value),
                         '0',
                         Value)
  ) %>% 
  mutate(Value=lapply(Value,function(x) unlist(strsplit(x,split='\\|')))) 
#----


messg<-c('accelerometer_data','gyroscope_data','magnetometer_data','barometer_data')   # IN THIS ORDER TO MAINTAIN THE NUMERICAL CODE ASSIGNED BY PROFILE.XLSX (+1)



calibration <- data_long %>% 
  filter(Message=='three_d_sensor_calibration'&Type=='Data') %>% 
  mutate(Field=if_else(Field %in% c('accel_cal_factor','calibration_factor','gyro_cal_factor'),
                       'cal_factor',
                       Field))%>% 
  select(-Units,-instance) %>% 
  spread(Field,Value) %>% 
  mutate(Sensor=messg[as.numeric(sensor_type)+1],
         timestamp=as.integer(timestamp),
         level_shift=as.integer(level_shift),
         calibration_divisor=as.integer(calibration_divisor),
         cal_factor=as.integer(cal_factor),
         offset_cal=lapply(offset_cal,as.numeric)) %>% 
  select(-rowID,-Type,-Message,-Local.Number)




master <- data_long %>% 
  filter(Message %in% c('gyroscope_data','accelerometer_data','magnetometer_data')&Type=='Data') %>% 
  mutate(Field=if_else(substr(Field, nchar(Field)-1,nchar(Field)) %in% c('_x','_y','_z'),
                       substr(Field, nchar(Field),nchar(Field)),
                       Field)) %>% 
  select(-Units,-instance) %>% 
  spread(Field,Value) %>% 
  mutate(timestamp=as.integer(timestamp)) %>% 
  left_join(calibration,by=c('Message'='Sensor','timestamp'))
  

replace_col= names(master) %in% names(select(calibration,-timestamp))

for(s in unique(calibration$Sensor)){
  for(t in unique(calibration$timestamp[calibration$Sensor==s])){
    master[master$timestamp>t&master$Message==s,replace_col] <- select(calibration,-timestamp,-Sensor)[calibration$timestamp==t&calibration$Sensor==s,]
  }
}

unnested <- master %>% 
  unnest(sample_time_offset,x,y,z,.drop=FALSE)
str(unnested)

unnested$real_value <-
  apply(unnested,1,function(r){
  #browser()
  omat <- matrix(as.numeric(unlist(r['orientation_matrix'])),byrow=TRUE,nrow=3,ncol=3)
  input <- c(as.integer(r['x']),as.integer(r['y']),as.integer(r['z']))
  ls <- unlist(r['level_shift'])
  oc <- matrix(as.integer(unlist(r['offset_cal'])))
  cf <- unlist(r['cal_factor'])
  d <- unlist(r['calibration_divisor'])
  rv <- omat%*%(input-ls-oc)*cf/d
  list(as.numeric(rv))
  #nest(as.character(rv))     
  }
)

#unnested$nest_axes<-with(unnested,list(x,y,z))
head(unnested)
str(head(unnested))
# final- do something with mutate to extract the xyz values
# possibly it is something like  unnest(unnest(data,...), ... )

un1<- unnested %>% unnest(real_value) %>% unnest(real_value) %>% head(.)
        gather(key,val,-(1:9)) 