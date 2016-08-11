input_time<-readline("input time in hh:mm:ss format:      ")
datetime<-as.POSIXct(paste(input_date,input_time,sep=" "))
  
file_time<-format(datetime,"%H%M%S")
  
path<-"c:/users/clai/Documents/EVA/L2_trial_080316"
  
  #"c:/users/clai/Documents/EVA/L2_trial_080316"
  
library(XML)
  
xmlfile<-xmlParse(paste(path,file_time,"_",file_date,".cmbl",sep=""))
data_nodes<-getNodeSet(xmlfile,"/Document/DataSet/DataColumn//ColumnCells")
  
ir_temp<-xmlToDataFrame(data_nodes)
  
library(splitstackshape)
  
ir_temp<-cSplit(ir_temp,1,direction="wide",sep = "\n")
ir_temp<-t(ir_temp)
ir_temp<-as.data.frame(ir_temp)
rownames(ir_temp)<-1:nrow(ir_temp)
colnames(ir_temp)<-c("time","roll_temp","sheet_temp","sheet_temp2")
  
for (i in 1:length(ir_temp$time)){
  if(is.na(ir_temp$time[i]==TRUE)){
    for (i in match(NA,ir_temp$time):length(ir_temp$time)){
      ir_temp$time[i]<-ir_temp$time[i-1]+ir_temp$time[2]
    }
  }
}
  
  input_complete<-readline("input complete? y/n      ")
}