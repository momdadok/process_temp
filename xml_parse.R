input_date<-readline("input date in yyyy-mm-dd format:   ")
input_time<-readline("input time in hh:mm:ss format:      ")
datetime<-as.POSIXct(paste(input_date,input_time,sep=" "))

file_time<-format(datetime,"%H%M%S")

path<-"/home/christin/Documents/work/"

library(XML)

xmlfile<-xmlParse(paste(path,file_time,"_exp13.cmbl",sep=""))
data_nodes<-getNodeSet(xmlfile,"/Document/DataSet/DataColumn//ColumnCells")

ir_temp<-xmlToDataFrame(data_nodes)

library(splitstackshape)

ir_temp<-cSplit(ir_temp,1,direction="wide",sep = "\n")
ir_temp<-t(ir_temp)
ir_temp<-as.data.frame(ir_temp)
rownames(ir_temp)<-1:nrow(ir_temp)
colnames(ir_temp)<-c("time","roll_temp","sheet_temp","sheet_temp2")

View(ir_temp)