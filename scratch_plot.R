if(exists("scratch_gap")==FALSE){
  scratch_gap<-data.frame(time=character(),temp=numeric(),gap=numeric(),date=character(),Time=character(),state=numeric())
  scratch_gap$time<-as.POSIXct(scratch_gap$time)
  scratch_gap$date<-as.POSIXct(scratch_gap$date)
  scratch_gap$Time<-as.POSIXct(scratch_gap$Time)
  }

first_date<-as.POSIXct(readline("enter file date in yyyy-mm-dd format:      "))
path<-"C:\\Users\\Clai\\Documents\\Line_2\\"
first_filedate<-format(first_date,"%m%d%y")

all_data_file_path<-paste(path,"composite_data_",first_filedate,".txt",sep="")
gap_data_file_path<-paste(path,"scratch_in_gap_",first_filedate,".txt",sep="")

all_data<-read.delim(all_data_file_path)
gap_data<-read.delim(gap_data_file_path)

all_data$time<-as.POSIXct(all_data$time)
gap_data$time<-as.POSIXct(gap_data$time)

new_scratch_data<-all_data[all_data$loc=="scratcher_in",]
new_scratch_data$time<-as.POSIXct(new_scratch_data$time)

start_time<-new_scratch_data[1,1]

new_gap_data<-gap_data[match(start_time,gap_data$time):dim(gap_data)[1],]

new_gap_data$date<-first_date
new_gap_data$Time<-as.POSIXct(new_gap_data$time)
new_gap_data$temp<-new_gap_data$temp*1.8+32
new_gap_data$state<-new_scratch_data$state[1:dim(new_gap_data)[1]]

scratch_gap<-rbind(scratch_gap, new_gap_data)

median_temp_data<-na.omit(new_gap_data[new_gap_data$state==2,2])

median_temp<-data.frame(date=character(),mean=numeric(),stdev=numeric())
individual_median_temp<-data.frame(date=first_date,t(quantile(median_temp_data)),mean=mean(median_temp_data),stdev=sd(median_temp_data))
colnames(individual_median_temp)[2:6]<-c("min","1qr","median","3qr","max")
median_temp<-rbind(median_temp,individual_median_temp)
View(median_temp)

input_complete<-readline("input completed? y/n          ")

while(input_complete=="n"){
  date<-as.POSIXct(readline("enter file date in yyyy-mm-dd format:      "))
  path<-"C:\\Users\\Clai\\Documents\\Line_2\\"
  filedate<-format(date,"%m%d%y")
  
  all_data_file_path<-paste(path,"composite_data_",filedate,".txt",sep="")
  gap_data_file_path<-paste(path,"scratch_in_gap_",filedate,".txt",sep="")
  
  all_data<-read.delim(all_data_file_path)
  gap_data<-read.delim(gap_data_file_path)
  
  all_data$time<-as.POSIXct(all_data$time)
  gap_data$time<-as.POSIXct(gap_data$time)
  
  new_scratch_data<-all_data[all_data$loc=="scratcher_in",]
  new_scratch_data$time<-as.POSIXct(new_scratch_data$time)
  
  start_time<-new_scratch_data[1,1]
  
  new_gap_data<-gap_data[match(start_time,gap_data$time):dim(gap_data)[1],]
  new_gap_data$date<-date
  new_gap_data$Time<-as.POSIXct(new_gap_data$time)
  new_gap_data$temp<-new_gap_data$temp*1.8+32
  new_gap_data$state<-new_scratch_data$state[1:dim(new_gap_data)[1]]
  
  if(first_date<date){
    new_gap_data$Time<-new_gap_data$Time-(date-first_date)
  }
  scratch_gap<-rbind(scratch_gap, new_gap_data)
  
  median_temp_data<-na.omit(new_gap_data[new_gap_data$state==2,2])
  
  individual_median_temp<-data.frame(date=date,t(quantile(median_temp_data)),mean=mean(median_temp_data),stdev=sd(median_temp_data))
  colnames(individual_median_temp)[2:6]<-c("min","1qr","median","3qr","max")
  median_temp<-rbind(median_temp,individual_median_temp)
  View(median_temp)
  
  input_complete<-readline("input completed? y/n          ")
  
}

library(ggplot2)

scratch_plot<-ggplot(data=scratch_gap)+geom_point(aes(x=Time-3600*4,y=temp,color=factor(gap)),size=1)+theme_bw()
scratch_plot<-scratch_plot+scale_x_datetime(date_breaks="15 min",date_labels = "%H:%M")
scratch_plot<-scratch_plot+scale_y_continuous(breaks=c(100,125,150,175,200,225,250,275,300,325))
scratch_plot<-scratch_plot+scale_color_discrete(name="gap in stock?",labels=c("no video","startup/shutdown","no gap","gap"))
scratch_plot<-scratch_plot+theme(axis.text.x=element_text(angle=90))+facet_grid(date~.)
scratch_plot<-scratch_plot+guides(color=guide_legend(override.aes=list(size=3)))+xlab("time")
print(scratch_plot)