invisible(readline("check file path.  Press [enter] to continue..."))
date<-as.POSIXct(readline("input date in yyyy-mm-dd format:    "))

input_complete<-"n"

while (input_complete=="n"){
  data_source<-readline("input data source?    ")
  path<-"c:/Users/Clai/Documents/EVA/L2_trial_080216/"
  file_date<-format(date,"%m%d%y")
  file_path<-paste(path,data_source,"_",file_date,".txt",sep="")
  
  L2temp<-read.delim(file_path)
  
  L2temp$time<-as.POSIXct(strptime(L2temp$time,format="%Y-%m-%d %H:%M:%S"))
  L2temp$temp<-L2temp$temp*1.8+32 
  
  if(exists("mincut")==FALSE){
    mincut<-data.frame(mincut=numeric(),loc=character())
  }
  
  if(exists("minpeak")==FALSE){
    minpeak<-data.frame(minpeak=numeric())
  }
  
  accept_result<-"n"
  
  while(accept_result=="n"){
    print("mincut:banbury(default<0.382>-0.44),mill(0.45-0.63),scratch_in(0.388-0.45),sheeter_out(0.46-0.665)
      #cal#1_in/cal#2_out(0.665-0.8),cal#1_out(0.512),cal#2_in(0.56-0.665)")
    mincut_value<-as.numeric(readline("input mincut:  "))
    
    insert_mincut<-data.frame(mincut=mincut_value,loc=data_source)
    
    if(data_source=="banbury"){
      print("minpeak banbury (0.88)")
      minpeak_value<-as.numeric(readline("input minpeak:  "))
      insert_minpeak<-data.frame(minpeak=minpeak_value)
      
       }  else {
          minpeak_value<-0.1 #default cardidate minpeak value
    }
    library("cardidates")
    batch<-peakwindow(L2temp,minpeak=minpeak_value,mincut=mincut_value) 
    L2temp_plotdata<-data.frame(L2temp,batch$peakid)
    colnames(L2temp_plotdata)[3]<-"peak"
    
    raw_plotdata<-L2temp_plotdata
    
    library("ggplot2")
    if(data_source=="banbury"|data_source=="mill"){
      raw_plot<-ggplot(data=raw_plotdata[raw_plotdata$temp>230,])+geom_line(aes(x=time-3600*4,y=temp,color=factor(peak)),size=0.5)+theme_bw()
      raw_plot<-raw_plot+theme(axis.text.x=element_text(angle = 90),legend.position="none")+xlab("time")
      raw_plot<-raw_plot+scale_x_datetime(date_breaks = "10 min",date_labels = "%H:%M")
      print(raw_plot)
    }
    if(data_source!="banbury"&data_source!="mill"){
      raw_plot<-ggplot(data=raw_plotdata)+geom_line(aes(x=time-3600*4,y=temp,color=factor(peak)))+theme_bw()+xlab("time")
      raw_plot<-raw_plot+theme(axis.text.x=element_text(angle=90))+scale_x_datetime(date_breaks = "10 min",date_labels = "%H:%M")
      raw_plot2<-raw_plot2+geom_text(data=banbury_peak,aes(x=time-3600*4,y=temp,label=round(temp)),vjust=.8,hjust=.4,check_overlap = TRUE)
      print(raw_plot)
    }
    run_all<-readline("run rest of program? y/n       ")
    if(data_source=="banbury"&run_all=="y"){
      
      v<-c()
      for(i in 1:(length(batch$peaks$index)-1)){
        for(j in batch$peaks$index[i]:(batch$peaks$index[i+1]-1)){
          v[j]=i}
      }
      
      for(j in batch$peaks$index[i+1]:nrow(L2temp)){
        v[j]=i+1}
      L2temp_plotdata$batch<-v
      L2temp_plotdata<-na.omit(L2temp_plotdata)
      
      w<-c()
      oldbatch<--1
      counter<-0
      for(i in 1:nrow(L2temp_plotdata)){
        if(L2temp_plotdata$batch[i]!=oldbatch){
          counter=0
          oldbatch=L2temp_plotdata$batch[i]
        }
        w[i]=counter
        counter=counter+1
      }
      L2temp_plotdata$batch_time<-w 
      
      median_temp<-data.frame(loc=character(),mean=numeric(),stdev=numeric())
      
      banbury_plotdata<-L2temp_plotdata
      banbury_peak<-raw_plotdata[batch$peaks$index,]
      cycle_time<-c(NA,(diff(banbury_peak$time)*60))
      banbury_peak$cycle_time<-cycle_time
      median_temp<-data.frame(loc="banbury",t(quantile(banbury_peak$temp)),mean=mean(banbury_peak$temp),stdev=sd(banbury_peak$temp))
      colnames(median_temp)[2:6]<-c("min","1qr","median","3qr","max")
      
      library(ggplot2)
      cycle_time_plot<-ggplot(data=banbury_peak)+geom_point(aes(x=time-3600*4,y=cycle_time),size=4)+theme_bw()
      cycle_time_plot<-cycle_time_plot+scale_x_datetime(date_breaks="10 min",date_labels="%H:%M")
      cycle_time_plot<-cycle_time_plot+theme(axis.text.x=element_text(angle=90,size=15),axis.title=element_text(size=15,face="bold"))+xlab("time")+geom_text(aes(x=time-3600*4,y=cycle_time,label=cycle_time),vjust=-1,hjust=.5,check_overlap = TRUE)
      
      raw_plot2<-ggplot()+geom_line(data=raw_plotdata[raw_plotdata$temp>240,],aes(x=time-3600*4,y=temp,color=factor(peak)))
      raw_plot2<-raw_plot2+theme_bw()+theme(legend.position="none",axis.text.x=element_text(angle=90),axis.title=element_text(size=15,face="bold"))
      raw_plot2<-raw_plot2+scale_x_datetime(date_breaks="10 min",date_labels = "%H:%M")+xlab("time")
      raw_plot2<-raw_plot2+geom_text(data=banbury_peak,aes(x=time-3600*4,y=temp,label=round(temp))
                                     ,vjust=.8,hjust=.4,check_overlap = TRUE,angle=20)
      
      library(gridExtra)
      print(grid.arrange(raw_plot2,cycle_time_plot,ncol=1))
      
      View(banbury_peak)
      View(median_temp)
      
      u<-c()
      u<-ifelse(banbury_plotdata$temp<(max(banbury_plotdata$temp)*minpeak_value),1,2) #assign state
      
      }#banbury/run_all if bracket  
    
    if(data_source=="mill"&run_all=="y"){
      source("c:\\Users\\Clai\\Documents\\Line_2\\check_peak.R")
      mill_peak<-raw_plotdata[batch$peaks$index,]
      View(mill_peak)
      invisible(readline("check mill peaks in View... Press [Enter] to continue "))
      individual_median_temp<-data.frame(loc="mill",t(quantile(mill_peak$temp)),mean=mean(mill_peak$temp),stdev=sd(mill_peak$temp))
      colnames(individual_median_temp)[2:6]<-c("min","1qr","median","3qr","max")
      median_temp<-rbind(median_temp,individual_median_temp)
      
      start_time<-raw_plotdata[batch$peaks$index[1],1]
      L2temp_plotdata<-raw_plotdata[match(start_time,raw_plotdata$time):dim(raw_plotdata)[1],]
      
      short_banbury_plotdata<-banbury_plotdata[1:dim(L2temp_plotdata)[1],]
      
      L2temp_plotdata$batch<-short_banbury_plotdata$batch
      L2temp_plotdata$batch_time<-short_banbury_plotdata$batch_time
      
      s<-c()
      s<-ifelse(L2temp_plotdata$temp<(max(L2temp_plotdata$temp)*.75),1,2)
      
      L2temp_plotdata$loc<-"mill"
      L2temp_plotdata$state<-s
    }
    
    if(data_source!="banbury"|data_source!="mill"){
      source("c:/Users/Clai/Documents/Line_2/merge_peak.R")
    }
      accept_result<-readline("accept result? y/n      ")
  
  }#accept result bracket #1
    save_mincut<-readline("save mincut values? y/n          ")
    if(save_mincut=="y"){
      mincut<-rbind(mincut,insert_mincut)
      mincut_path<-paste(path,"mincut_",file_date,".txt")
      write.table(mincut,mincut_path,sep="\t")
    }
    
    if(data_source=="banbury"&run_all=="y"){
      banbury_peak_path<-paste(path,"banbury_peak_",file_date,".txt",sep="")
      write.table(banbury_peak,banbury_peak_path,sep="\t")
      
      save_minpeak<-readline("save minpeak value? y/n      ")
      
      if(save_minpeak=="y"){
        minpeak<-rbind(minpeak,insert_minpeak)
        minpeak_path<-paste(path,"minpeak_",file_date,".txt",sep="")
        write.table(minpeak,minpeak_path,sep="\t")
      }
      
      save_composite<-readline("save to composite data? y/n        ")
      
      if(save_composite=="y"){
        composite_plotdata<-banbury_plotdata
        composite_plotdata$loc<-"banbury"
        composite_plotdata$state<-u
      }
    }#banbury save bracket  
    
    if(data_source=="mill"&run_all=="y"){
      mill_peak_path<-paste(path,"mill_peak_",file_date,".txt",sep="")
      write.table(mill_peak,mill_peak_path,sep="\t")
      
      save_composite<-readline("save to composite data? y/n        ")
      if(save_composite=="y"){
        composite_plotdata<-rbind(composite_plotdata,L2temp_plotdata)
      }
    }#mill save bracket
    
    if(data_source!="banbury"&data_source!="mill"){
      source("c:\\Users\\Clai\\Documents\\Line_2\\batch_plot.R")
    }#other data source bracket
    
    input_complete<-readline("input complete? y/n          ")

}#input complete bracket