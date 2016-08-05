print(data_source)
invisible(readline("hmm algorithum (depmixS4) at 2 states will be run.  Press [enter] to continue...."))

states<-2
library("depmixS4")
set.seed(1)
hmm<-depmix(temp~1,data=raw_plotdata,nstates=states,family=gaussian())
hmm_fit<-fit(hmm,verbose=FALSE)
hmm_post<-posterior(hmm_fit)
hmm_state<-cbind(raw_plotdata,hmm_post)

state1_mean<-mean(hmm_state$temp[hmm_state$state==1])
state2_mean<-mean(hmm_state$temp[hmm_state$state==2])

state<-c()
ifelse(state1_mean>state2_mean,state<-1,state<-2)
hmm_maxstate<-hmm_state[hmm_state$state==state,]
hmm_minstate<-hmm_state[hmm_state$state!=state,]
hmm_maxstate$state<-2
hmm_minstate$state<-1
hmm_state<-rbind(hmm_minstate,hmm_maxstate)

library("ggplot2")
hmm_plot<-ggplot(data=hmm_state)+geom_point(aes(x=time-3600*4,y=temp,color=factor(state)),size=0.5)+theme_bw()
hmm_plot<-hmm_plot+theme(axis.text.x=element_text(angle=90))+scale_x_datetime(date_breaks = "10 min",date_labels = "%H:%M")+xlab("time")
hmm_plot<-hmm_plot+guides(colour = guide_legend(override.aes = list(size=3)))
print(hmm_plot)

accept_hmm<-readline("accept hmm results? y/n         ")

while(accept_hmm=="n"){
  states<-as.numeric(readline("input # of states:    "))
  print("hmm running.....")
  set.seed(1)
  hmm<-depmix(temp~1,data=raw_plotdata,nstates=states,family=gaussian())
  hmm_fit<-fit(hmm,verbose=FALSE)
  hmm_post<-posterior(hmm_fit)
  hmm_state<-cbind(raw_plotdata,hmm_post)
  hmm_plot<-ggplot(data=hmm_state)+geom_point(aes(x=time-3600*4,y=temp,color=factor(state)),size=0.5)+theme_bw()
  hmm_plot<-hmm_plot+theme(axis.text.x=element_text(angle=90))+scale_x_datetime(date_breaks = "10 min",date_labels = "%H:%M")+xlab("time")
  hmm_plot<-hmm_plot+guides(colour = guide_legend(override.aes = list(size=3)))
  print(hmm_plot)
  
  library(psych)
  print(describeBy(hmm_state$temp,hmm_state$state))
  
  addl_state<-readline("need additonal states separated via temp? y/n        ")
  while(addl_state=="y"){
    change_state<-as.numeric(readline("which state to change?     "))
    temp_sep<-as.numeric(readline("temperature for break?      "))
    new_state<-max(hmm_state$state)+1
    hmm_state$state[hmm_state$state==change_state&hmm_state$temp>temp_sep]<-new_state
    hmm_plot<-ggplot(data=hmm_state)+geom_point(aes(x=time-3600*4,y=temp,color=factor(state)),size=0.5)+theme_bw()
    hmm_plot<-hmm_plot+theme(axis.text.x=element_text(angle=90))+scale_x_datetime(date_breaks = "10 min",date_labels = "%H:%M")+xlab("time")
    hmm_plot<-hmm_plot+guides(colour = guide_legend(override.aes = list(size=3)))
    print(hmm_plot)
    addl_state<-readline("need additonal states separated via temp? y/n        ")
  }
  
  accept_hmm<-readline("accept hmm results? y/n         ")
}

if(states>2){
  if(exists("actual_hmm_state")==FALSE){
    actual_hmm_state<-c()
  }
  insert_actual_hmm_state<-hmm_state[,1:4]
  insert_actual_hmm_state$loc<-data_source
  
  hmm_hi<-readline("input high states, separated by comma (,):     ") 
  hmm_lo<-readline("input low states, separated by comma (,):      ")
  hmm_hi<-as.numeric(unlist(strsplit(hmm_hi,",")))
  hmm_lo<-as.numeric(unlist(strsplit(hmm_lo,",")))
  hmm_maxstate<-c()
  hmm_minstate<-c()
  for(i in 1:length(hmm_hi)){
    insert_hmm_maxstate<-hmm_state[hmm_state$state==hmm_hi[i],]
    hmm_maxstate<-rbind(hmm_maxstate,insert_hmm_maxstate)
  }
  for(i in 1:length(hmm_lo)){
    insert_hmm_minstate<-hmm_state[hmm_state$state==hmm_lo[i],]
    hmm_minstate<-rbind(hmm_minstate,insert_hmm_minstate)
  }
  
  hmm_maxstate<-hmm_maxstate[order(hmm_maxstate$time),]
  hmm_minstate<-hmm_minstate[order(hmm_minstate$time),]
  
  hmm_maxstate$state<-2
  hmm_minstate$state<-1
  hmm_state<-rbind(hmm_maxstate,hmm_minstate)
  hmm_state<-hmm_state[order(hmm_state$time),]
  hmm_plot<-ggplot(data=hmm_state)+geom_point(aes(x=time-3600*4,y=temp,color=factor(state)),size=0.5)+theme_bw()
  hmm_plot<-hmm_plot+theme(axis.text.x=element_text(angle=90))+scale_x_datetime(date_breaks = "10 min",date_labels = "%H:%M")+xlab("time")
  hmm_plot<-hmm_plot+guides(colour = guide_legend(override.aes = list(size=3)))
  print(hmm_plot)
  
  save_actual_hmm<-readline("save actual hmm states? y/n          ")
  
  if(save_actual_hmm=="y"){
    actual_hmm_state<-rbind(actual_hmm_state,insert_actual_hmm_state)
    actual_hmm_path<-paste(path,"actual_hmm_state_",file_date,".txt",sep="")
    write.table(actual_hmm_state,actual_hmm_path,sep="\t")
  }
}

if(exists("state_used")==FALSE){
  state_used<-data.frame(state=numeric(),loc=character())  
}
insert_state_used<-data.frame(state=states,loc=data_source)
state_used<-rbind(state_used,insert_state_used)
state_path<-paste(path,"state_used_",file_date,".txt",sep="")
write.table(state_used,state_path,sep="\t")

use_hmm<-readline("use HMM or peak separation? hmm/peak            ")
if(use_hmm=="hmm"){  
  start_time<-hmm_maxstate$time[1]
}

if(use_hmm=="peak"){
  start_time<-raw_plotdata[batch$peaks$index[1],1]
}

L2temp_plotdata<-hmm_state[match(start_time,raw_plotdata$time):dim(raw_plotdata)[1],1:4]
L2temp_plotdata<-L2temp_plotdata[order(L2temp_plotdata$time),]

short_banbury_plotdata<-banbury_plotdata[1:dim(L2temp_plotdata)[1],]

L2temp_plotdata$batch<-short_banbury_plotdata$batch
L2temp_plotdata$batch_time<-short_banbury_plotdata$batch_time
L2temp_plotdata$loc<-data_source

batch_time_0<-na.omit(L2temp_plotdata[L2temp_plotdata$batch_time==0,])
View(batch_time_0)

invisible(readline("check View window for data separation.  Press [Enter] to continue..."))

calc_temp_stats<-readline("calculate temperature stats?  y/n        ")

if(calc_temp_stats=="y"){
  individual_median_temp<-data.frame(loc=data_source,t(quantile(hmm_maxstate$temp)),mean=mean(hmm_maxstate$temp),stdev=sd(hmm_maxstate$temp))
  colnames(individual_median_temp)[2:6]<-c("min","1qr","median","3qr","max")
  median_temp<-rbind(median_temp,individual_median_temp)
  View(median_temp)
}

composite_plotdata$loc<-factor(composite_plotdata$loc, levels=c("banbury","mill","scratch_in","sheeter_out","cal#1in","cal#1out","cal#2in","cal#2out"))

save_composite<-readline("save to composite data? y/n          ")

if(save_composite=="y"){
  composite_plotdata<-rbind(composite_plotdata,L2temp_plotdata)
  composite_plotdata<-na.omit(composite_plotdata)
  composite_path<-paste(path,"composite_data_",file_date,".txt",sep="")
  write.table(composite_plotdata,composite_path,sep = "\t")
}

print_plot<-readline("print composite and time plot? y/n        ")

if(print_plot=="y"){
  library("ggplot2")
    source("c:/Users/Clai/Documents/Line_2/print_plot.R")
  } #print plot if bracket

save_median<-readline("save median values in tab-delimited *.txt file? y/n          ")
if(save_median=="y"){
  median_temp$date<-as.POSIXct(date)
  station_temp<-median_temp
  median_path<-paste(path,"station_temp_",file_date,".txt",sep="")
  write.table(station_temp,median_path,sep="\t")
  print("remember to add date to file names before starting the next run")
}
