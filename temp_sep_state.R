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

if(addl_state=="y"){
  states<-new_state 
} 
 
if(exists("actual_hmm_state")==FALSE){
  actual_hmm_state<-c()
}
insert_actual_hmm_state<-hmm_state[,1:4]
insert_actual_hmm_state$loc<-data_source

save_actual_hmm<-readline("save actual hmm states? y/n          ")

if(save_actual_hmm=="y"){
  actual_hmm_state<-rbind(actual_hmm_state,insert_actual_hmm_state)
  actual_hmm_path<-paste(path,"actual_hmm_state_",file_date,".txt",sep="")
  write.table(actual_hmm_state,actual_hmm_path,sep="\t")
}