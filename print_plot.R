invisible(readline("make sure composite_plotdata & [startup_stats] are assigned.  Press [Enter] to continue..."))

library("ggplot2")

min_limit<-data.frame(startup_stats$group1,startup_stats$group2,startup_stats$min*.95)
max_limit<-data.frame(startup_stats$group1,startup_stats$group2,startup_stats$max*1.05)
colnames(min_limit)<-c("group1","group2","facet_limit")
colnames(max_limit)<-c("group1","group2","facet_limit")
facet_limit<-rbind(min_limit,max_limit)

composite_plotdata$loc<-factor(composite_plotdata$loc, levels=c("banbury","mill","scratch_in","sheeter_out","cal#1in","cal#1out","cal#2in","cal#2out"))

composite_plot<-ggplot(data=composite_plotdata[composite_plotdata$batch_time<200,],)+geom_line(aes(x=batch_time,y=temp,color=loc))
composite_plot<-composite_plot+facet_wrap(~batch)+theme_bw()+theme(axis.text.x=element_text(angle=90))
composite_plot<-composite_plot+scale_color_discrete(breaks=c("banbury","mill","scratch_in","sheeter_out","cal#1in","cal#1out","cal#2in","cal#2out"))
composite_plot<-composite_plot+scale_y_continuous(limit=c(140,320))
print(composite_plot)

min_limit$time<-min(composite_plotdata[composite_plotdata$state==2,1]-5*60)
max_limit$time<-max(composite_plotdata[composite_plotdata$state==2,1]+5*60)
facet_limit<-rbind(min_limit,max_limit)

colnames(facet_limit)<-c("loc","startup","temp","time")

time_plot<-ggplot()+geom_point(data=composite_plotdata[composite_plotdata$state==2,],aes(x=time-3600*4,y=temp,color=loc))
time_plot<-time_plot+theme_bw()+theme(axis.text.x=element_text(angle=90,size=15),
                                      axis.text.y=element_text(size=15),axis.title=element_text(size=15,face="bold"),
                                      strip.text.y=element_text(size=15),legend.text=element_text(size=15),
                                      legend.title=element_text(size=15,face="bold"))
time_plot<-time_plot+scale_x_datetime(date_breaks="10 min", date_labels="%H:%M")+xlab("time")+facet_grid(loc~.,scale="free_y")
time_plot<-time_plot+scale_color_discrete(breaks=c("banbury","mill","scratch_in","sheeter_out","cal#1in","cal#1out","cal#2in","cal#2out"))
time_plot<-time_plot+guides(color=guide_legend(override.aes=list(size=5)))
time_plot<-time_plot+geom_blank(data=facet_limit,aes(x=time-3600*4,y=temp))
print(time_plot)

colnames(facet_limit)<-c("group1","group2","lim","time")

if(input_startup_stats=="y"){
  startup_stats_plot<-ggplot()+geom_crossbar(data=startup_stats,aes(x=group2,y=median,ymin=min,ymax=max,color=group1),width=.1)
  startup_stats_plot<-startup_stats_plot+geom_line(data=startup_stats,aes(x=group2,y=median,color=group1))
  startup_stats_plot<-startup_stats_plot+theme_bw()
  startup_stats_plot<-startup_stats_plot+theme(axis.text.x=element_text(size=15),
                                               axis.title=element_text(size=15,face="bold"),
                                               strip.text.y=element_text(size=15),
                                               legend.text=element_text(size=15),
                                               legend.title=element_text(size=15,face="bold"))
  startup_stats_plot<-startup_stats_plot+xlab("startup")+scale_x_continuous(breaks=c(1,2,3,4,5,6))
  startup_stats_plot<-startup_stats_plot+guides(color=guide_legend(title="loc",override.aes=list(size=1)))
  startup_stats_plot<-startup_stats_plot+facet_grid(group1~.,scales="free_y")
  startup_stats_plot<-startup_stats_plot+geom_blank(data=facet_limit,aes(x=group2,y=lim))
  startup_stats_plot<-startup_stats_plot+geom_text(data=startup_stats,aes(x=group2,y=median,label=round(median))
                                                   ,vjust=.4,hjust=.4,check_overlap = TRUE,angle=20)
  print(startup_stats_plot)
  
}

