invisible(readline("make sure composite_plot is assigned.  Press [Enter] to continue..."))

library("ggplot2")
  
composite_plot<-ggplot(data=composite_plotdata[composite_plotdata$batch_time<200,],)+geom_line(aes(x=batch_time,y=temp,color=loc))
composite_plot<-composite_plot+facet_wrap(~batch)+theme_bw()+theme(axis.text.x=element_text(angle=90))
composite_plot<-composite_plot+scale_y_continuous(limits=c(150,NA))
composite_plot<-composite_plot+scale_color_discrete(breaks=c("banbury","mill","scratcher_in","sheeter_out","cal#1_in","cal#1_out","cal#2_in","cal#2_out"))
print(composite_plot)

time_plot<-ggplot()+geom_point(data=composite_plotdata[composite_plotdata$state==2,],aes(x=time-3600*4,y=temp,color=loc))
time_plot<-time_plot+theme_bw()+theme(axis.text.x=element_text(angle=90,size=15),
                                      axis.text.y=element_text(size=15),axis.title=element_text(size=15,face="bold"),
                                      strip.text.y=element_text(size=15),legend.text=element_text(size=15),
                                      legend.title=element_text(size=15,face="bold"))
time_plot<-time_plot+scale_x_datetime(date_breaks="10 min", date_labels="%H:%M")+xlab("time")+facet_grid(loc~.,scale="free_y")
time_plot<-time_plot+scale_color_discrete(breaks=c("banbury","mill","scratch_in","sheeter_out","cal#1in","cal#1out","cal#2in","cal#2out"))
time_plot<-time_plot+guides(color=guide_legend(override.aes=list(size=5)))
print(time_plot)
