counter<-2
L2temp_plotdata$startup<-1

for(i in 2:dim(banbury_peak)[1]){
  if(banbury_peak$cycle_time[i]>210){
    index_val<-match(banbury_peak$time[i],L2temp_plotdata$time)
    L2temp_plotdata$startup[index_val:dim(L2temp_plotdata)[1]]<-counter
    counter<-counter+1
  }
}