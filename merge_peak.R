merge_peak_input<-readline("merge peaks?  y/n       ")

merge_peak_min<-0

while(merge_peak_input=="y"){
  if(merge_peak_min==0){
    peak_combine<-readline("input peaks to be merged starting from the smallest to largest, 
separated by comma (,):           ")
    peak_combine<-as.numeric(unlist(strsplit(peak_combine,",")))
    merge_peak_min<-min(peak_combine)
    for (i in 1:length(peak_combine))
    raw_plotdata$peak[raw_plotdata$peak==peak_combine[i]]<-merge_peak_min
  }
    else{
      peak_combine<-readline("input peaks to be merged starting from the smallest to largest, 
separated by comma (,):                 ")
      peak_combine<-as.numeric(unlist(strsplit(peak_combine,",")))
      merge_peak_min<-merge_peak_min+1
      for (i in 1:length(peak_combine))
        raw_plotdata$peak[raw_plotdata$peak==peak_combine[i]]<-merge_peak_min
    }
  
  merge_peak_input<-readline("merge more peaks? y/n        ")
}