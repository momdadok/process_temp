#source("C:\\Users\\Clai\\Documents\\Line_2\\peak.R")
#source("C:\\Users\\Clai\\Documents\\Line_2\\batch_plot.R")

#for banbury/mill:  check the last peak to ensure proper mincut values are used  

View(raw_plotdata[raw_plotdata$peak==max(raw_plotdata$peak)&raw_plotdata$temp>270,])
View(raw_plotdata[raw_plotdata$peak==(max(raw_plotdata$peak)-1)&raw_plotdata$temp>240,])
View(raw_plotdata[raw_plotdata$peak==(max(raw_plotdata$peak)-2)&raw_plotdata$temp>240,])