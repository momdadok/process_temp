library("depmixS4")
set.seed(1)
hmm<-depmix(temp~1,data=raw_plotdata,nstates=states,family=gaussian())
hmm_fit<-fit(hmm,verbose=FALSE)
hmm_post<-posterior(hmm_fit)
hmm_state<-cbind(raw_plotdata,hmm_post)

