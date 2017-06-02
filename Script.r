library(plyr)

editorial.methods=c('none','revision','tiebreak','overrule','match.quality','blacklist.50','blacklist.90','blacklist.100') 

N=1000																				## Total number of scientists
sigma.author=10																		## std dev of distribution of proficiency among authors
sigma.quality=5																		## std dev of dbn of quality of submitted ms by a single author
Q.author=rnorm(N,mean=100,sd=sigma.author)											## Proficiency is normally distributed
Qmin.submit=90																		## Min quality accepted by indifferent selfish refs
Q.min.fixed.standards=qnorm(.9,mean=100,sd=sqrt(sigma.author^2+sigma.quality^2))	## Min quality accepted by fixed-standard disinterested refs
lambda=.1																			## Controls ``stickiness'' of the moving standard
steps=500																			## Total number of review cycles

## --------------------- Functions ---------------------------------------------------

## Referee decisison on a manuscript given the referee's nature and the quality of the manuscript.
## Inputs: ref.type --> list of referee types across the pool, ref --> index of the ref in question
## q.sub --> quality of submitted manuscript, q.min --> current min standard of moving-std refs
## Output: 0 if rejection, 1 if acceptance
Decision=function(ref,q.sub,q.min,ref.type){
	if(ref.type[ref]=='moving.standards') return(1*(q.sub>=q.min))
	if(ref.type[ref]=='indifferent') return(1*(q.sub>=Qmin.submit & q.sub<=Q.author[ref]))
	if(ref.type[ref]=='random') return(round(runif(1)))
	if(ref.type[ref]=='fixed.standards') return(1*(q.sub>=Q.min.fixed.standards))
	if(ref.type[ref]=='conscientious') return(1*(q.sub>=Q.min.fixed.standards & q.sub<=Q.author[ref]))
}

## Simulates the peer review process.
## Inputs: fr, fc, fr, frnd, ff, fd --> proportion in the ref pool of indiff. selfish, moving-std disint., random, fixed-std disint., conscientious selfish refs, respectively
## run --> controls the random number generator, method --> editorial method (string with one of the values listed in editorial.methods, except for blacklisting.)
## Output: list with: 
## data --> data frame showing avg quality of accepted papers per round, avg quality of submitted papers per round, percentage of papers rejected in each round.
## A.list, R.list --> list of the quality of accepted and rejected papers, respectively, across all rounds.
PeerReview=function(fr,fc=1-fr,frnd=0,ff=0,fd=0,run=0,method='none'){
	stopifnot(method%in%editorial.methods)
	if(fc+fr+frnd+ff+fd>1) stop('Ref types add to > 1')
	set.seed(run)	
	ref.type=sample(c(	rep('moving.standards',round(fc*N)),
						rep('indifferent',round(fr*N)),
						rep('random',round(frnd*N)),
						rep('fixed.standards',round(ff*N)),
						rep('conscientious',round(fd*N))
	))

	if(method=='overrule') Qmin.submit=101
		
	simtime=0
	Q.accept.avg=Q.submit.avg=Rejected=A.list=R.list=NULL
	Q.min=M=Q.accept=0
	while(simtime<steps){
		simtime=simtime+1
		if(simtime==1) authors=sample(N,size=N/2) else authors=setdiff(seq(N),authors)
		Q.submitted=rnorm(length(authors),mean=sample(Q.author[authors]),sd=sigma.quality)
		if(method=='match.quality'){ 
			refs=sapply(seq_along(authors),function(j) sample(seq(N)[-authors[j]],size=3,prob=dnorm(Q.author[-authors[j]]-Q.submitted[j],mean=5,sd=5)))
		}else refs=sapply(authors,function(author) sample(seq(N)[-author],size=3))
		accepted=sapply(seq(ncol(refs)),function(j){
			ref1=as.integer(refs[1,j]); ref2=as.integer(refs[2,j]); ref3=as.integer(refs[3,j])
			q.sub=as.numeric(Q.submitted[j])
			recommendation1=Decision(ref1,q.sub,Q.min,ref.type); recommendation2=Decision(ref2,q.sub,Q.min,ref.type)
			prob.accept=mean(c(recommendation1,recommendation2))
			if(method=='tiebreak' & prob.accept==.5) prob.accept=Decision(ref3,q.sub,Q.min,ref.type)
			if(method=='revision' & prob.accept<1){
				q.sub=q.sub+abs(rnorm(1,mean=0,sd=sigma.quality))
				recommendation1=Decision(ref1,q.sub,Q.min,ref.type); recommendation2=Decision(ref2,q.sub,Q.min,ref.type)
				prob.accept=mean(recommendation1,recommendation2)
			}
			return(sample(c(1,0),1,prob=c(prob.accept,1-prob.accept)))
		})
		if(method=='overrule') accepted[Q.submitted>quantile(Q.accept,.9)]=1
		M=lambda*M+(1-lambda)*mean(Q.accept)
		Q.min=M
		if(any(accepted==1)){ 
			Q.accept=Q.submitted[accepted==1]
			if(simtime>=.2*steps) A.list=c(A.list,Q.accept)			## Lists the quality of every accepted paper
		}
		if(any(accepted==0)){ 
			Q.reject=Q.submitted[accepted==0]
			if(simtime>=.2*steps) R.list=c(R.list,Q.reject)			## Lists the quality of every rejected paper
		}
		Q.accept.avg=c(Q.accept.avg,mean(Q.accept))					## Average quality of accepted manuscripts in the round
		Q.submit.avg=c(Q.submit.avg,mean(Q.submitted))				## Average quality of submitted manuscripts in the round
		Rejected=c(Rejected,sum(accepted==0)/length(authors))		## Percentage of total submitted manuscripts that were rejected in the round
		
	}
	return(list(
		data=data.frame(Q.accept.avg=Q.accept.avg,Q.submit.avg=Q.submit.avg,Rejected=Rejected),
		A.list=A.list,
		R.list=R.list
	))
}

## Simulates the peer review process when the editorial method is blacklisting. Assumes pool consists only of moving-std disinterested refs and indifferent selfish refs. 
## Inputs: fr, fc --> proportion of indifferent selfish refs and moving-std disinterested refs, respectively. 
## p0 --> desired probability threshold for blacklisting
## career --> number of years a referee remains in the pool
## run --> controls random number generator
## Output: similar to PeerReview, plus the record showing the number of times that each active scientist served as a referee.
PeerReviewBlacklist=function(fr,fc,p0,career,run=0,Qa=NULL){
	time.factor=2				## Converts time from years to review cycles. Eg time.factor = 2 means 2 review cycles per year
	career=career*time.factor	## This converts the time in years which is fed to the function to the time in review cycles.
	
	stopifnot(fc+fr==1)
	
	if(is.null(Qa)) Qa=c(129,114,111,109,107,105,104,103,101,100)[match(fr,c(.001,1:9/10))]	## Those values were numerically obtained by running PeerReview(fr) 
	
	set.seed(run)	
	ref.type=sample(c(rep('moving.standards',round(fc*N)),rep('indifferent',round(fr*N))))

	simtime=0; age=sample(rep(seq(career),each=N/career)); age=c(age,sample(career,N-length(age)))
	Q.accept.avg=Q.submit.avg=Rejected=A.list=R.list=NULL
	Q.min=M=Q.accept=0
	
	tally=data.frame(ref=seq(N),n=0,k=0)			## keeps score of the number of reviews (n) and disagreements (k)
	mat=Blacklist(fc=fc,fr=fr,Qa=Qa)				## lists the probs that a ref is indifferent based on n and k
	
	blacklist=integer(0)
	reftrack=refrecord=NULL
	while(simtime<steps){
		simtime=simtime+1
		
		## submission stage
		if(simtime==1) authors=sample(N,size=N/2) else authors=setdiff(seq(N),authors)
		Q.submitted=rnorm(length(authors),mean=sample(Q.author[authors]),sd=sigma.quality)
		
		## choosing refs
		refs=sapply(authors,function(author) sample(seq(N)[-author],size=2,prob=ifelse(seq(N)[-author]%in%blacklist,0,1)))
		reftrack=c(reftrack,as.numeric(refs))
		
		## reviewing process
		reviews=sapply(seq(ncol(refs)),function(j){
			ref1=as.integer(refs[1,j]); ref2=as.integer(refs[2,j])
			q.sub=as.numeric(Q.submitted[j])
			recommendation1=Decision(ref1,q.sub,Q.min,ref.type); recommendation2=Decision(ref2,q.sub,Q.min,ref.type)
			prob.accept=mean(c(recommendation1,recommendation2))
			return(prob.accept)
		}) 
		
		## accepting and rejecting submitted papers 
		accepted=sapply(reviews,function(p) sample(c(1,0),1,prob=c(p,1-p)))
		M=lambda*M+(1-lambda)*mean(Q.accept)
		Q.min=M
		
		if(any(accepted==1)){ 
			Q.accept=Q.submitted[accepted==1]
			if(simtime>=.2*steps) A.list=c(A.list,Q.accept)
		}
		if(any(accepted==0)){ 
			Q.reject=Q.submitted[accepted==0]
			if(simtime>=.2*steps) R.list=c(R.list,Q.reject)
		}		
		Q.accept.avg=c(Q.accept.avg,mean(Q.accept))
		Q.submit.avg=c(Q.submit.avg,mean(Q.submitted))
		Rejected=c(Rejected,sum(accepted==0)/length(authors))
		
		## blacklisting refs 
		disagreements=sapply(reviews,function(x) 1*(x==.5))
		res=ddply(data.frame(ref=c(refs[1,],refs[2,]),n=1,k=rep(disagreements,2)),.(ref),function(v) c(n=sum(v$n),k=sum(v$k)))
		ind=tally$ref%in%res$ref; tally$n[ind]=tally$n[ind]+res$n; tally$k[ind]=tally$k[ind]+res$k
		tmp=subset(merge(tally,mat,all.x=TRUE),!is.na(pR))
		blacklist=with(tmp,ref[pR>=p0])
		
		## scholar turnover 
		age=age+1
		Q.author[age>career]=rnorm(sum(age>career),mean=100,sd=sigma.author)
		blacklist=setdiff(blacklist,seq(N)[age>career])
		tally$n[age>career]=0; tally$k[age>career]=0
		ref.type[age>career]=sample(c('moving.standards','indifferent'),size=sum(age>career),replace=TRUE,prob=c(fc,fr))
		retiring=which(age>career); refrecord=c(refrecord,sapply(retiring,function(x) sum(reftrack==x)))
		reftrack=setdiff(reftrack,retiring)
		age[age>career]=1		
	}
	refrecord=c(refrecord,plyr::count(reftrack)$freq)
	
	return(list(
		data=data.frame(Q.accept.avg=Q.accept.avg,Q.submit.avg=Q.submit.avg,Rejected=Rejected),
		A.list=A.list,
		R.list=R.list,
		ref.record=refrecord
	))
}

## Receives as input the percentage of each type of referee, plus the minimum standard of fixed-standard referees.
## Returns a data frame with columns n (number of papers reviewed), k (number of disagreements), and pR (probability of being a selfish referee given n and k)
## See Suplementary Information for the model
Blacklist=function(fc,fr,Qa){
	PA=function(b) dnorm(b,mean=100,sd=sigma.author) 
	PP=function(x) dnorm(x,mean=100,sd=sqrt(sigma.author^2+sigma.quality^2))
	
	I0=Vectorize(function(b) PA(b)*integrate(PP,lower=Qa,upper=b)$value)
	qrc=1-integrate(PA,lower=-Inf,upper=ifelse(fr>0,Qmin.submit,Q.min.fixed.standards))$value-integrate(I0,lower=Qa,upper=Inf)$value
	
	I2=Vectorize(function(b2,b1) PA(b2)*integrate(PP,lower=b1,upper=b2)$value)
	I1=Vectorize(function(b1) PA(b1)*integrate(I2,lower=b1,upper=Inf,b1=b1)$value)
	qrr=integrate(I1,lower=ifelse(fr>0,Qmin.submit,Q.min.fixed.standards),upper=Inf)$value
	
	qc=fr*qrc; qr=fr*qrr+fc*qrc
	res=ddply(merge(data.frame(n=1:50),data.frame(k=0:50)),.(n,k),function(v){
		n=v$n; k=v$k
		if(n>=k){ 
			PkRn=qr^k*(1-qr)^(n-k)
			PkCn=qc^k*(1-qc)^(n-k)
			return(1/(1+PkCn/PkRn*fc/fr))
		} else return(NA)
	}); names(res)[3]='pR'
	return(res)
}
