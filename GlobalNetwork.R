

model0<-read.csv('model0.csv',stringsAsFactors=F,header=T,sep=' ')
#model0 contains the numbers of S, E, I, R for each country, initially all in S
world13pAgg3<-read.csv('world13pAgg3.csv',sep=' ',stringsAsFactors=F)
#world13pAgg3 details the number of average daily passengers that travel between each pair of countries worldwide 

g<-1/5.61 #deaths
sy<-1/5.3 # E -> I
b<- .21 # contact


for(h in 1:50){
  model_t<-model0
  
  #WHO situation report 3 Dec
  model_t[model_t$country=='Guinea',]$I <- 102
  model_t[model_t$country=='Guinea',]$E <- 90 #calibrated
  model_t[model_t$country=='Sierra Leone',]$I <- 93
  model_t[model_t$country=='Sierra Leone',]$E <- 100
  model_t[model_t$country=='Liberia',]$I <- 485
  model_t[model_t$country=='Liberia',]$E <- 494
  
  cat('',file=paste('australia1/run',as.character(h),'.txt',sep=''))

  
  i<-0
  for(i in 1:211){
    i<- i +1
    #first step - outbreak progresses
    for (j in 1:251){
      if(model_t[j,]$I == 0 & model_t[j,]$E == 0){next}
      #print(j)
      t1<-max(min(rpois(1,b*model_t[j,]$S * model_t[j,]$I / (model_t[j,]$S+model_t[j,]$I+model_t[j,]$E+0.5*model_t[j,]$R+1) ),model_t[j,]$S),0) #S -> E
      t2<-max(min(rpois(1,model_t[j,]$E * sy),model_t[j,]$E),0) # E -> I
      t3<-max(min(rpois(1,model_t[j,]$I * g),model_t[j,]$I),0) # I -> R
      #print(t1)
      #print(t2)
      #print(t3)
      if(is.na(t1)|is.na(t2)|is.na(t3)){print(j)}
      if(is.na(t1)|is.na(t2)|is.na(t3)){print(model_t[j,])}
      model_t[j,]$S <- model_t[j,]$S - t1
      model_t[j,]$E <- model_t[j,]$E + t1 - t2
      model_t[j,]$I <- model_t[j,]$I + t2 - t3
      model_t[j,]$R <- model_t[j,]$R + t3
      if(is.na(model_t[j,]$E)){stop}
      
    }
    #second step - susceptible or exposed individuals fly
    for (j in 1:251){
      if(model_t[j,]$E == 0){next}
      for(k in 1:251){
        if (j==k){
          next
        }
        spd<-world13pAgg3[world13pAgg3$depCountry==model_t[j,]$iso2 & world13pAgg3$arrCountry==model_t[k,]$iso2,6]
        if(length(spd)>0){
          nMvE<-rbinom(1,round(spd),min(model_t[j,]$E/(model_t[j,]$S+model_t[j,]$E+1),1))
          if(nMvE > model_t[j,]$E){
            print('too many flying out, abort')
            print(model_t[j,])
            print(spd)
            print(nMvE)
            next
          }
          if(is.na(nMvE)){
            print(j)
            print(model_t[j,])
            print(spd)
          }
          model_t[j,]$E <- model_t[j,]$E - nMvE
          model_t[k,]$E <- model_t[k,]$E + nMvE
        }
      }
    }
    tm<-model_t[model_t$E>0 | model_t$I > 0 | model_t$R > 0,]
    write.table(tm,paste('run1/world',as.character(h),'-',as.character(i),'.txt',sep=''))
    cat(c(i,model_t[14,]$S,model_t[14,]$E,model_t[14,]$I,model_t[14,]$R,'\n'),file=paste('australia1/run',as.character(h),'.txt',sep=''),append=T)
  }
}