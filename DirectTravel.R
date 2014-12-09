
data<-read.csv('daily-rates.csv')
# csv file that contains average daily numbers of passengers travelling to Australia from relevant countries; not actually included

ppdL<-data[data$county=='Liberia',]$ppd
ppdS<-data[data$county=='Sierra Leone',]$ppd
ppdG<-data[data$county=='Guniea',]$ppd

g<-1/5.61 #deaths
sy<-1/5.3 # E -> I
b<- .21 # contact rate

#store the cumulative probability of not having seen a case by day j; 1000 runs
tcp<-matrix(nrow=211,ncol=1000) #total
lcp<-matrix(nrow=211,ncol=1000) #Liberia
scp<-matrix(nrow=211,ncol=1000) #Sierra Leone
gcp<-matrix(nrow=211,ncol=1000) #Guinea

for(k in 1:1000){
  ldf<-data.frame(t=0:211,S=rep(0,212),E=rep(0,212),I=rep(0,212),R=rep(0,212),pThisN=rep(0,212),pCumulativeN=rep(0,212))
  ldf[1,]<-c(0,3685076,90,102,0,1,1) #initial conditions
  for(j in 1:211){
    t1<-max(min(rpois(1,b*ldf[j,]$S * ldf[j,]$I / (ldf[j,]$S+ldf[j,]$I+ldf[j,]$E+0.5*ldf[j,]$R+1) ),ldf[j,]$S),0) #S -> E
    t2<-max(min(rpois(1,ldf[j,]$E * sy),ldf[j,]$E),0) # E -> I
    t3<-max(min(rpois(1,ldf[j,]$I * g),ldf[j,]$I),0) # I -> R
    if(is.na(t1)|is.na(t2)|is.na(t3)){print(j)}
    if(is.na(t1)|is.na(t2)|is.na(t3)){print(ldf[j,])}
    ldf[j+1,]$S <- ldf[j,]$S - t1
    ldf[j+1,]$E <- ldf[j,]$E + t1 - t2
    ldf[j+1,]$I <- ldf[j,]$I + t2 - t3
    ldf[j+1,]$R <- ldf[j,]$R + t3
    ldf[j+1,]$pThisN <- ( ldf[j+1,]$S / (ldf[j+1,]$S + ldf[j+1,]$E) )^ppdL
    ldf[j+1,]$pCumulativeN <- ldf[j+1,]$pThisN*ldf[j,]$pCumulativeN
    lcp[j,k]<-ldf[j+1,]$pCumulativeN
  }
  
  ## Sierra Leone
  
  sdf<-data.frame(t=0:211,S=rep(0,212),E=rep(0,212),I=rep(0,212),R=rep(0,212),pThisN=rep(0,212),pCumulativeN=rep(0,212))
  sdf[1,]<-c(0,5245695,494,485,0,1,1)
  for(j in 1:211){
    t1<-max(min(rpois(1,b*sdf[j,]$S * sdf[j,]$I / (sdf[j,]$S+sdf[j,]$I+sdf[j,]$E+0.5*sdf[j,]$R+1) ),sdf[j,]$S),0) #S -> E
    t2<-max(min(rpois(1,sdf[j,]$E * sy),sdf[j,]$E),0) # E -> I
    t3<-max(min(rpois(1,sdf[j,]$I * g),sdf[j,]$I),0) # I -> R
    if(is.na(t1)|is.na(t2)|is.na(t3)){print(j)}
    if(is.na(t1)|is.na(t2)|is.na(t3)){print(sdf[j,])}
    sdf[j+1,]$S <- sdf[j,]$S - t1
    sdf[j+1,]$E <- sdf[j,]$E + t1 - t2
    sdf[j+1,]$I <- sdf[j,]$I + t2 - t3
    sdf[j+1,]$R <- sdf[j,]$R + t3
    sdf[j+1,]$pThisN <- ( sdf[j+1,]$S / (sdf[j+1,]$S + sdf[j+1,]$E) )^ppdL
    sdf[j+1,]$pCumulativeN <- sdf[j+1,]$pThisN*sdf[j,]$pCumulativeN
    scp[j,k]<-sdf[j+1,]$pCumulativeN
  }
  
  #Guinea
  
  gdf<-data.frame(t=0:211,S=rep(0,212),E=rep(0,212),I=rep(0,212),R=rep(0,212),pThisN=rep(0,212),pCumulativeN=rep(0,212))
  gdf[1,]<-c(0,10324025,100,93,0,1,1)
  for(j in 1:211){
    t1<-max(min(rpois(1,b*gdf[j,]$S * gdf[j,]$I / (gdf[j,]$S+gdf[j,]$I+gdf[j,]$E+0.5*gdf[j,]$R+1) ),gdf[j,]$S),0) #S -> E
    t2<-max(min(rpois(1,gdf[j,]$E * sy),gdf[j,]$E),0) # E -> I
    t3<-max(min(rpois(1,gdf[j,]$I * g),gdf[j,]$I),0) # I -> R
    if(is.na(t1)|is.na(t2)|is.na(t3)){print(j)}
    if(is.na(t1)|is.na(t2)|is.na(t3)){print(gdf[j,])}
    gdf[j+1,]$S <- gdf[j,]$S - t1
    gdf[j+1,]$E <- gdf[j,]$E + t1 - t2
    gdf[j+1,]$I <- gdf[j,]$I + t2 - t3
    gdf[j+1,]$R <- gdf[j,]$R + t3
    gdf[j+1,]$pThisN <- ( gdf[j+1,]$S / (gdf[j+1,]$S + gdf[j+1,]$E) )^ppdG
    gdf[j+1,]$pCumulativeN <- gdf[j+1,]$pThisN*gdf[j,]$pCumulativeN
    gcp[j,k]<-gdf[j+1,]$pCumulativeN
  }
}


for(i in 1:211){
  for (j in 1:1000){
    tcp[i,j]<-lcp[i,j]*scp[i,j]*gcp[i,j]
  }
}

#store the quantiles at each timestep
tq<-matrix(nrow=211,ncol=3)
lq<-matrix(nrow=211,ncol=3)
sq<-matrix(nrow=211,ncol=3)
gq<-matrix(nrow=211,ncol=3)

for(i in 1:211){
  tq[i,]<-quantile(tcp[i,],c(.025,.5,.975))
  lq[i,]<-quantile(lcp[i,],c(.025,.5,.975))
  sq[i,]<-quantile(scp[i,],c(.025,.5,.975))
  gq[i,]<-quantile(gcp[i,],c(.025,.5,.975))
}

#plot
pdf(file='cumulative-prob-1.pdf', width=12, height=8) 
plot(1:211,1-lq3[,2],type='l',col='red',lwd=3,ylim=c(0,0.5),xlab='days',ylab='Cumulative probability',
     panel.first={
       axis(1, tck=1, col.ticks="light gray")
       axis(1, tck=-0.015, col.ticks="black")
       axis(2, tck=1, col.ticks="light gray", lwd.ticks="1")
       axis(2, tck=-0.015)
       #minor.tick(nx=5, ny=2, tick.ratio=0.5)
       box()
     })
points(1:211,1-sq[,2],type='l',col='orange',lwd=3)
points(1:211,1-gq[,2],type='l',col='maroon',lwd=3)
points(1:211,1-tq[,2],type='l',lwd=3)

points(1:211,1-lq[,1],type='l',lty=2,col='red',lwd=2)
points(1:211,1-sq[,1],type='l',lty=2,col='orange',lwd=2)
points(1:211,1-gq[,1],type='l',lty=2,col='maroon',lwd=2)
points(1:211,1-tq[,1],type='l',lty=2,lwd=2)

points(1:211,1-lq[,3],type='l',lty=2,col='red',lwd=2)
points(1:211,1-sq[,3],type='l',lty=2,col='orange',lwd=2)
points(1:211,1-gq[,3],type='l',lty=2,col='maroon',lwd=2)
points(1:211,1-tq[,3],type='l',lty=2,lwd=2)
legend('topleft',col=c('black','red','orange','maroon'),legend=c('Combined','Liberia','Sierra Leone','Guinea'),lwd=2,lty=1)
dev.off() 