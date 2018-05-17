data1=subset(fgl,type=="WinF"|type=="WinNF"|type=="Head")
data1
for(i in 1:9){          
  mean<- mean(data1[,i])
  sd<- sd(data1[,i])
  for(k in 1:nrow(data1)){
    data1[k,i]= (data1[k,i]-mean)/sd
    
  }

}

#part B
par(mfrow=c(2,2))
d=dist(data1, method = 'euclidean')
hc<- hclust(d,method='single')
gr=as.factor((as.character(data1$type)))
gr1=as.integer(gr)             
plot(hc,gr1)

hc2<- hclust(d,method='complete')
plot(hc2,gr1)

hc3<- hclust(d,method='average')
plot(hc3,gr1)
#in general the dendograms tend cluster in the single agglomderation mehtod


#part d

f0=function(x){
  x[1]*x[2]*(1-x[3])
  +x[1]*x[3]*(1-x[2])
  +x[2]*x[3]*(1-x[1])
  +x[1]*x[2]*x[3]
}

n=dim(data1)[]


par(mfrow=c(1,1))
l2 = NULL
for (i in 1:10) {
  mm1 = table(gr,cutree(hc,k=i))
  mmp=apply(mm1,1,function(x){x/sum(x)})
  mmp=matrix(mmp,ncol=3)
  mmp=t(mmp)
  pp1 = 1-sum(apply(mmp,2,f0))
 
  mm2 = table(gr,cutree(hc2,k=i))
  mmp=apply(mm2,1,function(x){x/sum(x)})
  mmp=matrix(mmp,ncol=3)
  mmp=t(mmp)
  pp2 = 1-sum(apply(mmp,2,f0))  
  
  mm3 = table(gr,cutree(hc3,k=i))
  mmp=apply(mm3,1,function(x){x/sum(x)})
  mmp=matrix(mmp,ncol=3)
  mmp=t(mmp)
  pp3 = 1-sum(apply(mmp,2,f0))
  
  l2 = rbind(l2, c(pp1,pp2,pp3))
}
matplot(l2,type='l',xlab='cluster size k', ylab='alpha[k]',lwd=2)
legend('topright',legend=c('single','complete','average'),pch=NA,lty=1:3,col=1:3)
## in terms of accuracy,The single mthod is the worst. Complete is the best,
#part e

l2.perm.list=list()
method.names=c('single','complete','average')
hfit.list=list(hc,hc2,hc3)

set.seed(12345)
nperm=25

for(iii in 1:3){
  l2.perm=matrix(NA,10,nperm)
  for(jjj in 1:nperm){
    grp=sample(gr)
    l2v=NULL
    for(i in 1:10){
      mm = table(grp,cutree(hfit.list[[iii]],k=i))
      mmp=apply(mm,1,function(x){x/sum(x)})
      mmp=matrix(mmp,ncol=3)
      mmp=t(mmp)
      pp = 1-sum(apply(mmp,2,f0))
     l2v=c(l2v,pp)
    }
    l2.perm[,jjj]=l2v
  }
  l2.perm.list[[iii]]=l2.perm
}
par(mfrow=c(2,2))
for(iii in 1:3){
  
  matplot(1:10,l2.perm.list[[iii]],col='gray',lty=1,pch=NULL,type='l',xlab='cluster size k', ylab='alpha[k]',ylim=c(0,1),main=method.names[iii])
  lines(1:10,l2[,iii],col='green',type='b') 
 legend('topleft',legend=c('original','permuted'),lty=1,col=c('green','gray')) 
}

#Complete and single are the most compatible with the actual glass types,
