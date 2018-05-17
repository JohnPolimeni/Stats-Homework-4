#part A
library(glmnet)
library(MASS)
biopsyData=biopsy
biopsyData[1]=NULL
biopsyData=na.omit(biopsyData)
biopsyData
dim(biopsyData)



for(i in 1:9){          
  for(k in 1:nrow(biopsyData)){
    biopsyData[k,i]= log(biopsyData[k,i])
    
  }
}
biopsyData
#part b
vect=c(0)
for (i in 1:25) {
  fit = kmeans(biopsyData[, 1:9],i)
  rsq= (1-fit$tot.withinss)/fit$totss
  print(fit$tot.withinss)
        print(fit$totss)
        vect[i]=rsq
}
par(mfrow = c(1, 2))
plot(c(1:25),vect,xlab="i",ylab="r squared")
#the greatest increase occurs between k=1 and k=2
#This is close to the number of clusters








#part c

pr.fit=prcomp(biopsyData[,1:9],center=TRUE, scale=FALSE)
pairs(pr.fit$x[,1:4], col=1+(biopsyData$class=='malignant'))
#this shows that the majority of the data lies in the first principal component which is weighted 
#equally among all of the predictors therefore the pairwise plots of any two predictors vi,vj should 
#approximate to the line Vi=Vj which is the case in the pairwise plots







#d .0-.
lasso1=cv.glmnet(as.matrix(biopsyData[,1:9]),biopsyData[,10],family='binomial', alpha=1)
lasso2=cv.glmnet(as.matrix(pr.fit$x), biopsyData[,10],family='binomial',alpha=1)

coeff=coef(lasso1,s=lasso1$lambda.1se)
coeff
coeff2=coef(lasso2,s=lasso2$lambda.1se)
coeff2
#Yes they conform to what we see in part, because the first principal component it the most important
