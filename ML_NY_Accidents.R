rm(list=ls())
base=read.csv('/users/pivotalit/downloads/NYPD_Motor_Vehicle_Collisions_Selcols.csv',header=T,sep=',')
cbase=base[c("ZIPCODE","Population_2016","ALAND_SQMI","Total_Accidents")]
library(sqldf)
sbase=sqldf('select zipcode,
                    avg(population_2016) as Population,
                    avg(ALAND_SQMI) as Area,
                    sum(Total_Accidents) as Total_accidents
             from cbase
             where population_2016 is not null
             group by zipcode')

scaledbase=scale(sbase[,c(2,3,4)])
scaledbase=data.frame(sbase$ZIPCODE,scaledbase)
names(scaledbase)[1]=c('Zipcode')
log(0)
sse=0
set.seed(123)
for(i in 1:30) {
  print(i)
  fit=kmeans(scaledbase[,c(2:4)],centers=i)
  sse[i]=sum(fit$withinss)
}
plot(1:i,sse,xlab='cluster',ylab='sse')
fit=kmeans(scaledbase[,c(2:4)],centers=3)
sbase$cluster=fit$cluster
write.csv(sbase,'/users/pivotalit/downloads/clusters.csv',row.names = F)
summary(sbase[which(sbase$cluster=='1'),])
summary(sbase[which(sbase$cluster=='2'),])
summary(sbase[which(sbase$cluster=='3'),])
sbase$cluster=factor(sbase$cluster)
levels(sbase$cluster)=c('1-<6202','2-<12519','3-<3480')


rm(list=ls())
base=read.csv('/users/pivotalit/downloads/NYPD_Motor_Vehicle_Collisions_Selcols.csv',header=T,sep=',')
cbase=base[c("ZIPCODE","Population_2016","ALAND_SQMI","Total_Accidents")]
library(sqldf)
sbase=sqldf('select zipcode,
                    avg(population_2016) as Population,
                    avg(ALAND_SQMI) as Area,
                    sum(Total_Accidents) as Total_accidents
             from cbase
             where population_2016 is not null
             group by zipcode')
cor(sbase$Area,sbase$Total_accidents)
cor(sbase$Population,sbase$Total_accidents)
cor(log(sbase$Area),sbase$Total_accidents)
