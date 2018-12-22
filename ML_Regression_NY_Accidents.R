rm(list=ls())
options(max.print=100000) 
base=read.csv('/users/pivotalit/downloads/NYPD_Motor_Vehicle_Collisions_Selcols.csv',header=T,sep=',')
str(base)
library(sqldf)
sbase=sqldf('select zipcode,borough,month,woy,
            avg(population_2016) as Population,
            avg(ALAND_SQMI) as Area,
            sum(Total_Accidents) as Total_accidents
            from base
            where population_2016 is not null
            group by zipcode,borough,month,woy
            ')
summary(sbase$Total_accidents)
str(sbase)
sbase$WOY=factor(sbase$WOY)
#sbase$Day_of_Week=factor(sbase$Day_of_Week)
sbase$ZIPCODE=factor(sbase$ZIPCODE)
sbase$Population=scale(sbase$Population)
sbase$Area=scale(sbase$Area)
sbase$Population=as.numeric(sbase$Population)
sbase$Area=as.numeric(sbase$Area)
#sbase$Total_accidents=log1p(sbase$Total_accidents)
str(sbase)
library(dummies)
convrtd=dummy.data.frame(sbase)
str(convrtd)
rows=seq(1,nrow(convrtd),1)
set.seed(123)
trainRows=sample(rows,nrow(convrtd)*.8)
set.seed(123)
remainingRows=rows[-(trainRows)]
testRows=sample(remainingRows)
train = convrtd[trainRows,]
test = convrtd[testRows,]

### Basic Linear regression model
lrmdl=lm(Total_accidents~.,data=train)
summary(lrmdl)
pred=predict.lm(lrmdl,test)
pred[pred<0 ] = 0
library(MLmetrics)
RMSE(pred,test$Total_accidents)  # 23.39




### Decision Tree

train=sbase[trainRows,]
test1=sbase[testRows,]
str(train)
str(test)
library(rpart)
dtmdl=rpart(Total_accidents~.,data=train,cp=.0001)
pred=predict(dtmdl,test1)
pred[pred<0]=0
library(MLmetrics)
RMSE(pred,test1$Total_accidents) # 16.575
test1$predictions_dt=round(pred)


###3 GBM
str(train)
library(h2o)
h2o.init()
train.set=as.h2o(train)
test.set=as.h2o(test1[,c(1:7)])
predictors <- c("ZIPCODE","BOROUGH","Month","WOY","Population","Area")
response <- "Total_accidents"
gbmmdl=h2o.gbm(y = response,  x=predictors, training_frame = train.set,
               ntrees = 1300,  distribution = "gaussian",learn_rate=.01,nbins=1000)
pred=h2o.predict(gbmmdl,test.set)
preddf=as.data.frame(pred)
RMSE(round(preddf$predict),test1$Total_accidents) #14.45
test1$predictions_gbm=round(preddf$predict)

test$rowid=row.names(test)
test1$rowid=row.names(test1)
sbase$rowid=row.names(sbase)
final_test=sqldf('select a.rowid,a.month,a.total_accidents,predictions_lr,predictions_gbm,predictions_dt
             from sbase a,test b,test1 c
            where a.rowid=b.rowid and a.rowid=c.rowid')

RMSE(final_test$Total_accidents,final_test$predictions_lr)
RMSE(final_test$Total_accidents,final_test$predictions_gbm)
RMSE(final_test$Total_accidents,final_test$predictions_dt)

write.csv(final_test,'/users/pivotalit/downloads/test.csv',row.names = F)



