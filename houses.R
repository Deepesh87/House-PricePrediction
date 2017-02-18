train_input=read.csv("train.csv")#read input file
test_input=read.csv("test.csv")
train=train_input
test=test_input
cat_var <- names(train)[which(sapply(train, is.factor))]#all factor variables
#cc=which(sapply(train[,],is.factor)) or this can be used
#cat_var2=colnames(train)[cc]#instead of the above line

numeric_var <- names(train)[which(sapply(train, is.numeric))] #all numerical variables
#cc2=which(sapply(train[,],is.numeric))
#numeric_var2=colnames(train)[cc2]
dim(train)
num=colSums(sapply(train[,numeric_var ], is.na))#na's in numerical variables
num=num[num>0] #all na >0
sort(num,decreasing = T)
fac=colSums(sapply(train[,cat_var ], is.na))#na's in categorical variables
fac=fac[fac>0]
sort(fac,decreasing = T)
#MOST MISSSING VALUES IN cATEGORICAL VARIABLES ARE DUE TO MISSING ITEMS (LIKE nO GARAGE, NO TENNIS COURT ETC)
#the missing values will be filled later
#*****************************************************************************
#Visualization for the missing data
library(Amelia)
library(dplyr)
library(ggplot2)

plot_Missing <- function(data_in, title = NULL){
  temp_df <- as.data.frame(ifelse(is.na(data_in), 0, 1))
  temp_df <- temp_df[,order(colSums(temp_df))]
  data_temp <- expand.grid(list(x = 1:nrow(temp_df), y = colnames(temp_df)))
  data_temp$m <- as.vector(as.matrix(temp_df))
  data_temp <- data.frame(x = unlist(data_temp$x), y = unlist(data_temp$y), m = unlist(data_temp$m))
  ggplot(data_temp) + geom_tile(aes(x=x, y=y, fill=factor(m))) + scale_fill_manual(values=c("white", "black"), name="Missing\n(0=Yes, 1=No)")
}

plot_Missing(train[,colSums(is.na(train)) > 0])
#*******Another Plot
library(VIM)
aggr_plot <- aggr(train, col = c('navyblue','red'), numbers = TRUE, sortVars = TRUE,
                  labels= names(train),cex.axis = .9, gap = 2, ylab =c("Histogram of missing data","Pattern"))
#******************************************************************************
library(corrgram)
library(corrplot)
library(reshape2)
#do remodeled houses carry higher price
y=which(train[,'YearRemodAdd'] != train[,'YearBuilt'])
train$remodelled=0
train$remodelled[y]=1
train$remodelled=as.factor(train$remodelled)
ggplot(data=train,aes(x=SalePrice,fill=remodelled))+geom_histogram()
#No they do not
#Saleprice
ggplot(data=train,aes(x=log(SalePrice)))+geom_histogram(col="white")+ theme_light()
#almost normal distribution
#plot co-relation
cor_df=as.matrix(cor(train[,numeric_var]))
d_cor_melt <- arrange(melt(cor_df), -abs(value))
High_cor=subset(d_cor_melt,value>0.65 & value<0.99)
Dep_cor=subset(d_cor_melt,Var1=="SalePrice")
Dep_cor=subset(Dep_cor,abs(value)>0.3)
#variables with highest co-relation is shown above
#we will avoid to include co-related variables (given  by High_cor) in a linear reg model
#____________________________________________
lin=lm(log(SalePrice)~ OverallQual+(GrLivArea)+GarageCars+TotalBsmtSF+YearBuilt+YearRemodAdd+Fireplaces+BsmtFinSF1+WoodDeckSF,data=train)
summary(lin)
pred=exp(predict(lin))
SSE=sum((pred-train$SalePrice)^2)
SSE
RMSE=(SSE/nrow(train))^(0.5)
#RMSE=48425,R2=83.59--> previously without log, RMSE=36621;R2=0.78
#_____________________________________________
#predict on test data
pred2=exp(predict(lin,newdata=test))
test$SalePrice=pred2
sum(is.na(test$SalePrice))
test$SalePrice[which(is.na(test$SalePrice))]=mean(test$SalePrice,na.rm=T)
final=test[,c(1,ncol(test))]
write.csv(final,"final.csv",row.names=F)
#15.72% error On leaderboard,Rank=3250
#previously i tried without the log, i had got 32% RMSE and 4500 rank
#*********************************Outliers in continuous IV
#let us check if the variables we used has outliers. As outliers tend to have a bad effect on regression
#plot SalePrice and all the imp IV's 
par(mfrow=c(2,3))
plot(train$SalePrice,train$GrLivArea)
plot(train$SalePrice,train$TotalBsmtSF)
plot(train$SalePrice,train$YearBuilt)
plot(train$SalePrice,train$BsmtFinSF1)
plot(train$SalePrice,train$WoodDeckSF)
plot(train$SalePrice,train$WoodDeckSF)
plot(train$SalePrice,train$WoodDeckSF)
plot(train$SalePrice,train$WoodDeckSF)
#there are a few outliers but most dont much affect the analysis.
#I will ignore them for now
#_______________________________________________
#find important categorical variables
#we will do ANOVA to find out the most imp variables
#http://stats.stackexchange.com/questions/31690/how-to-test-the-statistical-significance-for-categorical-variable-in-linear-regr
#i will make boxplots for most of them to find the important ones
par(mfrow=c(2,3))
plot(train$MSZoning,train$SalePrice,xlab="",ylab = "SalePrice")
plot(train$Street,train$SalePrice,xlab="",ylab = "SalePrice")
plot(train$KitchenQual,train$SalePrice,xlab="",ylab = "SalePrice")
plot(train$Neighborhood,train$SalePrice,xlab="",ylab = "SalePrice")
plot(train$LandContour,train$SalePrice,xlab="",ylab = "SalePrice")
plot(train$BldgType,train$SalePrice,xlab="",ylab = "SalePrice")
plot(train$Heating,train$SalePrice,xlab="",ylab = "SalePrice")
plot(train$Foundation,train$SalePrice,xlab="",ylab = "SalePrice")
plot(train$LotShape,train$SalePrice,xlab="",ylab = "SalePrice")
plot(train$HouseStyle,train$SalePrice,xlab="",ylab = "SalePrice")
plot(train$LandSlope,train$SalePrice,xlab="",ylab = "SalePrice")
#+++++++++++++++++++++++++++++++++++++++++++
lin2=lm(log(SalePrice)~ OverallQual+(GrLivArea)+GarageCars+TotalBsmtSF+YearBuilt+
          YearRemodAdd+Fireplaces+BsmtFinSF1+WoodDeckSF+
          MSZoning+Street+KitchenQual+Neighborhood+LandContour+
          BldgType+Heating+Foundation+LotShape+HouseStyle+LandSlope,data=train)
summary(lin2)
pred=exp(predict(lin2))
SSE=sum((pred-train$SalePrice)^2)
SSE
RMSE=(SSE/nrow(train))^(0.5)
#RMSE=30754,R2=87.94
#_____________________________________________
#predict on test data
pred2=exp(predict(lin2,newdata=test))
test=test_input
test$SalePrice=pred2
sum(is.na(test$SalePrice))
test$SalePrice[which(is.na(test$SalePrice))]=mean(test$SalePrice,na.rm=T)
final=test[,c(1,ncol(test))]
write.csv(final,"final2.csv",row.names=F)
#*****************************************************************************
#Missing value imputation
#lets merge train and test and then merge
dim(train);dim(test)
train$SalePrice=NULL;test$SalePrice=NULL
train$remodelled=NULL#this was created by us
all=rbind(train,test)
#find total missing values in numerical columns
num=which(sapply(all[,],is.numeric))
num=colSums(sapply(all[,num],is.na))
num=num[num>0]
sort(num,decreasing = T)
#lets fill them one by one--->LotFrontage
cor(all$LotFrontage,all$LotArea,use="complete")
#Lotfromtage can be squareroot of Lot area. Look at their definition
all$LotFrontage[which(is.na(all$LotFrontage))]=(all$LotArea[which(is.na(all$LotFrontage))])
#*******---->GarageYrBlt
#this I believe is missing only where garage is not present
#lets find out
table(is.na(all$GarageYrBlt),all$GarageCars)
all$GarageYrBlt[which(is.na(all$GarageYrBlt))]='None'
#*******--->MasVnrArea
table(is.na(all$MasVnrType),is.na(all$MasVnrArea))
all$MasVnrArea[which(is.na(all$MasVnrArea))]=0
#**********BsmtFullBath BsmtHalfBath   BsmtFinSF1 etc
all$BsmtFullBath[which(is.na(all$BsmtFullBath))]=0
all$BsmtHalfBath[which(is.na(all$BsmtHalfBath))]=0
all$BsmtFinSF1[which(is.na(all$BsmtFinSF1))]=0
all$BsmtFinSF2[which(is.na(all$BsmtFinSF2))]=0
all$BsmtUnfSF[which(is.na(all$BsmtUnfSF))]=0
all$TotalBsmtSF[which(is.na(all$TotalBsmtSF))]=0
all$GarageCars[which(is.na(all$GarageCars))]=0
all$GarageArea[which(is.na(all$GarageArea))]=0
#*********************
cc2=which(sapply(all[,],is.numeric)) #or this can be used
num_var3=colnames(all)[cc2]#instead of the above line
x=colSums(sapply(all[,num_var3],is.na))
x[x>0]
#******************ALl good with numerical variables
#lets fill the blank categorical variables
sort(colSums(sapply(all[,cat_var],is.na)),decreasing = T)
all[,cat_var]=(sapply(all[,cat_var],as.character))
#PoolQC is 0 probably bcoz NO pool
#lets check
table(all$PoolArea>0,is.na(all$PoolQC))
#so 2906 does not have a pool. we can have 0 for all 2909
all$PoolQC[is.na(all$PoolQC)]='none'
#MiscFeature
all$MiscFeature[is.na(all$MiscFeature)]='none'
#Alley
table(all$Alley)
all$Alley[is.na(all$Alley)]='none'
#Fence
all$Fence[is.na(all$Fence)]='none'
# FireplaceQu
all$FireplaceQu[is.na(all$FireplaceQu)]='none'
#Garage related variables
table(all$GarageArea==0)
garage_cols=c('GarageType','GarageQual','GarageCond','GarageFinish')
for(i in garage_cols){
  d=which(is.na(all[[i]]))
  all[[i]][d]='none'
}
#**********************BASEMENT
basement_cols=c('BsmtQual','BsmtCond','BsmtExposure','BsmtFinType1','BsmtFinType2')
for(i in basement_cols){
  d=which(is.na(all[[i]]))
  all[[i]][d]='none'
}
#***********************
table(all$Utilities)
all$MasVnrType[is.na(all$MasVnrType)]='None'
all$MSZoning[is.na(all$MSZoning)]='RM'
all$Utilities[is.na(all$Utilities)]='AllPub'
#******
cols=c('Functional','Exterior1st','Exterior2nd','Electrical','KitchenQual')
#****************
#find mode
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}
#******
missi=c("Functional","Exterior1st","Exterior2nd","Electrical","KitchenQual","SaleType")
for(i in missi){
all[[i]][which(is.na(all[[i]]))]=Mode(all[[i]])
}
all[sapply(all, is.character)] = lapply(all[sapply(all, is.character)],as.factor)
#******************************************ALL missing values filled
train=head(all,nrow(train_input))
train$SalePrice=train_input$SalePrice
test=tail(all,nrow(test_input))
#********taking log of IV as well
lin3=lm(log(SalePrice)~ log(OverallQual+1)+log(GrLivArea+1)+GarageCars+log(TotalBsmtSF+1)+YearBuilt+
          log(YearRemodAdd+1)+Fireplaces+log(BsmtFinSF1+1)+WoodDeckSF+
          MSZoning+Street+KitchenQual+Neighborhood+LandContour+
          BldgType+Heating+Foundation+LotShape+HouseStyle+LandSlope,data=train)
summary(lin3)
plot(lin3)
pred=exp(predict(lin3))
SSE=sum((pred-train$SalePrice)^2)
SSE
RMSE=(SSE/nrow(train))^(0.5)
#RMSE=27217,R2=89.25
#_____________________________________________
#predict on test data
pred2=exp(predict(lin3,newdata=test))
test=test_input
test$SalePrice=pred2
sum(is.na(test$SalePrice))
test$SalePrice[which(is.na(test$SalePrice))]=mean(test$SalePrice,na.rm=T)
final=test[,c(1,ncol(test))]
write.csv(final,"final3.csv",row.names=F)
#************************************************
#&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&





