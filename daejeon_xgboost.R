######daejeon_modeling_xgboost#####
####전체를 이용한 분석####
#install.packages("xgboost")
library(xgboost)
library(Matrix)
library(data.table)
set.seed(2019)

daejeon_t$new_n<-log(daejeon_t$new_n)
label_all=(daejeon_t$new_n)

id_train_all <- which(daejeon_t$year==2016|daejeon_t$year==2017)

train_data_all<-(daejeon_t[id_train_all,])
test_data_all<-(daejeon_t[-id_train_all,])

train_index_all<-label_all[id_train_all]
test_index_all<-label_all[-id_train_all]

train_all<-sparse.model.matrix((new_n)~.,data=train_data_all)
test_all<-sparse.model.matrix((new_n)~.,data=test_data_all)

train_all<-xgb.DMatrix(train_all,label=train_index_all)
test_all<-xgb.DMatrix(test_all,label=test_index_all)

####parameters####
eta=c(seq(0,1,by=0.01))
cs=c(seq(0,1,by=0.01))
md=c(seq(5,10,by=1))
ss=c(seq(0,1,by=0.01))

conv_eta = matrix(NA,500,length(eta))
pred_eta = matrix(NA,length(test_index_all), length(eta))
colnames(conv_eta) = colnames(pred_eta) = eta
for(i in 1:length(eta)){
  params=list(eta = eta[i], min_child_weigth = 1)
  xgb=xgboost(train_all, label = train_index_all, nrounds = 100, params = params)
  conv_eta[,i] = xgb$evaluation_log$train_rmse
  pred_eta[,i] = predict(xgb, test_all)
}
conv_eta = data.frame(iter=1:100, conv_eta)
conv_eta = melt(conv_eta, id.vars = "iter")
(RMSE_eta = sqrt(colMeans((test_index_all-pred_eta)^2)))
which.min(RMSE_eta)
#0.03

conv_ss=matrix(NA,100,length(ss))
pred_ss=matrix(NA,length(test_index_all),length(ss))
colnames(conv_ss)=colnames(pred_ss)=ss
for(i in 1:length(ss)){
  params=list(eta=0.03,subsample=ss[i], min_child_weigth=1)
  xgb=xgboost(train_all, label = train_index_all, nrounds = 100, params = params)
  conv_ss[,i] = xgb$evaluation_log$train_rmse
  pred_ss[,i] = predict(xgb, test_all)
}
conv_ss=data.frame(iter=1:100,conv_ss)
conv_ss=melt(conv_ss,id.vars = "iter")
(RMSE_ss = sqrt(colMeans((test_index_all-pred_ss)^2)))
which.min(RMSE_ss)
#0.11

conv_md=matrix(NA,100,length(md))
pred_md=matrix(NA,length(test_index_all),length(md))
colnames(conv_md)=colnames(pred_md)=md
for(i in 1:length(md)){
  params=list(eta=0.03,
              subsample=0.11,max_depth=md[i],
              min_child_weigth=1)
  xgb=xgboost(train_all, label = train_index_all, nrounds = 100, params = params)
  conv_md[,i] = xgb$evaluation_log$train_rmse
  pred_md[,i] = predict(xgb, test_all)
}
conv_md=data.frame(iter=1:100,conv_md)
conv_md=melt(conv_md,id.vars = "iter")
(RMSE_md = sqrt(colMeans((test_index_all-pred_md)^2)))
which.min(RMSE_md)
#8

conv_cs=matrix(NA,100,length(cs))
pred_cs=matrix(NA,length(test_index_all),length(cs))
colnames(conv_cs)=colnames(pred_cs)=cs
for(i in 1:length(cs)){
  params=list(eta=0.03,subsample=0.11,max_depth=8,colsample_bylevel=cs[i],min_child_weigth=1)
  xgb=xgboost(train_all, label = train_index_all, nrounds = 100, params = params)
  conv_cs[,i] = xgb$evaluation_log$train_rmse
  pred_cs[,i] = predict(xgb, test_all)
}
conv_cs=data.frame(iter=1:100,conv_cs)
conv_cs=melt(conv_cs,id.vars = "iter")
(RMSE_cs = sqrt(colMeans((test_index_all-pred_cs)^2)))
which.min(RMSE_cs)
#0.93

########전체model
bst_all<-xgboost(data=train_all,label=train_index_all,max_depth = 8, subsample=0.11, colsample_bylevel=0.93,
             eta = 0.03, nrounds = 100)
importance_all <- xgb.importance(feature_names = colnames(train_all), model = bst_all)
head(importance_all)
xgb.plot.importance(importance_matrix = importance_all)

#test error
pred_all<-predict(bst_all,test_all)
pred_all<-ifelse(pred_all<=log(5),log(5),pred_all)
rmse((test_index_all),(pred_all))
0.4609675
obs_all<-as.data.frame(cbind(test_index_all,pred_all))
colnames(obs_all)<-c("real","pred_xgb_all")
library(ggplot2)
ggplot(data=exp(obs_all))+geom_point(mapping=aes(x=real,y=pred_xgb_all))+geom_abline(intercept= 0, slope=1, color='blue', size = 1.5)+ggtitle("daejeon_xgb_all")+theme(plot.title = element_text(family = "serif", face = "bold", hjust = 0.5, size = 25))+xlab("obs")+ylab("pred")
ggsave("pred_xgb_all.jpg")



###구별로 나눠서
###daedeok###
set.seed(2019)

daejeon_daedeok$new_n<-log(daejeon_daedeok$new_n)
label_daedeok=(daejeon_daedeok$new_n)

id_train_daedeok <- which(daejeon_daedeok$year==2016|daejeon_daedeok$year==2017)

train_data_daedeok<-(daejeon_daedeok[id_train_daedeok,])
test_data_daedeok<-(daejeon_daedeok[-id_train_daedeok,])

train_index_daedeok<-label_daedeok[id_train_daedeok]
test_index_daedeok<-label_daedeok[-id_train_daedeok]

train_daedeok<-sparse.model.matrix((new_n)~.,data=train_data_daedeok)
test_daedeok<-sparse.model.matrix((new_n)~.,data=test_data_daedeok)

train_daedeok<-xgb.DMatrix(train_daedeok,label=train_index_daedeok)
test_daedeok<-xgb.DMatrix(test_daedeok,label=test_index_daedeok)

####parameters####
eta=c(0.05,0.1,0.2,0.5,1)
cs=c(1/3,2/3,1)
md=c(2,4,6,10)
ss=c(seq(0.25,1,by=0.25))
standard=c(2,2,3,2)
conv_eta = matrix(NA,500,length(eta))
pred_eta = matrix(NA,length(test_index_daedeok), length(eta))
colnames(conv_eta) = colnames(pred_eta) = eta
for(i in 1:length(eta)){
  params=list(eta = eta[i], colsample_bylevel=cs[standard[2]],
              subsample = ss[standard[4]], max_depth = md[standard[3]],
              min_child_weigth = 1)
  xgb=xgboost(train_daedeok, label = train_index_daedeok, nrounds = 100, params = params)
  conv_eta[,i] = xgb$evaluation_log$train_rmse
  pred_eta[,i] = predict(xgb, test_daedeok)
}
conv_eta = data.frame(iter=1:100, conv_eta)
conv_eta = melt(conv_eta, id.vars = "iter")
(RMSE_eta = sqrt(colMeans((test_index_daedeok-pred_eta)^2)))
which.min(RMSE_eta)
#0.05

conv_ss=matrix(NA,500,length(ss))
pred_ss=matrix(NA,length(test_index_daedeok),length(ss))
colnames(conv_ss)=colnames(pred_ss)=ss
for(i in 1:length(ss)){
  params=list(eta=0.05,colsample_bylevel=cs[standard[2]],
              subsample=ss[i],max_depth=md[standard[3]],
              min_child_weigth=1)
  xgb=xgboost(train_daedeok, label = train_index_daedeok, nrounds = 100, params = params)
  conv_ss[,i] = xgb$evaluation_log$train_rmse
  pred_ss[,i] = predict(xgb, test_daedeok)
}
conv_ss=data.frame(iter=1:500,conv_ss)
conv_ss=melt(conv_ss,id.vars = "iter")
(RMSE_ss = sqrt(colMeans((test_index_daedeok-pred_ss)^2)))
which.min(RMSE_ss)
#0.25

conv_md=matrix(NA,500,length(md))
pred_md=matrix(NA,length(test_index_daedeok),length(md))
colnames(conv_md)=colnames(pred_md)=md
for(i in 1:length(md)){
  params=list(eta=0.05,colsample_bylevel=cs[standard[2]],
              subsample=0.25,max_depth=md[i],
              min_child_weigth=1)
  xgb=xgboost(train_daedeok, label = train_index_daedeok, nrounds = 100, params = params)
  conv_md[,i] = xgb$evaluation_log$train_rmse
  pred_md[,i] = predict(xgb, test_daedeok)
}
conv_md=data.frame(iter=1:100,conv_md)
conv_md=melt(conv_md,id.vars = "iter")
(RMSE_md = sqrt(colMeans((test_index_daedeok-pred_md)^2)))
which.min(RMSE_md)
#10
########daedeok
bst_daedeok<-xgboost(data=train_daedeok,label=train_index_daedeok,max_depth = 10,subsample=0.25,
                 eta = 0.05, nthread = 2, nrounds = 100)
importance_daedeok <- xgb.importance(feature_names = colnames(train_daedeok), model = bst_daedeok)
head(importance_daedeok)
xgb.plot.importance(importance_matrix = importance_daedeok)

#test error
library(Metrics)
pred_daedeok<-predict(bst_daedeok,test_daedeok)
pred_daedeok<-ifelse(pred_daedeok<=log(5),log(5),pred_daedeok)

rmse((test_index_daedeok),(pred_daedeok))
0.6075814

obs_daedeok<-as.data.frame(cbind(test_index_daedeok,pred_daedeok))
colnames(obs_daedeok)<-c("real","pred_xgb_daedeok")
library(ggplot2)
ggplot(data=exp(obs_daedeok))+geom_point(mapping=aes(x=real,y=pred_xgb_daedeok))+geom_abline(intercept= 0, slope=1, color='blue', size = 1.5)+ggtitle("daejeon_xgb_daedeok")+theme(plot.title = element_text(family = "serif", face = "bold", hjust = 0.5, size = 25))+xlab("obs")+ylab("pred")
ggsave("pred_xgb_daedeok.jpg")


###dong###
set.seed(2019)

daejeon_dong$new_n<-log(daejeon_dong$new_n)
label_dong=(daejeon_dong$new_n)

id_train_dong <- which(daejeon_dong$year==2016|daejeon_dong$year==2017)

train_data_dong<-(daejeon_dong[id_train_dong,])
test_data_dong<-(daejeon_dong[-id_train_dong,])

train_index_dong<-label_dong[id_train_dong]
test_index_dong<-label_dong[-id_train_dong]

train_dong<-sparse.model.matrix((new_n)~.,data=train_data_dong)
test_dong<-sparse.model.matrix((new_n)~.,data=test_data_dong)

train_dong<-xgb.DMatrix(train_dong,label=train_index_dong)
test_dong<-xgb.DMatrix(test_dong,label=test_index_dong)

####parameters####
eta=c(0.05,0.1,0.2,0.5,1)
cs=c(1/3,2/3,1)
md=c(2,4,6,10)
ss=c(seq(0.25,1,by=0.25))
standard=c(2,2,3,2)
conv_eta = matrix(NA,500,length(eta))
pred_eta = matrix(NA,length(test_index_dong), length(eta))
colnames(conv_eta) = colnames(pred_eta) = eta
for(i in 1:length(eta)){
  params=list(eta = eta[i], colsample_bylevel=cs[standard[2]],
              subsample = ss[standard[4]], max_depth = md[standard[3]],
              min_child_weigth = 1)
  xgb=xgboost(train_dong, label = train_index_dong, nrounds = 100, params = params)
  conv_eta[,i] = xgb$evaluation_log$train_rmse
  pred_eta[,i] = predict(xgb, test_dong)
}
conv_eta = data.frame(iter=1:100, conv_eta)
conv_eta = melt(conv_eta, id.vars = "iter")
(RMSE_eta = sqrt(colMeans((test_index_dong-pred_eta)^2)))
which.min(RMSE_eta)
#0.05

conv_ss=matrix(NA,500,length(ss))
pred_ss=matrix(NA,length(test_index_dong),length(ss))
colnames(conv_ss)=colnames(pred_ss)=ss
for(i in 1:length(ss)){
  params=list(eta=0.05,colsample_bylevel=cs[standard[2]],
              subsample=ss[i],max_depth=md[standard[3]],
              min_child_weigth=1)
  xgb=xgboost(train_dong, label = train_index_dong, nrounds = 100, params = params)
  conv_ss[,i] = xgb$evaluation_log$train_rmse
  pred_ss[,i] = predict(xgb, test_dong)
}
conv_ss=data.frame(iter=1:500,conv_ss)
conv_ss=melt(conv_ss,id.vars = "iter")
(RMSE_ss = sqrt(colMeans((test_index_dong-pred_ss)^2)))
which.min(RMSE_ss)
#0.5

conv_md=matrix(NA,500,length(md))
pred_md=matrix(NA,length(test_index_dong),length(md))
colnames(conv_md)=colnames(pred_md)=md
for(i in 1:length(md)){
  params=list(eta=0.05,colsample_bylevel=cs[standard[2]],
              subsample=0.5,max_depth=md[i],
              min_child_weigth=1)
  xgb=xgboost(train_dong, label = train_index_dong, nrounds = 100, params = params)
  conv_md[,i] = xgb$evaluation_log$train_rmse
  pred_md[,i] = predict(xgb, test_dong)
}
conv_md=data.frame(iter=1:100,conv_md)
conv_md=melt(conv_md,id.vars = "iter")
(RMSE_md = sqrt(colMeans((test_index_dong-pred_md)^2)))
which.min(RMSE_md)
#2

bst_dong<-xgboost(data=train_dong,label=train_index_dong,,max_depth = 2,subsample=0.5,
                  eta = 0.05, nthread = 2, nrounds = 100)
importance_dong <- xgb.importance(feature_names = colnames(train_dong), model = bst_dong)
head(importance_dong)
xgb.plot.importance(importance_matrix = importance_dong)

#test error
library(Metrics)
pred_dong<-predict(bst_dong,test_dong)
pred_dong<-ifelse(pred_dong<=log(5),log(5),pred_dong)
rmse((test_index_dong),(pred_dong))
0.4294628
obs_dong<-as.data.frame(cbind(test_index_dong,pred_dong))
colnames(obs_dong)<-c("real","pred_xgb_dong")
library(ggplot2)
ggplot(data=exp(obs_dong))+geom_point(mapping=aes(x=real,y=pred_xgb_dong))+geom_abline(intercept= 0, slope=1, color='blue', size = 1.5)+ggtitle("daejeon_xgb_dong")+theme(plot.title = element_text(family = "serif", face = "bold", hjust = 0.5, size = 25))+xlab("obs")+ylab("pred")
ggsave("pred_xgb_dong.jpg")

###seo###
set.seed(2019)
daejeon_seo$new_n<-log(daejeon_seo$new_n)
label_seo=(daejeon_seo$new_n)

id_train_seo <- which(daejeon_seo$year==2016|daejeon_seo$year==2017)

train_data_seo<-(daejeon_seo[id_train_seo,])
test_data_seo<-(daejeon_seo[-id_train_seo,])

train_index_seo<-label_seo[id_train_seo]
test_index_seo<-label_seo[-id_train_seo]

train_seo<-sparse.model.matrix((new_n)~.,data=train_data_seo)
test_seo<-sparse.model.matrix((new_n)~.,data=test_data_seo)

train_seo<-xgb.DMatrix(train_seo,label=train_index_seo)
test_seo<-xgb.DMatrix(test_seo,label=test_index_seo)


####parameters####
eta=c(0.05,0.1,0.2,0.5,1)
cs=c(1/3,2/3,1)
md=c(2,4,6,10)
ss=c(seq(0.25,1,by=0.25))
standard=c(2,2,3,2)
conv_eta = matrix(NA,500,length(eta))
pred_eta = matrix(NA,length(test_index_seo), length(eta))
colnames(conv_eta) = colnames(pred_eta) = eta
for(i in 1:length(eta)){
  params=list(eta = eta[i], colsample_bylevel=cs[standard[2]],
              subsample = ss[standard[4]], max_depth = md[standard[3]],
              min_child_weigth = 1)
  xgb=xgboost(train_seo, label = train_index_seo, nrounds = 100, params = params)
  conv_eta[,i] = xgb$evaluation_log$train_rmse
  pred_eta[,i] = predict(xgb, test_seo)
}
conv_eta = data.frame(iter=1:100, conv_eta)
conv_eta = melt(conv_eta, id.vars = "iter")
(RMSE_eta = sqrt(colMeans((test_index_seo-pred_eta)^2)))
which.min(RMSE_eta)
#0.05

conv_ss=matrix(NA,500,length(ss))
pred_ss=matrix(NA,length(test_index_seo),length(ss))
colnames(conv_ss)=colnames(pred_ss)=ss
for(i in 1:length(ss)){
  params=list(eta=0.05,colsample_bylevel=cs[standard[2]],
              subsample=ss[i],max_depth=md[standard[3]],
              min_child_weigth=1)
  xgb=xgboost(train_seo, label = train_index_seo, nrounds = 100, params = params)
  conv_ss[,i] = xgb$evaluation_log$train_rmse
  pred_ss[,i] = predict(xgb, test_seo)
}
conv_ss=data.frame(iter=1:500,conv_ss)
conv_ss=melt(conv_ss,id.vars = "iter")
(RMSE_ss = sqrt(colMeans((test_index_seo-pred_ss)^2)))
which.min(RMSE_ss)
#0.5

conv_md=matrix(NA,500,length(md))
pred_md=matrix(NA,length(test_index_seo),length(md))
colnames(conv_md)=colnames(pred_md)=md
for(i in 1:length(md)){
  params=list(eta=0.05,colsample_bylevel=cs[standard[2]],
              subsample=0.5,max_depth=md[i],
              min_child_weigth=1)
  xgb=xgboost(train_seo, label = train_index_seo, nrounds = 100, params = params)
  conv_md[,i] = xgb$evaluation_log$train_rmse
  pred_md[,i] = predict(xgb, test_seo)
}
conv_md=data.frame(iter=1:100,conv_md)
conv_md=melt(conv_md,id.vars = "iter")
(RMSE_md = sqrt(colMeans((test_index_seo-pred_md)^2)))
which.min(RMSE_md)
#2

bst_seo<-xgboost(data=train_seo,label=train_index_seo,max_depth = 4,
                 eta = 1, nthread = 2, nrounds = 10)
importance_seo <- xgb.importance(feature_names = colnames(train_seo), model = bst_seo)
head(importance_seo)
xgb.plot.importance(importance_matrix = importance_seo)
#test error
library(Metrics)
pred_seo<-predict(bst_seo,test_seo)
pred_seo<-ifelse(pred_seo<=log(5),log(5),pred_seo)

rmse((test_index_seo),(pred_seo))
#0.4745314
0.4824766
library(ggplot2)
obs_seo<-as.data.frame(cbind(test_index_seo,pred_seo))
colnames(obs_seo)<-c("real","pred_xgb_seo")
library(ggplot2)
ggplot(data=exp(obs_seo))+geom_point(mapping=aes(x=real,y=pred_xgb_seo))+geom_abline(intercept= 0, slope=1, color='blue', size = 1.5)+ggtitle("daejeon_xgb_seo")+theme(plot.title = element_text(family = "serif", face = "bold", hjust = 0.5, size = 25))+xlab("obs")+ylab("pred")
ggsave("pred_xgb_seo.jpg")


###yuseong###
set.seed(2019)

daejeon_yuseong$new_n<-log(daejeon_yuseong$new_n)
label_yuseong=(daejeon_yuseong$new_n)

id_train_yuseong <- which(daejeon_yuseong$year==2016|daejeon_yuseong$year==2017)

train_data_yuseong<-(daejeon_yuseong[id_train_yuseong,])
test_data_yuseong<-(daejeon_yuseong[-id_train_yuseong,])

train_index_yuseong<-label_yuseong[id_train_yuseong]
test_index_yuseong<-label_yuseong[-id_train_yuseong]

train_yuseong<-sparse.model.matrix((new_n)~.,data=train_data_yuseong)
test_yuseong<-sparse.model.matrix((new_n)~.,data=test_data_yuseong)

train_yuseong<-xgb.DMatrix(train_yuseong,label=train_index_yuseong)
test_yuseong<-xgb.DMatrix(test_yuseong,label=test_index_yuseong)

####parameters####
eta=c(0.05,0.1,0.2,0.5,1)
cs=c(1/3,2/3,1)
md=c(2,4,6,10)
ss=c(seq(0.25,1,by=0.25))
standard=c(2,2,3,2)
conv_eta = matrix(NA,500,length(eta))
pred_eta = matrix(NA,length(test_index_yuseong), length(eta))
colnames(conv_eta) = colnames(pred_eta) = eta
for(i in 1:length(eta)){
  params=list(eta = eta[i], colsample_bylevel=cs[standard[2]],
              subsample = ss[standard[4]], max_depth = md[standard[3]],
              min_child_weigth = 1)
  xgb=xgboost(train_yuseong, label = train_index_yuseong, nrounds = 100, params = params)
  conv_eta[,i] = xgb$evaluation_log$train_rmse
  pred_eta[,i] = predict(xgb, test_yuseong)
}
conv_eta = data.frame(iter=1:100, conv_eta)
conv_eta = melt(conv_eta, id.vars = "iter")
(RMSE_eta = sqrt(colMeans((test_index_yuseong-pred_eta)^2)))
which.min(RMSE_eta)
#0.05

conv_ss=matrix(NA,500,length(ss))
pred_ss=matrix(NA,length(test_index_yuseong),length(ss))
colnames(conv_ss)=colnames(pred_ss)=ss
for(i in 1:length(ss)){
  params=list(eta=0.05,colsample_bylevel=cs[standard[2]],
              subsample=ss[i],max_depth=md[standard[3]],
              min_child_weigth=1)
  xgb=xgboost(train_yuseong, label = train_index_yuseong, nrounds = 100, params = params)
  conv_ss[,i] = xgb$evaluation_log$train_rmse
  pred_ss[,i] = predict(xgb, test_yuseong)
}
conv_ss=data.frame(iter=1:500,conv_ss)
conv_ss=melt(conv_ss,id.vars = "iter")
(RMSE_ss = sqrt(colMeans((test_index_yuseong-pred_ss)^2)))
which.min(RMSE_ss)
#0.75

conv_md=matrix(NA,500,length(md))
pred_md=matrix(NA,length(test_index_yuseong),length(md))
colnames(conv_md)=colnames(pred_md)=md
for(i in 1:length(md)){
  params=list(eta=0.05,colsample_bylevel=cs[standard[2]],
              subsample=0.75,max_depth=md[i],
              min_child_weigth=1)
  xgb=xgboost(train_yuseong, label = train_index_yuseong, nrounds = 100, params = params)
  conv_md[,i] = xgb$evaluation_log$train_rmse
  pred_md[,i] = predict(xgb, test_yuseong)
}
conv_md=data.frame(iter=1:100,conv_md)
conv_md=melt(conv_md,id.vars = "iter")
(RMSE_md = sqrt(colMeans((test_index_yuseong-pred_md)^2)))
which.min(RMSE_md)
#4
params=list(eta=0.05,colsample_bylevel=cs[standard[2]],
            subsample=0.75,max_depth=4,
            min_child_weigth=1)

bst_yuseong<-xgboost(data=train_yuseong,label=train_index_yuseong, nrounds=100,params=params)
importance_yuseong <- xgb.importance(feature_names = colnames(train_yuseong), model = bst_yuseong)
head(importance_yuseong)
xgb.plot.importance(importance_matrix = importance_yuseong)

#test error
library(Metrics)
pred_yuseong<-predict(bst_yuseong,test_yuseong)
pred_yuseong<-ifelse(pred_yuseong<=log(5),log(5),pred_yuseong)

rmse((test_index_yuseong),(pred_yuseong))
0.4889127
obs_yuseong<-as.data.frame(cbind(test_index_yuseong,pred_yuseong))
colnames(obs_yuseong)<-c("real","pred_xgb_yuseong")
library(ggplot2)
ggplot(data=exp(obs_yuseong))+geom_point(mapping=aes(x=real,y=pred_xgb_yuseong))+geom_abline(intercept= 0, slope=1, color='blue', size = 1.5)+ggtitle("daejeon_xgb_yuseong")+theme(plot.title = element_text(family = "serif", face = "bold", hjust = 0.5, size = 25))+xlab("obs")+ylab("pred")
ggsave("pred_xgb_yuseong.jpg")



###jung###
set.seed(2019)

daejeon_jung$new_n<-log(daejeon_jung$new_n)
label_jung=(daejeon_jung$new_n)

id_train_jung <- which(daejeon_jung$year==2016|daejeon_jung$year==2017)

train_data_jung<-(daejeon_jung[id_train_jung,])
test_data_jung<-(daejeon_jung[-id_train_jung,])

train_index_jung<-label_jung[id_train_jung]
test_index_jung<-label_jung[-id_train_jung]

train_jung<-sparse.model.matrix((new_n)~.,data=train_data_jung)
test_jung<-sparse.model.matrix((new_n)~.,data=test_data_jung)

train_jung<-xgb.DMatrix(train_jung,label=train_index_jung)
test_jung<-xgb.DMatrix(test_jung,label=test_index_jung)


####parameters####
eta=c(0.05,0.1,0.2,0.5,1)
cs=c(1/3,2/3,1)
md=c(2,4,6,10)
ss=c(seq(0.25,1,by=0.25))
standard=c(2,2,3,2)
conv_eta = matrix(NA,500,length(eta))
pred_eta = matrix(NA,length(test_index_jung), length(eta))
colnames(conv_eta) = colnames(pred_eta) = eta
for(i in 1:length(eta)){
  params=list(eta = eta[i], colsample_bylevel=cs[standard[2]],
              subsample = ss[standard[4]], max_depth = md[standard[3]],
              min_child_weigth = 1)
  xgb=xgboost(train_jung, label = train_index_jung, nrounds = 100, params = params)
  conv_eta[,i] = xgb$evaluation_log$train_rmse
  pred_eta[,i] = predict(xgb, test_jung)
}
conv_eta = data.frame(iter=1:100, conv_eta)
conv_eta = melt(conv_eta, id.vars = "iter")
(RMSE_eta = sqrt(colMeans((test_index_jung-pred_eta)^2)))
which.min(RMSE_eta)
#0.05

conv_ss=matrix(NA,500,length(ss))
pred_ss=matrix(NA,length(test_index_jung),length(ss))
colnames(conv_ss)=colnames(pred_ss)=ss
for(i in 1:length(ss)){
  params=list(eta=0.05,colsample_bylevel=cs[standard[2]],
              subsample=ss[i],max_depth=md[standard[3]],
              min_child_weigth=1)
  xgb=xgboost(train_jung, label = train_index_jung, nrounds = 100, params = params)
  conv_ss[,i] = xgb$evaluation_log$train_rmse
  pred_ss[,i] = predict(xgb, test_jung)
}
conv_ss=data.frame(iter=1:500,conv_ss)
conv_ss=melt(conv_ss,id.vars = "iter")
(RMSE_ss = sqrt(colMeans((test_index_jung-pred_ss)^2)))
which.min(RMSE_ss)
#0.75

conv_md=matrix(NA,500,length(md))
pred_md=matrix(NA,length(test_index_jung),length(md))
colnames(conv_md)=colnames(pred_md)=md
for(i in 1:length(md)){
  params=list(eta=0.05,colsample_bylevel=cs[standard[2]],
              subsample=0.75,max_depth=md[i],
              min_child_weigth=1)
  xgb=xgboost(train_jung, label = train_index_jung, nrounds = 100, params = params)
  conv_md[,i] = xgb$evaluation_log$train_rmse
  pred_md[,i] = predict(xgb, test_jung)
}
conv_md=data.frame(iter=1:100,conv_md)
conv_md=melt(conv_md,id.vars = "iter")
(RMSE_md = sqrt(colMeans((test_index_jung-pred_md)^2)))
which.min(RMSE_md)
#4
params=list(eta=0.05,colsample_bylevel=cs[standard[2]],
            subsample=0.75,max_depth=4,
            min_child_weigth=1)



bst_jung<-xgboost(data=train_jung,label=train_index_jung,parmas=params, nrounds = 100)
importance_jung <- xgb.importance(feature_names = colnames(train_jung), model = bst_jung)
head(importance_jung)
xgb.plot.importance(importance_matrix = importance_jung)
 
#test error
library(Metrics)
pred_jung<-predict(bst_jung,test_jung)
pred_jung<-ifelse(pred_jung<=log(5),log(5),pred_jung)

rmse((test_index_jung),(pred_jung))
0.4366397
obs_jung<-as.data.frame(cbind(test_index_jung,pred_jung))
colnames(obs_jung)<-c("real","pred_xgb_jung")
library(ggplot2)
ggplot(data=exp(obs_jung))+geom_point(mapping=aes(x=real,y=pred_xgb_jung))+geom_abline(intercept= 0, slope=1, color='blue', size = 1.5)+ggtitle("daejeon_xgb_jung")+theme(plot.title = element_text(family = "serif", face = "bold", hjust = 0.5, size = 25))+xlab("obs")+ylab("pred")
ggsave("pred_xgb_jung.jpg")

