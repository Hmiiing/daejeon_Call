######daejeon_modeling_svm#####
#예측값이 양수->log transform
####전체를이용한분석####
library(e1071)
library(Metrics)
set.seed(2019)
###svm_lin###
#tune.out<-tune(svm,new_n~.,data=train,kernel="linear",ranges=list(cost=c(0.01,0.1,1,5,10),gamma=c(0.5,1,2,3,4)))
svm.lin_all<-svm(log(new_n)~.,data=train,kernel="linear")
#svm.lin_all2<-svm(log(new_n)~.,data=train,kernel="linear",cost=0.1)
#svm.lin_all3<-svm(log(new_n)~.,data=train,kernel="linear",cost=5)  #max iterations
#svm.lin_all$cost
#1
#svm.lin_all$gamma
#0.03846154

#test error
pred_svm.lin_all<-predict(svm.lin_all,test)
pred_svm.lin_all<-ifelse(pred_svm.lin_all<=log(5),log(5),pred_svm.lin_all)
rmse(pred_svm.lin_all,log(test$new_n))
#0.5078467
0.5088509
0.5103335
library(ggplot2)
obs_all<-as.data.frame(cbind(log(test$new_n),pred_svm.lin_all))
colnames(obs_all)<-c("real","pred_svm.lin_all")
ggplot(data=exp(obs_all))+geom_point(mapping=aes(x=real,y=pred_svm.lin_all))+geom_abline(intercept= 0, slope=1, color='blue', size = 1.5)+ggtitle("daejeon_svm.lin_all")+theme(plot.title = element_text(family = "serif", face = "bold", hjust = 0.5, size = 25)) +xlab("obs")+ylab("pred")
ggsave("pred_svm.lin_all.jpg")

###svm_rad###
#tune.out<-tune(svm,new_n~.,data=train,kernel="radial",ranges=list(cost=c(0.01,0.1,1,5,10),gamma=c(0.5,1,2,3,4)))
svm.rad_all<-svm(log(new_n)~.,data=train,kernel="radial")

#test error
pred_svm.rad_all<-predict(svm.rad_all,test)
pred_svm.rad_all<-ifelse(pred_svm.rad_all<=log(5),log(5),pred_svm.rad_all)
rmse(pred_svm.rad_all,log(test$new_n))
#0.469252
0.4687197
0.4664164
obs_all_r<-as.data.frame(cbind(log(test$new_n),pred_svm.rad_all))
colnames(obs_all_r)<-c("real","pred_svm.rad_all")
ggplot(data=exp(obs_all_r))+geom_point(mapping=aes(x=real,y=pred_svm.rad_all))+geom_abline(intercept= 0, slope=1, color='blue', size = 1.5)+ggtitle("daejeon_svm.rad_all")+theme(plot.title = element_text(family = "serif", face = "bold", hjust = 0.5, size = 25)) +xlab("obs")+ylab("pred")
ggsave("pred_svm.rad_all.jpg")

####구별 분석####

##daedeok###
###svm_lin###
#tune.out<-tune(svm,new_n~.,data=train,kernel="linear",ranges=list(cost=c(0.01,0.1,1,5,10),gamma=c(0.5,1,2,3,4)))
svm.lin_daedeok<-svm(log(new_n)~.,data=train_daedeok,kernel="linear")

#test error
pred_svm.lin_daedeok<-predict(svm.lin_daedeok,test_daedeok)
pred_svm.lin_daedeok<-ifelse(pred_svm.lin_daedeok<=log(5),log(5),pred_svm.lin_daedeok)
rmse(pred_svm.lin_daedeok,log(test_daedeok$new_n))
0.5672919
0.5696686
obs_daedeok<-as.data.frame(cbind(log(test_daedeok$new_n),pred_svm.lin_daedeok))
colnames(obs_daedeok)<-c("real","pred_svm.lin_daedeok")
ggplot(data=exp(obs_daedeok))+geom_point(mapping=aes(x=real,y=pred_svm.lin_daedeok))+geom_abline(intercept= 0, slope=1, color='blue', size = 1.5)+ggtitle("daejeon_svm.lin_daedeok")+theme(plot.title = element_text(family = "serif", face = "bold", hjust = 0.5, size = 25)) +xlab("obs")+ylab("pred")
ggsave("pred_svm.lin_daedeok.jpg")


###svm_rad###
#tune.out<-tune(svm,new_n~.,data=train,kernel="radial",ranges=list(cost=c(0.01,0.1,1,5,10),gamma=c(0.5,1,2,3,4)))
svm.rad_daedeok<-svm(log(new_n)~.,data=train_daedeok,kernel="radial")

#test error
pred_svm.rad_daedeok<-predict(svm.rad_daedeok,test_daedeok)
pred_svm.rad_daedeok<-ifelse(pred_svm.rad_daedeok<=log(5),log(5),pred_svm.rad_daedeok)
rmse(pred_svm.rad_daedeok,log(test_daedeok$new_n))
0.5464712
0.5421353
obs_daedeok_r<-as.data.frame(cbind(log(test_daedeok$new_n),pred_svm.rad_daedeok))
colnames(obs_daedeok_r)<-c("real","pred_svm.rad_daedeok")
ggplot(data=exp(obs_daedeok_r))+geom_point(mapping=aes(x=real,y=pred_svm.rad_daedeok))+geom_abline(intercept= 0, slope=1, color='blue', size = 1.5)+ggtitle("daejeon_svm.rad_daedeok")+theme(plot.title = element_text(family = "serif", face = "bold", hjust = 0.5, size = 25)) +xlab("obs")+ylab("pred")
ggsave("pred_svm.rad_daedeok.jpg")


##dong###
###svm_lin###
#tune.out<-tune(svm,new_n~.,data=train,kernel="linear",ranges=list(cost=c(0.01,0.1,1,5,10),gamma=c(0.5,1,2,3,4)))
svm.lin_dong<-svm(log(new_n)~.,data=train_dong,kernel="linear")

#test error
pred_svm.lin_dong<-predict(svm.lin_dong,test_dong)
pred_svm.lin_dong<-ifelse(pred_svm.lin_dong<=log(5),log(5),pred_svm.lin_dong)
rmse(pred_svm.lin_dong,log(test_dong$new_n))
0.4062091
0.4065698
obs_dong<-as.data.frame(cbind(log(test_dong$new_n),pred_svm.lin_dong))
colnames(obs_dong)<-c("real","pred_svm.lin_dong")
ggplot(data=exp(obs_dong))+geom_point(mapping=aes(x=real,y=pred_svm.lin_dong))+geom_abline(intercept= 0, slope=1, color='blue', size = 1.5)+ggtitle("daejeon_svm.lin_dong")+theme(plot.title = element_text(family = "serif", face = "bold", hjust = 0.5, size = 25)) +xlab("obs")+ylab("pred")
ggsave("pred_svm.lin_dong.jpg")


###svm_rad###
#tune.out<-tune(svm,new_n~.,data=train,kernel="radial",ranges=list(cost=c(0.01,0.1,1,5,10),gamma=c(0.5,1,2,3,4)))
svm.rad_dong<-svm(log(new_n)~.,data=train_dong,kernel="radial")

#test error
pred_svm.rad_dong<-predict(svm.rad_dong,test_dong)
pred_svm.rad_dong<-ifelse(pred_svm.rad_dong<=log(5),log(5),pred_svm.rad_dong)
rmse(pred_svm.rad_dong,log(test_dong$new_n))
0.4505512
0.4529693
obs_dong_r<-as.data.frame(cbind(log(test_dong$new_n),pred_svm.rad_dong))
colnames(obs_dong_r)<-c("real","pred_svm.rad_dong")
ggplot(data=exp(obs_dong_r))+geom_point(mapping=aes(x=real,y=pred_svm.rad_dong))+geom_abline(intercept= 0, slope=1, color='blue', size = 1.5)+ggtitle("daejeon_svm.rad_dong")+theme(plot.title = element_text(family = "serif", face = "bold", hjust = 0.5, size = 25)) +xlab("obs")+ylab("pred")
ggsave("pred_svm.rad_dong.jpg")

##seo###
###svm_lin###
#tune.out<-tune(svm,new_n~.,data=train,kernel="linear",ranges=list(cost=c(0.01,0.1,1,5,10),gamma=c(0.5,1,2,3,4)))
svm.lin_seo<-svm(log(new_n)~.,data=train_seo,kernel="linear")

#test error
pred_svm.lin_seo<-predict(svm.lin_seo,test_seo)
pred_svm.lin_seo<-ifelse(pred_svm.lin_seo<=log(5),log(5),pred_svm.lin_seo)
rmse(pred_svm.lin_seo,log(test_seo$new_n))
0.4669439
0.4706249
obs_seo<-as.data.frame(cbind(log(test_seo$new_n),pred_svm.lin_seo))
colnames(obs_seo)<-c("real","pred_svm.lin_seo")
ggplot(data=exp(obs_seo))+geom_point(mapping=aes(x=real,y=pred_svm.lin_seo))+geom_abline(intercept= 0, slope=1, color='blue', size = 1.5)+ggtitle("daejeon_svm.lin_seo")+theme(plot.title = element_text(family = "serif", face = "bold", hjust = 0.5, size = 25)) +xlab("obs")+ylab("pred")
ggsave("pred_svm.lin_seo.jpg")


###svm_rad###
#tune.out<-tune(svm,new_n~.,data=train,kernel="radial",ranges=list(cost=c(0.01,0.1,1,5,10),gamma=c(0.5,1,2,3,4)))
svm.rad_seo<-svm(log(new_n)~.,data=train_seo,kernel="radial")

#test error
pred_svm.rad_seo<-predict(svm.rad_seo,test_seo)
pred_svm.rad_seo<-ifelse(pred_svm.rad_seo<=log(5),log(5),pred_svm.rad_seo)
rmse(pred_svm.rad_seo,log(test_seo$new_n))
0.4421106
0.4428484
obs_seo_r<-as.data.frame(cbind(log(test_seo$new_n),pred_svm.rad_seo))
colnames(obs_seo_r)<-c("real","pred_svm.rad_seo")
ggplot(data=exp(obs_seo_r))+geom_point(mapping=aes(x=real,y=pred_svm.rad_seo))+geom_abline(intercept= 0, slope=1, color='blue', size = 1.5)+ggtitle("daejeon_svm.rad_seo")+theme(plot.title = element_text(family = "serif", face = "bold", hjust = 0.5, size = 25)) +xlab("obs")+ylab("pred")
ggsave("pred_svm.rad_seo.jpg")

##yuseong###
###svm_lin###
#tune.out<-tune(svm,new_n~.,data=train,kernel="linear",ranges=list(cost=c(0.01,0.1,1,5,10),gamma=c(0.5,1,2,3,4)))
svm.lin_yuseong<-svm(log(new_n)~.,data=train_yuseong,kernel="linear")

#test error
pred_svm.lin_yuseong<-predict(svm.lin_yuseong,test_yuseong)
pred_svm.lin_yuseong<-ifelse(pred_svm.lin_yuseong<=log(5),log(5),pred_svm.lin_yuseong)
rmse(pred_svm.lin_yuseong,log(test_yuseong$new_n))
0.4210648
0.424506
obs_yuseong<-as.data.frame(cbind(log(test_yuseong$new_n),pred_svm.lin_yuseong))
colnames(obs_yuseong)<-c("real","pred_svm.lin_yuseong")
ggplot(data=exp(obs_yuseong))+geom_point(mapping=aes(x=real,y=pred_svm.lin_yuseong))+geom_abline(intercept= 0, slope=1, color='blue', size = 1.5)+ggtitle("daejeon_svm.lin_yuseong")+theme(plot.title = element_text(family = "serif", face = "bold", hjust = 0.5, size = 25)) +xlab("obs")+ylab("pred")
ggsave("pred_svm.lin_yuseong.jpg")


###svm_rad###
#tune.out<-tune(svm,new_n~.,data=train,kernel="radial",ranges=list(cost=c(0.01,0.1,1,5,10),gamma=c(0.5,1,2,3,4)))
svm.rad_yuseong<-svm(log(new_n)~.,data=train_yuseong,kernel="radial")

#test error
pred_svm.rad_yuseong<-predict(svm.rad_yuseong,test_yuseong)
pred_svm.rad_yuseong<-ifelse(pred_svm.rad_yuseong<=log(5),log(5),pred_svm.rad_yuseong)
rmse(pred_svm.rad_yuseong,log(test_yuseong$new_n))
0.4376473
0.4409287
obs_yuseong_r<-as.data.frame(cbind(log(test_yuseong$new_n),pred_svm.rad_yuseong))
colnames(obs_yuseong_r)<-c("real","pred_svm.rad_yuseong")
ggplot(data=exp(obs_yuseong_r))+geom_point(mapping=aes(x=real,y=pred_svm.rad_yuseong))+geom_abline(intercept= 0, slope=1, color='blue', size = 1.5)+ggtitle("daejeon_svm.rad_yuseong")+theme(plot.title = element_text(family = "serif", face = "bold", hjust = 0.5, size = 25)) +xlab("obs")+ylab("pred")
ggsave("pred_svm.rad_yuseong.jpg")

##jung###
###svm_lin###
#tune.out<-tune(svm,new_n~.,data=train,kernel="linear",ranges=list(cost=c(0.01,0.1,1,5,10),gamma=c(0.5,1,2,3,4)))
svm.lin_jung<-svm(log(new_n)~.,data=train_jung,kernel="linear")

#test error
pred_svm.lin_jung<-predict(svm.lin_jung,test_jung)
pred_svm.lin_jung<-ifelse(pred_svm.lin_jung<=log(5),log(5),pred_svm.lin_jung)
rmse(pred_svm.lin_jung,log(test_jung$new_n))
0.4433398
0/4436554
obs_jung<-as.data.frame(cbind(log(test_jung$new_n),pred_svm.lin_jung))
colnames(obs_jung)<-c("real","pred_svm.lin_jung")
ggplot(data=exp(obs_jung))+geom_point(mapping=aes(x=real,y=pred_svm.lin_jung))+geom_abline(intercept= 0, slope=1, color='blue', size = 1.5)+ggtitle("daejeon_svm.lin_jung")+theme(plot.title = element_text(family = "serif", face = "bold", hjust = 0.5, size = 25)) +xlab("obs")+ylab("pred")
ggsave("pred_svm.lin_jung.jpg")


###svm_rad###
#tune.out<-tune(svm,new_n~.,data=train,kernel="radial",ranges=list(cost=c(0.01,0.1,1,5,10),gamma=c(0.5,1,2,3,4)))
svm.rad_jung<-svm(log(new_n)~.,data=train_jung,kernel="radial")

#test error
pred_svm.rad_jung<-predict(svm.rad_jung,test_jung)
pred_svm.rad_jung<-ifelse(pred_svm.rad_jung<=log(5),log(5),pred_svm.rad_jung)
rmse(pred_svm.rad_jung,log(test_jung$new_n))
0.4280654
0.4289579
obs_jung_r<-as.data.frame(cbind(log(test_jung$new_n),pred_svm.rad_jung))
colnames(obs_jung_r)<-c("real","pred_svm.rad_jung")
ggplot(data=exp(obs_jung_r))+geom_point(mapping=aes(x=real,y=pred_svm.rad_jung))+geom_abline(intercept= 0, slope=1, color='blue', size = 1.5)+ggtitle("daejeon_svm.rad_jung")+theme(plot.title = element_text(family = "serif", face = "bold", hjust = 0.5, size = 25)) +xlab("obs")+ylab("pred")
ggsave("pred_svm.rad_jung.jpg")
