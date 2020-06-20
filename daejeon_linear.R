######daegeon_modeling_linear regression#####
#예측값이 양수->log transform
####전체를이용한분석####
###original_rmse###
library(Metrics)
set.seed(2019)
sqrt(mean((predict(lm(mean(log(new_n))~1.,data=train),newdata = test)-log(test$new_n))^2))
library(Metrics)
rmse(predict(lm(mean(log(new_n))~1.,data=train),newdata = test),log(test$new_n))
##0.764992
summary(train)

###simple_linear###
lm_all<-lm(log(new_n)~.,data=train)
lm_all_summary<-summary(lm_all)
write.csv(lm_all_summary$coefficients,"lm_all_summary.csv")
#train error
fitted_lm_all<-ifelse(lm_all$fitted.values<=log(5),log(5),lm_all$fitted.values)
rmse(fitted_lm_all,log(train$new_n))
#0.4337806
0.4331446
0.4337169
#test error
pred_lm_all<-predict(lm_all,test)
pred_lm_all<-ifelse(pred_lm_all<=log(5),log(5),pred_lm_all)
rmse(log(test$new_n),(pred_lm_all))
#0.5229548
0.5236249
0.524703
obs_lm_all<-as.data.frame(cbind(log(test$new_n),pred_lm_all))
colnames(obs_lm_all)<-c("real","pred_lm_all")
library(ggplot2)

ggplot(data=obs_lm_all) + geom_point(mapping=aes(x=exp(real),y=exp(pred_lm_all)))+geom_abline(intercept= 0, slope=1, color='blue', size = 1.5)+ggtitle("daejeon_lm_all")+theme(plot.title = element_text(family = "serif", face = "bold", hjust = 0.5, size = 25)) +xlab("obs")+ylab("pred")
ggsave("pred_lm_all.jpg")


#5이하인것들 5로일괄적용
#pred_lm_all<-ifelse(pred_lm_all<=log(5),log(5),pred_lm_all)
#ggsave("pred_lm_all_5.jpg")


######구별로나누어분석####
#daedeok
lm_daedeok<-lm(log(new_n)~.,data=train_daedeok)
lm_daedeok_summary<-summary(lm_daedeok)
write.csv(lm_daedeok_summary$coefficients,"lm_daedeok_summary.csv")
#train error
fitted_lm_daedeok<-ifelse(lm_daedeok$fitted.values<=log(5),log(5),lm_daedeok$fitted.values)
rmse(fitted_lm_daedeok,log(train_daedeok$new_n))
#0.4655092
0.4645112
0.465376
#test error
pred_lm_daedeok<-predict(lm_daedeok,test_daedeok)
pred_lm_daedeok<-ifelse(pred_lm_daedeok<=log(5),log(5),pred_lm_daedeok)
rmse(log(test_daedeok$new_n),pred_lm_daedeok)
#0.6291933
0.5939541
0.5976513
a<-as.data.frame(cbind(log(test_daedeok$new_n),pred_lm_daedeok))
colnames(a)<-c("real","pred_lm_daedeok")
library(ggplot2)
ggplot(data=exp(a))+geom_point(mapping=aes(x=real,y=pred_lm_daedeok))+geom_abline(intercept= 0, slope=1, color='blue', size = 1.5)+ggtitle("daejeon_lm_daedeok")+theme(plot.title = element_text(family = "serif", face = "bold", hjust = 0.5, size = 25))+xlab("obs")+ylab("pred")
ggsave("pred_lm_daedeok.jpg")

#dong
lm_dong<-lm(log(new_n)~.,data=train_dong)
lm_dong_summary<-summary(lm_dong)
write.csv(lm_dong_summary$coefficients,"lm_dong_summary.csv")
#train error
fitted_lm_dong<-ifelse(lm_dong$fitted.values<=log(5),log(5),lm_dong$fitted.values)
rmse(fitted_lm_dong,log(train_dong$new_n))
#0.4256949
0.4246244
0.4249205
#test error
pred_lm_dong<-predict(lm_dong,test_dong)
pred_lm_dong<-ifelse(pred_lm_dong<=log(5),log(5),pred_lm_dong)

rmse(log(test_dong$new_n),pred_lm_dong)
#0.4093099
0.4101709
0.4097641
a<-as.data.frame(cbind(log(test_dong$new_n),pred_lm_dong))
colnames(a)<-c("real","pred_lm_dong")
library(ggplot2)
ggplot(data=exp(a))+geom_point(mapping=aes(x=real,y=pred_lm_dong))+geom_abline(intercept= 0, slope=1, color='blue', size = 1.5)+ggtitle("daejeon_lm_dong")+theme(plot.title = element_text(family = "serif", face = "bold", hjust = 0.5, size = 25))+xlab("obs")+ylab("pred")
ggsave("pred_lm_dong.jpg")

#seo
lm_seo<-lm(log(new_n)~.,data=train_seo)
lm_seo_summary<-summary(lm_seo)
write.csv(lm_seo_summary$coefficients,"lm_seo_summary.csv")

#train error
fitted_lm_seo<-ifelse(lm_seo$fitted.values<=log(5),log(5),lm_seo$fitted.values)
rmse(fitted_lm_seo,log(train_seo$new_n))
#0.3902521
0.3892703
0.3899579
#test error
pred_lm_seo<-predict(lm_seo,test_seo)
pred_lm_seo<-ifelse(pred_lm_seo<=log(5),log(5),pred_lm_seo)

rmse(log(test_seo$new_n),pred_lm_seo)
#0.4815595
0.4815223
0.4825923
a<-as.data.frame(cbind(log(test_seo$new_n),pred_lm_seo))
colnames(a)<-c("real","pred_lm_seo")
library(ggplot2)
ggplot(data=exp(a))+geom_point(mapping=aes(x=real,y=pred_lm_seo))+geom_abline(intercept= 0, slope=1, color='blue', size = 1.5)+ggtitle("daejeon_lm_seo")+theme(plot.title = element_text(family = "serif", face = "bold", hjust = 0.5, size = 25))+xlab("obs")+ylab("pred")
ggsave("pred_lm_seo.jpg")

#yuseong
lm_yuseong<-lm(log(new_n)~.,data=train_yuseong)
lm_yuseong_summary<-summary(lm_yuseong)
write.csv(lm_yuseong_summary$coefficients,"lm_yuseong_summary.csv")
#train error
fitted_lm_yuseong<-ifelse(lm_yuseong$fitted.values<=log(5),log(5),lm_yuseong$fitted.values)
rmse(fitted_lm_yuseong,log(train_yuseong$new_n))
#0.3947048
0.3942627
0.396508
#test error
pred_lm_yuseong<-predict(lm_yuseong,test_yuseong)
pred_lm_yuseong<-ifelse(pred_lm_yuseong<=log(5),log(5),pred_lm_yuseong)

rmse(log(test_yuseong$new_n),pred_lm_yuseong)
#0.4287863
0.4277372
0.4315214
a<-as.data.frame(cbind(log(test_yuseong$new_n),pred_lm_yuseong))
colnames(a)<-c("real","pred_lm_yuseong")
library(ggplot2)
ggplot(data=exp(a))+geom_point(mapping=aes(x=real,y=pred_lm_yuseong))+geom_abline(intercept= 0, slope=1, color='blue', size = 1.5)+ggtitle("daejeon_lm_yuseong")+theme(plot.title = element_text(family = "serif", face = "bold", hjust = 0.5, size = 25))+xlab("obs")+ylab("pred")
ggsave("pred_lm_yuseong.jpg")

#jung
lm_jung<-lm(log(new_n)~.,data=train_jung)
lm_jung_summary<-summary(lm_jung)
write.csv(lm_jung_summary$coefficients,"lm_jung_summary.csv")

#train error
fitted_lm_jung<-ifelse(lm_jung$fitted.values<=log(5),log(5),lm_jung$fitted.values)
rmse(fitted_lm_jung,log(train_jung$new_n))
#0.4032288
0.4025624
0.4028036
#test error
pred_lm_jung<-predict(lm_jung,test_jung)
pred_lm_jung<-ifelse(pred_lm_jung<=log(5),log(5),pred_lm_jung)

rmse(log(test_jung$new_n),pred_lm_jung)
#0.4571876
0.4580719
0.4586944
a<-as.data.frame(cbind(log(test_jung$new_n),pred_lm_jung))
colnames(a)<-c("real","pred_lm_jung")
library(ggplot2)
ggplot(data=exp(a))+geom_point(mapping=aes(x=real,y=pred_lm_jung))+geom_abline(intercept= 0, slope=1, color='blue', size = 1.5)+ggtitle("daejeon_lm_jung")+theme(plot.title = element_text(family = "serif", face = "bold", hjust = 0.5, size = 25))+xlab("obs")+ylab("pred")
ggsave("pred_lm_jung.jpg")
