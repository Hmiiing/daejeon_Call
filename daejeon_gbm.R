######daejeon_modeling_boosting#####
########tuning
library(gbm)
set.seed(2019)
pows = seq(-10, 0, by = 1)
lambdas = 10^pows
train.err = rep(NA, length(lambdas))
for (i in 1:length(lambdas)) {
  boost.hitters = gbm(log(new_n) ~ ., data = train, distribution = "gaussian", n.trees = 1000, shrinkage = lambdas[i])
  pred.train = predict(boost.hitters, train, n.trees = 1000)
  train.err[i] = mean((pred.train - log(train$new_n))^2)
}
plot(lambdas, train.err, type = "b", xlab = "Shrinkage values", ylab = "Training MSE")
which.min(train.err)

lambdas[9]
#0.01

summary(train)
####전체를 이용한 분석####
install.packages("gbm")
library(gbm)
library(Metrics)
set.seed(2019)
daejeon_all_boost<-gbm(log(new_n)~.,data=train,distribution="gaussian",n.trees=5000,shrinkage=0.01)
(sum_all<-summary(daejeon_all_boost))

var      rel.inf
time         time 47.861981827
gu             gu 32.662722883
year         year 13.201346815
day           day  2.233981495
month       month  2.090186181
rainfall rainfall  0.696756777
temp         temp  0.663877150
hum           hum  0.294021662
PM10         PM10   0.153457878
wind_d     wind_d  0.113193678
wind_s     wind_s  0.026069815
rain         rain  0.002403839
#test error
yhat.boost_all<-predict(daejeon_all_boost,newdata=test,n.trees=5000,shrinkage=0.01)
yhat.boost_all<-ifelse(yhat.boost_all<=log(5),log(5),yhat.boost_all)
rmse(log(test$new_n),yhat.boost_all)
#0.5142644
0.5159988
obs_all<-as.data.frame(cbind(log(test$new_n),yhat.boost_all))
colnames(obs_all)<-c("real","yhat.boost_all")
library(ggplot2)
ggplot(data=exp(obs_all))+geom_point(mapping=aes(x=real,y=yhat.boost_all))+geom_abline(intercept= 0, slope=1, color='blue', size = 1.5)+ggtitle("daejeon_all_gbm")+theme(plot.title = element_text(family = "serif", face = "bold", hjust = 0.5, size = 25))
ggsave("ypred_gbm_all.jpg")
#time gu year 중요변수

####구별로 분리한 분석####
###daedeok###
set.seed(2019)

#train.err_d = rep(NA, length(lambdas))
#for (i in 1:length(lambdas)) {
  boost.hitters = gbm(log(new_n) ~ ., data = train_daedeok, distribution = "gaussian", n.trees = 1000, shrinkage = lambdas[i])
  pred.train = predict(boost.hitters, train_daedeok, n.trees = 1000)
  train.err_d[i] = mean((pred.train - log(train_daedeok$new_n))^2)
}
#plot(lambdas, train.err_d, type = "b", xlab = "Shrinkage values", ylab = "Training MSE")
#0.01

daejeon_daedeok_boost<-gbm(log(new_n)~.,data=train_daedeok,distribution="gaussian",n.trees=5000,shrinkage = 0.01)
summary(daejeon_daedeok_boost)
#year time month

var    rel.inf
year         year 51.4376161
time         time 30.8804014
month       month 11.4193406
day           day  1.9913691
temp         temp  1.3570887
rainfall rainfall  0.8359450
hum           hum  0.8282797
PM10         PM10  0.5247935
wind_s     wind_s  0.3409212
wind_d     wind_d  0.2738945
rain         rain  0.1103502
#test error
yhat.boost_daedeok<-predict(daejeon_daedeok_boost,newdata=test_daedeok,n.trees=5000,shrinkage=0.01,interaction.depth=4)
yhat.boost_daedeok<-ifelse(yhat.boost_daedeok<=log(5),log(5),yhat.boost_daedeok)
rmse(log(test_daedeok$new_n),yhat.boost_daedeok)
#0.6221262
obs_daedeok<-as.data.frame(cbind(log(test_daedeok$new_n),yhat.boost_daedeok))
colnames(obs_daedeok)<-c("real","yhat.boost_daedeok")
ggplot(data=exp(obs_daedeok))+geom_point(mapping=aes(x=real,y=yhat.boost_daedeok))+geom_abline(intercept= 0, slope=1, color='blue', size = 1.5)+ggtitle("daejeon_daedeok_gbm")+theme(plot.title = element_text(family = "serif", face = "bold", hjust = 0.5, size = 25))
ggsave("ypred_gbm_daedeok.jpg")

###dong###
set.seed(2019)
#train.err_d = rep(NA, length(lambdas))
#for (i in 1:length(lambdas)) {
  boost.hitters = gbm(log(new_n) ~ ., data = train_dong, distribution = "gaussian", n.trees = 1000, shrinkage = lambdas[i])
  pred.train = predict(boost.hitters, train_dong, n.trees = 1000)
  train.err_d[i] = mean((pred.train - log(train_dong$new_n))^2)
}
#plot(lambdas, train.err_d, type = "b", xlab = "Shrinkage values", ylab = "Training MSE")
#lambdas

daejeon_dong_boost<-gbm(log(new_n)~.,data=train_dong,distribution="gaussian",n.trees=5000,interaction.depth=4,shrinkage=0.01)
summary(daejeon_dong_boost)
#time
var    rel.inf
time         time 64.2730784
month       month  9.2588333
day           day  5.8597518
temp         temp  5.8029760
year         year  5.2915362
PM10         PM10  2.4361055
wind_d     wind_d  2.2380873
wind_s     wind_s  1.9450470
rainfall rainfall  1.4640588
hum           hum  1.1303099
rain         rain  0.3002157

#test error
yhat.boost_dong<-predict(daejeon_dong_boost,newdata=test_dong,n.trees=5000)
yhat.boost_dong<-ifelse(yhat.boost_dong<=log(5),log(5),yhat.boost_dong)
rmse(log(test_dong$new_n),yhat.boost_dong)
#0.4290691
obs_dong<-as.data.frame(cbind(log(test_dong$new_n),yhat.boost_dong))
colnames(obs_dong)<-c("real","yhat.boost_dong")
ggplot(data=exp(obs_dong))+geom_point(mapping=aes(x=real,y=yhat.boost_dong))+geom_abline(intercept= 0, slope=1, color='blue', size = 1.5)+ggtitle("daejeon_dong_gbm")+theme(plot.title = element_text(family = "serif", face = "bold", hjust = 0.5, size = 25))
ggsave("ypred_gbm_dong.jpg")

###seo###
set.seed(2019)
daejeon_seo_boost<-gbm(log(new_n)~.,data=train_seo,distribution="gaussian",n.trees=5000,interaction.depth=4)
summary(daejeon_seo_boost)
#time hum

#test error
yhat.boost_seo<-predict(daejeon_seo_boost,newdata=test_seo,n.trees=5000)
yhat.boost_seo<-ifelse(yhat.boost_seo<=log(5),log(5),yhat.boost_seo)
rmse(log(test_seo$new_n),yhat.boost_seo)
#0.494755
obs_seo<-as.data.frame(cbind(log(test_seo$new_n),yhat.boost_seo))
colnames(obs_seo)<-c("real","yhat.boost_seo")
ggplot(data=exp(obs_seo))+geom_point(mapping=aes(x=real,y=yhat.boost_seo))+geom_abline(intercept= 0, slope=1, color='blue', size = 1.5)+ggtitle("daejeon_seo_gbm")+theme(plot.title = element_text(family = "serif", face = "bold", hjust = 0.5, size = 25))
ggsave("ypred_gbm_seo.jpg")

###yuseong###
set.seed(2019)
daejeon_yuseong_boost<-gbm(log(new_n)~.,data=train_yuseong,distribution="gaussian",n.trees=5000,interaction.depth=4)
summary(daejeon_yuseong_boost)
#time month
var     rel.inf
time         time 37.20823998
month       month 12.95870679
day           day 10.03023063
temp         temp  9.38182091
year         year  8.32443018
hum           hum  7.19545213
wind_s     wind_s  5.60513589
PM10         PM10  5.37702787
wind_d     wind_d  2.79025547
rainfall rainfall  1.06518264
rain         rain  0.06351751

#test error
yhat.boost_yuseong<-predict(daejeon_yuseong_boost,newdata=test_yuseong,n.trees=5000)
yhat.boost_yuseong<-ifelse(yhat.boost_yuseong<=log(5),log(5),yhat.boost_yuseong)
rmse(log(test_yuseong$new_n),yhat.boost_yuseong)
#0.5366034
obs_yuseong<-as.data.frame(cbind(log(test_yuseong$new_n),yhat.boost_yuseong))
colnames(obs_yuseong)<-c("real","yhat.boost_yuseong")
ggplot(data=exp(obs_yuseong))+geom_point(mapping=aes(x=real,y=yhat.boost_yuseong))+geom_abline(intercept= 0, slope=1, color='blue', size = 1.5)+ggtitle("daejeon_yuseong_gbm")+theme(plot.title = element_text(family = "serif", face = "bold", hjust = 0.5, size = 25))
ggsave("ypred_gbm_yuseong.jpg")


###jung###
set.seed(2019)
daejeon_jung_boost<-gbm(log(new_n)~.,data=train_jung,distribution="gaussian",n.trees=5000,interaction.depth=4)
summary(daejeon_jung_boost)
var    rel.inf
time         time 34.3743930
month       month 14.0294359
hum           hum 13.3982314
temp         temp  8.9862316
day           day  8.6380416
wind_d     wind_d  7.1026395
PM10         PM10  5.0226904
wind_s     wind_s  4.5597137
year         year  2.7889101
rainfall rainfall  0.8924492
rain         rain  0.2072634
#test error
yhat.boost_jung<-predict(daejeon_jung_boost,newdata=test_jung,n.trees=5000)
yhat.boost_jung<-ifelse(yhat.boost_jung<=log(5),log(5),yhat.boost_jung)
rmse(log(test_jung$new_n),yhat.boost_jung)
0.4460456
obs_jung<-as.data.frame(cbind(log(test_jung$new_n),yhat.boost_jung))
colnames(obs_jung)<-c("real","yhat.boost_jung")
ggplot(data=exp(obs_jung))+geom_point(mapping=aes(x=real,y=yhat.boost_jung))+geom_abline(intercept= 0, slope=1, color='blue', size = 1.5)+ggtitle("daejeon_jung_gbm")+theme(plot.title = element_text(family = "serif", face = "bold", hjust = 0.5, size = 25))
ggsave("ypred_gbm_jung.jpg")