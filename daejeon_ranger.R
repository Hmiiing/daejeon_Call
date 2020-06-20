######daejeon_modeling_randomforest###
#####전체를 이용한 분석####
#p:25
install.packages("ranger")
library(ranger)
library(Metrics)
set.seed(2019)
random_all <- ranger(log(new_n)~.,data=train,mtry=8,importance='impurity',write.forest=TRUE)

#test error
pred_random_all <- predict(random_all,data=test)
p.v_all<-pred_random_all$predictions
p.v_all<-ifelse(p.v_all<=log(5),log(5),p.v_all)
rmse(p.v_all,log(test$new_n))
0.4972781
obs_random_all<-as.data.frame(cbind(log(test$new_n),p.v_all))
colnames(obs_random_all)<-c("real","pred_random_all")
library(ggplot2)
ggplot(data=obs_random_all) + geom_point(mapping=aes(x=exp(real),y=exp(pred_random_all)))+geom_abline(intercept= 0, slope=1, color='blue', size = 1.5)+ggtitle("daejeon_random_all")+theme(plot.title = element_text(family = "serif", face = "bold", hjust = 0.5, size = 25))+xlab("obs")+ylab("pred")
ggsave("pred_randomforest_all.jpg")

(import_all<-random_all$variable.importance)
gu         day        temp    rainfall      wind_s      wind_d         hum        year 
7747.97350  1383.10366  3434.41961   251.56362  1935.95877  1818.27445  2841.01086  3069.98998 
month        PM10        rain        time 
1387.89480  1885.04308    72.39641 10488.12412


import_all<-sort(import_all)
plot(import_all,horiz=TRUE,las=1,main="VI plot for RF_all")

barplot(import_all,horiz=TRUE,las=1,main="VI plot for RF_all")

#####구별로 나눠서 분석#####
###daedeok###
set.seed(2019)
random_daedeok <- ranger(log(new_n)~.,data=train_daedeok,mtry=7,importance='impurity',write.forest=TRUE)
#test error
pred_random_daedeok <- predict(random_daedeok,data=test_daedeok)
p.v_daedeok<-pred_random_daedeok$predictions
p.v_daedeok<-ifelse(p.v_daedeok<=log(5),log(5),p.v_daedeok)
rmse(p.v_daedeok,log(test_daedeok$new_n))
0.62274546

obs_random_daedeok<-as.data.frame(cbind(log(test_daedeok$new_n),p.v_daedeok))
colnames(obs_random_daedeok)<-c("real","pred_random_daedeok")
library(ggplot2)
ggplot(data=obs_random_daedeok) + geom_point(mapping=aes(x=exp(real),y=exp(pred_random_daedeok)))+geom_abline(intercept= 0, slope=1, color='blue', size = 1.5)+ggtitle("daejeon_random_daedeok")+theme(plot.title = element_text(family = "serif", face = "bold", hjust = 0.5, size = 25))+xlab("obs")+ylab("pred")
ggsave("pred_randomforest_daedeok.jpg")

(import_daedeok<-random_daedeok$variable.importance)
day       temp   rainfall     wind_s     wind_d        hum       year      month 
262.24587  790.04281   50.46349  426.85871  414.97050  752.52315 1785.17116  501.93304 
PM10       rain       time 
437.66566   17.25063 1036.48269 

import_daedeok<-sort(import_daedeok)
barplot(import_daedeok,horiz=TRUE,las=1,main="VI plot for RF_daedeok")


###dong###
set.seed(2019)
random_dong <- ranger(log(new_n)~.,data=train_dong,mtry=7,importance='impurity',write.forest=TRUE)
#test error
pred_random_dong <- predict(random_dong,data=test_dong)
p.v_dong<-pred_random_dong$predictions
p.v_dong<-ifelse(p.v_dong<=log(5),log(5),p.v_dong)
rmse(p.v_dong,log(test_dong$new_n))
0.4402837
obs_random_dong<-as.data.frame(cbind(log(test_dong$new_n),p.v_dong))
colnames(obs_random_dong)<-c("real","pred_random_dong")
library(ggplot2)
ggplot(data=obs_random_dong) + geom_point(mapping=aes(x=exp(real),y=exp(pred_random_dong)))+geom_abline(intercept= 0, slope=1, color='blue', size = 1.5)+ggtitle("daejeon_random_dong")+theme(plot.title = element_text(family = "serif", face = "bold", hjust = 0.5, size = 25))+xlab("obs")+ylab("pred")
ggsave("pred_randomforest_dong.jpg")

(import_dong<-random_dong$variable.importance)
day       temp   rainfall     wind_s     wind_d        hum       year      month 
296.89935  911.59064   55.90498  508.87952  412.48830   52.32017  159.42128  296.10366 
PM10       rain       time 
456.34071   22.28796 1924.49989 

import_dong<-sort(import_dong)
barplot(import_dong,horiz=TRUE,las=1,main="VI plot for RF_dong")


###seo###
set.seed(2019)
random_seo <- ranger(log(new_n)~.,data=train_seo,mtry=7,importance='impurity',write.forest=TRUE)
#test error
pred_random_seo <- predict(random_seo,data=test_seo)
p.v_seo<-pred_random_seo$predictions
p.v_seo<-ifelse(p.v_seo<=log(5),log(5),p.v_seo)
rmse(p.v_seo,log(test_seo$new_n))
0.4744343
obs_random_seo<-as.data.frame(cbind(log(test_seo$new_n),p.v_seo))
colnames(obs_random_seo)<-c("real","pred_random_seo")
library(ggplot2)
ggplot(data=obs_random_seo) + geom_point(mapping=aes(x=exp(real),y=exp(pred_random_seo)))+geom_abline(intercept= 0, slope=1, color='blue', size = 1.5)+ggtitle("daejeon_random_seo")+theme(plot.title = element_text(family = "serif", face = "bold", hjust = 0.5, size = 25))+xlab("obs")+ylab("pred")
ggsave("pred_randomforest_seo.jpg")

(import_seo<-random_seo$variable.importance)
day       temp   rainfall     wind_s     wind_d        hum       year      month 
251.10196  577.64101   48.63902  357.96262  374.89718  608.19229  656.52105  250.68911 
PM10       rain       time 
304.23681   16.04989 2570.53420 
import_seo<-sort(import_seo)
barplot(import_seo,horiz=TRUE,las=1,main="VI plot for RF_seo")


###yuseong###
set.seed(2019)
random_yuseong <- ranger(log(new_n)~.,data=train_yuseong,mtry=7,importance='impurity',write.forest=TRUE)
#test error
pred_random_yuseong <- predict(random_yuseong,data=test_yuseong)
p.v_yuseong<-pred_random_yuseong$predictions
p.v_yuseong<-ifelse(p.v_yuseong<=log(5),log(5),p.v_yuseong)
rmse(p.v_yuseong,log(test_yuseong$new_n))
0.5156177
obs_random_yuseong<-as.data.frame(cbind(log(test_yuseong$new_n),p.v_yuseong))
colnames(obs_random_yuseong)<-c("real","pred_random_yuseong")
library(ggplot2)
ggplot(data=obs_random_yuseong) + geom_point(mapping=aes(x=exp(real),y=exp(pred_random_yuseong)))+geom_abline(intercept= 0, slope=1, color='blue', size = 1.5)+ggtitle("daejeon_random_yuseong")+theme(plot.title = element_text(family = "serif", face = "bold", hjust = 0.5, size = 25))+xlab("obs")+ylab("pred")
ggsave("pred_randomforest_yuseong.jpg")

(import_yuseong<-random_yuseong$variable.importance)
day       temp   rainfall     wind_s     wind_d        hum       year      month 
378.00609  635.18446   56.41274  426.19320  271.35291  530.04762  769.17454  261.93155 
PM10       rain       time 
343.44258   12.96311 2906.90192 
import_yuseong<-sort(import_yuseong)
barplot(import_yuseong,horiz=TRUE,las=1,main="VI plot for RF_yuseong")


###jung###
set.seed(2019)
random_jung <- ranger(log(new_n)~.,data=train_jung,mtry=7,importance='impurity',write.forest=TRUE)
#test error
pred_random_jung <- predict(random_jung,data=test_jung)
p.v_jung<-pred_random_jung$predictions
p.v_jung<-ifelse(p.v_jung<=log(5),log(5),p.v_jung)
rmse(p.v_jung,log(test_jung$new_n))
0.4260065
obs_random_jung<-as.data.frame(cbind(log(test_jung$new_n),p.v_jung))
colnames(obs_random_jung)<-c("real","pred_random_jung")
library(ggplot2)
ggplot(data=obs_random_jung) + geom_point(mapping=aes(x=exp(real),y=exp(pred_random_jung)))+geom_abline(intercept= 0, slope=1, color='blue', size = 1.5)+ggtitle("daejeon_random_jung")+theme(plot.title = element_text(family = "serif", face = "bold", hjust = 0.5, size = 25))+xlab("obs")+ylab("pred")
ggsave("pred_randomforest_jung.jpg")

(import_jung<-random_jung$variable.importance)
day       temp   rainfall     wind_s     wind_d        hum       year      month 
221.56803  583.83548   41.46418  373.87826  417.62365  623.39529  192.00406  172.37536 
PM10       rain       time 
324.29245   15.57502 2255.32507 
import_jung<-sort(import_jung)
barplot(import_jung,horiz=TRUE,las=1,main="VI plot for RF_jung")
