setwd("C:/Users/a4985/OneDrive/desktop/taxi/daejeon")
######daejeon_전처리#####
daejeoncall <- read.csv("daejeoncall.csv")

###구나누기####
#유성구  133
#중구/서구  642
#동구  643
#대덕구  648

#########################################################################
######날씨#####
w1<-read.csv("2016.csv")
colnames(w1)<-c("지점","일시","기온","강수량","풍속","풍향","습도")
w2<-read.csv("2017.csv")
colnames(w2)<-c("지점","일시","기온","강수량","풍속","풍향","습도")
w3<-read.csv("2018.csv")
colnames(w3)<-c("지점","일시","기온","강수량","풍속","풍향","습도")
w11<-read.csv("2016_2.csv")
colnames(w11)<-c("지점","일시","기온","풍향","풍속","강수량","습도")
w22<-read.csv("2017_2.csv")
colnames(w22)<-c("지점","일시","기온","풍향","풍속","강수량","습도")
w33<-read.csv("2018_2.csv")
colnames(w33)<-c("지점","일시","기온","풍향","풍속","강수량","습도")
daejeon_w<-rbind(w1,w2,w3,w11,w22,w33)
write.csv(daejeon_w,"daejeon_w.csv")
#######################################################################

daejeoncall <- daejeoncall[,-1]
daejeon_w <- read.csv("daejeon_w.csv")
daejeon_w <- daejeon_w[,-1]

daejeoncall$기준년월일 <- as.character(daejeoncall$기준년월일)
daejeoncall$기준년월일 <- as.Date(daejeoncall$기준년월일,"%Y%m%d")
daejeoncall$time2 <- format(daejeoncall$time2, format="%H:%M")
daejeoncall$날짜 <- paste(daejeoncall$기준년월일,daejeoncall$time2,sep=" ")
daejeoncall$날짜 <- as.POSIXct(daejeoncall$날짜, format="%Y-%m-%d %H:%M")
summary(daejeoncall)

#지점코드를 콜택시에 넣어주기
daejeoncall$지점코드 <- ifelse(daejeoncall$발신지_시군구=="유성구",133,
                         ifelse(daejeoncall$발신지_시군구=="중구"|daejeoncall$발신지_시군구=="서구",642,
                                  ifelse(daejeoncall$발신지_시군구=="동구",643,
                                ifelse(daejeoncall$발신지_시군구=="대덕구",648,0))))

###동지우고 건수합하기###
library(dplyr)
daejeoncall <- daejeoncall %>% 
  group_by(기준년월일,요일,시간대,발신지_시도,발신지_시군구,time2,지점코드,날짜) %>%
  summarise(new_n=sum(통화건수))

daejeon_w$일시 <- as.POSIXct(daejeon_w$일시,format="%Y-%m-%d %H:%M") 

#합치기(날씨&콜택시)
daejeon <- merge(daejeoncall,daejeon_w,by.x=c("날짜","지점코드"),by.y=c("일시","지점"),all.x=TRUE)
daejeon$year <- lubridate::year(daejeon$기준년월일) 
daejeon$month <- lubridate::month(daejeon$기준년월일) 
daejeon$day <- lubridate::day(daejeon$기준년월일) 
daejeon$기준년월일 <- as.Date(daejeon$기준년월일,"%Y-%m-%d")

#holiday 부르기
holiday <- read.csv("holiday.csv")
holiday$X <- as.Date(holiday$X,"%Y-%m-%d")
holiday<-holiday[,-4]

#holiday랑 대전 합치기
daejeon <- merge(daejeon,holiday,by.x = '기준년월일',by.y='X',all.x=TRUE)

#daejeon_t랑 대기오염 합치기
library(readxl)
air <- read_excel("air.xlsx")

air$일시 <- as.character(air$일시)
air$일시 <- as.Date(air$일시,"%Y%m%d")
air$구 <- as.character(air$구)
air$시군구 <- air$구
air$시군구 <- as.factor(air$시군구)
air <- air[,-2]

summary(daejeon_w)# 기온 29개 NA
daejeon2 <- daejeon[,-c(3,6,8)]

daejeon_t <- merge(daejeon2,air,by.x=c('발신지_시군구','기준년월일'),by.y = c('시군구','일시'),all.x=TRUE)
colnames(daejeon_t)
daejeon_t<-daejeon_t[,-14]

####강수량 결측치 처리 및 RAIN 변수 생성
daejeon_t$강수량[is.na(daejeon_t$강수량)]<-0
daejeon_t$rain <- ifelse(daejeon_t$강수량>0,1,0)
summary(daejeon_t) #기온 131개 NA ## WHY???

write.csv(daejeon_t,"daejeon_t.csv")


####결측치 채우기##########################################################
library(mice)
library(randomForest)
#miceMod<-mice(daejeon_t[,!names(daejeon_t) %in% "new_n"],method="rf")
#miceMod$predictorMatrix
#miceOutput<-complete(miceMod)
#write.csv(miceOutput,"miceMod.csv")
##########################################################################


#####전처리 start#####
setwd("C:/Users/a4985/OneDrive/desktop/taxi/daejeon")
daejeon_t<-read.csv("daejeon_t.csv")
head(daejeon_t)
daejeon_t<-daejeon_t[,-c(1)]
daejeon_t<-daejeon_t[,-2]#기준년월일
daejeon_t<-daejeon_t[,-2]#날짜
daejeon_t$요일<-as.factor(daejeon_t$요일)
daejeon_t$발신지_시군구<-as.factor(daejeon_t$발신지_시군구)
daejeon_t$rain<-as.factor(daejeon_t$rain)
daejeon_t$holiday<-as.factor(daejeon_t$holiday)
daejeon_t$holiday1<-as.factor(daejeon_t$holiday1)

summary(daejeon_t)

####시간대 확인해보기
daejeon_t$시간대<-as.factor(daejeon_t$시간대)

library(ggplot2)
mean_n <- daejeon_t %>%
  group_by(시간대) %>%
  summarise(mean = mean(new_n))
median_n <- daejeon_t %>%
  group_by(시간대) %>%
  summarise(median = median(new_n))

plot(median_n)
abline(h=40)
abline(h=30)
abline(h=20)

median_n$median
median_n$시간대[which(median_n$median>=40)]
median_n$시간대[which(median_n$median>=30 & median_n$median<40)]
median_n$시간대[which(median_n$median>=20 & median_n$median<30)]
median_n$시간대[which(median_n$median<20)]


ggplot(data=daejeon_t)+
  geom_bar(mapping=aes(x=시간대,y=mean_n))
ggplot(data=daejeon_t)+
  geom_boxplot(mapping=aes(x=시간대,y=new_n))

##group1 : 8,9,10,18
##group2 : 7,11:19
##group3 : 6,20:23
##group4 : 0:5

###시간대 group화###
daejeon_t$시간대<-as.factor(daejeon_t$시간대)
daejeon_t$time<-ifelse(daejeon_t$시간대 %in% c(8,9,10,18),1,
                       ifelse(daejeon_t$시간대 %in% c(17,11:19),2,
                              ifelse(daejeon_t$시간대 %in% c(6,20:23),3,4)))
daejeon_t$time<-as.factor(daejeon_t$time)
daejeon_t<-daejeon_t[,-3] #시간대 삭제

miceOutput<-read.csv("miceMod.csv")
miceOutput$new_n<-daejeon_t$new_n
daejeon_t<-miceOutput[,-c(1,4)]
write.csv(daejeon_t,"daejeon_t_final.csv")

###############최종모델로 전처리#####
setwd("C:/Users/a4985/OneDrive/desktop/taxi/daejeon")
daejeon_t<-read.csv("daejeon_t_final.csv")
head(daejeon_t)
daejeon_t<-daejeon_t[,-1]
daejeon_t$day<-as.factor(daejeon_t$day)
daejeon_t$month<-as.factor(daejeon_t$month)
daejeon_t$gu<-as.factor(daejeon_t$gu)
daejeon_t$rain<-as.factor(daejeon_t$rain)
daejeon_t$holiday<-as.factor(daejeon_t$holiday)
daejeon_t$holiday1<-as.factor(daejeon_t$holiday1)
daejeon_t$time<-as.factor(daejeon_t$time)
daejeon_t<-daejeon_t[,-c(10,11)]  #holiday 삭제
summary(daejeon_t)

library(dplyr)
train <- daejeon_t %>% filter(year %in% c("2016","2017"))
test<-daejeon_t %>% filter(year=="2018")
head(train)

###TODO : 결측치 채우기
(86501-69132)/86501
train<-na.omit(train)
(41832-33000)/41832
test<-na.omit(test)

############구별로 나누기###############
summary(daejeon_t$gu)
####daedeok####
daejeon_daedeok<-daejeon_t %>%
  filter(gu=="daedeok")
summary(daejeon_daedeok)
daejeon_daedeok<-na.omit(daejeon_daedeok)
daejeon_daedeok<-daejeon_daedeok[,-1] #발신지시군구 제거
train_daedeok<- daejeon_daedeok %>% filter(year %in% c("2016","2017"))
test_daedeok<-daejeon_daedeok %>% filter(year=="2018")

####yuseong####
daejeon_yuseong<-daejeon_t %>%
  filter(gu=="yuseong")
summary(daejeon_yuseong)
daejeon_yuseong<-na.omit(daejeon_yuseong)
daejeon_yuseong<-daejeon_yuseong[,-1]
train_yuseong<- daejeon_yuseong %>% filter(year %in% c("2016","2017"))
test_yuseong<-daejeon_yuseong %>% filter(year=="2018")

####dong####
daejeon_dong<-daejeon_t %>%
  filter(gu=="dong")
summary(daejeon_dong)
daejeon_dong<-na.omit(daejeon_dong)
daejeon_dong<-daejeon_dong[,-1]
train_dong<- daejeon_dong %>% filter(year %in% c("2016","2017"))
test_dong<-daejeon_dong %>% filter(year=="2018")

####seo####
daejeon_seo<-daejeon_t %>%
  filter(gu=="seo")
summary(daejeon_seo)
daejeon_seo<-na.omit(daejeon_seo)
daejeon_seo<-daejeon_seo[,-1]
train_seo<- daejeon_seo %>% filter(year %in% c("2016","2017"))
test_seo<-daejeon_seo %>% filter(year=="2018")

####jung####
daejeon_jung<-daejeon_t %>%
  filter(gu=="jung")
summary(daejeon_jung)
daejeon_jung<-na.omit(daejeon_jung)
daejeon_jung<-daejeon_jung[,-1]
train_jung<- daejeon_jung %>% filter(year %in% c("2016","2017"))
test_jung<-daejeon_jung %>% filter(year=="2018")


#####TODO : quantile_regression
summary(train)


