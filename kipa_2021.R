library(dplyr)
library(ggplot2)
install.packages("foreign")
library(foreign)
mental <- read.spss("KIPA_DATA_2021.SAV")
class(mental)
mental <- as.data.frame(mental)
class(mental)
mental <-mental %>% select(q32_2,q1_4,q32_1,q34_1,q55,d17,d1,d2,ara) %>% rename(suicide=q32_2, satisfaction=q1_4, loneliness=q32_1, family_belief=q34_1, wealth=q55, health=d17, sex=d1, age=d2, area=ara)
str(mental)
table(mental$suicide)
table(mental$health)
table(mental$satisfaction)
table(mental$loneliness)
table(mental$family_belief)
table(mental$wealth)
#범주형 데이터를 정수형으로 변환
mental$suicide <- as.integer(mental$suicide)
mental$satisfaction <- as.integer(mental$satisfaction)
mental$loneliness <- as.integer(mental$loneliness)
mental$family_belief <- as.integer(mental$family_belief)
mental$wealth <- as.integer(mental$wealth)
mental$health <- as.integer(mental$health)
#변수 빈도 확인
table(mental$suicide)
table(mental$health)
table(mental$wealth)
table(mental$satisfaction)
table(mental$loneliness)
table(mental$family_belief)
#11점 척도로 조사된 2개 변수에서 1씩 차감
mental$satisfaction<-mental$satisfaction-1
mental$wealth<-mental$wealth-1
#변수 보정결과 확인
table(mental$wealth)
table(mental$satisfaction)
#age, sex, area 변수의 유형을 범주형에서 문자형으로 변경
mental$age<-as.character(mental$age)
mental$sex<-as.character(mental$sex)
mental$area<-as.character(mental$area)
#변경한 변수의 빈도 확인
table(mental$sex)
table(mental$age)
table(mental$area)
#19~29세로 되어있는 범주를 20대로 변경
mental$age<-ifelse(mental$age=="19~29세","20대",mental$age)
#변경 후 빈도 확인
table(mental$age)
#결측치와 이상치 확인
summary(mental)
#성별 빈도 분석
mental%>%group_by(sex)%>%summarise(n=n())%>%mutate(total=sum(n),pct=round(n/total*100,1))
#연령대별 빈도 분석
mental%>%group_by(age)%>%summarise(n=n())%>%mutate(total=sum(n),pct=round(n/total*100,1))
#성별과 연령대의 교차분석
table(mental$sex, mental$age)
#교차백분율 계산
round(prop.table(table(mental$sex,mental$age),1)*100,1)
#성별, 연령대, 만족도 교차분석
table(mental$sex, mental$age, mental$satisfaction)

chisq.test(mental$sex,mental$age)
#회귀분석에 사용할 6개변수의 평균 분석
mental%>%summarise(m1=mean(suicide),m2=mean(satisfaction),m3=mean(loneliness),m4=mean(family_belief),m5=mean(wealth),m6=mean(health))
#회귀분석:삶의 만족도와 외로움이 자살충동에 미치는 영향
RA<-lm(data=mental,suicide~satisfaction+loneliness)
summary(RA)
#상관분석:삶의 만족도와 외로움의 상관관계
cor.test(mental$satisfaction,mental$loneliness)
#회귀분석:가족신뢰도,경제안정도,건강상태가 삶의 만족도에 미치는 영향
install.packages("ztable")
library(ztable)
RA<-lm(data=mental,satisfaction~family_belief+wealth+health)
summary(RA)
options(ztable.type="viewer")
ztable(RA)
#회귀분석:가족신뢰도,경제안정도,건강상태가 외로움에 미치는 영향
RA<-lm(data=mental,loneliness~family_belief+wealth+health)
summary(RA)
options(ztable.type="viewer")
ztable(RA)
#회귀분석:가족신뢰도,경제안정도,건강상태가 자살충동에 미치는 영향
RA<-lm(data=mental,suicide~family_belief+wealth+health)
summary(RA)
options(ztable.type="viewer")
ztable(RA)
#독립표본t검정:성별 삶의 만족도 차이
t.test(data=mental,satisfaction~sex)
#성별 자살생각 차이
t.test(data=mental,suicide~sex)
#성별 외로움 차이
t.test(data=mental,loneliness~sex)
#연령대별 삶의 만족도 평균
mental%>%group_by(age)%>%summarise(m=mean(satisfaction))%>%arrange(desc(m))
#성별 삶의 만족도 평균
mental%>%group_by(sex)%>%summarise(m=mean(satisfaction))%>%arrange(desc(m))
#지역별 삶의 만족도 평균
mental%>%group_by(area)%>%summarise(m=mean(satisfaction))%>%arrange(desc(m))
#지역별 삶의 만족도 분석과 그래프 작성
area_satisfaction <- mental %>% group_by(area) %>% summarise(m=mean(satisfaction)) %>% arrange(desc(m))
ggplot(data=area_satisfaction, aes(x=reorder(area,m),y=m)) + geom_col() + ggtitle("지역별만족도") + xlab("지역") + ylab("만족도") + coord_flip()