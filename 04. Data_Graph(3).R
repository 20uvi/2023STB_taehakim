# 도수분포표 작성
table(X2023_STB_survey_ $Gender)
# 상대도수분포표 작성
ECN <- table(X2023_STB_survey_ $Gender)
prop.table(ECN)
# 교차표 작성
table(X2023_STB_survey_ $Gender, X2023_STB_survey_ $Grade)
# 막대그래프 작성
barplot(table(X2023_STB_survey_ $Nationality))
# (가로) 막대그래프 작성
barplot(table(X2023_STB_survey_ $Nationality), horiz=T)
# 막대그래프 작성

# 파이차트 작성
pie(table(X2023_STB_survey_ $Grade))
# 히스토그램 작성
hist(X2023_STB_survey_$'Age', main="Age", col=terrain.colors(12))
# 박스 플롯
boxplot(X2023_STB_survey_$`Grade`, X2023_STB_survey_$`Age`, main="comparison", col="yellow", names = c("Grade","Age"))
# 산점도
plot(x=X2023_STB_survey_$`Grade`, y=X2023_STB_survey_$`Age`, xlab="Grade", ylab="Age", main="Grade and Age")