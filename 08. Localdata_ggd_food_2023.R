# 실습에 필요한 library 등록
library(dplyr)
library(ggplot2)
# csv 데이터 불러오기
foodshop <- read.csv("ggd_food_2023.csv", na="", stringsAsFactors = F)
# 데이터 구조 확인
str(foodshop)
# 6개의 변수 추출 후 이름 변경
foodshop <- foodshop %>% rename(open_date=인허가일자, status=상세영업상태명, close_date=폐업일자, name=사업장명, type=업태구분명, address=소재지전체주소) %>% select("name","type","status","open_date","close_date", "address")
# 데이터 구조 확인
str(foodshop)
#날짜 데이터 형식 변경
foodshop$open_date <- gsub("-","",foodshop$open_date)
foodshop$close_date <- gsub("-","",foodshop$close_date)
#문자형 데이터를 정수형 데이터로 변환
foodshop$open_date <- as.integer(foodshop$open_date)
foodshop$close_date <- as.integer(foodshop$close_date)
#데이터 구조 확인
str(foodshop)
#status 변수 확인
table(foodshop$status)
#영업/폐업이 아닌 데이터 제외
foodshop <- foodshop %>% filter(status == '영업' | status == '폐업') %>% select(name,type,status,open_date,close_date,address)
#처리결과 확인
table(foodshop$status)
#type 변수 확인
table(foodshop$type)
#open_date 변수의 range 확인
range(foodshop$open_date)
#NA값 제외
foodshop <- foodshop %>% filter(open_date != '') %>% select(name,type,status,open_date,close_date,address)
#처리결과 확인
range(foodshop$open_date)
#결측치 확인
table(is.na(foodshop$open_date))
#인허가년도 변수 생성
foodshop$open_year <- substr(foodshop$open_date,1,4)
#close_date 변수의 range 확인인
range(foodshop$close_date, na.rm = T)
#폐업년도 변수 생성
foodshop$close_year <- substr(foodshop$close_date,1,4)
#시정보 분리
foodshop$district <- substr(foodshop$address,5,8)
#이상치 확인
table(foodshop$district)
#이상치 제거
foodshop$district <- ifelse(foodshop$district%in%c("영암군","106호","가평군","번지","시 강남","시 계양","시 관악","시 금천","시 남동","시 노원","시 마포","시 미추","시 서구","시 용산","시 은평","양평군","여주군","연천군","회"),NA,foodshop$district)
#데이터 확인
table(foodshop$district)
#최종 데이터 확인
str(foodshop)
#정수형으로 변경
foodshop$open_year <- as.integer(foodshop$open_year)
foodshop$close_year <- as.integer(foodshop$close_year)
#최종 확인
str(foodshop)

#가장 오래 영업중인 음식점
foodshop %>% filter(!is.na(open_date)&status=="영업") %>% filter(open_date==min(open_date)) %>%  select(name, type, open_date, address)
#업종별 가장 오래 영업중인 음식점
foodshop %>% filter(!is.na(open_date)&status=="영업") %>% filter(type%in%c("기타","경양식","분식","한식","뷔페식","일식","호프/통닭","횟집")) %>% group_by(type) %>% filter(open_date==min(open_date)) %>% select(name, type, open_date, address)
#업종별 개업 비율
foodshop %>% filter(!is.na(open_date)&!is.na(type)&!is.na(district)) %>% group_by(type) %>% summarise(n=n()) %>% mutate(total=sum(n),pct=round(n/total*100,1)) %>% arrange(desc(n)) %>% head(10)
#영업 중인 음식점의 업종별 비율
foodshop %>% filter(!is.na(open_date)&!is.na(type)&!is.na(district)) %>% filter(status=="영업") %>% group_by(type) %>% summarise(n=n()) %>% mutate(total=sum(n),pct=round(n/total*100,1)) %>% arrange(desc(n)) %>% head(5)
#전체 음식점의 영업과 폐업 비율
foodshop %>% filter(!is.na(open_date)&!is.na(type)&!is.na(district)) %>% group_by(status) %>% summarise(n=n()) %>% mutate(total=sum(n),pct=round(n/total*100,1)) 
#주요 업종별 영업과 폐업 비율
foodshop %>% filter(!is.na(open_date)&!is.na(type)&!is.na(district)) %>% filter(type%in%c("기타","경양식","분식","한식","뷔페식","일식","호프/통닭","횟집")) %>% group_by(type,status) %>% summarise(n=n()) %>% mutate(total=sum(n),pct=round(n/total*100,1)) %>% filter(status=="영업") %>% arrange(desc(n))
#개업이 많았던 연도
foodshop %>% filter(!is.na(open_date)&!is.na(district)) %>% group_by(open_year) %>% summarise(n=n()) %>% arrange(desc(n)) %>% head(5)
#폐업이 많았던 연도
foodshop %>% filter(!is.na(close_date)&!is.na(district)) %>% group_by(close_year) %>% summarise(n=n()) %>% arrange(desc(n)) %>% head(5)
#연도별 개업 음식점 수 그래프
open_trend <- foodshop %>% filter(!is.na(open_date)&!is.na(district)) %>% group_by(open_year) %>% summarise(open_n=n())
str(open_trend)
ggplot(data=open_trend,aes(x=open_year,y=open_n))+ geom_col()+ xlab("연도") + ylab("개업수")
#연도별 폐업 음식점 수 그래프
close_trend <- foodshop %>% filter(!is.na(open_date)&!is.na(district)) %>% group_by(close_year) %>% summarise(close_n=n())
str(close_trend)
ggplot(data=close_trend,aes(x=close_year,y=close_n))+ geom_col()+ xlab("연도") + ylab("폐업수")
#개업과 폐업 음식점 통합 그래프
open_trend1<-rename(open_trend,year=open_year)
close_trend1<-rename(close_trend,year=close_year) 
open_close_trend<-left_join(open_trend1,close_trend1,by="year")
ggplot() + geom_line(data=open_close_trend, aes(year,open_n)) + geom_line(data=open_close_trend, aes(year,close_n,color="red")) + xlab("연도") + ylab("개수")
#폐업음식점 수가 개업음식점 수보다 많았던 기간 확인
open_close_trend %>% filter(close_n>open_n)
#영업중인 음식점 수가 가장 많은 5개 시
district_business <- foodshop %>% filter(!is.na(open_date)&!is.na(district)&status=="영업") %>% group_by(district) %>% summarise(n=n())
district_business %>% arrange(desc(n)) %>% head(5)
#31개 시(3개 군 포함)의 음식점 수 막대그래프
ggplot(data = district_business, aes(x=reorder(district,n),y=n)) + geom_col() + coord_flip() + xlab("영업시") + ylab("영업 음식점 수")
#주요 업종별로 영업하는 음식점이 많은 시
foodshop %>% filter(!is.na(open_date)&!is.na(district)) %>% filter(type%in%c("기타","경양식","분식","한식","뷔페식","일식","호프/통닭","횟집")) %>% filter(status=="영업") %>% group_by(type,district) %>% summarise(n=n()) %>% mutate(total=sum(n),pct=round(n/total*100,1)) %>% group_by(type) %>% filter(pct==max(pct))