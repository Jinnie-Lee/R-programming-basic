#excel 파일 불러오기
install.packages("readxl")
library(readxl)

data<-read_excel('R_report_data.xlsx', sheet=1)

head(data)
summary(data)


library(dplyr)

#C28 진학희망대학전공영역 피어슨 적합도 검정
major<- data %>% select(C28)

head(major)
str(major)
summary(major)

table(major$C28)

chisq.test(table(major$C28), p=c(1/11, 1/11, 1/11, 1/11, 1/11, 1/11, 1/11, 1/11, 1/11, 1/11, 1/11))

#C28 빈도 그래프 그리기
library(ggplot2)
ggplot(data=major, aes(x=C28))+geom_bar()


# C20, C25 부모의 경제활동과 학생성적 독립성 검정
eco_score<-data %>% select(C20, C25)

head(eco_score)
str(eco_score)
summary(eco_score)

table(eco_score$C25, eco_score$C20)

chisq.test(table(eco_score$C25, eco_score$C20))

#C20, C25 빈도 그래프 그리기
ggplot(data=eco_score, aes(x=C20))+geom_bar()
ggplot(data=eco_score, aes(x=C25))+geom_bar()


# C1, C5 지역과 방과후학교참여여부 독립성 검정
area_after<-data %>% select(C1, C5)

head(area_after)
str(area_after)
summary(area_after)

table(area_after$C5, area_after$C1)

chisq.test(table(area_after$C5, area_after$C1))

#C1, C5 빈도 그래프 그리기
ggplot(data=area_after, aes(x=C1))+geom_bar()
ggplot(data=area_after, aes(x=C5))+geom_bar()


#C3, C16 학교에 따른 사교육비총금액에 차이가 있는지 독립표본 t검정
library(lawstat)

sch_mon<-data %>% select(C3, C16)

#기초통계량 뽑기
sch_mon %>% group_by(C3) %>% 
  summarise(mean=mean(C16),
            sd=sd(C16),
            sum=sum(C16),
            median=median(C16),
            n=n())

#등분산검정하기
levene.test(sch_mon$C16, sch_mon$C3, location="mean")

t.test(data=sch_mon, C16~C3, var.equal=F)

#C3, C16 평균 막대그래프 그리기
temp<-sch_mon %>% group_by(C3) %>% 
  summarise(mean_C16=mean(C16))

ggplot(data=temp, aes(x=C3, y=mean_C16))+geom_col()


#C25, C6 부모 경제활동(부모모두참여, 부모모두미참여)에 따라 
#방과후학교참여시간 차이가 있는지 독립표본 t검정
eco_after<-data %>% filter(C25==3|C25==4) %>% 
  select(C25, C6)

#기초통계량 뽑기
eco_after %>% group_by(C25) %>% 
  summarise(mean=mean(C6),
            sd=sd(C6),
            sum=sum(C6),
            median=median(C6),
            n=n())

#등분산 검정하기
levene.test(eco_after$C6, eco_after$C25, location="mean")

t.test(data=eco_after, C6~C25, var.equal=F)

#C25, C6 평균 막대그래프 그리기
temp2<-eco_after %>% group_by(C25) %>% 
  summarise(mean_C6=mean(C6))

ggplot(data=temp2, aes(x=C25, y=mean_C6))+geom_col()


#C5, C20 방과후학교참여여부와 학생성적 독립성 검정
after_score<-data %>% select(C20, C5)

head(after_score)
str(after_score)
summary(after_score)

table(after_score$C5, after_score$C20)

chisq.test(table(after_score$C5, after_score$C20))

#C20, C5 빈도 그래프 그리기
ggplot(data=after_score, aes(x=C20))+geom_bar()
ggplot(data=area_after, aes(x=C5))+geom_bar()
