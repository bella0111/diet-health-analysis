## 1. 데이터 불러오기 및 편집
library(readxl)
library(dplyr)
library(ggplot2)
library(corrplot)
# 1-1. 데이터 불러오기(독립변수)
Eating_habits <- read_excel("./독립변수/2022_식습관_유형(10점기준).xlsx")
skip <- read_excel("./독립변수/2022_아침식사결식.xlsx")
check <- read_excel("./독립변수/2022_영양성분확인.xlsx")
drink <- read_excel("./독립변수/2022_음주빈도.xlsx")
over <- read_excel("./독립변수/2022_지방과잉섭취.xlsx")

# 1-2. 데이터 불러오기(종속변수)
Highblood <- read_excel('./종속변수/2022_고혈압.xlsx')
Diabetics <- read_excel('./종속변수/2022_당뇨병.xlsx')
Obese <- read_excel('./종속변수/2022_비만.xlsx')
Atopy <- read_excel('./종속변수/2022_아토피피부염.xlsx')
Hypertriglyceridemia <- read_excel('./종속변수/2022_고콜레스테롤혈증.xlsx')

#1-2. 60세 이상 데이터 삭제(노화로 인해 분석방해 예상)
#독립변수
Eating_habits <- Eating_habits %>% slice(-c(5, 6))

#종속변수
Highblood <- Highblood %>% slice(-c(5, 6))
Diabetics <- Diabetics %>% slice(-c(5, 6))
Obese <- Obese %>% slice(-c(5, 6))
Atopy <- Atopy %>% slice(-c(5, 6))
Hypertriglyceridemia <- Hypertriglyceridemia %>% slice(-c(5, 6))

#1-3. 나이열 값 맞춰주기
Eating_habits$나이 <- c("20대", "30대", "40대", "50대")
skip$나이 <- c("20대", "30대", "40대", "50대")
check$나이 <- c("20대", "30대", "40대", "50대")
drink$나이 <- c("20대", "30대", "40대", "50대")
over$나이 <- c("20대", "30대", "40대", "50대")
Highblood$나이 <- c("20대", "30대", "40대", "50대")
Diabetics$나이 <- c("20대", "30대", "40대", "50대")
Obese$나이 <- c("20대", "30대", "40대", "50대")
Atopy$나이 <- c("20대", "30대", "40대", "50대")
Hypertriglyceridemia$나이 <- c("20대", "30대", "40대", "50대")

#1-4 데이터프레임 합치기
all_df <- data.frame(Eating_habits$`저칼로리·저염식 선호`,
                     Eating_habits$`인스턴트 음식을 먹지 않으려 노력`,
                     Eating_habits$`편의식품 선호`,
                     skip$응답자수,
                     check$응답자수,
                     drink$`거의 안먹음`,
                     drink$`월 1-3회`,
                     drink$`주 1-2회`,
                     drink$`주 3-6회`,
                     drink$`일 1회 이상`,
                     over$응답자수)
colnames(all_df) <- c("저칼로리·저염식 선호", 
                      "인스턴트 음식을 먹지 않으려 노력", 
                      "편의식품 선호", 
                      "응답자수_skip", 
                      "응답자수_check", 
                      "음주 거의 안먹음", 
                      "음주 월 1-3회", 
                      "음주 주 1-2회", 
                      "음주 주 3-6회", 
                      "음주 일 1회 이상", 
                      "응답자수_over")

#1-5 환자 수 시각화
#고혈압
gg1 <- ggplot(data = Highblood, aes(x=나이, y=응답자수)) +
  geom_bar(stat = "identity", fill = "#01B1AE", width = 0.7) +
  geom_text(aes(label = 응답자수), vjust = -0.3) +
  labs(x = "나이", y = "고혈압(명)") +
  theme_minimal()+
  ggtitle("고혈압 환자 수")+
  theme(plot.title = element_text(hjust = 0.5))
gg1 + scale_x_discrete(limits = unique(Highblood$나이))

#당뇨
gg2 <- ggplot(data = Diabetics, aes(x=나이, y=응답자수)) +
  geom_bar(stat = "identity", fill = "#01B1AE", width = 0.7) +
  geom_text(aes(label = 응답자수), vjust = -0.3) +
  labs(x = "나이", y = "당뇨(명)") +
  theme_minimal()+
  ggtitle("당뇨 환자 수")+
  theme(plot.title = element_text(hjust = 0.5))
gg2 + scale_x_discrete(limits = unique(Diabetics$나이))

#비만
gg3 <- ggplot(data = Obese, aes(x=나이, y=응답자수)) +
  geom_bar(stat = "identity", fill = "#01B1AE", width = 0.7) +
  geom_text(aes(label = 응답자수), vjust = -0.3) +
  labs(x = "나이", y = "비만(명)") +
  theme_minimal()+
  ggtitle("비만 환자 수")+
  theme(plot.title = element_text(hjust = 0.5))
gg3 + scale_x_discrete(limits = unique(Obese$나이))

#아토피
gg4 <- ggplot(data = Atopy, aes(x=나이, y=응답자수)) +
  geom_bar(stat = "identity", fill = "#01B1AE", width = 0.7) +
  geom_text(aes(label = 응답자수), vjust = -0.3) +
  labs(x = "나이", y = "아토피(명)") +
  theme_minimal()+
  ggtitle("아토피 환자 수")+
  theme(plot.title = element_text(hjust = 0.5))
gg4 + scale_x_discrete(limits = unique(Atopy$나이))

#고콜레스테롤혈증 
gg5 <- ggplot(data = Hypertriglyceridemia, aes(x=나이, y=응답자수)) +
  geom_bar(stat = "identity", fill = "#01B1AE", width = 0.7) +
  geom_text(aes(label = 응답자수), vjust = -0.3) +
  labs(x = "나이", y = "고콜레스테롤혈증(명)") +
  theme_minimal()+
  ggtitle("고콜레스테롤혈증 환자 수")+
  theme(plot.title = element_text(hjust = 0.5))
gg5 + scale_x_discrete(limits = unique(Hypertriglyceridemia$나이))

# 2. 상관분석
#응답자수는 현재 질환을 가지고 있는 상태에서 응답한 환자의 수이므로 
#환자수를 예측하기 위해 상관분석을 하는 것이므로 
#결과로 나온 상관계수를 반대로 생각
#(응답 환자들은 질환으로 인해 특정 생활습관을 가지기 때문)
#모든 종속변수에 대해여 상관분석결과 기준을 #0.8~0.9로 설정

# 2-1. 고혈압과 식습관과의 상관분석
-cor(Highblood$응답자수, Eating_habits$`저칼로리·저염식 선호`)
-cor(Highblood$응답자수, Eating_habits$`인스턴트 음식을 먹지 않으려 노력`)
-cor(Highblood$응답자수, Eating_habits$`편의식품 선호`)
-cor(Highblood$응답자수, skip$응답자수)
-cor(Highblood$응답자수, check$응답자수)
-cor(Highblood$응답자수, drink$`거의 안먹음`)
-cor(Highblood$응답자수, drink$`월 1-3회`)
-cor(Highblood$응답자수, drink$`주 1-2회`)
-cor(Highblood$응답자수, drink$`주 3-6회`)
-cor(Highblood$응답자수, drink$`일 1회 이상`)
-cor(Highblood$응답자수, over$응답자수)
#-cor(Highblood$응답자수, all_df)
a=-cor(all_disease, all_df)
corrplot(a, method="num")
#결과: Eating_habits$`저칼로리·저염식 선호` ,Eating_habits$`인스턴트 음식을 먹지 않으려 노력`
#drink$`월 1-3회`, 


# 2-2. 당뇨병과 식습관과의 상관분석
-cor(Diabetics$응답자수, Eating_habits$`저칼로리·저염식 선호`)
-cor(Diabetics$응답자수, Eating_habits$`인스턴트 음식을 먹지 않으려 노력`)
-cor(Diabetics$응답자수, Eating_habits$`편의식품 선호`)
-cor(Diabetics$응답자수, skip$응답자수)
-cor(Diabetics$응답자수, check$응답자수)
-cor(Diabetics$응답자수, drink$`거의 안먹음`)
-cor(Diabetics$응답자수, drink$`월 1-3회`)
-cor(Diabetics$응답자수, drink$`주 1-2회`)
-cor(Diabetics$응답자수, drink$`주 3-6회`)
-cor(Diabetics$응답자수, drink$`일 1회 이상`)
-cor(Diabetics$응답자수, over$응답자수)
#-cor(Diabetics$응답자수, all_df)
#결과: Eating_habits$`저칼로리·저염식 선호`, Eating_habits$`인스턴트 음식을 먹지 않으려 노력`
#drink$`월 1-3회`


# 2-3. 비만과 식습관과의 상관분석
-cor(Obese$응답자수, Eating_habits$`저칼로리·저염식 선호`)
-cor(Obese$응답자수, Eating_habits$`인스턴트 음식을 먹지 않으려 노력`)
-cor(Obese$응답자수, Eating_habits$`편의식품 선호`)
-cor(Obese$응답자수, skip$응답자수)
-cor(Obese$응답자수, check$응답자수)
-cor(Obese$응답자수, drink$`거의 안먹음`)
-cor(Obese$응답자수, drink$`월 1-3회`)
-cor(Obese$응답자수, drink$`주 1-2회`)
-cor(Obese$응답자수, drink$`주 3-6회`)
-cor(Obese$응답자수, drink$`일 1회 이상`)
-cor(Obese$응답자수, over$응답자수)
#-cor(Obese$응답자수, all_df)

#결과: Eating_habits$`저칼로리·저염식 선호`, Eating_habits$`인스턴트 음식을 먹지 않으려 노력`
#drink$`월 1-3회`


# 2-4. 고콜레스테롤혈증과 식습관과의 상관분석
-cor(Hypertriglyceridemia$응답자수, Eating_habits$`저칼로리·저염식 선호`)
-cor(Hypertriglyceridemia$응답자수, Eating_habits$`인스턴트 음식을 먹지 않으려 노력`)
-cor(Hypertriglyceridemia$응답자수, Eating_habits$`편의식품 선호`)
-cor(Hypertriglyceridemia$응답자수, skip$응답자수)
-cor(Hypertriglyceridemia$응답자수, check$응답자수)
-cor(Hypertriglyceridemia$응답자수, drink$`거의 안먹음`)
-cor(Hypertriglyceridemia$응답자수, drink$`월 1-3회`)
-cor(Hypertriglyceridemia$응답자수, drink$`주 1-2회`)
-cor(Hypertriglyceridemia$응답자수, drink$`주 3-6회`)
-cor(Hypertriglyceridemia$응답자수, drink$`일 1회 이상`)
-cor(Hypertriglyceridemia$응답자수, over$응답자수)
#-cor(Hypertriglyceridemia$응답자수, all_df)

#결과: Eating_habits$`저칼로리·저염식 선호`, Eating_habits$`인스턴트 음식을 먹지 않으려 노력`
#drink$`월 1-3회`


# 2-5. 아토피피부염과 식습관과의 상관분석
-cor(Atopy$응답자수, Eating_habits$`저칼로리·저염식 선호`)
-cor(Atopy$응답자수, Eating_habits$`인스턴트 음식을 먹지 않으려 노력`)
-cor(Atopy$응답자수, Eating_habits$`편의식품 선호`)
-cor(Atopy$응답자수, skip$응답자수)
-cor(Atopy$응답자수, check$응답자수)
-cor(Atopy$응답자수, drink$`거의 안먹음`)
-cor(Atopy$응답자수, drink$`월 1-3회`)
-cor(Atopy$응답자수, drink$`주 1-2회`)
-cor(Atopy$응답자수, drink$`주 3-6회`)
-cor(Atopy$응답자수, drink$`일 1회 이상`)
-cor(Atopy$응답자수, over$응답자수)
#-cor(Atopy$응답자수, all_df)


#결과: Eating_habits$`저칼로리·저염식 선호`, Eating_habits$`인스턴트 음식을 먹지 않으려 노력`
#drink$`월 1-3회`
#모든 종속변수에 대해여 상관분석결과 기준을 #0.8~0.9로 설정 했을 때, 
#저칼로리·저염식 선호, 인스턴트 음식을 먹지 않으려 노력, 음주 월 1-3회
#항목이 상관관계가 높은 것을 알 수 있다.

#전체상관분석 시각화
a=-cor(all_disease, all_df)
corrplot(a, method="num")


#0.8~0.9로 기준 설정
cor_0809<-cbind(Eating_habits$`저칼로리·저염식 선호` ,
                  Eating_habits$`인스턴트 음식을 먹지 않으려 노력` ,
                  drink$`월 1-3회` )

cor_0809_df <- data.frame(cor_0809)
colnames(cor_0809_df) <- c("저칼로리저염식 선호", "인스턴트 제외 노력", "음주 월1-3회")

#질병 환자수 데이터프레임 합치기
all_disease<-data.frame(Highblood$응답자수,
                   Diabetics$응답자수,
                   Obese$응답자수,
                   Atopy$응답자수,
                   Hypertriglyceridemia$응답자수)
colnames(all_disease)<-c("고혈압","당뇨","비만","아토피","고콜레스테롤혈증")
#상관분석 결과 기준 내 항목과 질병 환자수 상관분석 시각화
result=-cor(cor_0809_df, all_disease)
corrplot(result, method="num")

#질병 환자수와 식습관 데이터 합치기
all<-data.frame(all_df,
                all_disease)

# 3. 회귀분석
#상관계수가 0.8~0.9인 항목만 회귀분석

#3-1. 고혈압과 식습관과의 회귀분석
summary(lm(all_disease$고혈압 ~ cor_0809_df$`저칼로리저염식 선호`))
summary(lm(all_disease$고혈압 ~ cor_0809_df$`인스턴트 제외 노력`))
summary(lm(all_disease$고혈압 ~ cor_0809_df$`음주 월1-3회`))
#3-1-1 회귀분석 시각화
#메모장에 써놓은 이유로 인해, 반대로 생각
plot(cor_0809_df$`저칼로리저염식 선호`, all_disease$고혈압, 
     xlab='저칼로리저염식선호', 
     ylab='고혈압 환자수', 
     main='고혈압과 저칼로리저염식선호 습관 간의 관계')
abline(lm(all_disease$고혈압~ cor_0809_df$`저칼로리저염식 선호`), col='red', lwd=2)


#3-2. 당뇨과 식습관과의 회귀분석
summary(lm(all_disease$당뇨 ~ cor_0809_df$`저칼로리저염식 선호`))
summary(lm(all_disease$당뇨 ~ cor_0809_df$`인스턴트 제외 노력`))
summary(lm(all_disease$당뇨 ~ cor_0809_df$`음주 월1-3회`))


#3-2-1.회귀분석 시각화
plot(cor_0809_df$`저칼로리저염식 선호`, all_disease$당뇨, 
     xlab='저칼로리저염식선호', 
     ylab='당뇨 환자수', 
     main='당뇨와 저칼로리저염식선호 습관 간의 관계')
abline(lm(all_disease$당뇨~ cor_0809_df$`저칼로리저염식 선호`), col='red', lwd=2)

plot(cor_0809_df$`인스턴트 제외 노력`, all_disease$당뇨, 
     xlab='인스턴트 제외 노력', 
     ylab='당뇨 환자수', 
     main='당뇨와 인스턴트 제외 노력 간의 관계')
abline(lm(all_disease$당뇨~ cor_0809_df$`인스턴트 제외 노력`), col='red', lwd=2)

plot(cor_0809_df$`음주 월1-3회`, all_disease$당뇨, 
     xlab='음주 월1-3회', 
     ylab='당뇨 환자수', 
     main='당뇨와 음주 월1-3회 간의 관계')
abline(lm(all_disease$당뇨~ cor_0809_df$`음주 월1-3회`), col='red', lwd=2)


#3-3. 비만과 식습관과의 회귀분석
summary(lm(all_disease$비만 ~ cor_0809_df$`저칼로리저염식 선호`))
summary(lm(all_disease$비만 ~ cor_0809_df$`인스턴트 제외 노력`))
summary(lm(all_disease$비만 ~ cor_0809_df$`음주 월1-3회`))
#3-3-1.회귀분석 시각화
plot(cor_0809_df$`저칼로리저염식 선호`, all_disease$비만, 
     xlab='저칼로리저염식선호', 
     ylab='비만 환자수', 
     main='비만과 저칼로리저염식선호 습관 간의 관계')
abline(lm(all_disease$비만~ cor_0809_df$`저칼로리저염식 선호`), col='red', lwd=2)

plot(cor_0809_df$`인스턴트 제외 노력`, all_disease$비만, 
     xlab='인스턴트 제외 노력', 
     ylab='비만 환자수', 
     main='비만과 인스턴트 제외 노력 간의 관계')
abline(lm(all_disease$비만~ cor_0809_df$`인스턴트 제외 노력`), col='red', lwd=2)

plot(cor_0809_df$`음주 월1-3회`, all_disease$비만, 
     xlab='음주 월1-3회', 
     ylab='비만 환자수', 
     main='비만과 음주 월1-3회 간의 관계')
abline(lm(all_disease$비만~ cor_0809_df$`음주 월1-3회`), col='red', lwd=2)

#3-4. 아토피와 식습관과의 회귀분석
summary(lm(all_disease$아토피 ~ cor_0809_df$`저칼로리저염식 선호`))
summary(lm(all_disease$아토피 ~ cor_0809_df$`인스턴트 제외 노력`))
summary(lm(all_disease$아토피 ~ cor_0809_df$`음주 월1-3회`))
#3-4-1.회귀분석 시각화
plot(cor_0809_df$`저칼로리저염식 선호`, all_disease$아토피, 
     xlab='저칼로리저염식선호', 
     ylab='아토피 환자수', 
     main='아토피와 저칼로리저염식선호 습관 간의 관계')
abline(lm(all_disease$아토피~ cor_0809_df$`저칼로리저염식 선호`), col='red', lwd=2)

plot(cor_0809_df$`인스턴트 제외 노력`, all_disease$아토피, 
     xlab='인스턴트 제외 노력', 
     ylab='아토피 환자수', 
     main='아토피와 인스턴트 제외 노력 간의 관계')
abline(lm(all_disease$아토피~ cor_0809_df$`인스턴트 제외 노력`), col='red', lwd=2)

plot(cor_0809_df$`음주 월1-3회`, all_disease$아토피, 
     xlab='음주 월1-3회', 
     ylab='아토피 환자수', 
     main='아토피와 음주 월1-3회 간의 관계')
abline(lm(all_disease$아토피~ cor_0809_df$`음주 월1-3회`), col='red', lwd=2)

#3-5. 고콜레스테롤혈증과 식습관과의 회귀분석
summary(lm(all_disease$고콜레스테롤혈증 ~ cor_0809_df$`저칼로리저염식 선호`))
summary(lm(all_disease$고콜레스테롤혈증 ~ cor_0809_df$`인스턴트 제외 노력`))
summary(lm(all_disease$고콜레스테롤혈증 ~ cor_0809_df$`음주 월1-3회`))
#3-5-1.회귀분석 시각화
plot(cor_0809_df$`저칼로리저염식 선호`, all_disease$고콜레스테롤혈증, 
     xlab='저칼로리저염식선호', 
     ylab='고콜레스테롤혈증 환자수', 
     main='고콜레스테롤혈증과 저칼로리저염식선호 습관 간의 관계')
abline(lm(all_disease$고콜레스테롤혈증~ cor_0809_df$`저칼로리저염식 선호`), col='red', lwd=2)

plot(cor_0809_df$`인스턴트 제외 노력`, all_disease$고콜레스테롤혈증, 
     xlab='인스턴트 제외 노력', 
     ylab='고콜레스테롤혈증 환자수', 
     main='고콜레스테롤혈증과 인스턴트 제외 노력 간의 관계')
abline(lm(all_disease$고콜레스테롤혈증~ cor_0809_df$`인스턴트 제외 노력`), col='red', lwd=2)

plot(cor_0809_df$`음주 월1-3회`, all_disease$고콜레스테롤혈증, 
     xlab='음주 월1-3회', 
     ylab='고콜레스테롤혈증 환자수', 
     main='고콜레스테롤혈증과 음주 월1-3회 간의 관계')
abline(lm(all_disease$고콜레스테롤혈증~ cor_0809_df$`음주 월1-3회`), col='red', lwd=2)

#3-3. 비만과 식습관과의 회귀분석
summary(lm(all_disease$비만 ~ cor_0809_df$`저칼로리저염식 선호`))
summary(lm(all_disease$비만 ~ cor_0809_df$`인스턴트 제외 노력`))
summary(lm(all_disease$비만 ~ cor_0809_df$`음주 월1-3회`))
#3-4. 아토피과 식습관과의 회귀분석
summary(lm(all_disease$아토피 ~ cor_0809_df$`저칼로리저염식 선호`))
summary(lm(all_disease$아토피 ~ cor_0809_df$`인스턴트 제외 노력`))
summary(lm(all_disease$아토피 ~ cor_0809_df$`음주 월1-3회`))
#3-5. 고콜레스테롤혈증과 식습관과의 회귀분석
summary(lm(all_disease$고콜레스테롤혈증 ~ cor_0809_df$`저칼로리저염식 선호`))
summary(lm(all_disease$고콜레스테롤혈증 ~ cor_0809_df$`인스턴트 제외 노력`))
summary(lm(all_disease$고콜레스테롤혈증 ~ cor_0809_df$`음주 월1-3회`))

#3-6 다중회귀분석 및 환자수 예측
model1 <- lm(formula = 고혈압 ~ 저칼로리.저염식.선호 + 편의식품.선호, data=all)
predict(model1)
summary(model1)


