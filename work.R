
# 개인실습 #1

# 1. (2015년 10차 한국복지 패널조사) 조사설계서-가구용(beta1).xlsx 파일과
#    (2015년 10차 한국복지 패널조사) 조사설계서-가구원용(beta1).xlsx 파일 내용을
#    분석하여 문제정의 5개를 작성한다.
# 2. 각각의 문제 정의를 결정하여 작성
# 3. 각 문제 정의에 대한 변수 검토 및 전처리 작성
# 4. 변수 간 관계 분석을 수행한 후
# 5. Excel을 이용하여 각 문제에 대한 문제 정의, 변수 검토, 전처리 대상, 분석 결과를
#    시트 하나당 한 문제로 작성한다ㅣ


library(dplyr)
library(ggplot2)
library(readxl)
library(foreign)



raw_data <- read.spss(file = "D:/big/190617/h10_2015.sav", to.data.frame = TRUE)
raw_data

data <- raw_data
str(data)
dim(data)
View(data)
summary(data)
head(data)


data <- rename(data,
               code_area = h10_reg7,
               famsize = h1001_1,
               sex = h1001_4,
               birth = h1001_5,
               edu = h1001_6,
               marriage = h1001_11,
               religion = h1001_12,
               code_job = h1003_8,
               homebuy = h1006_3,
               tele = h1007_5aq4,
               card_debt = h1009_aq3)
View(data)

(da <- select(data, c("code_area",
                      "famsize",
                      "sex",
                      "birth",
                      "edu",
                      "marriage",
                      "religion",
                      "code_job",
                      "homebuy",
                      "tele",
                      "card_debt")))
da
str(da)
dim(da)
View(da)



# 7개 권역별 지역구분 _ area
class(da$code_area)
table(da$code_area)

(list_area <- data.frame(code_area = c(1:7),
                         area = c("서울", "수도권(인천/경기)", "부산/경남/울산", "대구/경북",
                                  "대전/충남", "강원/충북", "광주/전남/전북/제주도")))

da <- left_join(da, list_area, id = "code_area")
head(da)



#  가구원수 _ famsize
class(da$famsize)
table(da$famsize)



# 성별 _ sex
class(da$sex)
table(da$sex)

da$sex <- ifelse(da$sex == 1, "male", "female")
table(da$sex)
qplot(da$sex)



# 나이, 연령대 _ age, ages
class(da$birth)
table(da$birth)
da$birth <- ifelse(da$birth == 9999, NA, da$birth)
table(is.na(da$birth))

da$age <- 2014 - da$birth + 1
table(da$age)
summary(da$age)
qplot(da$age)

da$ages <- ifelse(da$age < 30, "young",
                  ifelse(da$age <= 59, "middle", "old"))
table(da$ages)



# 교육수준 _ edu
class(da$edu)
table(da$edu)
da$edu <- ifelse(da$edu == 99, NA, da$edu)
table(da$edu)
qplot(da$edu)

da$edu <- ifelse(da$edu < 5, "low",
                 ifelse(da$edu <= 7, "mid", "high"))
table(da$edu)
qplot(da$edu)



# 혼인상태 _ marriage
class(da$marriage)
table(da$marriage)
da$group_marriage <- ifelse(da$marriage == 1, "marriage",
                            ifelse(da$marriage == 3, "divorce", "NA"))
table(da$group_marriage)
table(is.na(da$group_marriage))
qplot(da$group_marriage)



# 종교 _ religion
class(da$religion)
table(da$religion)
da$religion <- ifelse(da$religion == 9, NA, da$religion)
table(da$religion)

da$religion <- ifelse(da$religion == 1, "yes", "no")
table(da$religion)
qplot(da$religion)



# 직종 _ job
class(da$code_job)
table(da$code_job)
da$code_job <- ifelse(da$code_job == 9999, NA, da$code_job)
table(is.na(da$code_job))

(joblist <- read_excel("F:/big/190617/joblist.xlsx", col_names = T, sheet = 2))
head(joblist)

da <- left_join(da, joblist, id = "code_job")
head(da)



# 집의 점유형태 _ homebuy
class(da$homebuy)
table(da$homebuy)
da$homebuy <- ifelse(da$homebuy == 9, NA, da$homebuy)
table(da$homebuy)
qplot(da$homebuy)

da$homebuy <- ifelse(da$homebuy == 1, "자가", 
                     ifelse(da$homebuy == 2, "전세",
                            ifelse(da$homebuy == 3, "보증부월세",
                                   ifelse(da$homebuy == 4, "월세(사글세)", "기타"))))
table(da$homebuy)



# 통신비 _ tele
class(da$tele)
table(da$tele)
da$tele <- ifelse(da$tele == 9999, NA, da$tele)
table(is.na(da$tele))
summary(da$tele)
qplot(da$tele)



# 카드빚 _ card_debt
class(da$card_debt)
table(da$card_debt)
da$card_debt <- ifelse(da$card_debt == 9999999, NA, da$card_debt)
table(is.na(da$card_debt))
summary(da$card_debt)
qplot(da$card_debt)

da



# 문제1 : 연령대 및 성별에 따른 통신비 차이

ages_sex_tele <- da %>% filter(!is.na(tele)) %>% group_by(ages, sex) %>%
                summarise(mean_tele = mean(tele))
head(ages_sex_tele)

ggplot(data = ages_sex_tele, aes(x=ages, y=mean_tele, fill=sex)) + geom_col() +
       scale_x_discrete( limits = c("young", "middle", "old"))

ggplot(data = ages_sex_tele, aes(x=ages, y=mean_tele, fill=sex)) +
       geom_col(position = "dodge") + scale_x_discrete(limits = c("young", "middle", "old"))



# 문제 2 : 가구원수에 따른 카드 빚 차이

famsize_card <- da %>% filter(!is.na(card_debt)) %>% group_by(famsize) %>%
                summarise(mean_card  = mean(card_debt))
head(famsize_card)

ggplot(data = famsize_card, aes(x = famsize, y=mean_card)) + geom_line()



# 문제 3 : 연령대에 따른 집의 점유 형태 차이
(ages_homebuy <- da %>% group_by(ages, homebuy) %>%
                       summarise(n = n()) %>% mutate(tot_homebuy = sum(n)) %>%
                       mutate(pct_homebuy = round(n/tot_homebuy*100,2)))
head(ages_homebuy)

ggplot(data = ages_homebuy, aes(x=homebuy, y=pct_homebuy, fill=ages)) +
              geom_col() + coord_flip()

ggplot(data = ages_homebuy, aes(x=ages, y=pct_homebuy, fill=homebuy)) + geom_col() +
       scale_x_discrete(limits = c("young", "middle", "old"))



# 문제 4 : 권역 별 지역 및 종교 유무에 따른 이혼율 분석
(area_religion_marriage <- da %>% filter(!is.na(group_marriage)) %>%
                                  group_by(area, religion, group_marriage) %>%
                                  summarise(n = n()) %>%
                                  mutate(tot = sum(n)) %>%
                                  mutate(pct = round(n/tot*100, 1)))

(divorce <- area_religion_marriage %>% filter(group_marriage == "divorce") %>%
                                       select(religion, pct))

ggplot(data=divorce, aes(x=area, y=pct, fill=religion)) + geom_col(position = "dodge") +
       scale_x_discrete(limits = c("서울", "수도권(인천/경기)", "강원/충북", "대전/충남",
                                   "대구/경북", "부산/경남/울산", "광주/전남/전북/제주도"))



# 문제 5 : 교육 수준 별 직종 빈도

# 교육수준 "high" 상위 10개 직종
(highedu_job <- da %>% filter(!is.na(job) & edu == "high") %>%
                       group_by(job) %>%
                       summarise(n=n()) %>%
                       arrange(desc(n)) %>% head(10))
ggplot(data = highedu_job, aes(x=reorder(job, n), y=n)) + geom_col() + coord_flip()

# 교육수준 "mid" 상위 10개 직종
(midedu_job <- da %>% filter(!is.na(job) & edu == "mid") %>%
                      group_by(job) %>%
                      summarise(n=n()) %>%
                      arrange(desc(n)) %>% head(10))
ggplot(data = midedu_job, aes(x=reorder(job, n), y=n)) +geom_col() + coord_flip()

# 교육수준 "low" 상위 10개 직종
(lowedu_job <- da %>% filter(!is.na(job) & edu == "low") %>% 
                      group_by(job) %>% 
                      summarise(n=n()) %>%
                      arrange(desc(n)) %>% head(10))
ggplot(data = lowedu_job, aes(x=reorder(job,n), y=n)) + geom_col() + coord_flip()


