# Install & load data.table package
if(!require(data.table)) install.packages("data.table"); library(data.table)
if(!require(zoo)) install.packages("zoo"); library(zoo)
install.packages("imputeTS"); library(imputeTS)
install.packages("UsingR")
install.packages("plyr")
install.packages("dplyr")
install.packages("bindrcpp")
install.packages("date")
install.packages("lubridate")
install.packages("stringr")
library(lubridate)
library(knitr)
library(printr)
library(date)
library(bindrcpp)
library(bindr)
library(dplyr)
library(UsingR)
library(plyr)
library(date)
library(stringr)

#usage raw data load as data table
#raw data name : usage15min_0601-1114
usage <- fread("usage15min_0601-1114.csv")
#check number of user
unique(usage$id)
max(usage$id)
usage <- usage[,-4]
usage <- usage %>% mutate(day = as.Date(substring(date,1,10)))
usage <- usage %>% mutate(cnt = 1)

#Observation period 0601 - 1031
usage <- usage %>% filter(day <= '2017-10-31')
max(usage$day)

#unstable data point 
#1. remove the usage is less than 0
usage_less <- usage %>% filter(usage<0)
usage <- usage %>% filter(usage >0)

#Check each household have the data after 2017-07-30 or not
usage_after_0731 <- usage %>% filter(day >= '2017-07-31')
day_usage_after_0731 <- usage_after_0731[,c(1,4,5)]
day_usage_after_0731 <- day_usage_after_0731[!duplicated(day_usage_after_0731), ]

check_data_after_campaign <- aggregate(day_usage_after_0731$cnt,by = list(day_usage_after_0731$id),FUN =sum )

colnames(check_data_after_campaign)[1] <- 'id'
colnames(check_data_after_campaign)[2] <- 'count_of_day_after_0731'

#group assign info. join
groupinfo<- fread("groupAssignmentInfo.csv")
colnames( check_data_after_campaign)[1] <- 'idx'
idx_group <- check_data_after_campaign %>% left_join(groupinfo)

final_idx_group <- idx_group %>% filter(!is.na(treat))
count(final_idx_group$treat)

#outlier handle

#reference
#outliers are those observations that lie outside 1.5*IQR, 
#where IQR, the ¡®Inter Quartile Range¡¯ is the difference between 75th and 25th quartiles. 
#Look at the points outside the whiskers in below box plot

outlier_values <- boxplot.stats(usage$usage)$out  # outlier values.
boxplot(usage$usage, main="15Min_Usage", boxwex=0.1)

outlier_values <- as.data.frame(outlier_values)
2*min(outlier_values$outlier_values)*0.000001

quantile(usage$usage, probs = c(0, 0.25, 0.5, 0.99, 1)) # quartile

# condition 1. larger than 3 IQR
#4340637
usage <- usage %>% filter(usage <=2*min(outlier_values$outlier_values))

#4287994
boxplot(usage$usage, main="15Min_Usage_after_removing_outliers", boxwex=0.1)
###################################
#####outlier remove finished#######
###################################

#check missing data for day-level aggregation
checkmissingAmong96 <- aggregate(usage$cnt, by = list(usage$id,usage$day), FUN =sum)


colnames(checkmissingAmong96)[1] <- 'id'
colnames(checkmissingAmong96)[2] <- 'day'
colnames(checkmissingAmong96)[3] <- 'count_of_15min'
count(checkmissingAmong96$count_of_15min)
usage2 <- usage %>% left_join(checkmissingAmong96)

#remove the day with more than 4 missing 15-min
usage3 <-usage2 %>%filter(count_of_15min>=92)

#groupinfo join with usage data
colnames(usage3)[1] <- 'idx'
usage4 <- usage3 %>% left_join(final_idx_group)
usage_of_final_spl <- usage4 %>% filter(!is.na(treat))
usage_of_final_spl <- usage_of_final_spl[,c(1,2,3,4,6,8)]
usage_of_final_spl<- usage_of_final_spl[with(usage_of_final_spl, order(idx,day,date)), ]
length(unique(usage_of_final_spl$idx))
#95-92 : impute the missing value as avg of before and after
idx_day <- usage_of_final_spl[,c(1,4)]
idx_day <- idx_day[!duplicated(idx_day), ]
idx_day <- idx_day %>% mutate(x = 'x')

#full time data frame 
full_time <- usage_of_final_spl %>% filter(idx==2) %>% filter(day=="2017-07-03")
full_time <- full_time %>% mutate(time= substring(date,12,19))
full_time <- full_time[,7]
full_time <- as.data.frame(full_time)
full_time <-full_time %>%mutate(full_time = as.character(full_time ))
full_time <- full_time %>% mutate(x='x')
  
idx_day_time <- idx_day %>% left_join(full_time)
idx_day_time <- idx_day_time %>% mutate(date = as.character(paste(as.character(day),full_time)))

idx_day_time <- idx_day_time[,-c(3,4)]

#make new data frame with missing value as NA
final_usage <- idx_day_time %>% left_join(usage_of_final_spl)

#count na, which is needed to impute
final_usage <- final_usage %>% mutate(missing = ifelse(is.na(usage),1,0))
sum(final_usage$missing)

final_usage_2 <- final_usage %>% filter(idx==2) %>% filter(day=='2017-08-08')

#841/4178592 = 0.2% impute
final_usage<- final_usage[with(final_usage, order(idx,day,date)), ]

#run mean-before-after imputation for 814 missing value
#0.5*(na.locf(final_usage2$usage,fromlast=TRUE) + na.locf(final_usage2$usage))

final_usage2 <- (final_usage[,c(4)])
library(imputeTS)
final_usage2 <- na.ma(final_usage2, k=1, weighting = "simple")

final_usage2 <- as.data.frame(final_usage2)


colnames(final_usage2)[1]<-'usage_no_missing'
final_usage <- final_usage %>% cbind(final_usage2)



#remove used var. for storage saving
remove(full_time)
remove(idx_day_time)

#check missing imputation
final_usage <- final_usage %>% mutate(missing = ifelse(is.na(usage_no_missing),1,0))
sum(final_usage$missing)
length(unique(final_usage$idx))
#save in kwh

final_usage <- final_usage %>% mutate(usage= usage*0.000001)
final_usage <- final_usage %>% mutate(usage_no_missing= usage_no_missing*0.000001)
final_usage <- final_usage[,-c(4,5)]
final_usage <- final_usage[,-c(4)]
final_usage <- final_usage %>% left_join(groupinfo)
colnames(final_usage)[5] <- 'usage'
summary(final_usage$usage)


final_usage <- final_usage[!duplicated(final_usage), ]
write.csv(final_usage, "15min_usage_in_kwh_after_preprocessing.csv")


####################################
#####imputation finished###########
###################################

#daylevel aggregation
electricity_usage_panel_day <- aggregate(final_usage$usage, by =list(final_usage$idx, final_usage$day), FUN = sum)
summary(electricity_usage_panel_day$x)


colnames(electricity_usage_panel_day)[1] <- 'idx'
colnames(electricity_usage_panel_day)[2] <- 'day'
colnames(electricity_usage_panel_day)[3] <- 'daysum'
electricity_usage_panel_day <- electricity_usage_panel_day %>% left_join(groupinfo)
boxplot(electricity_usage_panel_day$daysum, main="day_Usage", boxwex=0.1)
#campaign coding

electricity_usage_panel_day <- electricity_usage_panel_day %>% mutate(campaign= ifelse(day < '2017-08-02',0, ifelse(day <'2017-08-27',1,2)))
unique(electricity_usage_panel_day$campaign)

electricity_usage_panel_day <- electricity_usage_panel_day[,-c('group')]
write.csv(electricity_usage_panel_day, "electricity_usage_panel_daylevel.csv")

summary(electricity_usage_panel_day$daysum)

data <- electricity_usage_panel_day %>% filter(campaign!=2)
quick<- glm(log(daysum) ~ as.factor(treat)*as.factor(campaign)+as.factor(idx)+as.factor(day),data=data )
summary(quick)

library(stargazer)
stargazer(quick, type="text",
          out="guick_check_model_1.txt")
##skewed distribution check
median(electricity_usage_panel_day$daysum)
mean(electricity_usage_panel_day$daysum)

##check total day count per idx for 0601 - 1031 = 153
electricity_usage_panel_day <- electricity_usage_panel_day %>% mutate(cnt=1)
totalcnt <- aggregate(electricity_usage_panel_day$cnt , by =list(electricity_usage_panel_day$idx), FUN =sum)
count(totalcnt$x)

electricity_usage_panel_day_11 <- electricity_usage_panel_day %>% filter(idx==11)

day <- as.data.frame(electricity_usage_panel_day_11[,2])
colnames(day)[1] <- 'day'
idx <- as.data.frame(unique(electricity_usage_panel_day$idx))
colnames(idx)[1] <- 'idx'

day <- day %>% mutate(x='x')
idx <- idx %>% mutate(x = 'x')

idx_full_153day <- idx %>% left_join(day)
idx_full_153day <- idx_full_153day[,-2]

write.csv(idx_full_153day, "idx_full_153day.csv")

##########################################
####app&survey&temperature data join######
##########################################

appsurvey <- fread("DailyAppOpen_join_with_survey.csv")

appsurvey <- appsurvey[,-1]

appsurvey <- appsurvey %>% mutate(day = as.Date(day))
appsurvey  <- appsurvey[,-c('group')]

electricity_usage_panel_day <- electricity_usage_panel_day %>% left_join(appsurvey)
electricity_usage_panel_day <- electricity_usage_panel_day[,-6]

#remove the household with no data in pre-observation
electricity_usage_panel_day <- electricity_usage_panel_day %>% filter(idx!=330)
#remove the lowest 0.5% value in daysum
electricity_usage_panel_day <- electricity_usage_panel_day %>% filter(daysum>=0.5)


write.csv(electricity_usage_panel_day, "EnergyUsage_with_AppOpen&survey_295.csv")

summary(electricity_usage_panel_day$`total0827-1031`)


  