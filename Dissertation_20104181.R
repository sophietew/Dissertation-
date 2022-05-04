library(foreign)
library(ggplot2)
library(rcompanion)
library(psych)
library(lmtest)
library(sjmisc)
library(haven)
library(tidyverse)
library(survey)
library(srvyr)
library(lmtest)
library(psych)
library(ggfortify)
library(MASS)
library(sjstats)
library(sjPlot)
library(jtools)
library(interactions)
library(car)

undsociety<-sjlabelled::read_spss("cb_indresp_w.sav")

names(undsociety)

wfh_new <- undsociety %>% 
  filter(between(cb_wah, 1, 3))

# possible analysis weights for pre modelling
cb_betaindin_xw
cb_betaindin_xw_t

#### DV sex ######
table(wfh_new$cb_sex)
wfh_new$sex <- wfh_new$cb_sex
wfh_new$sex[wfh_new$cb_sex==1] <- 1
wfh_new$sex[wfh_new$cb_sex==2] <- 2
wfh_new$sex <- as.factor(wfh_new$sex)
levels(wfh_new$sex)<-c("Male", "Female")
table(wfh_new$sex)


#### satisfaction ####
describe(undsociety$cb_sclfsato_cv)
table(wfh_new$cb_sclfsato_cv)

# as numeric 
wfh_new$cb_sclfsato_cv[wfh_new$cb_sclfsato_cv == -1 | wfh_new$cb_sclfsato_cv == -2 |
                         wfh_new$cb_sclfsato_cv == -8 |
                         wfh_new$cb_sclfsato_cv == -9] <- NA

describe(wfh_new$cb_sclfsato_cv)

# as factor 
wfh_new$satisfactionfact <- NA
wfh_new$satisfactionfact[wfh_new$cb_sclfsato_cv==1] <- 1
wfh_new$satisfactionfact[wfh_new$cb_sclfsato_cv==2] <- 2
wfh_new$satisfactionfact[wfh_new$cb_sclfsato_cv==3] <- 3
wfh_new$satisfactionfact[wfh_new$cb_sclfsato_cv==4] <- 4
wfh_new$satisfactionfact[wfh_new$cb_sclfsato_cv==5] <- 5
wfh_new$satisfactionfact[wfh_new$cb_sclfsato_cv==6] <- 6
wfh_new$satisfactionfact[wfh_new$cb_sclfsato_cv==7] <- 7


wfh_new$satisfactionfact <- as.factor(wfh_new$satisfactionfact)
levels(wfh_new$satisfactionfact)<-c("Completely dissatisfied", "Mostly dissatisfied", 
                        "Somewhat dissatisfied", "Neither", "Somewhat satisfied", "Mostly satisfied", "Completely satisfied")
table(wfh_new$satisfactionfact)



#### hours worked #####
summary(wfh_new$cb_ff_hours)
table(undsociety$cb_ff_hours)
# numeric 
wfh_new$hoursnum <- wfh_new$cb_ff_hours
describe(wfh_new$age)
wfh_new$hoursnum[wfh_new$cb_ff_hours == -1 | wfh_new$cb_ff_hours == -2 |
                         wfh_new$cb_ff_hours == -8 |
                         wfh_new$cb_ff_hours == -9] <- NA

summary(wfh_new$hoursnum)

wfh_new$hoursnum[wfh_new$cb_ff_hours >= 0 & wfh_new$cb_ff_hours <= 17] <-1
wfh_new$hoursnum[wfh_new$cb_ff_hours >= 18 & wfh_new$cb_ff_hours <= 35] <-2
wfh_new$hoursnum[wfh_new$cb_ff_hours >= 36 & wfh_new$cb_ff_hours <= 39] <-3
wfh_new$hoursnum[wfh_new$cb_ff_hours >= 40 & wfh_new$cb_ff_hours <= 168] <-4

wfh_new$hoursnum <- as.factor(wfh_new$hoursnum)
levels(wfh_new$hoursnum)<-c("1st quartile", "2nd", 
                           "3rd", "4th")
table(wfh_new$hoursnum)




#### Age #####
summary(wfh_new$cb_age) 
describe(wfh_new$age)
wfh_new$age <- wfh_new$cb_age
ggplot(data=wfh_new, aes(x=undsociety$age)) + geom_histogram()
IQR(wfh_new$age)
describe(wfh_new$age)

# new age variable in quintiles for bivariate test
wfh_new$agefact <- NA

wfh_new$agefact[wfh_new$age >= 16 & wfh_new$age <= 36] <-1
wfh_new$agefact[wfh_new$age >= 37 & wfh_new$age <= 46] <-2
wfh_new$agefact[wfh_new$age >= 47 & wfh_new$age <= 54] <-3
wfh_new$agefact[wfh_new$age >= 55 & wfh_new$age <= 88] <-4

wfh_new$agefact <- as.factor(wfh_new$agefact)
levels(wfh_new$agefact)<-c("1st quarter", "2nd", 
                                    "3rd", "4th")
table(wfh_new$agefact)



#### Gross income ####
describe(undsociety$cb_hhincome_amount)
table(undsociety$cb_hhincome_period)

library(dplyr)

wfh_new$cb_hhincome_amount[wfh_new$cb_hhincome_amount<0]<-NA
wfh1 = wfh_new %>% 
  mutate(inc_std = case_when(cb_hhincome_period == "1" ~ cb_hhincome_amount,
                             cb_hhincome_period == "5" ~ cb_hhincome_amount,
                             cb_hhincome_period == "2" ~ cb_hhincome_amount / 2,
                             cb_hhincome_period == "3" ~ cb_hhincome_amount / 4,
                            cb_hhincome_period == "4" ~ cb_hhincome_amount / 52))

unique(wfh_new$cb_blhhearn_period)
summary(wfh1$inc_std)

wfh1$inc_std[wfh1$cb_hhincome_amount >= 0 & wfh1$cb_hhincome_amount <= 591] <-1
wfh1$inc_std[wfh1$cb_hhincome_amount >= 592 & wfh1$cb_hhincome_amount <= 964] <-2
wfh1$inc_std[wfh1$cb_hhincome_amount >= 965 & wfh1$cb_hhincome_amount <= 1249] <-3
wfh1$inc_std[wfh1$cb_hhincome_amount >= 1250 & wfh1$cb_hhincome_amount <= 8000 ] <-4

wfh1$inc_std <- as.factor(wfh1$inc_std)

levels(wfh1$inc_std)
levels(wfh1$inc_std)<-c("1st quartile", "2nd", "3rd", "4th")
table(wfh1$inc_std)


#####Ethnicity #####

table(wfh_new$racel_dv)

wfh_new$ethnicity <- NA
#white
wfh_new$ethnicity[wfh_new$racel_dv=="1" |
                    wfh_new$racel_dv== "2" |
                    wfh_new$racel_dv== "3)" |
                    wfh_new$racel_dv== "4" ] <- 1
#mixed
wfh_new$ethnicity[wfh_new$racel_dv=="5" |
                    wfh_new$racel_dv=="6" |
                    wfh_new$racel_dv== "7" |
                    wfh_new$racel_dv=="8"] <- 2

#asian or asian british
wfh_new$ethnicity[wfh_new$racel_dv=="9" |
                    wfh_new$racel_dv=="10" |
                    wfh_new$racel_dv=="11" |
                    wfh_new$racel_dv=="12)" |
                    wfh_new$racel_dv=="13"] <- 3
              
#black
wfh_new$ethnicity[wfh_new$racel_dv=="14" |
                    wfh_new$racel_dv=="15" |
                    wfh_new$racel_dv=="16"] <- 4

#other ethnic group
wfh_new$ethnicity[wfh_new$racel_dv=="17" |
                    wfh_new$racel_dv=="97"] <- 5                              

wfh_new$ethnicity <- as.factor(wfh_new$ethnicity)
levels(wfh_new$ethnicity)<-c("White", "Mixed/multiple ethnic group", "Asian", "Black/ African/Caribbean", "Other ethnic group")
table(wfh_new$ethnicity)
prop.table(table(wfh_new$ethnicity))*100

#### HEALTH variable #####
table(undsociety$cb_clinvuln_dv)
wfh_new$healthrisk <- NA
wfh_new$healthrisk[wfh_new$cb_clinvuln_dv=="0"] <- 1
wfh_new$healthrisk[wfh_new$cb_clinvuln_dv=="1"] <- 2
wfh_new$healthrisk[wfh_new$cb_clinvuln_dv=="2"] <- 3
wfh_new$healthrisk <- as.factor(wfh_new$healthrisk)
levels(wfh_new$healthrisk)<-c("No risk", "Moderate risk (clinically vulnerable)", 
                              "High risk (Clinically extremely vulnerable)")
table(wfh_new$healthrisk)


#### weighting ####
wfh_newwt<- svydesign(ids = ~1, data = wfh_new, 
                     weights = ~cb_betaindin_xw)
#sex
prop.table(svytable(~sex, design=wfh_newwt))*100
tab1<-prop.table(svytable(~sex, design=wfh_newwt))
tab1<-prop.table(svytable(~sex, design=wfh_newwt))*100
tab1<-as.data.frame(tab1)
ggplot(data=tab1, aes(x=sex, y=Freq)) +
  geom_bar(stat="identity") +
  xlab("Sex") +
  ylab("Percentage") +
  ggtitle("Respondents sex")+ 
  labs(caption = "Source: Understanding Society - Weighted Estimates")

#satisfaction
table(wfh_new$satisfactionfact)
prop.table(svytable(~satisfactionfact, design=wfh_newwt))*100
tab3<-prop.table(svytable(~satisfactionfact, design=wfh_newwt))*100
tab3<-as.data.frame(tab3)
ggplot(data=tab3, aes(x=satisfactionfact, y=Freq)) +
  geom_bar(stat="identity") +
  xlab("Satisfaction") +
  ylab("Percentage") +
  ggtitle("Respondents satisfaction")+ 
  labs(caption = "Source: Understanding Society - Weighted Estimates")


#ethnicity
prop.table(svytable(~ethnicity, design=wfh_newwt))*100
tab2<-prop.table(svytable(~ethnicity, design=wfh_newwt))*100
tab2<-as.data.frame(tab2)

ggplot(data=tab2, aes(x=ethnicity, y=Freq)) +
  geom_bar(stat="identity") +
  xlab("Ethnicity") +
  ylab("Percentage") +
  ggtitle("Respondents ethnicity")+ 
  labs(caption = "Source: Understanding Society - Weighted Estimates")


#income
prop.table(svytable(~income3cat, design=wfh_newwt))*100
tab4<-prop.table(svytable(~income3cat, design=wfh_newwt))*100
tab4<-as.data.frame(tab4)
ggplot(data=tab4, aes(x=income3cat, y=Freq)) +
  geom_bar(stat="identity") +
  xlab("Income") +
  ylab("Percentage") +
  ggtitle("Respondents income")+ 
  labs(caption = "Source: Understanding Society - Weighted Estimates")

prop.table(svytable(~cb_hhincome_amount, design=wfh_newwt))*100
tabincome<-prop.table(svytable(~cb_hhincome_amount, design=wfh_newwt))
tabincome<-prop.table(svytable(~cb_hhincome_amount, design=wfh_newwt))*100
tabincome<-as.data.frame(tabincome)
ggplot(data=tab1, aes(x=tabincome, y=Freq)) +
  geom_(stat="identity") +
  xlab("Income") +
  ylab("Percentage") +
  ggtitle("Respondents income")+ 
  labs(caption = "Source: Understanding Society (Work from home subset) - Weighted Estimates")

#healthrisk
prop.table(svytable(~healthrisk, design=wfh_newwt))*100
tab5<-prop.table(svytable(~healthrisk, design=wfh_newwt))
tab5<-prop.table(svytable(~healthrisk, design=wfh_newwt))*100
tab5<-as.data.frame(tab5)
ggplot(data=tab5, aes(x=healthrisk, y=Freq)) +
  geom_bar(stat="identity") +
  xlab("Health risk") +
  ylab("Percentage") +
  ggtitle("Respondents health risk")+ 
  labs(caption = "Source: Understanding Society - Weighted Estimates")

#Age
prop.table(svytable(~age1, design=wfh_newwt))*100
tab6<-prop.table(svytable(~age1, design=wfh_newwt))
tab6<-prop.table(svytable(~age1, design=wfh_newwt))*100
tab6<-as.data.frame(tab6)
ggplot(data=tab6, aes(x=age3cat, y=Freq)) +
  geom_bar(stat="identity") +
  xlab("Age") +
  ylab("Percentage") +
  ggtitle("Respondents age")+ 
  labs(caption = "Source: Understanding Society - Weighted Estimates")

# hrs worked 
prop.table(svytable(~hoursw, design=wfh_newwt))*100
tab7<-prop.table(svytable(~hoursw, design=wfh_newwt))
tab7<-prop.table(svytable(~hoursw, design=wfh_newwt))*100
tab7<-as.data.frame(tab7)
ggplot(data=tab7, aes(x=hoursw, y=Freq)) +
  geom_bar(stat="identity") +
  xlab("Hours worked") +
  ylab("Percentage") +
  ggtitle("Respondents hours worked")+ 
  labs(caption = "Source: Understanding Society - Weighted Estimates")



#### bivariate ####
#stacked bar charts
tabsatissexwt<-prop.table(svytable(~satisfactionfact + sex, design=wfh_newwt), 2)*100
tabsatissexwt<-as.data.frame(tabsatissexwt)
ggplot(data=tabsatissexwt, aes(x=sex, y=Freq, fill=satisfactionfact)) +
  geom_bar(stat="identity", position= "dodge") +
  ggtitle("Satisfaction by sex") +
  xlab("sex") +
  ylab("Percentage") +
  labs(caption = "Source: NSFW - Weighted Estimates")

#ethnicity
tabsatisethnwt<-prop.table(svytable(~satisfactionfact + ethnicity, design=wfh_newwt), 2)*100
tabsatisethnwt<-as.data.frame(tabsatisethnwt)
ggplot(data=tabsatisethnwt, aes(x=ethnicity, y=Freq, fill=satisfactionfact)) +
  geom_bar(stat="identity") +
  ggtitle("Satisfaction by ethnicity") +
  xlab("ethnicity") +
  ylab("Percentage") +
  labs(caption = "Source: Understanding society - Weighted Estimates")

ggplot(data=tabsatisethnwt, aes(x=ethnicity, y=Freq, fill=satisfactionfact)) +
  geom_bar(stat="identity", position= "dodge") +
  ggtitle("Satisfaction by ethnicity") +
  xlab("Ethnicity") +
  ylab("Percentage") +
  labs(caption = "Source: Understanding society - Weighted Estimates")

#health risk 
tabsatishealthwt<-prop.table(svytable(~satisfactionfact + healthrisk, design=wfh_newwt), 2)*100
tabsatishealthwt<-as.data.frame(tabsatishealthwt)
ggplot(data=tabsatishealthwt, aes(x=healthrisk, y=Freq, fill=satisfactionfact)) +
  geom_bar(stat="identity", position= "dodge") +
  ggtitle("Satisfaction by health risk") +
  xlab("Health risk") +
  ylab("Percentage") +
  labs(caption = "Source: Understanding society - Weighted Estimates")



## age 
describe(wfh_new$age)

ggplot(data = wfh_new[!is.na(wfh_new$satisfactionfact),],
                      aes (x=age, y=satisfactionfact, na.rm=TRUE)) + geom_boxplot()


tabsatisagewt<-prop.table(svytable(~satisfactionfact + age, design=wfh_newwt), 2)*100
tabsatisagewt<-as.data.frame(tabsatisagewt)
ggplot(data=tabsatisagewt, aes(x=age, y=Freq, fill=satisfactionfact)) +
  geom_boxplot(stat="identity") +
  ggtitle("Satisfaction by age") +
  xlab("Age") +
  ylab("Percentage") +
  labs(caption = "Source: Understanding society - Weighted Estimates")

# age as factor cross tab

tabagefactsat<-prop.table(svytable(~satisfactionfact + agefact, design=wfh_newwt), 2)*100
tabagefactsat<-as.data.frame(tabagefactsat)
ggplot(data=tabagefactsat, aes(x=agefact, y=Freq, fill=satisfactionfact)) +
  geom_bar(stat="identity")+
  ggtitle("Satisfaction by age") +
  xlab("Age") +
  ylab("Percentage") +
  labs(caption = "Source: Understanding society - Weighted Estimates")



#income
tabsatisincomewt<-prop.table(svytable(~satisfaction + income3cat, design=wfh_newwt), 2)*100
tabsatisincomewt<-as.data.frame(tabsatisincomewt)
ggplot(data=tabsatisincomewt, aes(x=income3cat, y=Freq, fill=satisfaction)) +
  geom_bar(stat="identity") +
  ggtitle("Satisfaction by income") +
  xlab("Income") +
  ylab("Percentage") +
  labs(caption = "Source: Understanding society - Weighted Estimates")

#hours worked
tabhourswt<-prop.table(svytable(~satisfactionfact + hoursnum, design=wfh_newwt), 2)*100
tabhourswt<-as.data.frame(tabhourswt)
ggplot(data=tabhourswt, aes(x=hoursnum, y=Freq, fill=satisfactionfact)) +
  geom_bar(stat="identity", position= "dodge") + 
  ggtitle("Satisfaction by hours of paid work") +
  xlab("Hours") +
  ylab("Percentage") +
  labs(caption = "Source: Understanding society - Weighted Estimates")

##### chi squared ####

chisq.test(table(wfh_new$sex, wfh_new$satisfactionfact))

svychisq(~satisfactionfact + sex, design=wfh_newwt, statistic="Chisq")

# ETHNICITY
anovaethnicity <-aov(satisfactionfact ~ ethnicity, data=wfh_new)
summary(anovaethnicity)


# AGE


# HEALTH RISK
chisq.test(table(wfh_new$healthrisk, wfh_new$satisfactionfact))

svychisq(~satisfactionfact + healthrisk, design=wfh_newwt)

# INCOME


####Regression #####

library(MASS)

ordinalmodel1 = polr(satisfaction ~ sex, data=undsociety, Hess=TRUE)

summary(ordinalmodel1) 


logit2 = glm(CUSTODY~SERIOUS_REGIMP + APO_GREATER_HARM_1 + APO_GREATER_HARM_2 + APO_GREATER_HARM_3 + 
               APO_LESSER_HARM_1 + APO_HIGHER_CULP_STAT_1 + APO_LOWER_CULP_1 + APO_LOWER_CULP_2 + 
               APO_LOWER_CULP_3 + APO_LOWER_CULP_4 + APO_LOWER_CULP_5 + PREV_CONVICTIONS, 
             family="binomial", data=assa)

logit2 = glm(CUSTODY~SERIOUS_REGIMP + APO_GREATER_HARM_1 + APO_GREATER_HARM_2 + APO_GREATER_HARM_3 + 
               APO_LESSER_HARM_1 + APO_HIGHER_CULP_STAT_1 + APO_LOWER_CULP_1 + APO_LOWER_CULP_2 + 
               APO_LOWER_CULP_3 + APO_LOWER_CULP_4 + APO_LOWER_CULP_5 + PREV_CONVICTIONS, 
             family="binomial", data=assa)
summary(logit1)
summary(logit2)
