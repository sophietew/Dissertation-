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
#### complete cases
USnew <- undsociety[complete.cases(undsociety$sex,ethnicity,satisfaction,
                                   healthrisk,income,age,employmenttype,wfh), ]


# possible analysis weights for pre modelling

cb_betaindin_xw

cb_betaindin_xw_t

#### DV sex ######
table(undsociety$cb_sex)
undsociety$cb_sex <- as.factor(undsociety$cb_sex)
undsociety$sex <- NA
undsociety$sex[undsociety$cb_sex=="1"] <- 1
undsociety$sex[undsociety$cb_sex=="2"] <- 2
undsociety$sex <- as.factor(undsociety$sex)
levels(undsociety$sex)<-c("Male", "Female")
table(undsociety$sex)
prop.table(table(undsociety$sex))*100

#### satisfaction ####
table(undsociety$cb_sclfsato_cv)
undsociety$satisfaction <- NA

undsociety$satisfaction[undsociety$cb_sclfsato_cv=="1"] <- 1
undsociety$satisfaction[undsociety$cb_sclfsato_cv=="2" |
                        undsociety$cb_sclfsato_cv=="3"] <- 2
undsociety$satisfaction[undsociety$cb_sclfsato_cv=="5" | 
undsociety$cb_sclfsato_cv=="6"] <- 3
undsociety$satisfaction[undsociety$cb_sclfsato_cv=="7"] <- 4
undsociety$satisfaction <- as.factor(undsociety$satisfaction)
# need to write full reason for recategorising into four categories !!!!!!!
levels(undsociety$satisfaction)<-c("Completely dissatisfied ", "Partly dissatisfied", 
"Partly satisfied", "Completely satisfied")
table(undsociety$satisfaction)


#### living with partner #####
table(undsociety$cb_couple)


##### WFH #####
table(undsociety$cb_wah)

undsociety$WFH <- NA

undsociety$WFH[undsociety$cb_wah=="1"] <- 1
undsociety$WFH[undsociety$cb_wah=="2"] <- 1
undsociety$WFH[undsociety$cb_wah=="3"] <- 1
undsociety$WFH[undsociety$cb_wah=="4"] <- 2




undsociety$WFH <- as.factor(undsociety$WFH)

# define the variable levels. Make sure get the order right so that 
# the values defined earlier (1, 2) correspond to the levels 
levels(undsociety$WFH)<-c("Work from home", "Never work from home")


#check recoding with a frequency table
table(undsociety$WFH)


# subset the data according to the condition of living w partner and working from home 
wfhcouple<-undsociety[undsociety$cb_couple==1 & undsociety$WFH=="Work from home",]
# check you got the new data correctly
table(wfhcouple$satisfaction)





#### Age #####
summary(undsociety$cb_age) 
# age needs to be recoded into categories 
age1867 <- undsociety %>% 
  filter(between(cb_age, 18, 67))

# check you got the new data correctly
table(age1867$cb_age)


ggplot(data=undsociety, aes(x=undsociety$cb_age)) + geom_histogram()
       
#create new variable with missing values
undsociety$age3cat <- NA
undsociety$age3cat[undsociety$cb_age >= 18 & undsociety$cb_age <= 38] <- 1
undsociety$age3cat[undsociety$cb_age >= 39 & undsociety$cb_age <= 57] <- 2
undsociety$age3cat[undsociety$cb_age >= 58 & undsociety$cb_age <= 67] <- 3
undsociety$age3cat<-as.factor(undsociety$age3cat)
levels(undsociety$age3cat)<-c("18 to 38", "39 to 57", "58 to 67")
table(undsociety$age3cat)


undsociety$age4cat <- NA
undsociety$age4cat[undsociety$cb_age >= 18 & undsociety$cb_age <= 38] <- 1
undsociety$age4cat[undsociety$cb_age >= 39 & undsociety$cb_age <= 57] <- 2
undsociety$age4cat[undsociety$cb_age >= 58 & undsociety$cb_age <= 96] <- 3
undsociety$age4cat<-as.factor(undsociety$age4cat)
levels(undsociety$age4cat)<-c("18 to 38", "39 to 57", "58 to 96")
table(undsociety$age4cat)
prop.table(table(undsociety$age4cat))*100



#### Gross income ####

describe(undsociety$cb_hhincome_amount)

table(undsociety$cb_hhincome_period)


#####Ethnicity #####

table(undsociety$racel_dv)

undsociety$ethnicity <- NA
#white
undsociety$ethnicity[undsociety$racel_dv=="1" |
                   undsociety$racel_dv== "2" |
                   undsociety$racel_dv== "3)" |
                   undsociety$racel_dv== "4" ] <- 1
#mixed
undsociety$ethnicity[undsociety$racel_dv=="5" |
                   undsociety$racel_dv=="6" |
                   undsociety$racel_dv== "7" |
                   undsociety$racel_dv=="8"] <- 2

#asian or asian british
undsociety$ethnicity[undsociety$racel_dv=="9" |
                   undsociety$racel_dv=="10" |
                   undsociety$racel_dv=="11" |
                   undsociety$racel_dv=="12)" |
                   undsociety$racel_dv=="13"] <- 3
              
#black
undsociety$ethnicity[undsociety$racel_dv=="14" |
                   undsociety$racel_dv=="15" |
                   undsociety$racel_dv=="16"] <- 4

#other ethnic group
undsociety$ethnicity[undsociety$racel_dv=="17" |
                   undsociety$racel_dv=="97"] <- 5                              

undsociety$ethnicity <- as.factor(undsociety$ethnicity)
levels(undsociety$ethnicity)<-c("White", "Mixed/multiple ethnic group", "Asian", "Black/ African/Caribbean", "Other ethnic group")
table(undsociety$ethnicity)
prop.table(table(undsociety$ethnicity))*100

#### HEALTH variable #####

table(undsociety$cb_clinvuln_dv)

undsociety$healthrisk <- NA

undsociety$healthrisk[undsociety$cb_clinvuln_dv=="0"] <- 1

undsociety$healthrisk[undsociety$cb_clinvuln_dv=="1"] <- 2

undsociety$healthrisk[undsociety$cb_clinvuln_dv=="2"] <- 3

undsociety$healthrisk <- as.factor(undsociety$healthrisk)

levels(undsociety$healthrisk)<-c("No risk", "Moderate risk (clinically vulnerable)", "High risk (Clinically extremely vulnerable)")
#frequency table
table(undsociety$healthrisk)



# loneliness predictor 

table(undsociety$cb_sclonely_cv)





#### part time work #### 




#### weighting ####

undsocwt<- svydesign(ids = ~1, data = undsociety, 
                   weights = ~cb_betaindin_xw)

prop.table(svytable(~sex, design=undsocwt))*100
tab1<-prop.table(svytable(~sex, design=undsocwt))
tab1<-prop.table(svytable(~sex, design=undsocwt))*100
tab1<-as.data.frame(tab1)
ggplot(data=tab1, aes(x=sex, y=Freq)) +
  geom_bar(stat="identity") +
  xlab("Sex") +
  ylab("Percentage") +
  ggtitle("Respondents sex")+ 
  labs(caption = "Source: Understanding Society - Weighted Estimates")


prop.table(svytable(~ethnicity, design=undsocwt))*100
tab2<-prop.table(svytable(~ethnicity, design=undsocwt))*100
tab2<-as.data.frame(tab2)
ggplot(data=tab2, aes(x=ethnicity, y=Freq)) +
  geom_bar(stat="identity") +
  xlab("Ethnicity") +
  ylab("Percentage") +
  ggtitle("Respondents ethnicity")+ 
  labs(caption = "Source: Understanding Society - Weighted Estimates")

prop.table(svytable(~satisfaction, design=undsocwt))*100
tab3<-prop.table(svytable(~satisfaction, design=undsocwt))*100
tab3<-as.data.frame(tab3)
ggplot(data=tab3, aes(x=satisfaction, y=Freq)) +
  geom_bar(stat="identity") +
  xlab("Satisfaction") +
  ylab("Percentage") +
  ggtitle("Respondents satisfaction")+ 
  labs(caption = "Source: Understanding Society - Weighted Estimates")



#### stacked bar chart ####

#stacked bar chart
ggplot(data=undsociety[!is.na(wfhcouple$satisfaction) & !is.na(wfhcouple$sex) , ], 
       aes(x=sex, fill=satisfaction)) +
  geom_bar(stat="count", aes(fill=satisfaction), position="fill")

ggplot(data=undsociety[!is.na(WFH1$satisfaction) & !is.na(WFH1$sex) , ], 
       aes(x=sex, fill=satisfaction)) +
  geom_bar(stat="count", aes(fill=satisfaction), position="fill")



#### univariate tests ####

#satisfaction
ggplot(data=undsociety[!is.na(undsociety$satisfaction),], aes(y = satisfaction, 
              x =..prop.., group=1)) + geom_bar(stat = "count") +
              ylab("Proportion of Respondents") +
              xlab("Proportion of Respondents") +
              ggtitle("Proportion of Respondents by life satisfaction")+ 
             theme(axis.text.x = element_text(angle= 90))

table(undsociety$sex)

#sex
ggplot(data = undsociety[!is.na(undsociety$sex), ], aes(x=sex, y = ..prop.., group = 1)) +
  geom_bar(stat="count") + scale_y_continuous(labels = scales::percent)+
  ylab("Percentage of respondents") + 
  ggtitle("Respondents sex") 


#age
ggplot(data = undsociety[!is.na(undsociety$age3cat), ], aes(x=age3cat, y = ..prop.., group = 1)) +
  geom_bar(stat="count") + scale_y_continuous(labels = scales::percent)+
  ylab("Percentage of respondents") + 
  ggtitle("Respondents age") 



#ethnicity
ggplot(data = undsociety[!is.na(undsociety$ethnicity), ], aes(x=ethnicity, y = ..prop.., group = 1)) +
  geom_bar(stat="count") + scale_y_continuous(labels = scales::percent)+
  ylab("Percentage of respondents") + 
  ggtitle("Respondents ethnicity") 



#health variable 


#SES




#### bivariate ####

# stacked bar chart
ggplot(undsociety[complete.cases(undsociety$satisfaction,undsociety$sex) , ], 
       aes(y=sex)) +
  geom_bar(stat="count", aes(fill=satisfaction), position="fill") + 
  scale_x_continuous(labels = scales::percent) +
  xlab("Satisfaction") +
  ylab("Percentage of respondents") + 
  ggtitle("Life satisfaction by sex")+
  theme(axis.text.x = element_text(angle= 90))


ggplot(undsociety[complete.cases(undsociety$satisfaction,undsociety$ethnicity) , ], 
       aes(y=ethnicity)) +
  geom_bar(stat="count", aes(fill=satisfaction), position="fill") + 
  scale_x_continuous(labels = scales::percent) +
  xlab("Satisfaction") +
  ylab("Percentage of respondents") + 
  ggtitle("Life satisfaction by ethnicity")+
  theme(axis.text.x = element_text(angle= 90))

#### SATISFACTION wfh chart ####

ggplot(undsociety[complete.cases(undsociety$satisfaction,undsociety$WFH) , ], 
       aes(y=WFH)) +
  geom_bar(stat="count", aes(fill=satisfaction), position="fill") + 
  scale_x_continuous(labels = scales::percent) +
  xlab("Satisfaction") +
  ylab("Percentage of respondents") + 
  ggtitle("Life satisfaction by WFH")+
  theme(axis.text.x = element_text(angle= 90))


#### age satisfaction chart #####
ggplot(undsociety[complete.cases(undsociety$satisfaction,undsociety$age3cat) , ], 
       aes(y=age3cat)) +
  geom_bar(stat="count", aes(fill=satisfaction), position="fill") + 
  scale_x_continuous(labels = scales::percent) +
  xlab("Satisfaction") +
  ylab("Percentage of respondents") + 
  ggtitle("Life satisfaction by age")+
  theme(axis.text.x = element_text(angle= 90))







#### cross tabs ####
##Tables
satisfactiontab<-table(undsociety$satisfaction)
addmargins(satisfactiontab)
prop.table(satisfactiontab)


gendertab<-table(undsociety$sex)
addmargins(gendertab)
prop.table(gendertab)

gendersatistab<-table(undsociety$satisfaction, undsociety$sex)

addmargins(gendersatistab)
prop.table(gendersatistab, 2)


ggplot(data=undsociety[!is.na(undsociety$satisfaction) & !is.na(undsociety$sex) , ], 
       aes(x=sex, fill=satisfaction)) +
  geom_bar(stat="count", aes(fill=satisfaction), position="fill")

chi_gendersatistab<-chisq.test(gendersatistab, correct = FALSE)
chi_gendersatistab


###### NEW

satisfactiontab<-table(wfhcouple$satisfaction)
addmargins(satisfactiontab)
prop.table(satisfactiontab)


gendertab<-table(wfhcouple$sex)
addmargins(gendertab)
prop.table(gendertab)

gendersatistab<-table(wfhcouple$satisfaction, wfhcouple$sex)

addmargins(gendersatistab)
prop.table(gendersatistab, 2)
##### chi squared ####

chi_wfhsatis<-chisq.test(gendersatistab, correct = FALSE)
chi_wfhsatis
names(undsociety)


####Regression #####

library(MASS)

ordinalmodel1 = polr(satisfaction ~ sex, data=undsociety, Hess=TRUE)

summary(ordinalmodel1) 

logit2 = glm(CUSTODY~SERIOUS_REGIMP + APO_GREATER_HARM_1 + APO_GREATER_HARM_2 + APO_GREATER_HARM_3 + 
               APO_LESSER_HARM_1 + APO_HIGHER_CULP_STAT_1 + APO_LOWER_CULP_1 + APO_LOWER_CULP_2 + 
               APO_LOWER_CULP_3 + APO_LOWER_CULP_4 + APO_LOWER_CULP_5 + PREV_CONVICTIONS, 
             family="binomial", data=assa)
summary(logit1)
summary(logit2)
