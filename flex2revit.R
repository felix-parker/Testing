##install.packages("usethis")

##library(usethis)
##use_git_config(user.name = "Felix Parker", user.email = "felix.parker@cqumail.com")
##^^to check config in git.bash git config --global --list 


##input dataset from csv/txt doc
flex2<-read.csv("C:\\Users\\HelloKitty4\\Documents\\0MOHBLAB\\2019.SummerResearch\\FlexRevit\\flex mult imputev3.csv")

#this function turns off default print scientific notation
options(scipen=999)

#table(flex2$P3_demo_gender)
#changing gender3 to missing data as outliers
flex2$gender<-ifelse(flex2$P3_demo_gender==3,NA,flex2$P3_demo_gender)

#lm.beta for standardised regression
library(lm.beta)

#creating factor for two groups (gender) instead of numbers
flex2$condition<-as.factor(flex2$condition)

#creating variable total.bb (total plays before plus total plays between)
flex2$total.bb<-flex2$total.before+flex2$total.between

###RUN ALL ABOVE FIRST###

###TABLE 1 ##seperate descriptives
##need to show seperately what their engagement was (total.bb, total.before)

##creating engagement for people in condition 2 (physical)
flexpa<-flex2[which(flex2$condition==2), ]
##descriptives
mean(flexpa$total.before)
sd(flexpa$total.before)
mean(flexpa$total.bb)
sd(flexpa$total.bb)

##creating condition 1, control, not pa
flexnotpa<-flex2[which(flex2$condition==1), ]
##descriptives
mean(flexnotpa$total.before)
sd(flexnotpa$total.before)
mean(flexnotpa$total.bb)
sd(flexnotpa$total.bb)

##TABLE 2 descriptives
mean(flex2$total.before)
mean(flex2$total.bb)
sd(flex2$total.before)
sd(flex2$total.bb)

##bivariate correlations of initial and next day assessments of automatic evaluations etc (table2)
##use printout to match correlations to dataset
##correlations for .BEFORE column 13/11
cor(flex2$total.before,flex2$P2_iat_d,use="pairwise")
cor(flex2$total.before,flex2$P4_iat_d,use="pairwise")
cor(flex2$total.before,flex2$Mean_SRBAI_3,use="pairwise")
cor(flex2$total.before,flex2$Mean_srbai_5,use="pairwise")
cor(flex2$total.before,flex2$P3_ex_minutes,use="pairwise")
cor(flex2$total.before,flex2$P5_ex_minutes,use="pairwise")
cor(flex2$total.before,flex2$Mean_ATT1_3,use="pairwise")
cor(flex2$total.before,flex2$Mean_ATT1_5,use="pairwise")
cor(flex2$total.before,flex2$Mean_ATT2_3,use="pairwise")
cor(flex2$total.before,flex2$Mean_ATT2_5,use="pairwise")
cor(flex2$total.before,flex2$P3_demo_age,use="pairwise")
cor(flex2$total.before,flex2$BMI,use="pairwise")
##blank -- as before.before

##correlation tests including p values .BEFORE 
cor.test (flex2$total.before,flex2$P2_iat_d,use="pairwise")
cor.test(flex2$total.before,flex2$P4_iat_d,use="pairwise")
cor.test(flex2$total.before,flex2$Mean_SRBAI_3,use="pairwise")
cor.test(flex2$total.before,flex2$Mean_srbai_5,use="pairwise")
cor.test(flex2$total.before,flex2$P3_ex_minutes,use="pairwise")
cor.test(flex2$total.before,flex2$P5_ex_minutes,use="pairwise")
cor.test(flex2$total.before,flex2$Mean_ATT1_3,use="pairwise")
cor.test(flex2$total.before,flex2$Mean_ATT1_5,use="pairwise")
cor.test(flex2$total.before,flex2$Mean_ATT2_3,use="pairwise")
cor.test(flex2$total.before,flex2$Mean_ATT2_5,use="pairwise")
cor.test(flex2$total.before,flex2$P3_demo_age,use="pairwise")
cor.test(flex2$total.before,flex2$BMI,use="pairwise")
cor.test(flex2$total.before,flex2$total.bb,use="pairwise")
##blank -- as before.before

##correlations for .BB column 14/12
cor(flex2$total.bb,flex2$P2_iat_d,use="pairwise")
cor(flex2$total.bb,flex2$P4_iat_d,use="pairwise")
cor(flex2$total.bb,flex2$Mean_SRBAI_3,use="pairwise")
cor(flex2$total.bb,flex2$Mean_srbai_5,use="pairwise")
cor(flex2$total.bb,flex2$P3_ex_minutes,use="pairwise")
cor(flex2$total.bb,flex2$P5_ex_minutes,use="pairwise")
cor(flex2$total.bb,flex2$Mean_ATT1_3,use="pairwise")
cor(flex2$total.bb,flex2$Mean_ATT1_5,use="pairwise")
cor(flex2$total.bb,flex2$Mean_ATT2_3,use="pairwise")
cor(flex2$total.bb,flex2$Mean_ATT2_5,use="pairwise")
cor(flex2$total.bb,flex2$P3_demo_age,use="pairwise")
cor(flex2$total.bb,flex2$BMI,use="pairwise")
cor(flex2$total.bb,flex2$total.bb,use="pairwise")
##blank -- as bb.bb

##correlation tests including p values .BB
cor.test (flex2$total.bb,flex2$P2_iat_d,use="pairwise")
cor.test(flex2$total.bb,flex2$P4_iat_d,use="pairwise")
cor.test(flex2$total.bb,flex2$Mean_SRBAI_3,use="pairwise")
cor.test(flex2$total.bb,flex2$Mean_srbai_5,use="pairwise")
cor.test(flex2$total.bb,flex2$P3_ex_minutes,use="pairwise")
cor.test(flex2$total.bb,flex2$P5_ex_minutes,use="pairwise")
cor.test(flex2$total.bb,flex2$Mean_ATT1_3,use="pairwise")
cor.test(flex2$total.bb,flex2$Mean_ATT1_5,use="pairwise")
cor.test(flex2$total.bb,flex2$Mean_ATT2_3,use="pairwise")
cor.test(flex2$total.bb,flex2$Mean_ATT2_5,use="pairwise")
cor.test(flex2$total.bb,flex2$P3_demo_age,use="pairwise")
cor.test(flex2$total.bb,flex2$BMI,use="pairwise")
cor.test(flex2$total.bb,flex2$total.before,use="pairwise")
##blank -- as bb.bb

##TABLE 3 ##needs further edits AR

##HYPOTHESIS1,1B
#regression for hyp1=dependent(regressed~onto)condition
#is there a difference at first iat (P2_iat) depending on whether exercise/not
hyp1<-lm(P2_iat_d~ condition, data=flex2)
summary(hyp1)

# standardize
hyp1 <- lm.beta(hyp1)
coef(hyp1)

##NEW HYPOTHESIS 1B
#regression for hyp2... d~condition
#is there a difference at 2nd iat (p4_iat) depending on condition exercise/not
hyp2<-lm(P4_iat_d~ condition+P2_iat_d, data=flex2)
summary(hyp2)

# standardize
hyp2 <- lm.beta(hyp2)
coef(hyp2)

##NEW HYPOTHESIS2? 
#regression for hyp1x=dependent(regressed~onto)condition*total.before
#is there a difference at first iat depending on exercise/not*if they played games before

hyp1x<-lm(P2_iat_d~ condition*total.before, data=flex2)
summary(hyp1x)

#standardize
hyp1 <- lm.beta(hyp1x)
coef(hyp1x)

#regression for hyp2B =dependent(regressed~onto)condition*total.bb
#is there a difference at 2nd iat (P4_iat) depending on exercise/not*total plays before&between
hyp2x<-lm(P4_iat_d~ condition*total.bb, data=flex2)
summary(hyp2x)

# standardize
hyp2 <- lm.beta(hyp2x)
coef(hyp2x)

#residualised change, moderation effect
##NEEDS MEAN CENTERING? AR
hyp2b<-lm(P4_iat_d~ condition*total.bb + P2_iat_d, data=flex2)
summary(hyp2b)
# to get standardize for table 3
hyp2b <- lm.beta(hyp2b)
coef(hyp2b)

##when sig moderation effect then you have to do probing
#two way interaction,,, need variance matrix
#preacher and hayes moderation analysis
vcov(hyp2b)

#####END OF EDITS FP, AR FOR SRS####
#Edit FP, AR 2020.01.15
##Edit FP, AR 2020.01.28 
#cor.test added FP 2020.01.29

hyp3<-lm(Mean_SRBAI_3~ condition, data=flex.mice)
summary(hyp3)

hyp3 <- lm.beta(hyp3)
coef(hyp3)


hyp3<-lm(Mean_srbai_5~ condition, data=flex.mice)
summary(hyp3)
# standardize
hyp3 <- lm.beta(hyp3)
coef(hyp3)


hyp4<-lm(Mean_ATT1_3~ condition, data=flex)
summary(hyp4)
# standardize
hyp4 <- lm.beta(hyp4)
coef(hyp4)

hyp4<-lm(Mean_ATT1_5~ condition, data=flex.mice)
summary(hyp4)
# standardize
hyp4 <- lm.beta(hyp4)
coef(hyp4)

hyp4<-lm(Mean_ATT2_3~ condition, data=flex)
summary(hyp4)
# standardize
hyp4 <- lm.beta(hyp4)
coef(hyp4)

hyp4<-lm(Mean_ATT2_5~ condition, data=flex)
summary(hyp4)
# standardize
hyp4 <- lm.beta(hyp4)
coef(hyp4)

hyp4<-lm(P5_ex_minutes~ condition, data=flex)
summary(hyp4)
# standardize
hyp4 <- lm.beta(hyp4)
coef(hyp4)

flex$gender<-as.factor(flex$gender)
hyp5<-lm(P4_iat_d~ condition + P2_iat_d + gender +P3_demo_age +BMI, data=flex)
summary(hyp5)
# standardize
hyp5 <- lm.beta(hyp5)
coef(hyp5)

hyp6<-lm(P4_iat_d~ condition + P2_iat_d + gender +P3_demo_age +BMI + P5_ex_goals + Mean_ATT1_5 + Mean_ATT2_5, data=flex)
summary(hyp6)
# standardize
hyp6 <- lm.beta(hyp6)
coef(hyp6)

#inputting data (you need to change the file path)
#library(foreign)
#flex.before = read.spss("C:\\Users\\hydea\\ownCloud\\Projects (data)\\2019 Flex revitalise\\flex pilot.sav", to.data.frame=TRUE, use.value.labels=F)
#usage <-read.csv("C:\\Users\\hydea\\ownCloud\\Projects (data)\\2019 Flex revitalise\\flex usage clean.csv",stringsAsFactors=FALSE)
#flex <- merge(flex.before,usage,by="study_ID",all.x=T)


##multiple imputation##
###Multiple imputaiton for missing data - see van Buuren, S. & Groothuis-Oudshoorn, K. (2011) mice: Multivariate imputation by chained equations in R##

#library(mice)
#md.pattern(flex)

#imp<-mice(flex, m=5)
#print(imp)

#flex.mice<-data.frame(complete(imp))
#write.table(flex.mice, file = "C:\\Users\\hydea\\ownCloud\\Projects (data)\\2019 Flex revitalise\\flex mult impute.csv", sep = ",", col.names = NA,
#            qmethod = "double")


names(flex.games)[names(flex.games) == 'accountId'] <- 'id'

flex <- merge(flex.users,flex.games,by="id")

#games
#4 basket case
#3 switch up
#2 scrambled
#1 flashback

#groups
#1 not physical activity
#2 physical activity

##scoring a scale##

# t-test
t.test(flex$highscore.all ~ flex$interventionGroup,paired=F)

library(QuantPsyc)

#####Pilot data analyses

