# Inferential-Statistics-Gender-and-Job-Satisfaction

Statistical inference with the GSS data


Setup


Load packages

library(ggplot2)
library(dplyr)
library(statsr)


Load data

load("gss.Rdata")



Part 1: Data

Data: According to GSS, the target population of the GSS is adults (18+) living in households in the United States. From 1972 to 2004 it was further restricted to those able to do the survey in English. From 2006 to present it has included those able to do the survey in English or Spanish. Those unable to do the survey in either English or Spanish are out-of-scope. Residents of institutions and group quarters are out-of-scope. Those with mental and/or physical conditions that prevent them from doing the survey, but who live in households are part of the target population and in-scope. In the reinterviews those who have died, moved out of the United States, or who no longer live in a household have left the target population and are out-of-scope. Therefore the information from the data only stands for 18+ normal household-living persons. They are survey-like data, not experimental designed data, therefore, the conclusion from the GSS data only means relation between datas but not causation.However, the available data are based on randomly selected people in specific condition, therefore the conclusion can be generalized into similiar groups of people.However, since the data exclude people who can not speak English or Spanish (after 2006), therefore, there is potential bias in the data



Part 2: Research question

Who is more satisfied with their jobs(if SATJOB is labeled VERY SATISFIED OR MOD. SATISFIED, we will count SATISFIED here;otherwise, if SATJOB is labeled A LITTLE DISSAT or VERY DISSATISFIED, we will count DISSAT.), men or women?

We hear a lot that someone loves the job, however someone hate his/her job. it would be very interested to see whether there is any relation between gender and work satisfaction.



Part 3: Exploratory data analysis

getting the information of sex and satisfied level of the job

mycol = c("sex","satjob")
gss_new = gss[mycol]

remove the NA value

gss_new = na.omit(gss_new)

Let’s have a look at the summary of the data

summary(gss_new)
##      sex                      satjob     
##  Male  :18042   Very Satisfied   :19717  
##  Female:23235   Mod. Satisfied   :15736  
##                 A Little Dissat  : 4109  
##                 Very Dissatisfied: 1715

let’s have a look at the distribution of Male and Female in the new data set

ggplot(gss_new, aes(x=sex))+geom_bar(color="blue",fill="blue")+ggtitle("Gender distribution")



let’s have a look at the distribution of satisfaction of job in the new data set

ggplot(gss_new, aes(x=satjob))+geom_bar(color="red",fill="red")+ggtitle("Job satisfaction distribution")



get the subset of data who are female and subset of data who are male

gss_female = subset(gss_new, sex=="Female")
gss_male = subset(gss_new, sex=="Male")

let’s have a look at the distribution of satisfaction of job in female

ggplot(gss_female, aes(x=satjob))+geom_bar()+ggtitle("Job Satisfaction in Female")



let’s have a look at the distribution of satisfaction of job in male

ggplot(gss_male, aes(x=satjob))+geom_bar(fill="grey")+ggtitle("Job Satisfaction in Male")



From the bar figure, it is not very obvious that whether there is difference of job satisfaction between male and female. Let explore in another way.

calculate the exact rate of job satisfaction in male and female

total_male = length(which(gss_new$sex =="Male"))
total_female = length(which(gss_new$sex =="Female"))
male_vsat = length(which(gss_new$sex =="Male" & gss_new$satjob=="Very Satisfied"))
male_msat = length(which(gss_new$sex =="Male" & gss_new$satjob=="Mod. Satisfied"))
male_adsat = length(which(gss_new$sex =="Male" & gss_new$satjob=="A Little Dissat"))
male_vdsat = length(which(gss_new$sex =="Male" & gss_new$satjob== "Very Dissatisfied"))
male_sat = male_vsat + male_msat
male_dissat = male_adsat + male_vdsat
male_freq = c(male_sat/total_male, male_dissat/total_male)

female_vsat = length(which(gss_new$sex =="Female" & gss_new$satjob=="Very Satisfied"))
female_msat = length(which(gss_new$sex =="Female" & gss_new$satjob=="Mod. Satisfied"))
female_adsat = length(which(gss_new$sex =="Female" & gss_new$satjob=="A Little Dissat"))
female_vdsat = length(which(gss_new$sex =="Female" & gss_new$satjob== "Very Dissatisfied"))
female_sat = female_msat+ female_vsat
female_dissat = female_adsat + female_vdsat
female_freq = c(female_sat/total_female, female_dissat/total_male)

make pie figure to show the distribution of female and their satisfaction of their jobs

pie(female_freq, label = c("sat","disat"), main="pie chart of job satisfaction in female")



make pie figure to show the distribution of female and their satisfaction of their jobs

pie(male_freq, label = c("sat","disat"), main="pie chart of job satisfaction in male")



percentage of female who satisfied her job is

rfemale=female_sat/total_female
print(rfemale)
## [1] 0.8566387

percentage of male who satisfied her job is

rmale=male_sat/total_male
print(rmale)
## [1] 0.8618224

So from the gss data, the difference of job satisfaction between male and female is (%)

male_sat/total_male*100-female_sat/total_female*100
## [1] 0.5183723

From the data and also the pie charts, it seems men more satisfied with their jobs, however we should consider sample viariaton. Therefore in next pat let’s do inference to see whether men are more likely satisfied with their job.



Part 4: Inference

Based information from Part 3, we make hypothesis as following: H0: There is no difference in the rate of job satisfaction between men and women rmale-rfemale==0 HA: There is some difference in the rate of job satisfaction between men and women rmale-rfemale!=0

First let’s check the conditions necessary for the sampling distribution of rmale-rfemale to be normal 1.each proportion separately follows a normal model sample number is 41277 in gss_new data set, which is << 10% of the total population of USA, and the people were randomly selected, therefore we can say that the first condition is met 2.the two samples are independent of each other the data in this case are for male and female, which is independent

rmale-rfemale= 0.52%

The standard error SE = sqrt(rmale(1-rmale)/total_male+rfemale(1-rfemale)/total_female)

SE = sqrt(rmale*(1-rmale)/total_male+rfemale*(1-rfemale)/total_female)
print(SE)
## [1] 0.003447596

For a 90% confidence interval, we use z* = 1.65,then point estimator + 1.65*SE

print(0.0052+1.65*SE)
## [1] 0.01088853

For a 90% confidence interval, we use z* = 1.65,then point estimator - 1.65*SE

print(0.0052-1.65*SE)
## [1] -0.0004885333

The job satisfaction difference in (-0.00049, 0.011) at 90% confidence. We are 90% condident that the difference of job satisfaction between male and female is in (-0.00049, 0.011), and 0.0052 is in this range, therefore we can not reject our H0. In other word, we are 90% confident that there is no difference of job satisfaction between male and female
