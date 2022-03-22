##W. Keith Tan TFF3 gland count analysis
##Supplementary Code for TFF3 gland count analysis (1st part of the results)

library(tidyverse)
library(rstatix)
library('lemon')
library(jtools)

#Adjust the path according to the location of your barretts-segment-length-predictor repository
best3 <- read_csv('~/barretts-segment-length-predictor/data/BEST3_SHORT_VS_LONG.csv')

best3$BOlength <- as.factor(best3$BOlength)

##Checking equality of variance using Levene test
best3 %>%
  levene_test(TFF3CountSlide1 ~ BOlength)

best3 %>%
  levene_test(TFF3CountSlide2 ~ BOlength)

best3 %>%
  levene_test(mean_count~ BOlength)

best3 %>%
  levene_test(highest~ BOlength)

#Calculating summary statistics mean and median
#This code gives you all the summary stats
best3 %>%
  group_by(BOlength) %>%
  get_summary_stats(type='common') %>%
  print(n=25)

#T test
t.test(TFF3CountSlide1~BOlength, mu=0, alt='two.sided', conf=0.95, var.equal=F, paired=F)
t.test(TFF3CountSlide2~BOlength, mu=0, alt='two.sided', conf=0.95, var.equal=F, paired=F)
t.test(mean_count~BOlength, mu=0, alt='two.sided', conf=0.95, var.equal=F, paired=F)
t.test(highest~BOlength, mu=0, alt='two.sided', conf=0.95, var.equal=F, paired=F)
t.test(total~BOlength, mu=0, alt='two.sided', conf=0.95, var.equal=F, paired=F)

model <- glm(BOlength~TFF3CountSlide1, data=best3, family = binomial)
summary(model)
exp(0.2563)
exp(confint(model))
summ(model, confint = TRUE, digits=3, exp=TRUE)

model2 <- glm(BOlength~TFF3CountSlide2, data=best3, family = binomial)
summary(model2)
exp(0.27390)
exp(confint(model2))
summ(model2, confint = TRUE, digits=3, exp=TRUE)

model3 <- glm(BOlength~mean_count, data=best3, family = binomial)
summary(model3)
exp(0.27987)
exp(confint(model3))
summ(model3, confint = TRUE, digits=3, exp=TRUE)

model4 <- glm(BOlength~highest, data=best3, family = binomial)
summary(model4)
exp(0.26226)
exp(confint(model4))
summ(model4, confint = TRUE, digits=3, exp=TRUE)

model5 <- glm(BOlength ~ total, data=best3, family=binomial)
summary(model5)
exp(0.14023)
summ(model5, confint = TRUE, digits=3, exp=TRUE)
