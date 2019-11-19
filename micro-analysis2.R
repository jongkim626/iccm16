### Microgenetic Subtask Analysis
### Jong Kim (jongkim626@gmail.com)
### Date created: 18 Feb 2016
### Last Modified: 19 Nov 2019


##################################################################################
###   Table of Contents
###
###   1. Setting up the environment
###   2. Load data
###   3. Plot
###     3.1. Plot: scatter, mean, KLM prediction
###   4. Mixed effect model
##################################################################################

###
###   1. Setting up the environment (packages and library)
###

# library
library(nlme)
library(lattice)
library(ggplot2)
library(lme4)  # linear mixed effect

setwd("~/rdata/iccm16")

###
###   2. Load data
###

micro.learning.k <- read.table("~/rdata/iccm16/micro-learning-k.txt", 
                               header=T)
attach(micro.learning.k)
names(micro.learning.k)

micro.learning.k$subtask <- factor(micro.learning.k$subtask, levels = c(1:14),
                                   labels =c("S1:FileOpen",
                                             "S2:SaveAs",
                                             "S3:NormCalc",
                                             "S4:Sum",
                                             "S5:FreqCalc",
                                             "S6:Sum",
                                             "S7:Length",
                                             "S8:TotalLength",
                                             "S9:TypdChar",
                                             "S10:TotalTypdChar",
                                             "S11:InsRows",
                                             "S12:Name",
                                             "S13:Date",
                                             "S14:SavePrn"))

# check the variables
str(micro.learning.k)
head(micro.learning.k)

###
###   3. Plot
###

###  3.1. Plot: scatter, mean, KLM prediction 

# base
p <- ggplot(micro.learning.k, aes(x = day, y = time)) +
  geom_point(colour="black", size=0.5, na.rm=T) +
  facet_wrap(~subtask, ncol=7) 
print(p)

# for label & linear line
p1 <- p+
  labs(title="Subtask Completion Time", x ="Trials(Day)", y="Time(s)")+
  geom_smooth(method=lm, na.rm = T, formula =y~x, se=F)

print(p1)


### stat function for plot (retrieved from cookbook-r)
# Gives count, mean, standard deviation, standard error of the mean, and 
# confidence interval (default 95%).
# data: a data frame.
# measurevar: the name of a column that contains the var. to be summariezed
# groupvars: a vector containing names of columns that contain grouping var.
# na.rm: a boolean indicating whether to ignore NA's
# conf.interval: the percent range of the conf. interval (default is 95%)
summarySE <- function(data=micro.learning.k, measurevar, 
                      groupvars=subtask, na.rm=T,
                      conf.interval=.95, .drop=TRUE) {
  library(plyr)
  # New version of length which can handle NA's: if na.rm==T, don't count them
  length2 <- function (x, na.rm=FALSE) {
    if (na.rm) sum(!is.na(x))
    else       length(x)
  }
  # This does the summary. For each group's data frame, return a vector with
  # N, mean, and sd
  datac <- ddply(data, groupvars, .drop=.drop,
                 .fun = function(xx, col) {
                   c(N    = length2(xx[[col]], na.rm=na.rm),
                     mean = mean   (xx[[col]], na.rm=na.rm),
                     sd   = sd     (xx[[col]], na.rm=na.rm)
                   )
                 },
                 measurevar
  )
  
  # Rename the "mean" column    
  datac <- rename(datac, c("mean" = measurevar))
  
  datac$se <- datac$sd / sqrt(datac$N)  # Calculate standard error of the mean
  
  # Confidence interval multiplier for standard error
  # Calculate t-statistic for confidence interval: 
  # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
  ciMult <- qt(conf.interval/2 + .5, datac$N-1)
  datac$ci <- datac$se * ciMult
  
  return(datac)
}

new.data <- summarySE(micro.learning.k, measurevar = "time", 
                 groupvars = c("subtask", "day"))

## plot the stat
p.mean <- ggplot(new.data, aes(x=day, y=time))+
  facet_wrap(~subtask, ncol=7)+
  geom_errorbar(aes(ymin=time-se, ymax=time+se), width=.3) +
  geom_line() +
  geom_point(size=2)+
  labs(title="Subtask Completion Time", x ="Trials(Day)", y="Time(s)")

## plot with mean and SEM
print(p.mean)

## mean
#p.mean2 <- p + 
#  stat_summary(fun.y="mean", na.rm = T, colour="blue", size=1, geom="line")
#print(p.mean2)

#alternative for mean plot
#p0 <- ggplot(micro.learning.k, aes(x = day, y = time)) +
#  facet_wrap(~subtask, ncol=7) 
#p0.mean <- p0 + 
#  stat_summary(fun.y="mean", na.rm = T, colour="red", size=1, geom="line")
#p0.mean
# aggregate the data by day and subtask for returning mean


## Plot with KLM prediction
klm.data <- data.frame(z= c(rep(19.96,4), rep(16.27,4), rep(101.26,4), 
                              rep(18.09,4), rep(106.43,4), rep(22.79,4), 
                              rep(143.83,4), rep(18.09,4), rep(141.42,4), 
                              rep(18.09,4), rep(21.03,4), rep(6.93,4),
                              rep(16.68,4), rep(15.80,4)), 
                         new.data)
p.mean.klm <- p.mean + 
  geom_hline(aes(yintercept = z), color="red", linetype="dashed",
                     klm.data)

## Figure 3 in ICCM16
print(p.mean.klm)


## 3.2. Scatter plot in log-log

# base: scatter
p.log <- p + scale_x_log10() + scale_y_log10() +
  #coord_trans(x="log10", y="log10") +
  labs(title="Subtask Completion Time in log-log Coordinates", 
       x ="logDay", y="logTime")
print(p.log)

## with linear line
p.log1 <- p.log+stat_smooth(method=lm, na.rm = T, se=F)
print(p.log1)

## add equations, r^2 // not done

# coefficient data from lme analysis from Section 4
#a=intercept, b =slope
lme.coeff <- data.frame(a = c(1.596300, 1.652294, 2.401673,
                              1.891016, 2.354749, 1.563019,
                              2.216870, 1.570739, 2.332884,
                              1.461958, 1.885475, 1.438642,
                              1.442136, 1.699970),
                        b = c(-0.4336350, -0.5494246, -0.7054928,
                              -1.2025624, -0.6239880, -0.6751516,
                              -0.4764150, -0.6212238, -0.5114052,
                              -0.4342365, -0.8917299, -0.9272653,
                              -0.6211262, -0.5312155),
                        type = c("S1:FileOpen",
                                 "S2:SaveAs",
                                 "S3:NormCalc",
                                 "S4:Sum",
                                 "S5:FreqCalc",
                                 "S6:Sum",
                                 "S7:Length",
                                 "S8:TotalLength",
                                 "S9:TypdChar",
                                 "S10:TotalTypdChar",
                                 "S11:InsRows",
                                 "S12:Name",
                                 "S13:Date",
                                 "S14:SavePrn"))

require(plyr)
eq <- substitute(italic(y) == a + b%.%itallic(x),
                 list(a=format(lme.coeff[1], digits=2),
                      b=format(lme.coeff[2], digits=2)))
as.character(as.expression(eq))

lme.coeff$group <-c(1:14)
#fix bug
eq1 <- ddply(lme.coeff, .(group, type), eq)

pt<- p.log1 + geom_text(data=eq,aes(x = 2, y = 420,label=V1), 
                        parse = TRUE, inherit.aes=FALSE, size=3, 
                        colour = "gray")
pt


q1 <- q + geom_smooth(method = lm, na.rm = T)
print(q1)

q2 <- p.log + geom_abline(aes(intercept=a, slope=b), 
                      color="blue", linetype="dashed",na.rm=T,lme.coeff)+
  facet_wrap(~type,ncol=7)
print(q2)

q3 <- p.log + geom_line

# regression line only
r <- ggplot(data=)

test <- ggplot(new.data, aes(x=day, y=time, group=subtask)) +
  facet_wrap(~subtask, ncol=7)+geom_point()+
  coord_trans(x="log10", y="log10")

## this is wrong...
p.log+geom_abline(aes(intercept = a, slope = b), data = lme.coeff)

###
###   4. Mixed Effect Model
###

#library
library(lme4)

##  Check with missing values in time column
which(is.na(micro.learning.k$time))
# 48 missing values

##  Constructing Model

## Check the assumption

# plot with ordinary least square regression line
plot(micro.learning.k$day, micro.learning.k$time, pch=16, 
     col=rgb(0,0,204,102,maxColorValue=255))
olsLine <- lm(micro.learning.k$time ~ micro.learning.k$day)
abline(olsLine, col="red")

# check the coorelation
summary((olsLine))
# THE SUMMARY OF THE BASIC O.L.S. REGRESSION SUGGESTS THAT 
# THERE IS A STATISICALLY SIGNIFICANT CORRELATION BETWEEN time and day

# check the normality assumption
qqnorm(residuals(olsLine))
qqline(residuals(olsLine))
# --> Q-Q Plot suggest the residuals are not normally distributed

# log transform
micro.learning.k$logDay <- log10(micro.learning.k$day)
micro.learning.k$logTime <- log10(micro.learning.k$time)
ols2 <- lm(micro.learning.k$logTime ~ micro.learning.k$logDay)
qqnorm(residuals(ols2))
qqline(residuals(ols2))

# linear mixed effects model
mod0 <-lmer(logTime~(1|pID)+(1|subtask), 
            data = micro.learning.k, REML = F)
mod1 <- lmer(logTime ~ (1 | pID), 
              data = micro.learning.k)
coef(mod1)$pID[1]
fixef(mod1)['(Intercept)'] + ranef(mod1)$pID

summary(mod1)

# including fixed effects
#install.packages("dplyr")
library(dplyr)

mod2 <- lmer(logTime~logDay+(1|pID), 
             data = micro.learning.k, REML = F)
summary(mod2)

coef(mod2)

mod3 <- lmer(logTime~logDay+(1|pID)+(1|subtask), 
             data = micro.learning.k, REML = F)

summary(mod3)

anova(mod2, mod1)

## result 1: 
anova(mod2, mod3)
#Data: micro.learning.k

#Models:
#  object: logTime ~ logDay + (1 | pID)
#..1: logTime ~ logDay + (1 | pID) + (1 | subtask)
#Df     AIC     BIC  logLik deviance  Chisq Chi Df Pr(>Chisq)    
#object  4  1672.5 1694.13 -832.27   1664.5                             
#..1     5 -1006.8 -979.84  508.41  -1016.8 2681.4      1  < 2.2e-16 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1


anova(mod3, mod0)


mod4 <- lmer(logTime~logDay+(1+logDay|pID)+(1+logDay|subtask), 
             data = micro.learning.k, REML = F)

summary(mod4)
coef(mod4)



## lmerTest
#install.packages("lmerTest")
library(lmerTest)

# random slopes for subtask
mod5 <- lmer(logTime~logDay+(1|pID)+(1+logDay|subtask), 
             data = micro.learning.k, REML = F)

summary(mod5)
coef(mod5)

#$subtask
#(Intercept)     logDay
#S1:FileOpen          1.596632 -0.4345600
#S2:SaveAs            1.657985 -0.5653091
#S3:NormCalc          2.401562 -0.7052524
#S4:Sum               1.891906 -1.2038078
#S5:FreqCalc          2.354736 -0.6240242
#S6:Sum               1.564564 -0.6782297
#S7:Length            2.217044 -0.4769718
#S8:TotalLength       1.569021 -0.6169571
#S9:TypdChar          2.332999 -0.5118074
#S10:TotalTypdChar    1.461769 -0.4342316
#S11:InsRows          1.885252 -0.8910907
#S12:Name             1.438460 -0.9266803
#S13:Date             1.441696 -0.6203430
#S14:SavePrn          1.701905 -0.5368610

## result 2
anova(mod5, mod3)



anova(mod5, mod4)


mod6 <- lmer(logTime~logDay+(1+logDay|pID)+(1|subtask), 
             data = micro.learning.k, REML = F)


summary(mod6)
anova(mod4, mod6)


qqnorm(residuals(mod1))
qqline(residuals(mod1))

summary(mod1)
# no p-value in "summary"



## coeffcients
coef(mod1)


## item effects
micro.learning.k$subtask = factor(micro.learning.k$subtask)
ggplot(micro.learning.k, aes(subtask, y=time, colour=subtask)) +
  geom_boxplot()+scale_x_discrete(breaks=c("S1:FileOpen",
                                           "S2:SaveAs",
                                           "S3:NormCalc",
                                           "S4:Sum",
                                           "S5:FreqCalc",
                                           "S6:Sum",
                                           "S7:Length",
                                           "S8:TotalLength",
                                           "S9:TypdChar",
                                           "S10:TotalTypdChar",
                                           "S11:InsRows",
                                           "S12:Name",
                                           "S13:Date",
                                           "S14:SavePrn"),
                                  labels=c("1","2","3","4","5","6","7",
                                           "8","9","10","11","12","13","14"))
                                  
                                  #labels=c("S1","S2","S3","S4","S5","S6","S7",
                                  #         "S8","S9","S10","S11","S12","S13","S14"))

mod7 <- lmer(logTime~logDay+(1|pID)+(1|subtask), data = micro.learning.k)
summary(mod7)

mod2 <- lmer(logTime~logDay+(1|pID), data = micro.learning.k)
summary(mod2)
anova(mod7, mod2, refit=F)
coef(mod7)
ranef(mod7)

#varying slopes
d_bysubtask = na.omit(micro.learning.k) %>%
  group_by(subtask, day) %>%
  summarise(mean_time = mean(time))

ggplot(d_bysubtask, aes(x=day, y=mean_time, 
                         colour=subtask, group=subtask, label=subtask)) +
  geom_line() + geom_point(shape=21, fill="white") +geom_text()

mod7b <- lmer(logTime~logDay+(1|pID)+(1 + logDay|subtask), data = micro.learning.k)
summary(mod7b)
coef(mod7b)

## Result 2
anova(mod7, mod7b, refit=F)

# interpretation
# 1. To assess the significance of practice trials (day) as a predictor, we 
# looked at the t-value of the fixed effects. The t-value of the slope estimate 
# is large enough, we can estimate that the predictor is significant
# since our dataset is fairly large with 1680 observations. 
# 
# 2. In our model, the intercepts and slopes are not correlated by
# the covariates of either participants and subtasks.  ????



###   slopes, mops, keys


#Slope <- lme.coeff[2]

#(Intercept)     logDay
#S1:FileOpen          1.596632 -0.4345600
#S2:SaveAs            1.657985 -0.5653091
#S3:NormCalc          2.401562 -0.7052524
#S4:Sum               1.891906 -1.2038078
#S5:FreqCalc          2.354736 -0.6240242
#S6:Sum               1.564564 -0.6782297
#S7:Length            2.217044 -0.4769718
#S8:TotalLength       1.569021 -0.6169571
#S9:TypdChar          2.332999 -0.5118074
#S10:TotalTypdChar    1.461769 -0.4342316
#S11:InsRows          1.885252 -0.8910907
#S12:Name             1.438460 -0.9266803
#S13:Date             1.441696 -0.6203430
#S14:SavePrn          1.701905 -0.5368610
smk <- data.frame(Slope=c(-0.4336350,-0.5494246,-0.7054928,-1.2025624, 
                          -0.6239880, -0.6751516, -0.4764150, -0.6212238,
                          -0.5114052, -0.4342365, -0.8917299, -0.9272653,
                          -0.6211262, -0.5312155), 
                  MOps=c(3,3,20,4,20,4,39,4,40,4,2,2,4,3), 
                  KOps=c(33,26,158,27,169,37,194,27,186,27,39,9,24,25))
attach(smk)
smk
# add subtask
smk$subtask <- c("S1:FileOpen",
                 "S2:SaveAs",
                 "S3:NormCalc",
                 "S4:Sum",
                 "S5:FreqCalc",
                 "S6:Sum",
                 "S7:Length",
                 "S8:TotalLength",
                 "S9:TypdChar",
                 "S10:TotalTypdChar",
                 "S11:InsRows",
                 "S12:Name",
                 "S13:Date",
                 "S14:SavePrn")

str(smk)

## Figure 5 in ICCM16
ggplot(smk, aes(MOps, Slope, KOps)) + 
  geom_point()
  

smk.data <- read.csv("~/rdata/iccm16/slope.csv", header = T)

smk.data$Subtask <- factor(smk.data$Subtask, 
                           levels = c("S1","S2","S3","S4","S5","S6","S7",
                                      "S8","S9","S10","S11","S12","S13","S14"),
                           labels =c("1","2","3","4","5","6","7",
                                     "8","9","10","11","12","13","14"))

ggplot(smk.data, aes(Count, Slope, color=Ops, label=Subtask, shape=Ops)) +
  geom_point(size=4) +
  geom_text(vjust = 0, nudge_y = 0, colour="black", check_overlap = F, size=4.5)+
  labs(x ="Operator Count", y="Slope")

