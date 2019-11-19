## microgenetic subtask analysis
## Jong Kim (jongkim626@gmail.com)
## Date created: 18Feb16
## Last Modified: 5April16


###
### Table of Contents
###   1. Setting up the environment
###   2. Load data
###   3. Plot with aggregate data
###     3.1. Data manipulation and aggregate
###     3.2. Plotting with lattice
###     3.3. Plotting with qplot
###     3.4. Plotting with ggplot2, aggregated data
###     3.5. Plotting in log-log coordinates
###     3.6. Plotting with ggplot2, scatter (not aggregated)
###   4. Mixed effect model


###
###   1. setting up the environment (packages and library)
###

# for initial data inspection, using built-in plotting functions
library(nlme)
library(lattice)

## ggplot library
## install package
#install.packages("ggplot2")
library(ggplot2)

###
###   2. load data
###

micro.learning.k <- read.table("~/rdata/dismal-subtask/micro-learning-k.txt", 
                               header=T)
attach(micro.learning.k)
names(micro.learning.k)

micro.learning.k$subtask <- factor(micro.learning.k$subtask, levels = c(1:14),
                                 labels =c("S1: FileOpen",
                                           "S2: SaveAs",
                                           "S3: NormCalc",
                                           "S4: Sum",
                                           "S5: FreqCalc",
                                           "S6: Sum",
                                           "S7: Length",
                                           "S8: TotalLength",
                                           "S9:TypdChar",
                                           "S10: TotalTypdChar",
                                           "S11: InsRows",
                                           "S12: Name",
                                           "S13: Date",
                                           "S14: SavePrn"))


# check the variables
str(micro.learning.k)
head(micro.learning.k)


###
###   3. Plot with aggregate data with mean
###

##  3.1. data manipulation and aggregate
agg.learning.k <-aggregate(micro.learning.k$time, by=list(day, subtask), 
                           FUN=mean, na.rm=TRUE)
names(agg.learning.k)
head(agg.learning.k)
agg.learning.k$Group.2 <- factor(agg.learning.k$Group.2, levels = c(1:14),
                                 labels =c("S1: FileOpen",
                                           "S2: SaveAs",
                                           "S3: NormCalc",
                                           "S4: Sum",
                                           "S5: FreqCalc",
                                           "S6: Sum",
                                           "S7: Length",
                                           "S8: TotalLength",
                                           "S9:TypdChar",
                                           "S10: TotalTypdChar",
                                           "S11: InsRows",
                                           "S12: Name",
                                           "S13: Date",
                                           "S14: SavePrn"))

dat <- print(agg.learning.k)
task.time <- dat[,3]
trials.day <- dat[,1]
subtask <- dat[,2]

#
#print(agg.learning.k)
#list(dat)
#as.data.frame(list(dat))
#

##  3.2. Plotting with lattice
xyplot(task.time ~ trials.day | subtask, xlab = "Day", ylab = "Time", 
       main = "Task Completion Time by Subtasks")

##  3.3. Plotting with qplot
qplot(trials.day, task.time, data=agg.learning.k, facets = . ~ Group.2,
      main="Subtask Completion Time (s)",
      xlab="Trials(Day)",
      ylab= "Time (s)")

##  3.4. Plotting with ggplot2, aggregated data with mean
p <- ggplot(agg.learning.k, aes(trials.day, task.time)) + 
  geom_point(colour="black", size=2) + 
  facet_wrap(~Group.2, ncol=7) +
  labs(title="Subtask Completion Time", x ="Trials(Day)", y="Time(s)") +
  geom_smooth(method=lm,se=FALSE, size=1)
print(p) 

# add KLM prediction
hline.data <- data.frame(z= c(rep(19.96,4), rep(16.27,4), rep(101.26,4), 
                              rep(18.09,4), rep(106.43,4), rep(22.79,4), 
                              rep(143.83,4), rep(18.09,4), rep(141.42,4), 
                              rep(18.09,4), rep(21.03,4), rep(6.93,4),
                              rep(16.68,4), rep(15.80,4)), 
                         agg.learning.k)


p1 <- p + geom_hline(aes(yintercept = z), color="red", linetype="dashed",
                     hline.data)

print(p1)

##  3.5. Plotting in log-log coordinates

p.log <- ggplot(agg.learning.k, aes(trials.day, task.time)) + 
  geom_point(colour="black", size=2) +
  facet_wrap(~Group.2, ncol=7)+
  labs(title="Subtask Completion Time (log-log coordinates)", 
       x ="log10(Day)", y="log10(Time)") 
p.log1 <- p.log + scale_x_log10() + scale_y_log10() +
  geom_smooth(method=lm, se=F, size=1)
print(p.log1)

# not working
p.log2 <- p.log + geom_smooth(method=lm, na.rm = T) + 
  coord_trans(x="log10", y="log10")
print(p.log2)

##  3.6. Plotting with ggplot2, scatter (not aggregated)
q <- ggplot(micro.learning.k, aes(x = day, y = time)) +
  geom_point(colour="black", size=1, na.rm = T) +
  facet_wrap(~subtask, ncol=7) +
  labs(title="Subtask Completion Time", x ="Trials(Day)", y="Time(s)")
q1 <- q+ geom_smooth(method=lm, na.rm = T)
print(q)
print(q1)

## 3.7. Plotting with ggplot2, scatter (not aggregated)
r <- ggplot(micro.learning.k, aes(x = day, y = time)) +
  geom_point(colour="black", size=1, na.rm = T) +
  facet_wrap(~subtask, ncol=7)
print(r)
# log scale
r.ls <- r + scale_x_log10() + scale_y_log10() +
  labs(title="Subtask Completion Time in log-log scale", 
       x ="logDay", y="logTime")
print(r.ls)
# add regression line
r.line <- r.ls+geom_smooth(method=lm, na.rm = T)
print(r.line)


# regression line dataframe
#intercept: a
rline.data.a <- data.frame(a= c(1.596300, 1.652294, 2.401673, 
                                1.891016, 2.354749, 1.563019, 
                                2.216870, 1.570739, 2.332884, 
                                1.461958, 1.885475, 1.438642,
                                1.442136, 1.699970), 
                         micro.learning.k)
#slope: b
rline.data.b <- data.frame(b= c(-0.4336350, -0.5494246, -0.7054928
                                -1.2025624, -0.6239880, -0.6751516
                                -0.4764150, -0.6212238, -0.5114052
                                -0.4342365, -0.8917299, -0.9272653
                                -0.6211262, -0.5312155), 
                           micro.learning.k)

q1 <- q + geom_smooth(method = lm, na.rm = T)
print(q1)
q1 <- q + geom_abline(aes(intercept=rline.data.a, slope=rline.data.b), 
                      color="blue", linetype="dashed", micro.learning.k)
print(q1)


###
###  4. Mixed effect model 
###

### 4.1. Data Preparation

## load the data, it'done, refer to Section 2

# micro.learning.k
# agg.learning.k

## load library. it is done 
library(lme4)
library(nlme)
library(lattice)

## Check the data, factors
str(micro.learning.k)
head(micro.learning.k)
tail(micro.learning.k)
summary(micro.learning.k)
attach(micro.learning.k)
names(micro.learning.k)

# cross-tabulating participants and days
xtabs(~ pID + day, micro.learning.k)
# the data is balanced both with respect to the number of observations on each
# subject, and with respect to the times at which these observations were taken
# this is confirmed by cross-tabulating participants and days
# subtasks and participants are completely crossed
xtabs(~ pID + subtask, micro.learning.k)

xtabs(~ subtask + day)
xtabs(~ subtask + pID)

mytable <- xtabs(~subtask+pID+day, data=micro.learning.k)
ftable(mytable)
summary(mytable)

# it is not nested

##  Check with missing values in time column
which(is.na(micro.learning.k$time))
# 48 missing values

##  Preliminary Plot
# boxplot

##  4.2. Constructing Model

##  4.2.1 Check the assumption

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

# logtransform
micro.learning.k$logDay <- log10(micro.learning.k$day)
micro.learning.k$logTime <- log10(micro.learning.k$time)
ols2 <- lm(micro.learning.k$logTime ~ micro.learning.k$logDay)
qqnorm(residuals(ols2))
qqline(residuals(ols2))

# linear mixed effects model

mod01 <- lmer(logTime~logDay+(1+logDay|subtask)+(1+logDay|pID), 
              data = micro.learning.k, REML = F)

qqnorm(residuals(mod01))
qqline(residuals(mod01))

summary(mod01)
# no p-value in "summary"

mod02 <- lmer(logTime~logDay+(1|subtask)+(1|pID), 
                data = micro.learning.k, REML = F)

summary(mod02)

anova(mod01, mod02)

## coeffcients
coef(mod01)

# interpretation
# 1. To assess the significance of practice trials (day) as a predictor, we 
# looked at the t-value of the fixed effects. The t-value of the slope estimate 
# is large enough, we can estimate that the predictor is significant
# since our dataset is fairly large with 1680 observations. 
# 
# 2. In our model, the intercepts and slopes are not correlated by
# the covariates of either participants and subtasks.  ????




#########                 ###########
#########     Appendix    ###########
#########                 ########### 

##  4.2.1 Uncorrelated and Correlated Random Effects

#correlated
gm01 <- lmer(time~day +(1+day|pID), micro.learning.k, REML = F)
summary(gm01)

# uncorrelated
gm02 <- lmer(time ~ 1 + day + (1|pID) + (0+day|pID), micro.learning.k,REML=F)
summary(gm02)
head(ranef(gm02)[["pID"]])
# compare
anova(gm02, gm01)

##  4.2.1.  Random Intercepts Model for both subtasks and participants
model00 <- lmer(time~1+(1|subtask)+(1|pID), micro.learning.k, REML=F)
summary(model00)

model01 <- lmer(time~day+(1|subtask)+(1|pID), micro.learning.k, REML = F)
summary(model01)

# compare 
anova(model01,model00)

##  4.2.2.  Random Slopes Model
# where subtasks and participants are allowed to have not
# only different intercepts but also different slopes for the effect of the 
# task time
model02 <- lmer(time~day+(1+day|subtask)+(1+day|pID), micro.learning.k, REML=F)


##check assumption, residual, qqplot
model <- lm(time~day)
plot(model)

plot(model01)
plot(model02)
#qqnorm(model01)

#------------------5april2016
## a model with correlated random effects
fm01 <- lmer(time ~ 1 + day +(1+day|subtask), micro.learning.k, REML=F)
# this model incorporates both an intercept and a slope with respect to days 
#std dev 19.90, 
# the estimated correlation of the random effect (subtask) for the intercept
# the slope is high, -.99, confirming impression that there is evidence 
# of a systemic relationship between these quantities.
# it gives much information for predicting whether the task time will be 
# stroingly affected by the task time of each subtask
head(ranef(fm01)["subtask"])

## a model without correlated random effects
# suppress the implicit intercept in the random effect term (0+day|subtask), which
# is no intercept and day by subtask
fm02 <- lmer(time ~ 1 + day + (1|subtask) + (0+day|subtask), micro.learning.k,
             REML = F)
# to extract the conditional modes fo the randome effects
head(ranef(fm01)["subtask"])
anova(fm01,fm02)

#-------------------------------------------------------------------------
fm03 <- lmer(time~day + (1|subtask) + (1|pID), micro.learning.k, REML = F)

fm04 <- lmer(time~day+(1+day|subtask)+(1+day|pID), micro.learning.k, REML=F)

fm05 <- lmer(time~day + (1|pID), micro.learning.k, REML = F)

fm06 <- lmer(time~day + (1|subtask), micro.learning.k, REML = F)

fm07 <- lmer(time~day+(1+day|subtask), micro.learning.k, REML=F)

fm08 <- lmer(time~day+(1+day|pID), micro.learning.k, REML=F)

anova(fm03,fm04)

anova(fm03, fm05)

anova(fm03, fm06)


coef(fm03)

coef(fm04)

summary(fm04)

## I think this is wrong
# subtask as a fixed effect
fm10 <- lmer(time~day + subtask + (1|pID), micro.learning.k, REML = F)
fm11 <- lmer(time~day + (1|pID), micro.learning.k, REML = F)
fm12 <- lmer(time~ day*subtask + (1|pID), micro.learning.k, REML = F)

anova(fm10, fm11)

coef(fm10)
coef(fm11)
fm12 <- lmer(time~day + subtask + (1+day|pID), micro.learning.k, REML = F)

anova(fm10, fm12)
coef(fm12)

#----------------------------------------------------------------------
##  Construct the model: fixed and random effects
# time: response variable to predict task completion time
# time can be predicted by pID (participants) and subtasks variablity 

micro.model01 <- lmer(time ~ day + 
                           (1 | pID) +
                           (1 | subtask), data = micro.learning.k)
summary(micro.model01)

## interpretation of the result

## Random effects ##
# in the std column, pID has much less variability than subtasks
# Residuls indicating the variability that is not due to either pID and subtasks

## fixed effects ##
## about the slope
# the slope (-19.6356), changes from Day1 to Day4 
# this coefficient is the slope for the effect of trial (day)

## about the intercept

## add gender effect, gender as the fixed effect 
politeness.model <- lmer(frequency ~ attitude + gender +
                           (1 | subject) + (1|scenario), data = politeness)

summary(politeness.model)

### Statistical Significance, to get p-value
# Likelihood Ratio Test to attain p-value, this compares the likelihood of 
# two models with each other

# for comparision, construct the null and alternative model
# without and with attitude factor 
politeness.null <- lmer(frequency ~ gender + (1|subject) + (1|scenario), 
                        data = politeness, REML = FALSE)
# REML=F, this changes the likelihood estimator, now we can compare models
# using the likelihood ratio test

## now the full model
politeness.model <- lmer(frequency ~ attitude + gender +
                           (1|subject) + (1|scenario), data = politeness,
                         REML = FALSE)

# to compare two models with each other
anova(politeness.null, politeness.model)
## ... politeness affected pitch (X^2(1)= 11.62, p=0.0006), by lowering it by 
# about 19.721 +-5.6 

## now, we consdier the gender
# interaction
politeness.interaction <- lmer(frequency ~ attitude*gender +
                                 (1|subject) + (1|scenario), data = politeness,
                               REML = FALSE)
anova(politeness.model, politeness.interaction)
# not significant

###############################################################################
### random slopes vs. random intercepts

# look at the coefficients of the model
coef(politeness.model)
# the effect of politeness would be different by different subjects and 
# different items.  
# consider random slope model where subjects and items are allowed to have not
# only different intercepts but also different slopes for the effect of the 
# politeness.  
politeness.model2 <- lmer(frequency ~ attitude + gender +
                            (1+attitude|subject) +
                            (1+attitude|scenario), data = politeness, 
                          REML = F)
coef(politeness.model2)
# however, the coefficients in gender column do not change because we didn't 
# specify random slopes for the by-subject or by-item effect of gender
politeness.model3 <- lmer(frequency ~ gender +
                            (1+attitude|subject) +
                            (1+attitude|scenario), data = politeness, 
                          REML = F)
anova(politeness.model3, politeness.model2)
# significant!!
coef(politeness.model3)

## influential point






###############################################################################
#####   Appendix: Further Exploration with the Data   #########################
###############################################################################


##
micro <- groupedData(time~day|pID, outer = ~ subtask, micro.learning.k)
plot(micro)
plot(micro, outer = T)

# need to consider missing values in linear mixed effect (lme) model
# https://stat.ethz.ch/pipermail/r-help/2003-November/041490.html

## factor variables
#is.factor(micro$day)

micro$day <- factor(micro$day, levels=c(1,2,3,4),
                    labels =c("Day1", "Day2", "Day3", "Day4"))

# is.factor(micro$day) ==> TRUE

micro$subtask <- factor(micro$subtask, 
                        levels = c(1:14),
                        labels = c("Sub1", "Sub2", "Sub3", "Sub4", "Sub5",
                                   "Sub6", "Sub(7)", "Sub8", "Sub9",
                                   "Sub10", "Sub11", "Sub12", "Sub13", "Sub4")) 
#is.factor(micro$subtask)
#is.factor(micro$pID)



# how many NAs in the time column
sum(is.na(micro$time)) # 48 missing values (outof 1680) in the column 

micro.without.na <- na.omit(micro)
mymodel <-lme(time~micro$subtask, random= ~ 1 |pID, data = micro.without.na)

summary(mymodel) # interpretation of the results
# the subtask main effect is significant 





###############################################################################



geom_smooth(method=lm,se=FALSE, size=1)
#p5 <- geom_abline(intercept = coef(fit)[**], slope=coef(fit)[##], col='red')
print(p.log)




## add linear model equation to the plot
mod <- list()

lm_eqn <- function(micro.learning.k){
  
  m <- for(i in 1:14){
    mod[[paste("run",i,sep="")]] <- lm(log(time)~log(day), 
                                       subtask==i, data=micro.learning.k)
  }
  
  
  
  ;
  eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(r)^2~"="~r2, 
                   list(a = format(coef(m)[1], digits = 2), 
                        b = format(coef(m)[2], digits = 2), 
                        r2 = format(summary(m)$r.squared, digits = 3)))
  as.character(as.expression(eq));                 
}

q1 <- q + geom_text(x = 25, y = 300, label = lm_eqn(micro.learning.k), 
                    parse = TRUE)

print(q1)

################################################################################
### plot and regression in log-log scale
################################################################################

reg <- lm(log10(time)~log10(day), data=micro.learning.k)
myreg <-function(i) lm(log(time[subtask==i])~log(day[subtask==i]))

# example of subtask1
myreg.sub1 <- lm(log(time)~log(day), subtask==1, data=micro.learning.k)
summary(myreg.sub1)
myreg.sub2 <- lm(log(time)~log(day), subtask==2, data=micro.learning.k)

# apply for 14 subtasks
myreg.fn <-function(x){
    mylm <- lm(log(time)~log(day), subtask==x, data=micro.learning.k)
} 
sapply(1:14, myreg.fn)

#coef(summary(s))
#coef(summary(myreg.fn))["log(day)", "Estimate"]

## using for loop
mod <- list()
myreg.fn1 <- for(i in 1:14){
  mod[[paste("run",i,sep="")]] <- lm(log(time)~log(day), subtask==i, 
                                     data = micro.learning.k)
}

# ?summary.lm

myFun <-
  function(lm)
  {
    out <- c(lm$coefficients[1],
             lm$coefficients[2],
             length(lm$model$y),
             summary(lm)$coefficients[2,2],
             pf(summary(lm)$fstatistic[1], summary(lm)$fstatistic[2],
                summary(lm)$fstatistic[3], lower.tail = FALSE),
             summary(lm)$r.squared)
    names(out) <- c("intercept","slope","n","slope.SE","p.value","r.squared")
    return(out)}

myFun(mod$run1)

results <- list()
for (i in 1:length(mod)) results[[names(mod)[i]]] <- myFun(mod[[i]])
as.data.frame(results)

d <- as.data.frame(results)

d[1,1]


# how to extract coefficients 

coef(summary(myreg.sub1))["(Intercept)", "Estimate"]

coef(summary(myreg.sub1))["log(day)", "Estimate"]

# it is not working for all subtasks
coef(summary(result))["log(day)", "Estimate"]


# subset of data to use lm
head(micro.learning.k[micro.learning.k$subtask, ])

summary(lm(log10(time)~log10(day), subtask==1, data=micro.learning.k))

lm(log10(time)~log10(day), subtask==1, data=micro.learning.k)


# sub1
lm(log(time[subtask==1])~log(day[subtask==1]))
model1 <- lm(log(time[subtask==1])~log(day[subtask==1]))
plot(model1)
plot(log(time[subtask==1])~log(day[subtask==1]))
abline(lm(log(time[subtask==1])~log(day[subtask==1])))
summary(model1)
abline(model1)

#sub2
lm(log(time[subtask==2])~log(day[subtask==2]))

#sub3
lm(log(time[subtask==3])~log(day[subtask==3]))
#sub4
lm(log(time[subtask==4])~log(day[subtask==4]))
#sub5
lm(log(time[subtask==5])~log(day[subtask==5]))

mymodel <-function(i) lm(log(time[subtask==i])~log(day[subtask==i]))
a.b <- sapply(1:14, mymodel)
abline(mymodel)



micro.learning.k$subtask
subtask.f <- factor(micro.learning.k$subtask)
levels(subtask.f)
myreg1 <- lm(log10(time)~log10(day)+subtask.f, data=micro.learning.k)

summary(myreg1)
# summary of the current data
str(micro.learning.k)
head(micro.learning.k)

# one response varible and two explanatory variables


