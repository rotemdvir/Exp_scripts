##  R script for Experimental Analysis  ##
##  Rotem Dvir  ##
##  June 2020  ##

# Set Working Directory
setwd("~/Dropbox/TAMU/Extra_Projects/Git_edits/Exp_scripts")
getwd()

# Packages
library(psych)  ###needed for describe()
library(car)    ###needed for Anova()
library(stargazer)  ###needed for Reg tables
library(xtable)     ###needed for ANOVA tables
options(xtable.floating=FALSE)
options(xtable.timestampt="")
library(foreign)
library(ggplot2)
library(gplots)
library(MASS)
library(Hmisc)
library(ggthemes)
library(plyr)
library(devtools)
library(gridExtra)
library(dplyr)
library(reshape2)
library(coefplot)
library(ggpubr)

# Set Randomizer
set.seed(2020)

# Upload Data 
# Insert file name; data file must in the working directory
MyData <- read.csv("Example_Data.csv", header=TRUE, sep = ",", strip.white = T, na.strings = "")
View(MyData)

# Set conditions IV's as factorial
MyData$trt1<-as.factor(MyData$trt1)
MyData$trt2<-as.factor(MyData$trt2)

# Create cont. DV
MyData$dv_edit <- ifelse(MyData$dv_binary == 0, -1, 1)
MyData$dv_cont <- MyData$dv_edit * MyData$binary_strgth

# Setting the contrasts for the sum of squares
options(contrasts=c("contr.helmert","contr.poly"))
  
#checking if the above command worked properly
options()

############# Descriptive ##############

# DV binary baplot

df1 <- MyData %>%
  group_by(dv_binary) %>%
  summarise(counts = n())

binary_plot <- ggplot(df1, aes(dv_binary, counts)) + 
  geom_bar(fill = "#0073C2FF", stat = "identity", width = 0.5) +
  geom_text(aes(label = counts), vjust = -0.3) + 
  xlab("Selected option 0/1") +
  theme_bw()

# DV CONT plot

cont_plot <- ggplot(MyData, aes(x = dv_cont)) +
  geom_histogram(aes(y = ..density..), colour="black", fill="white", binwidth = 1) +
  geom_density(alpha = 0.2, fill = "#FF6666") +
  xlab("DV Cont.Values") +
  theme_bw()

ggarrange(binary_plot, cont_plot,
          nrow = 1, ncol = 2)

# The size of each conditions (two treatments)

table(MyData$trt1)
table(MyData$trt2)

# Extract the means for DV based on either condition

mean_1 <- tapply(MyData$dv_binary, MyData$trt1, mean)
print(mean_1)

mean_2 <- tapply(MyData$dv_binary, MyData$trt2, mean)
print(mean_2)


############# ANALYSIS ##############

##  ANOVA Analysis Type I ##

# ANOVA model
summary(m3 <- aov(dv_binary ~ trt1 + trt2 + trt1 * trt2, data = MyData))
summary(m3a <- aov(dv_binary ~ trt1 + trt2 + trt1 * trt2 + Gender + Race + Partisanship, data = MyData))

# Table of means for DV based on either condition
model.tables(m3, "means", sd = T)

# Checking ANOVA assumptions

# The homogeneity of variance assumption: plot with/without a reference line
plot(m3, 1)
plot(m3$fitted,m3$res,xlab="Fitted",ylab="Residuals")
leveneTest(dv_binary ~ trt1*trt2, data = MyData)

# Normality Assumption: plot with/without a reference line
plot(m3, 2)
qqnorm(m3$res)


## Regression models
# This example uses Probit model; can be used with other model specifications 

summary(m1 <- glm(dv_binary ~ trt1 + trt2 + trt1 * trt2, data = MyData, family=binomial(link="probit")))
Anova(m1, type=3)

summary(m2 <- glm(dv_binary ~ trt1 + trt2 + trt1 * trt2 + Gender + Race + Partisanship, data = MyData, family=binomial(link="probit")))

# Linear model: Ordinal or Cont. DV model
summary(m1a <- lm(dv_cont ~ trt1 + trt2 + trt1 * trt2, data = MyData))
Anova(m1a, type=3)

summary(m2a <- lm(dv_cont ~ trt1 + trt2 + trt1 * trt2 + Gender + Race + Partisanship, data = MyData))

# Regression table for all models
stargazer(m1, m2, m1a, m2a, type = "latex", header = FALSE, 
          title = "Regression Results", 
          dep.var.labels = c("Binary DV", "Cont. DV"),
          covariate.labels = c("Treatment1", "Treatment2",
                               "Gender", "Race", "Partisanship",
                               "Treat1 x Treat2"),
          notes.align = "l",
          omit.stat = c("LL","ser", "f"), no.space = TRUE) 


######  Visualization options ########

# Plot Group means 

plotmeans(dv_binary ~ trt1, data = MyData, connect = FALSE, ylim = c(0.3,1), 
          xlab = "Treatment 1", ylab = "DV probability")
plotmeans(dv_binary ~ trt2, data = MyData, connect = FALSE, 
          xlab = "Treatment 2", ylab = "DV probability")

# Coefficient plots for main models

coef1 <- coefplot(m2, intercept=FALSE, title = "Binary DV Model", decreasing = T,
                  color="red", ylab = "", xlab = "Coefficient Value", pointSize = 2, zeroLWD = 1,
                  outerCI = 1.984, innerCI = 1.984, zeroType = 3, zeroColor = "black", sort="natural",
                  newNames=c(trt11="Treatment_1", trt21 = "Treatment_2"))#
coef1 <- coef1 + theme_pubr()

coef2 <- coefplot(m2a, intercept=FALSE, title = "Cont. DV Model", decreasing = T,
                  color="blue", ylab = "", xlab = "Coefficient Value", pointSize = 2, zeroLWD = 1,
                  outerCI = 1.984, innerCI = 1.984, zeroType = 3, zeroColor = "black", sort="natural",
                  newNames=c(trt11="Treatment_1", trt21 = "Treatment_2"))#
coef2 <- coef2 + theme_pubr()

ggarrange(coef1, coef2,
          nrow = 1, ncol = 2)

## Interaction plots

# This command is simpler, more useful for continous variables
interaction.plot(MyData$trt1, MyData$trt2, MyData$dv_binary)

# Another option for plotting the interaction of treatments (by conditions)  
library(FSA)
mm = Summarize(dv_binary ~ trt1 + trt2, data = MyData)
mm$se = mm$sd / sqrt(mm$n)
mm$se = signif(mm$se, digits=3)
pd = position_dodge(.2)

ggplot(mm, aes(x = trt1, y = mean, color = trt2)) +
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se), width=.2, size=0.7, position=pd) +
  geom_point(shape=20, size=3, position=pd) +
  xlab("Treatment 1") + ylab("Probability for DV") +
  ggtitle("Interaction plot") +
  theme_bw() + labs(color = "Treatment 2")

# Plot interaction of treatments using grouped barplot

MyData2 <- describeBy(MyData$dv_binary,list(MyData$trt1,MyData$trt2), mat=TRUE,digits=2)
MyData2

names(MyData2)[names(MyData2) == 'group1'] = 'Treatment1'
names(MyData2)[names(MyData2) == 'group2'] = 'Treatment2'

levels(MyData2$Treatment1)[levels(MyData2$Treatment1)=='0'] = 'V0'
levels(MyData2$Treatment1)[levels(MyData2$Treatment1)=='1'] = 'V1'

levels(MyData2$Treatment2)[levels(MyData2$Treatment2)=='0'] = 'Value0'
levels(MyData2$Treatment2)[levels(MyData2$Treatment2)=='1'] = 'Value1'

MyData2$se <- MyData2$sd/sqrt(MyData2$n)

limits = aes(ymax = mean + se, ymin=mean - se)
dodge = position_dodge(width=0.9)
apatheme=theme_bw()+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        panel.border=element_blank(),
        axis.line=element_line(),
        text=element_text(family='Times'))

plot.grp <- ggplot(MyData2, aes(x = Treatment2, y = mean, fill = Treatment1))+
  geom_bar(stat='identity', position=dodge)+
  geom_errorbar(limits, position=dodge, width=0.1)+
  apatheme + scale_y_continuous(expand=c(0,0)) +
  ylab('Probability of DV') +
  ggtitle("Interaction of Treatments")
plot.grp

# Plotting means with bootstrapping 
# The code below creates density plots of the mean results of the DV based on 2500 bootstraps
# To adjust the code: replace the DV and both conditions of the treatments
# Requires a binary IV - treatment, DV can be cont. or binary

B=2500
n =nrow(MyData)
c1a.sample <- with(MyData, matrix(sample(dv_binary[trt1 == 0], size = n[1]*B, replace = T), B, n[1]))
c1a.means <- apply(c1a.sample, 1, mean)
ggplot(data.frame(MeanChoice = c1a.means),aes(x=MeanChoice)) + ylim(0,35) + xlim(0.3,0.9) +
  geom_density(color="blue", fill="blue", alpha=0.3)

c1b.sample <- with(MyData, matrix(sample(dv_binary[trt1 == 1], size = n[1]*B, replace = T), B, n[1]))
c1b.means <- apply(c1b.sample, 1, mean)
ggplot(data.frame(MeanChoice = c1b.means),aes(x=MeanChoice)) + ylim(0,35) + xlim(0.3,0.9) +
  geom_density(color="red", fill="red", alpha=0.3)

c2a.sample <- with(MyData, matrix(sample(dv_binary[trt2 == 0], size = n[1]*B, replace = T), B, n[1]))
c2a.means <- apply(c2a.sample, 1, mean)
ggplot(data.frame(MeanChoice = c2a.means),aes(x=MeanChoice)) + ylim(0,35) + xlim(0.3,0.9) +
  geom_density(color="purple", fill="purple", alpha=0.3)

c2b.sample <- with(MyData, matrix(sample(dv_binary[trt2 == 1], size = n[1]*B, replace = T), B, n[1]))
c2b.means <- apply(c2b.sample, 1, mean)
ggplot(data.frame(MeanChoice = c2b.means),aes(x=MeanChoice)) + ylim(0,35) + xlim(0.3,1) +
  geom_density(color="green", fill="green", alpha=0.3)

df <- data.frame(x=c1a.means, y=c1b.means)
df.m <- melt(df)
names(df.m)[names(df.m)=="variable"] <- "Treatment_A"
df.m$Treatment_A <- as.character(df.m$Treatment_A)
df.m$Treatment_A[df.m$Treatment_A=="x"] <- "Value0"
df.m$Treatment_A[df.m$Treatment_A=="y"] <- "Value1"
plot1 <- ggplot(df.m) + geom_freqpoly(aes(x=value, y=..density.., colour=Treatment_A)) +
  labs(x='Means for Treatment A') + theme_bw()
plot1

df1 <- data.frame(x=c2a.means, y=c2b.means)
df1.m <- melt(df1)
names(df1.m)[names(df1.m)=="variable"] <- "Treatment_B"
df1.m$Treatment_B <- as.character(df1.m$Treatment_B)
df1.m$Treatment_B[df1.m$Treatment_B=="x"] <- "Value0"
df1.m$Treatment_B[df1.m$Treatment_B=="y"] <- "Value1"
plot2 <- ggplot(df1.m) + geom_freqpoly(aes(x=value, y=..density.., colour=Treatment_B)) +
  labs(x='Means for Treatment B') + theme_bw()
plot2

pp <- ggplot(df.m, aes(x=value)) + geom_density(aes(group=Treatment_A, colour=Treatment_A, fill=Treatment_A), alpha=0.3) +
  labs(x='Means for Treatment A') + theme_bw() + ylim(0,35) 
pp

pp1 <- ggplot(df1.m, aes(x=value)) + geom_density(aes(group=Treatment_B, colour=Treatment_B, fill=Treatment_B), alpha=0.3) +
  labs(x='Means for Treatment B') + theme_bw() + ylim(0,35)
pp1

pp.join <- ggarrange(pp, pp1, legend = "top",
                     nrow = 1, ncol = 2)

annotate_figure(pp.join,
                top =  text_grob("DV Means, by treatment (bootstrapped sample)", color = "black", face = "bold", size = 14))



