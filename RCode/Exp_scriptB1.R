##  R script for Experimental Analysis  ##
##  Rotem Dvir  ##
##  June 2020  ##

# Set Working Directory
setwd("~/Dropbox/TAMU/Extra_Projects/Git_edits")
getwd()

# Packages
library(psych)  
library(foreign)
library(MASS)
library(Hmisc)
library(plyr)
library(devtools)
library(dplyr)
library(fabricatr)

# This script contains procedures to generate random data for analysis
# I detail the commands using the fabricate package (Blair et al. 2019)
# All analyses procedures are detailed in Exp_scriptA.R

# Set Randomizer: important for the creation of random values
# If you want new random vars, change the seed value before creating the data again
set.seed(2020)

### Build dataset of 1000 observations

# I create  2 treatments with equal size groups 
d1 <- tibble(
  id = 1:250,
  trt1 = 0:0,
  trt2 = 0:0) 

d2 <- tibble(
  id = 251:500,
  trt1 = 0:0,
  trt2 = 1:1) 

d3 <- tibble(
  id = 501:750,
  trt1 = 1:1,
  trt2 = 0:0) 

d4 <- tibble(
  id = 751:1000,
  trt1 = 1:1,
  trt2 = 1:1) 

MyData <- bind_rows(d1, d2, d3, d4)

# create random variables:
# DV: binary; two categorical vars (values 1-5)
random_vars1 <- fabricate(
  N = 1000,
  edu_cat = draw_categorical(N = N, prob = c(0.1, 0.25, 0.3, 0.1, 0.05)),
  inc_cat = draw_categorical(N = N, prob = c(0.15, 0.35, 0.25, 0.15, 0.1)),
  x = 10 * rnorm(N),
  dv_binary = draw_binary(latent = x, link = "probit"))

rand_ed <- select(random_vars1, edu_cat, inc_cat, dv_binary)

# We can generate random variables that are correlated to existing variables
# I add two binary correlated DVs
# I add two continuous correlated IVs.
# The rho parameter sets the correlation level with the desired variable

random_vars2 <- fabricate(
  N = 1000,
  dv_cor1 = correlate(given = MyData$trt1, rho = 0.8,
                      draw_binary, N = 1000, prob = 0.65),
  dv_cor2 = correlate(given = MyData$trt2, rho = 0.65,
                      draw_binary, N = 1000, prob = 0.35),
  cont_cor1 = correlate(given = MyData$trt1, rho = 0.55,
                        rnorm, mean = 1500, sd = 300),
  cont_cor2 = correlate(given = MyData$trt2, rho = 0.75,
                        rnorm, mean = 3.5, sd = 1.5)
)

cr.dat <- select(random_vars2, dv_cor1, dv_cor2, cont_cor1, cont_cor2)

# Final dataset
MyData <- cbind(MyData, cr.dat, rand_ed)

# View the data file
head(MyData, n=10)

# To check the data, we can test for correlations between some of the variables
# The commands below utilize visual tools to show correlations among the generated variables
library(GGally)
library(corrplot)

c.dat <- select(MyData, trt1, trt2, dv_cor1, dv_cor2, cont_cor1, cont_cor2, edu_cat, inc_cat, dv_binary)

ggcorr(c.dat, palette = "RdBu", label = TRUE)
ggpairs(c.dat)

m <- cor(c.dat)
m2 <- round(m,2)

corrplot(m2, method = "number")
corrplot(m2, type="upper", sig.level = 0.05)

############### Analysis ###################
# Using the new dataset created, we can run all the different procedures detailed in Exp_scriptA R file
# First step is to set the (randomly generated) experimental treatments as factors

# Set conditions IV's as factorial
MyData$trt1<-as.factor(MyData$trt1)
MyData$trt2<-as.factor(MyData$trt2)

# Setting the contrasts for the sum of squares
options(contrasts=c("contr.helmert","contr.poly"))

#checking if the above command worked properly
options()

# Move to data description and analysis
