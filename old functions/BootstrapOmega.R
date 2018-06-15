###############################################################################
######################### BootstrapOmega.R ####################################
######################### Anna-Lena Schubert ##################################
#############anna-lena.schubert@psychologie.uni-heidelberg.de##################
###############################################################################

################################################################################
# Bootstrapping of Omega2 contains options for calculating Omega2 in between- 
# and in within-subject designs. Between-subject designs work for all factorial 
# designs; within-subject designs currently only work for one-factorial designs
################################################################################

################################################################################
# If you want to bootstrap by drawing a sub-group of the population, set the 
# percentage of data drawn per bootstrap iteration to a specific value (e.g., 
# 80 for 80 %) and replacement to FALSE.
# If you want to bootstrap by drawing with replacement, set the percentage of 
# data drawn per bootstrap to 1 and replacement to TRUE.
################################################################################

rm(list=ls()) 

## Install and load required R packages
#install.packages("afex")
#install.packages("tidyr")
#install.packages("dplyr")
#install.packages("ggplot2")
library(afex)
library(tidyr)
library(dplyr)
library(ggplot2)

###############################################################################
#### Omega2 for between-subject cases (works with all designs) ################
###############################################################################

# Simulate some between-subject data for demonstration
DV<-c(rnorm(20,mean=17,sd=3),rnorm(20,mean=15,sd=3),rnorm(20,mean=12, sd=3))
DV[DV>20]<-20
DV[DV<0]<-0
Subject<-factor(seq(1,60))
Condition<-c(rep("Condition1",20), rep("Condition2",20), rep("Condition3",20))
AllData<-data.frame(Subject,Condition, DV)

# Specify bootstrap parameters
nSamples <- 1000  # numer of bootstrap samples
nPerc <- 1 # percentage of data drawn per bootstrap iteration, set to 1 for sampling with replacement
CIWidth <- 0.95        # width of confidence interval
replace = TRUE # set to TRUE if sampling with replacement

# Calculate ANOVA
aov_ez("Subject", "DV", AllData,
       between = "Condition")

# Generalize data and run bootstrap for nSamples iterations
omegaSquared <- matrix(NA, nSamples,1)
for (i in 1:nSamples) {
  sampleData <- AllData[sample(unique(AllData$Subject), 
                               nPerc*length(unique(AllData$Subject)), 
                               replace = replace),]
  colnames(sampleData) = c("Subject", "Condition", "DV")
  sampleData$DV <- as.numeric(sampleData$DV)
  resultsANOVA <- aov_ez("Subject", "DV", sampleData,
                         between = "Condition")
  f_squared <- (resultsANOVA$anova_table$`num Df`*(resultsANOVA$anova_table$F-1))/length(sampleData$Subject)
  omegaSquared[i,1] <- f_squared/(1+f_squared) 
}

bootstrapResults <- as.data.frame(omegaSquared)
colnames(bootstrapResults) <- "omega2"
# Optional: Fix Omega2s < 0 to 0
# bootstrapResults$omega2[bootstrapResults$omega2 < 0] <- 0

# Plot distribution and calculate CI

CI <- quantile(bootstrapResults$omega2, probs = c((1-CIWidth)/2, 1-(1-CIWidth)/2))

y <- hist(bootstrapResults$omega2, freq = F, main = "", xlab = "", ylab = " ",
          xlim = c(min(bootstrapResults$omega2 - 0.1),max(bootstrapResults$omega2 + 0.1)), 
          breaks = seq(0,1,by = 0.05), 
          ylim = c(0,21), yaxt = "n", col = "grey")
lines(density(bootstrapResults$omega2, from = min((bootstrapResults$omega2)), 
              to = max(bootstrapResults$omega2)), lwd = 4)
arrows(x0 = CI[1], y0 = 19, x1 = CI[2], y1 = 19, angle = 90, length = 0.1, 
       code = 3, lwd = 2.2)
text(paste(CIWidth*100,"% CI  = [",round(CI[1],2),";",
           round(CI[2],2), "]"), x = mean(CI), y = 21, cex = 1)
mtext(expression("Bootstrap Results for" ~ omega^2), 
      side = 3, line = -3.6, outer = TRUE, 
      cex = 1.5)


###############################################################################
### Omega2 for within-subject cases (works only for one-factorial designs) ####
###############################################################################

# Simulate some within-subject data for demonstration
y1 <- rnorm(20, mean = 20, sd = 5)
y2 <- rnorm(20, mean = 30, sd = 5)
y1[y1 < 0] <- 0
y2[y2 < 0] <- 0
DV <- c(y1,y2)
Subject<-rep(seq(1,20),2)
Subject<-factor(Subject)
Condition <- c(rep("Condition1",20), rep("Condition2",20))
AllData <- data.frame(Subject,Condition,DV)
factorLevels <- length(unique(AllData$Condition))
AllData <- AllData %>% spread(key=Condition, value = DV)

# Specify bootstrap parameters
nSamples <- 1000  # numer of bootstrap samples
nPerc <- 1 # percentage of data drawn per bootstrap iteration, set to 1 for sampling with replacement
CIWidth <- 0.95        # width of confidence interval
replace = TRUE # set to TRUE if sampling with replacement

# Calculate ANOVA
sampleData <- expData %>% gather(Subject)
colnames(sampleData) = c("Subject", "Condition", "DV")
aov_ez("Subject", "DV", sampleData,
       within = "Condition")

# Generalize data and run bootstrap for nSamples iterations
omegaSquared <- matrix(NA, nSamples,1)
for (i in 1:nSamples) {
  sampleData <- AllData[sample(unique(AllData$Subject), 
                               nPerc*length(unique(AllData$Subject)), 
                               replace = replace),]
  sampleData$Subject <- seq(1,length(unique(AllData$Subject)))
  sampleData <- sampleData %>% gather(Subject)
  colnames(sampleData) = c("Subject", "Condition", "DV")
  sampleData$DV <- as.numeric(sampleData$DV)
  resultsANOVA <- aov_ez("Subject", "DV", sampleData,
                         within = "Condition")
  f_squared <- (resultsANOVA$anova_table$`num Df`*(resultsANOVA$anova_table$F-1))/(length(sampleData$Subject)*factorLevels)
  omegaSquared[i,1] <- f_squared/(1+f_squared) 
}

bootstrapResults <- as.data.frame(omegaSquared)
colnames(bootstrapResults) <- "omega2"
# Optional: Fix Omega2s < 0 to 0
# bootstrapResults$omega2[bootstrapResults$omega2 < 0] <- 0

# Plot distribution and calculate CI

CI <- quantile(bootstrapResults$omega2, probs = c((1-CIWidth)/2, 1-(1-CIWidth)/2))

y <- hist(bootstrapResults$omega2, freq = F, main = "", xlab = "", ylab = " ",
          xlim = c(min(bootstrapResults$omega2 - 0.1),max(bootstrapResults$omega2 + 0.1)), 
          breaks = seq(0,1,by = 0.05), 
          ylim = c(0,21), yaxt = "n", col = "grey")
lines(density(bootstrapResults$omega2, from = min((bootstrapResults$omega2)), 
              to = max(bootstrapResults$omega2)), lwd = 4)
arrows(x0 = CI[1], y0 = 19, x1 = CI[2], y1 = 19, angle = 90, length = 0.1, 
       code = 3, lwd = 2.2)
text(paste(CIWidth*100,"% CI  = [",round(CI[1],2),";",
           round(CI[2],2), "]"), x = mean(CI), y = 21, cex = 1)
mtext(expression("Bootstrap Results for" ~ omega^2), 
      side = 3, line = -3.6, outer = TRUE, 
      cex = 1.5)

