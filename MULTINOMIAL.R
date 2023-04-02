data <- read.csv("E:/PROJECT/multinom_data.csv")
View(data)
attach(data)

########### PART ONE: Prepare and review the data ###########################

summary(data)

#In this: case FP_methods is outcome variable while 
        # economicStatus is predictor variable
# Load the jmv package for frequency table
library(jmv)
# Use the descritptives function to get the descriptive data
descriptives(data, vars = vars(economicStatus,FP_Methods, AG_15_49,AG_20_29,
                               AG_30_39, AG_40_49), freq = TRUE)

# To see the crosstable, we need CrossTable function from gmodels package
library(gmodels)
# Build a crosstable between admit and rank
CrossTable(economicStatus, FP_Methods)

######### PART TWO: Run the Multinomial Model using “nnet” package #############

# Load the multinom package
library(nnet)
# Since we are going to use lowLevel as the reference group, we need relevel the group.
FP_Methods2 <- relevel(as.factor(FP_Methods), ref = 2)
economicStatus <- as.factor(economicStatus)
levels(FP_Methods2)

# Give the names to each level
levels(FP_Methods2) <- c("lowLevel","mediumLevel","highLevel")
# Run a "only intercept" model
OIM <- multinom(FP_Methods2 ~ 1, data =data)
summary(OIM)

# Run a multinomial model
multi_mo <- multinom(FP_Methods2 ~ economicStatus + AG_20_29 + AG_40_49 + AG_20_29*AG_40_49, data = data,model=TRUE)
summary(multi_mo)

# Check the Z-score for the model (wald Z)
z <- summary(multi_mo)$coefficients/summary(multi_mo)$standard.errors
z

# 2-tailed z test
p <- (1 - pnorm(abs(z), 0, 1)) * 2
p

############# PART THREE: Check the model fit information ###################
# the anova function is confilcted with JMV's anova function, so we need to unlibrary the JMV function before we use the anova function.
detach("package:jmv", unload=TRUE)
#Compare the our test model with the "Only intercept" model
anova(OIM,multi_mo)

############### PART FOUR: Calculate the Goodness of fit #################

# Check the predicted probability for each year_category
head(multi_mo$fitted.values,30)

# We can get the predicted result by use predict function
head(predict(multi_mo),30)

# Test the goodness of fit
chisq.test(FP_Methods2,predict(multi_mo))

################### PART FIVE: Calculate the Pseudo R-Square ################
# Load the DescTools package for calculate the R square
library("DescTools")
# Calculate the R Square
PseudoR2(multi_mo, which = c("CoxSnell","Nagelkerke","McFadden"))

################ PART SIX: Likelihood Ratio Tests ########################

# Use the lmtest package to run Likelihood Ratio Tests
library(lmtest)
lrtest(multi_mo,"economicStatus") # Chi-Square=12.922,p=0.01166*

lrtest(multi_mo, "AG_20_29") # Chi-Square=10.613,p=0.004959*

lrtest(multi_mo, "AG_40_49") # Chi-Square=5.3874,p=0.06763 

lrtest(multi_mo, "AG_20_29:AG_40_49") # Chi-Square=5.249,p=0.072

################## PART SEVEN: Parameter Estimates ######################

# extract the coefficients from the model and exponentiate
exp(coef(multi_mo))

#################### FINAL PART: Build a classification table ###########

# Load the summarytools package to use the classification function
library(summarytools)
# Build a classification table by using the ctable function
ctable <- table(FP_Methods2,predict(multi_mo))
ctable







