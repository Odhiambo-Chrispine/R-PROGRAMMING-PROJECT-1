#load libraries
library(tidyverse) #for manipulation of data, tidying and data visualization
library(caret) #one-stop solution for machine learning in R
library(ggplot2) #for creating graphics
library(GGally) #the extension of ggplot2
library(corrplot) #for plotting correlation
library(bayesplot) #plotting bayesian posterior
theme_set(bayesplot::theme_default(base_family = "sans"))
library(rstanarm) #for generating priors and posteriors
options(mc.cores = 1)
library(loo) #for data validation
library(projpred) #performs the projection predictive variable selection for 
#various regression models

#seed will be used to create reproducible results when writing code that 
#     involves creating variables that take on random values
SEED=53

#import dataset
data= read.csv("E:/PROJECT/DATA.csv")
attach(data)
head(data)

n=dim(data)[1]
p=dim(data)[2]

print(paste0("number of observations = ", n))
print(paste0("number of predictors = ", p))

#Plot correlation structure
corrplot(cor(data[, c(16,11:14)]))

#converting media exposure variable attributes into factors
MediaExposure <- factor(MediaExposure)

###################MULTIPLE LOGISTIC REGRESSION MODELLING#############################
model=glm(MediaExposure~No.Of.FPWorkers+LowClassFamily+HighClassFamily+EstimatesValues,binomial(link = "logit"))
summary(model)

y1=glm(MediaExposure~HighClassFamily,family = binomial)
y2=glm(MediaExposure~LowClassFamily,family = binomial)
y3=glm(MediaExposure~Number_Users,family = binomial)
y4=glm(MediaExposure~Number_NonUsers,family = binomial)
y5=glm(MediaExposure~No.Of.FPWorkers,family = binomial)
y6=glm(MediaExposure~EstimatesValues,family = binomial)

model2=glm(MediaExposure~HighClassFamily+LowClassFamily+EstimatesValues,
          family = binomial)

AIC(model2,k=2)
BIC(model2)

AIC(y1,y2,y3,y4,y5,y6,k=2)
BIC(y1,y2,y3,y4,y5,y6)

# preparing the inputs
x <- model.matrix(MediaExposure~No.Of.FPWorkers+LowClassFamily+HighClassFamily+
                    EstimatesValues, data = data)
y <- MediaExposure
(reg_formula <- formula(paste("MediaExposure~No.Of.FPWorkers+LowClassFamily+HighClassFamily+
                    EstimatesValues", collapse = " + ")))

##############BAYESIAN LOGISTIC REGRESSION MODELLING######################

t_prior <- student_t(df = 370, location = 0, scale = 5)
#t_prior=normal(location = 0, scale = NULL, autoscale = FALSE)
#t_prior=cauchy(location = 0, scale = NULL, autoscale = FALSE)
post1 <- stan_glm(reg_formula, data = data,
                  family = binomial(link = "logit"),
                  prior_PD = TRUE,
                  prior = t_prior, prior_intercept = t_prior, QR=TRUE,
                  seed = SEED, refresh=0, prior_aux = exponential(1/2))

post1
pplot<-plot(post1, prob = 0.95, prob_outer = 1,"hist")
pplot

#We can extract corresponding posterior median estimates using ‘coef’ function 
#   and to get a sense for the uncertainty in our estimates. 
round(coef(post1), 4)

#we can use the posterior_interval function to get Bayesian uncertainty 
#   intervals.Uncertainty intervals are computed by finding the relevant 
#   quantiles of the draws from the posterior distribution
round(posterior_interval(post1, prob = 0.9), 4)

#############Alternative: horseshoe prior on weights###############

p0 <- 2 # prior guess for the number of relevant variables
tau0 <- p0/(p-p0) * 1/sqrt(n)
hs_prior <- hs(df=1, global_df=1, global_scale=tau0) #The hierarchical shrinkage priors are normal with a mean of zero and a standard deviation that is also a random variable.
t_prior <- student_t(df = 370, location = 0, scale = 5)
post2 <- stan_gamm4(MediaExposure ~ s(No.Of.FPWorkers)+s(LowClassFamily)+
                      s(HighClassFamily)+s(EstimatesValues), data = data,
                    family = binomial(link = "logit"),prior = hs_prior, 
                    prior_intercept = t_prior, 
                    seed = SEED, refresh=0)
post2
pplot2<-plot(post2,"hist")
pplot2

#We can extract corresponding posterior median estimates using ‘coef’ function 
#   and to get a sense for the uncertainty in our estimates. 
round(coef(post2), 4)

#we can use the posterior_interval function to get Bayesian uncertainty 
#   intervals.Uncertainty intervals are computed by finding the relevant 
#   quantiles of the draws from the posterior distribution
round(posterior_interval(post2, prob = 0.9), 4)

#The latter question can be answered using leave-one-out cross-validation or 
#   the approximation thereof provided by the loo function in the loo package, 
#   for which a method is provided by the rstanarm package.
(loo2 <- loo(post2, save_psis = TRUE))

#plotting loo1
par(mfrow = 1:2, mar = c(5,3.8,1,0) + 0.1, las = 3)
plot(loo2, label_points = TRUE)

#Compute baseline result without covariates.The result will be good at (<0.5)
post3 <- update(post2, formula =MediaExposure ~ s(No.Of.FPWorkers)+s(LowClassFamily)+
                  s(HighClassFamily)+s(EstimatesValues), QR = FALSE, refresh=0)
(loo3 <- loo(post3))

plot(loo3, label_points = TRUE)

#When we compare the baseline model with and without covariates,the one with
#    covariates contain clearly useful information for predictions.
loo_compare(loo1, loo2)



##########For more easily interpretable predictive performance measures, we next
####compute posterior predictive probabilities and use them to compute 
#################classification error##########################################

# Predicted probabilities
linpred <- posterior_linpred(post2)
preds <- posterior_epred(post2)
pred <- colMeans(preds)
pr <- as.integer(pred >= 0.5)
# posterior classification accuracy
round(mean(xor(pr,as.integer(y==0))),2)
# posterior balanced classification accuracy
round((mean(xor(pr[y==0]>0.5,as.integer(y[y==0])))+
         mean(xor(pr[y==1]<0.5,as.integer(y[y==1]))))/2,2)

