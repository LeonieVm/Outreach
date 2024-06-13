#==============================================================================#
#                 Experimental Psychopathology Symposium 2023 
#                   by Joran Jongerling and Leonie Vogelsmeier
#==============================================================================#

### Packages required for Practical --------------------------------------------

#install.packages("ggplot2")
#install.packages("lmtest")
#install.packages("brms")
#install.packages("lme4")
#install.packages("tidyverse")
#install.pachages("tidybayes")


library(ggplot2)
library(lmerTest)
library(brms)
library(lme4)
library(tidyverse)
library(tidybayes)


### Longitudinal Data-----------------------------------------------------------

# load: Workspace-Emotions-Practical-Definitive.RData

#------------------------------------------------------------------------------#
# The dataset "gpa"  contains longitudinal data set on 200 college students. 
# The students' grade point average (GPA) has been recorded for six successive 
# semesters. At the same time, it was recorded whether the student held a job 
# in that semester, and for how many hours. This is recorded in a variable 
# 'job' (= hours worked). Finally, we also have the student-level variables     
# high school GPA and gender (0 = male, 1 = female).
#------------------------------------------------------------------------------#

as_tibble(gpa)

#### Spaghetti Plots------------------------------------------------------------

# Since you should always start with getting to know your data, let's plot 
# regression lines for the effects of time and job on GPA. This will allow us to
# to get an initial idea of the characteristics of our data that we likely have
# to account for in our model (e.g., shape of relations, 
# between-person differences, distribution of variables)

# Spaghetti plots                             
ggplot(data      = gpa,
       aes(x     = time,
           y     = gpa,
           col   = student,
           group = student))+ #to add the colors for different classes
  geom_point(size     = 1.2,
             alpha    = .8,
             position = "jitter")+ #to add some random noise for plotting
  theme_minimal()+
  theme(legend.position = "none")+
  scale_color_gradientn(colours = rainbow(100))+
  geom_smooth(method = lm,  #means regression per person                                                    
              se     = FALSE,
              size   = .5, 
              alpha  = .8)+ # to add regression line
  labs(title    = "GPA vs. Time",
       subtitle = "add colours for different students and regression lines")
# --> Overall, there seems to be an affect of time on GPA, but it differs      
#     across individuals.


ggplot(data      = gpa,
       aes(x     = job,
           y     = gpa,
           col   = student,
           group = student))+ #to add the colors for different classes
  geom_point(size     = 1.2,
             alpha    = .8,
             position = "jitter")+ #to add some random noise for plotting
  theme_minimal()+
  theme(legend.position = "none")+
  scale_color_gradientn(colours = rainbow(100))+
  geom_smooth(method = lm,
              se     = FALSE,
              size   = .5, 
              alpha  = .8)+ # to add regression line
  labs(title    = "GPA vs. Job",
       subtitle = "add colours for different students and regression lines")

# --> Something is  weird with job. It appears to be an ordinal variable 
#     more than a continuous one. Let's treat job as categorical.
#     Also note that slopes appear to differ across individuals. 

gpa$job_cat <- as.ordered(gpa$job)

#### ICC------------------------------------------------------------------------

# Start with the intercept only model in which you use gpa as the dependent 
# variable, and calculate the ICC. Also determine if multilevel analysis is 
# necessary based on the level-2 variance significance. If needed, ask for 
# instruction using ?lme
# Note that we use a model without predictors to get insight into the total(!!)    
# variance in our outcome and not just explained and/or residual variance.

IO <- lm(gpa ~ 1, gpa)                                                          
IO_ML <- lmer(gpa ~ 1 + (1 | student), gpa)
rand(IO_ML)# Or, instead of rand(), you can use:
anova(IO_ML,IO)

summary(IO_ML)
# The ICC is equal to 0.05714/(0.05714 + 0.09759) = 0.37                        


#### Multilevel Regression----------------------------------------------------- 
# Maximum approach: Run the full model in one go (using gpa as dependent
# variable, and as predictors time, the ordinal variable job_cat, sex, highgpa,
# and admitted. Start with checking the assumptions (see lecture slides).                                

# We use a Bayesian approach by using the brm because Bayesian estimation is
# more flexible when it comes to incorporating non-standard aspects (e.g. 
# non-normal distributions) of your data. In addition, it is more stable when 
# estimating complex models and also makes identifying and 
# diagnosing estimation issues easier.
# This model might take a long time to run, so the output is already added to
# the workspace
# MaximumModel_Bayes_GPA <- brm(gpa ~ 1 + time + mo(job_cat) + sex + highgpa +  
#                           admitted + (1 + time + mo(job_cat) | student), 
#                           iter = 5000,
#                           control = list(adapt_delta = 0.8), gpa)
# Note: the expression mo() simply means that we treat the categorical variable
# as ordinal.

# Check level 1 Residuals
hist(residuals(MaximumModel_Bayes_GPA)[,1])
qqnorm(residuals(MaximumModel_Bayes_GPA)[,1])

# Check level 2 Residuals
hist(ranef(MaximumModel_Bayes_GPA)$student[,,1][,1])                            
hist(ranef(MaximumModel_Bayes_GPA)$student[,,2][,1])
hist(ranef(MaximumModel_Bayes_GPA)$student[,,3][,1])
# note: ranef() extract the random effects of each level from a brmsfit object. 

qqnorm(ranef(MaximumModel_Bayes_GPA)$student[,,1][,1])
qqnorm(ranef(MaximumModel_Bayes_GPA)$student[,,2][,1])
qqnorm(ranef(MaximumModel_Bayes_GPA)$student[,,3][,1])
# --> Assumption checks look okay. 

# Let's inspect the results
summary(MaximumModel_Bayes_GPA)                                                     

# There is hardly any variation between individuals in the effect of job on gpa 
# (sd(mojob_cat) = 0.06) and the effect of time on gpa (sd(time) = 0.05). 
# If there is no between-person variance in slopes, we do not add cross-level 
# interactions as there is no variance in the slopes to explain, so adding a 
# predictor for it does not make sense.


#### Explained Variance---------------------------------------------------------

# Now, we calculate the explained variances on Level 1 and 2:
VarianceIO <- as.data.frame(VarCorr(IO_ML))


VarianceMaximumModel <- c(((VarCorr(MaximumModel_Bayes_GPA)$residual__$sd)[1])^2
                          ,((VarCorr(MaximumModel_Bayes_GPA)$student$sd)[1])^2)

# Explained Variance on Level 1
(VarianceIO[2,4] - VarianceMaximumModel[1])/ VarianceIO[2,4]
# .549

# Explained Variance on Level 2
(VarianceIO[1,4] - VarianceMaximumModel[2])/ VarianceIO[1,4]
# .684


#### AR residuals -------------------------------------------------------------- 
# In intensive longitudinal data, observations are very close together in time.
# Because of that, confounders that impact a measurement occasions are probably
# going to influence some successive measurement occasions as well. To model 
# this, we can add AR relations between the residuals. In normal longitudinal 
# data, confounders are less likely to effect multiple measurements, but it is
# always good to check what the autocorrelation is.
# For speed purposes, let's only focus on time and highgpa for the following    
# analyses

# First, let's run a normal growth model
GrowthModel_Bayes <- brm(gpa ~ 1 + time + highgpa +
                           (1 + time | student), 
                         iter = 2000,
                         control = list(adapt_delta = 0.8), gpa)

summary(GrowthModel_Bayes)


# And now one with AR residuals. What do you notice?                            

GrowthModel_Bayes_ARRes <- brm(gpa ~ 1 + time + highgpa + ar(p = 1) +           
                                 (1 + time | student), 
                               iter = 2000,
                               control = list(adapt_delta = 0.8), gpa)
# note that ar(p = 1) means that we include lag-1 autocorrelations on the 
# residuals.

summary(GrowthModel_Bayes_ARRes)                                                

# --> There is no mention of the AR at all! Because it's considered a nuisance.     
#     It is there though! All other results are pretty similar. This is because 
#     there isn't a lot of autocorrelation. We can see that as follows:

mean(GrowthModel_Bayes_ARRes$fit@sim$samples[[4]]$`ar[1]`) # .07
sd(GrowthModel_Bayes_ARRes$fit@sim$samples[[4]]$`ar[1]`) # .08

# So, we see there is little autocorrelation in this data (which makes sense 
# given the sampling frequency in the data). For Intensive longitudinal data
# the AR will tend to be higher.

# If we want to explicitly model the AR effect, we need to lag the dependent    
# variable and add it as a predictor. However, then you should remove time
# from the model. Otherwise, you end up with a very complicated model
# (the ALT-model), which is hard to interpret and therefore seldom used
# in practice. Instead, if there is a visible time-effect, you should first
# de-trend the data (which is just another way to control for time than 
# including it as a predictor). 
# There are different ways of de-trending the data. One could
# run a multilevel regression model with time as the only predictor and
# save the residuals. These residuals are the de-trented data and can then be
# analyzed using the AR model. Another method calculates successive differences 
# between observed scores after which these differences can be used as 
# de-trented data for the AR model. Finally, more advanced models would include
# the time-trend and AR part simultaneously in a way that the de-trenting and
# AR modeling of the de-trented data is being done simultaneously. 

#### Small N-------------------------------------------------------------------- 

# To demonstrate a few issues that you should be aware of using small N (i.e.,
# small number of individuals in longitudinal data), let's take only three
# individuals from the data that we have worked with above.

gpa_smallN <- subset(gpa, student <4 )

# Let's run a model with                                                        
SN <- brm(gpa ~ 1 + time +  
            (1 | student), gpa_smallN)                                    

summary(SN)

# Why is the above analysis not recommended?
# --> The level 2 variance is calculated on only 3 observations.

# Now, let's analyze the data using a fixed-effects model
FE_SN <- lm(gpa ~ 0 + factor(student) + time, 
            data = gpa_smallN)

# What does the "0" do in the code above?  
# --> We tell the model to estimate no intercept. This is the only way to obtain    
#     estimates for all person-specific means. This approach is basically
#     like using dummy coding, but in dummy coding you always have one reference
#     group, and so an intercept. In a nutshell, you have only a specific amount
#     of parameters that you can estimate and either you estimate an intercept,
#     or you estimate means for all persons.

# Note that the normal summary file does not work for the fixed-effects model
# because the explained variance is incorrect. Therefore, you use summary.FE(),
# which is a function that is part of your working environment.
summary.FE(FE_SN)                                                               


# Now, let's add a level 2 predictor for both the multilevel analysis and the   
# fixed effect model.

# Multilevel model                                                    
SN2 <- brm(gpa ~ 1 + time +  highgpa +
             (1 | student), gpa_smallN)                                    

summary(SN2)

# Fixed effects model
FE_SN2 <- lm(gpa ~ 0 + factor(student) + time + highgpa, 
             data = gpa_smallN)

summary.FE(FE_SN2) 


# What do you notice in the fixed effects model?
# --> There is no estimate for highgpa.

# Can you explain why?
# --> The dummies capture all level 2 differences, including the ones caused by
#     differences in highgpa, so the predictor is perfectly colinear with the
#     student dummies. In other words, all differences between students due to
#     differences in highgpa are already accounted for by the differences
#     between the estimated means of each student. Adding the highgpa
#     to the model therefore is like adding the same predictor twice.


#Now, let's add a random slope and cross-level interaction.                                        

# Multilevel model                                                    
SN3 <- brm(gpa ~ 1 + time +  highgpa + time:highgpa +
             (1 + time| student), gpa_smallN)                                    

summary(SN3)

# Fixed effects model
FE_SN3 <- lm(gpa ~ 0 + factor(student) + time + highgpa +
               time:highgpa, 
             data = gpa_smallN)

summary.FE(FE_SN3) 

# Notice that we add an interaction term between time and highgpa although 
# we cannot estimate the main effect of highgpa. This is weird, 
# but...the main effect of highgpa is in the model! Remember that it is part of 
# the difference between the dummies for each student


# Let's also run both options for the cross-level interaction on the larger
# dataset with all students.

# Multilevel model                                                    
SN3_all <- brm(gpa ~ 1 + time +  highgpa + time:highgpa +
                 (1 + time| student), gpa)                                    

summary(SN3_all)

# Fixed effects model
FE_SN3_all <- lm(gpa ~ 0 + factor(student) + time + highgpa +
                   time:highgpa, 
                 data = gpa)

summary.FE(FE_SN3_all) 

# Compare the individual intercepts of the multilevel model to the dummy scores
# from the fixed-effects model. What do you notice?
InterceptsML <- coef(SN3_all)$student[,,"Intercept"][,1]                         
InterceptsFE <- as.numeric(FE_SN3_all$coefficients[1:200])

cor(InterceptsML, InterceptsFE)
InterceptsFE - InterceptsML

# --> The values from the fixed-effects model are larger because it doesn't 
#     partially pool, but the rank ordering is rather similar. That means
#     that the students who have higher intercept scores in the multilevel
#     model also tend to have higher scores in the fixed effects model.

### Intensive Longitudinal Data-------------------------------------------------

#------------------------------------------------------------------------------#
# Self-generated data on 2 variables for 100 individuals each measured 50 times.
# The means of the 2 variables are 4 and 4.5 respectively, with SEs of .5.
# The correlation between the two variables is .3. The AR parameters are .4 and
# .3 respectively, while the two lagged effects are .2 and .1. All lagged
# parameters have SEs of .1. The residuals are standard normally distributed.
#------------------------------------------------------------------------------#

#### Multilevel Regression and AR residuals------------------------------------- 
# Let's start with a multilevel model with time as a predictor again.                                   

# This model takes a long time to run, so the results are already in the
# R workspace. The code is the following:

# GrowthModel_Bayes_ARRes2 <- brm(Y1 ~ 1 + time + ar(p = 1) +
#                                   (1 + time | individual), 
#                                 iter = 2000,
#                                 control = list(adapt_delta = 0.8), VARData)


# Is there systematic change over time? What is the amount of autocorrelation?

summary(GrowthModel_Bayes_ARRes2)
mean(GrowthModel_Bayes_ARRes2$fit@sim$samples[[4]]$`ar[1]`) # .51
sd(GrowthModel_Bayes_ARRes2$fit@sim$samples[[4]]$`ar[1]`) # .03

# --> Clearly there is no systematic change over time, but there is a           
#     substantial amount of autocorrelation. Since there is no trend, we can use 
#     AR models. If there were a trend, you should control for it by using time 
#     as a predictor in the model. Since this should not be combined with 
#     modeling AR processes via lagged observed variables, you should use the 
#     AR structure on residuals instead of AR modeling using lagged observed 
#     variables whenever you include time as a predictor.


#### AR model------------------------------------------------------------------- 

# Let's run an AR model.

AR1 <- brm(Y1 ~ Y1lag + (1 + Y1lag | individual), 
           iter = 5000, data = VARData)
summary(AR1)

# Is the intercept close to the true value of 4?

# --> No. This is because the intercept in AR models can't be interpreted as 
#     a mean. If we want means, we need to group-mean center.

# Let's rerun the model with person-mean centered predictor now
VARData <- VARData %>%
  group_by(individual) %>%
  mutate(Y1lag_c = Y1lag - mean(Y1lag, na.rm=T),
         Y2lag_c = Y2lag - mean(Y2lag, na.rm=T))

AR1_c <- brm(Y1 ~ Y1lag_c + (1 + Y1lag_c | individual ), 
             iter = 5000, data = VARData)
summary(AR1_c)

# Much better. Although using sample means to person-mean center isn't ideal  
# We can use estimates of the individual means, but then we have to write       
# our own code to do latent-mean centering (e.g., see the following STAN code
# https://experienced-sampler.netlify.app/post/stan-hierarchical-ar/#model) or
# use Mplus.

#### VAR model------------------------------------------------------------------ 
# Now, we typically want to model several longitudinal variables at the same 
# time. This can be done with VAR models, which are multivariate
# AR models.

# First, we create a model to model Y1 and Y2 simultaneously! We use the mvbind
# command for that. We also let the intercepts vary across individuals. We use
# the "p" notation because we want the random intercepts for both variables to
# also be correlated with each other. It's likely that if you score higher on 
# one, you score higher on the other for example.

bform1 <- 
  bf(mvbind(Y1, Y2) ~ Y1lag_c + Y2lag_c + (1|p|individual))                     

# The following model takes some time to run, so the result is already included.
#VAR <- brm(bform1, data = VARData, iter = 5000, chains = 2, cores = 2)
summary(VAR)





