## Ashley's code for install ##
packages <- c("data.table","tidyverse","skimr","here","remotes",
              "SuperLearner","glmnet","ranger","origami","caret")

for (package in packages) {
  if (!require(package, character.only=T, quietly=T)) {
    install.packages(package, repos='http://lib.stat.cmu.edu/R/CRAN')
  }
}

for (package in packages) {
  library(package, character.only=T)
}

remotes::install_github("tlverse/sl3")
library(sl3)

thm <- theme_classic() +
  theme(
    legend.position = "top",
    legend.background = element_rect(fill = "transparent", colour = NA),
    legend.key = element_rect(fill = "transparent", colour = NA)
  )
theme_set(thm)

## Julie's code for install ##
install.packages ("remotes")
install.packages ("SuperLearner")
install.packages(c("caret", "glmnet", "ggplot2", "RhpcBLASctl", "ranger"))
install.packages("randomForest")
install.packages("tidyverse")
install.packages("origami")
remotes::install_github("tlverse/sl3")
install.packages ("data.table")
install.packages ("dplyr")
install.packages ("readr")
install.packages ("knitr")
install.packages ("kableExtra")
install.packages ("ROCR")
install.packages("varImp")
install.packages("purrr")  
install.packages("caret")
install.packages("Rsolnp")

set.seed(49753)
library(tidyverse)
library(data.table)
library(SuperLearner)
library(origami)
library(sl3)
library(data.table)
library(dplyr)
library(readr)
library(knitr)
library(kableExtra)
library(ROCR) # for AUC calculation
library(varImp)
library(purrr)
library(caret)
library(Rsolnp)

## Reading in data ##
## This will need changing: we should use the "here()" function
if(Sys.info()["nodename"]=="EPIC02G44CTQ05P"|Sys.info()["nodename"]=="EPI9TPH7M3"){
  a <- read_csv(here("data","numom750.csv"))
  a_scaled <- read_csv(here("data","2022_03_18-scaled_hei.csv"))
} else{
  a <- read.csv("C:\\Diet R01\\HEI Super Learner\\numom750.csv") 
  a_scaled <- read_csv(here("C:\\Diet R01\\HEI Super Learner\\2022_04_05-scaled_hei.csv"))
}

names(a)
names(a_scaled)

sl3_list_learners(c("binomial"))

covars <- names(a_scaled)
mydata <- cbind(tibble(ptb37 = a$ptb37),a_scaled)

names(mydata)


# create training and testing data
## This step is not needed because SL will automatically create folds for 
## training and validation
set.seed(123)
#y = factor(mydata$ptb37)

#index <- createDataPartition(
#  y,
#  times = 1,
#  p = .7,
#  list = TRUE,
#  groups = min(5, length(y))
#)

#train <- mydata[index$Resample1,]
#test <- mydata[-index$Resample1,]

#nrow(train)
#nrow(test)

#Defining ML task - sl3_task object keeps track of the roles the variables 
#play in the machine learning problem, the data, and any metadata
#uses cross-validation (CV) to build the ensemble model
task <- make_sl3_Task(
  data = mydata,
  covariates = covars,
  outcome = "ptb37",
  folds = 10L
)

task

length(task$folds) # how many folds? 10 by default

head(task$folds[[1]]$training_set)
head(task$folds[[1]]$validation_set)

# set up learners via built-in functions and pipelines
glm_learner <- Lrnr_glm$new()
SL.glmnet_learner <- Lrnr_pkg_SuperLearner$new(SL_wrapper = "SL.glmnet")
SL.ranger <-Lrnr_ranger$new()
SL.mean <- Lrnr_mean$new()

# named vector of learners first
learners <- c(
  glm_learner, SL.glmnet_learner, SL.ranger, SL.mean
)
names(learners) <- c(
  "glm", "glm net", "ranger", "mean"
)

learner_stack <- make_learner(Stack, learners)
learner_stack

#Lrnr_sl object specifies the Super Learner
sl <- Lrnr_sl$new(learner_stack)
sl

#The SL algorithm fits a metalearner on the validation-set predictions 
#in a cross-validated manner, thereby avoiding overfitting.
sl_fit <- sl$train(task)
sl_fit

risks <- sl_fit$cv_risk(loss_loglik_binomial)
print(risks)

#SL predictions
sl_preds <- sl_fit$predict()
head(sl_preds)
summary(sl_preds)

qplot(mydata$ptb37, sl_preds, main = 
        # "lrn_mean"
        # "lrn_glm"
        # "lrn_ranger100"
        "SuperLearner"
) + theme_minimal()

AUCsl <- performance(prediction(sl_preds, mydata$ptb37), measure = "auc")@y.values[[1]]
plot(performance(prediction(sl_preds, mydata$ptb37), "tpr", "fpr"), main = 
       # "lrn_mean"
       # "lrn_glm"
       # "lrn_ranger100"
       "SuperLearner" 
)
AUCsl

# Cross validation using SL3 - if not specified 10 folds is default
#We can cross-validate the SL to see how well the SL performs on unseen data, 
#and obtain an estimate of the cross-validated risk of the SL
task_CVsl <- make_sl3_Task(
  data = mydata,
  outcome = "ptb37",
  covariates = covars,
  drop_missing_outcome = TRUE,
  folds = origami::make_folds(
    n = sum(!is.na(mydata$ptb37)),
    fold_fun = folds_vfold,
    V = 10
  )
)

#Not sure if below code is needed
#cv_stack <- Lrnr_cv$new(learner_stack)
#cv_stack

CVsl <- CV_lrnr_sl(sl_fit, task_CVsl, loss_loglik_binomial)
CVsl

# Permutation based variable importance plot

# sl3 variable importance plot
## why do we use sl_fit here, and not an output of the second CV run?
varimp <- importance(sl_fit, loss_loglik_binomial, type = "permute")

#importance automatically set based on outcome type
#for constant and binomial outcomes set to loss_squared_error
#see https://github.com/tlverse/sl3/blob/master/R/importance.R
    
dotchart(labels = varimp$covariate, varimp$NLL_difference, xlab="Variable Importance", col='darkblue', pch=16, cex=1.1)
abline(v=abs(min(varimp$NLL_difference)), col='red', lty='longdash', lwd=2)

