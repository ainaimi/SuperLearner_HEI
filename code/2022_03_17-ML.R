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

## WARNING! THIS NEEDS TO BE CHANGED
if(Sys.info()["nodename"]=="EPIC02G44CTQ05P"|Sys.info()["nodename"]=="EPI9TPH7M3"){
  a <- read_csv(here("data","numom750.csv"))
  a_scaled <- read_csv(here("data","2022_03_18-scaled_hei.csv"))
} else{
  mydata <- read.csv("I:\\Bodnar Julie\\numom750.csv")  
}

names(a)
names(a_scaled)

sl3_list_learners(c("binomial"))

covars <- names(a_scaled)
mydata <- cbind(tibble(ptb37 = a$ptb37),a_scaled)

head(mydata)

# create training and testing data
set.seed(123)
y = factor(mydata$ptb37)

index <- createDataPartition(
  y,
  times = 1,
  p = .7,
  list = TRUE,
  groups = min(5, length(y))
)

train <- mydata[index$Resample1,]
test <- mydata[-index$Resample1,]

nrow(test)
nrow(train)

task <- sl3_Task$new(
  data = train,
  covariates = covars,
  outcome = "ptb37"
  )

task_CV <- sl3_Task$new(
  data = train, ## should this be fit to entire dataset?
  covariates = covars,
  outcome = "ptb37",
  folds = origami::make_folds(train, fold_fun = folds_vfold, V = 3)
  )

# set up learners via built-in functions and pipelines
glm_learner <- Lrnr_glm$new()
SL.glmnet_learner <- Lrnr_pkg_SuperLearner$new(SL_wrapper = "SL.glmnet")
SL.ranger <-Lrnr_ranger$new()
SL.mean <- Lrnr_mean$new()

learner_stack <- Stack$new(SL.glmnet_learner, glm_learner, SL.ranger, SL.mean)

sl <- Lrnr_sl$new(learner_stack) ## check to see if this allows us to use CV SL?
sl_fit <- sl$train(task)

## this is not working, something wrong with the loss function...
CVsl <- CV_lrnr_sl(
  lrnr_sl = sl_fit,
  task = task_CV,
  loss_fun = loss_loglik_binomial
)


hei_varimp <- importance(sl_fit, loss = loss_squared_error, type = "permute")


cv_stack <- Lrnr_cv$new(learner_stack)
cv_fit <- cv_stack$train(task)
cv_preds <- cv_fit$predict()
risks <- cv_fit$cv_risk(loss_loglik_binomial)
print(risks)
head(cv_preds)
dim(cv_preds)

#2 - Permutation based variable importance
#3 - Create training and validation data set
#4 - Select loss function and performance metrics


#### playground ####

db_data <- url(
  "https://raw.githubusercontent.com/benkeser/sllecture/master/chspred.csv"
)
chspred <- read_csv(file = db_data, col_names = TRUE)
data.table::setDT(chspred)

# make task
chspred_task <- make_sl3_Task(
  data = chspred,
  covariates = colnames(chspred)[-1],
  outcome = "mi"
)

# make learners
glm_learner <- Lrnr_glm$new()
lasso_learner <- Lrnr_glmnet$new(alpha = 1)
ridge_learner <- Lrnr_glmnet$new(alpha = 0)
enet_learner <- Lrnr_glmnet$new(alpha = 0.5)
# curated_glm_learner uses formula = "mi ~ smoke + beta"
curated_glm_learner <- Lrnr_glm_fast$new(covariates = c("smoke", "beta"))
mean_learner <- Lrnr_mean$new() # That is one mean learner!
glm_fast_learner <- Lrnr_glm_fast$new()
ranger_learner <- Lrnr_ranger$new()
svm_learner <- Lrnr_svm$new()
xgb_learner <- Lrnr_xgboost$new()

# screening
screen_cor <- make_learner(Lrnr_screener_correlation)
glm_pipeline <- make_learner(Pipeline, screen_cor, glm_learner)

# stack learners together
stack <- make_learner(
  Stack,
  glm_pipeline, glm_learner,
  lasso_learner, ridge_learner, enet_learner,
  curated_glm_learner, mean_learner, glm_fast_learner,
  ranger_learner, svm_learner, xgb_learner
)

# make and train SL
sl <- Lrnr_sl$new(
  learners = stack
)
sl_fit <- sl$train(chspred_task)
sl_fit$cv_risk(loss_squared_error)

CVsl <- CV_lrnr_sl(sl_fit, chspred_task, loss_squared_error)
CVsl

varimp <- importance(sl_fit)
importance_plot(varimp) 
