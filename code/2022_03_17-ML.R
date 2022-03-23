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

# set up learners via built-in functions and pipelines
glm_learner <- Lrnr_glm$new()
SL.glmnet_learner <- Lrnr_pkg_SuperLearner$new(SL_wrapper = "SL.glmnet")
SL.ranger <-Lrnr_ranger$new()
SL.mean <- Lrnr_mean$new()

# stack learners into a model (including screeners and pipelines)
learner_stack <- Stack$new(SL.glmnet_learner, glm_learner, SL.ranger, SL.mean)
stack_fit <- learner_stack$train(task)
preds <- stack_fit$predict()
head(preds)

#TO DOs
#1 - Cross validation settings

cv_stack <- Lrnr_cv$new(learner_stack)
cv_fit <- cv_stack$train(task)
cv_preds <- cv_fit$predict()
risks <- cv_fit$cv_risk(loss_squared_error)
print(risks)
head(cv_preds)
dim(cv_preds)

#2 - Permutation based variable importance
#3 - Create training and validation data set
#4 - Select loss function and performance metrics



