packages <- c("data.table","tidyverse","skimr","here","haven","VIM")

for (package in packages) {
  if (!require(package, character.only=T, quietly=T)) {
    install.packages(package, repos='http://lib.stat.cmu.edu/R/CRAN')
  }
}

for (package in packages) {
  library(package, character.only=T)
}

thm <- theme_classic() +
  theme(
    legend.position = "top",
    legend.background = element_rect(fill = "transparent", colour = NA),
    legend.key = element_rect(fill = "transparent", colour = NA)
  )
theme_set(thm)

here()

# read in the raw stata file and select key variables
a <- read_dta(here("data","Cleaned_nuMoM2B dataset_draft_8.31.2021.dta")) %>%
  select(numomid, ptb37, ptb37sp, ptb37ind, pree_acog, starts_with("hei")) 

a <- a %>% select(-contains("height"), -contains("2005"), -contains("2010"))

# how much missing data does each selected variable have?
mean_func <- function(x){mean(is.na(x))}

apply(a,2,mean_func)

aggr(a)

a <- a %>% na.omit()

write_csv(a, here("data","2022_03_17-prediction_data.csv"))

