packages <- c("data.table","tidyverse","skimr","here","haven","GGally")

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

a <- read_csv(here("data","2022_03_17-prediction_data.csv"))

skim(a)

a %>% 
  select(starts_with("hei")) %>% 
  sample_n(size=100) %>% 
  GGally::ggpairs(.)

a_cluster <- a %>% 
  select(starts_with("hei"))





