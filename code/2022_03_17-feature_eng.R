packages <- c("skimr","here",
              "haven","GGally","cluster","factoextra",
              "NbClust","LICORS","VGAM")

for (package in packages) {
  if (!require(package, character.only=T, quietly=T)) {
    install.packages(package, repos='http://lib.stat.cmu.edu/R/CRAN')
  }
}

for (package in packages) {
  library(package, character.only=T)
}

remotes::install_github("tidyverse/tidyverse")
library(tidyverse)
library(broom)

thm <- theme_classic() +
  theme(
    legend.position = "top",
    legend.background = element_rect(fill = "transparent", colour = NA),
    legend.key = element_rect(fill = "transparent", colour = NA)
  )
theme_set(thm)

tidy.vglm <- function(x, conf.int=FALSE, conf.level=0.95) {
  co <- as.data.frame(coef(summary(x)))
  names(co) <- c("estimate","std.error","statistic","p.value")
  if (conf.int) {
    qq <- qnorm((1+conf.level)/2)
    co <- transform(co,
                    conf.low=estimate-qq*std.error,
                    conf.high=estimate+qq*std.error)
  }
  co <- data.frame(term=rownames(co),co)
  rownames(co) <- NULL
  return(co)
}

here()

## WARNING! THIS NEEDS TO BE CHANGED
if(Sys.info()["nodename"]=="EPIC02G44CTQ05P"|Sys.info()["nodename"]=="EPI9TPH7M3"){
  a <- read_csv(here("data","numom750.csv"))
} else{
  mydata <- read.csv("I:\\Bodnar Julie\\numom750.csv")  
}

skim(a)

a %>% 
  select(starts_with("hei")) %>% 
  sample_n(size=100) %>% 
  GGally::ggpairs(.)

ggsave(here("figures","2022_03_18-HEI_gally.pdf"))

a_cluster <- a %>% 
  select(starts_with("hei")) %>% 
  scale(.)

a_cluster[,] %>% 
  as_tibble(.) %>% 
  sample_n(size=100) %>% 
  GGally::ggpairs(.)

ggsave(here("figures","2022_03_18-HEI_gally_scaled.pdf"))

write_csv(data.frame(a_cluster[,]), 
          here("data","2022_03_18-scaled_hei.csv"))

# cluster analysis

clust <- NbClust(data = a_cluster[,], 
                 diss=NULL, 
                 distance = "euclidean", 
                 min.nc = 2, 
                 max.nc = 15, 
                 method = "kmeans", 
                 index = "all", 
                 alphaBeale = 0.1)



kmeans_hei <- kmeans(a_cluster[,],
                     centers = 3,
                     iter.max = 20L,
                     nstart = 10L)


kmeanspp_hei <- kmeanspp(a_cluster[,],
                         k = 3,
                         start = "random",
                         iter.max = 20L,
                         nstart = 10L)

kmeans_hei$centers

kmeanspp_hei$centers

plot(kmeans_hei$centers,kmeanspp_hei$centers)

table(kmeans_hei$cluster,kmeanspp_hei$cluster)

a_clust_data <- cbind(a_cluster[,],
                      tibble(cluster = kmeans_hei$cluster,
                             cluster_pp = kmeanspp_hei$cluster)
)

table(a_clust_data$cluster)
table(a_clust_data$cluster_pp)

mlr <- vglm(cluster ~ ., 
            data = subset(a_clust_data,select=-cluster_pp),
            family = multinomial(refLevel = 1))

mlr_dat <- tidy.vglm(mlr)

mlr_pp <- vglm(cluster_pp ~ ., 
               data = subset(a_clust_data,select=-cluster),
               family = multinomial(refLevel = 1))

mlrpp_dat <- tidy.vglm(mlr_pp) %>% 
  setNames(c(names(.)[1], paste0(names(.)[-1],"_pp"))) 


mlr_dat <- left_join(mlr_dat, mlrpp_dat, by="term")

mlr_dat %>% ggplot() +
  geom_point(aes(estimate,estimate_pp))


# feature engineering


