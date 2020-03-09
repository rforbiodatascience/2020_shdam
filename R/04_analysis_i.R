# Clear workspace
# ------------------------------------------------------------------------------
rm(list = ls())

# Load libraries
# ------------------------------------------------------------------------------
library("tidyverse")
# library("patchwork")

# Define functions
# ------------------------------------------------------------------------------
source(file = "R/99_project_functions.R")

# Load data
# ------------------------------------------------------------------------------
cancer_data <- read_tsv(file = "data/03_my_data_clean_aug.tsv")

# Wrangle data
# ------------------------------------------------------------------------------
my_pca <- cancer_data %>%
  select(starts_with("g")) %>%
  # select_if(is.numeric, ) %>%
  prcomp(center = TRUE, scale. = TRUE)

my_pca_aug <- my_pca %>% 
  broom::augment(cancer_data)

# Model data
# ------------------------------------------------------------------------------
# Cluster on original data
my_k_org <- my_pca_aug %>%
  select(starts_with("g")) %>% 
  kmeans(centers = 2)

my_pca_aug_k_org <- my_k_org %>%
  broom::augment(my_pca_aug) %>% 
  rename(cluster_org = .cluster)

my_k_pca <- my_pca_aug_k_org %>%
  select(.fittedPC1, .fittedPC2) %>%
  kmeans(centers = 2)

# Augment cancer with PCA results
my_pca_aug_k_org_pca <- my_k_pca %>%
  broom::augment(my_pca_aug_k_org) %>% 
  rename(cluster_pca = .cluster)

# Determine agreement with event_label
my_pca_aug_k_org_pca %>%
  select(event_label, cluster_org, cluster_pca) %>%
  mutate(cluster_org = case_when(cluster_org == 1 ~ "good",
                                 cluster_org == 2 ~ "poor"),
         cluster_pca = case_when(cluster_pca == 1 ~ "good",
                                 cluster_pca == 2 ~ "poor"),
         cluster_org_correct = case_when(event_label == cluster_org ~ 1,
                                         event_label != cluster_org ~ 0),
         cluster_pca_correct = case_when(event_label == cluster_pca ~ 1,
                                         event_label != cluster_pca ~ 0)) %>% 
  summarise(score_org = mean(cluster_org_correct),
            score_pca = mean(cluster_pca_correct))



# Visualise data
# ------------------------------------------------------------------------------
pca_p1 <- my_pca %>% broom::tidy("pcs") %>% 
  ggplot(aes(x = PC, y = percent)) +
  geom_col() +
  theme_bw()

pca_p2 <- my_pca_aug %>% 
  ggplot(aes(x = .fittedPC1, y = .fittedPC2, colour = event_label)) +
  geom_point()


pl1 <- my_pca_aug_k_org_pca %>%
  ggplot(aes(x = .fittedPC1, y = .fittedPC2, colour = event_label)) +
  geom_point() +
  theme(legend.position = "bottom")

pl2 <- my_pca_aug_k_org_pca %>%
  ggplot(aes(x = .fittedPC1, y = .fittedPC2, colour = cluster_org)) +
  geom_point() +
  theme(legend.position = "bottom")

pl3 <- my_pca_aug_k_org_pca %>%
  ggplot(aes(x = .fittedPC1, y = .fittedPC2, colour = cluster_pca)) +
  geom_point() +
  theme(legend.position = "bottom")

# (pl1 + pl2 + pl3)

# Write data
# ------------------------------------------------------------------------------
# write_tsv(...)
ggsave(filename = "results/04_pca_pl1.png", plot = pca_p1)
ggsave(filename = "results/04_pca_pl2.png", plot = pca_p2)
ggsave(filename = "results/04_pl1.png", plot = pl1)
ggsave(filename = "results/04_pl2.png", plot = pl2)
ggsave(filename = "results/04_pl3.png", plot = pl3)