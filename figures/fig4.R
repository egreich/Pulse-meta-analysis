# Figure 4. Histogram? of t.peak and y.peaks for each variable

library(tidyverse)
library(ggforce) # for facet grids
library(cowplot) # for ggsave2

path_out = "./figures/" # set save path

# Load output data
load("data_output/df_all.Rdata") # df_all

df_all <- df_all %>%
  mutate(varGroup = as.factor(varGroup), Sample.unit = as.factor(Sample.unit), 
         Pulse.type = as.factor(Pulse.type), response_cat = as.factor(response_cat))
df_all$Sample.unit <- factor(df_all$Sample.unit, levels = c("leaf", "individual", "plot/collar", "footprint"))

df_all2 <- df_all %>% 
  pivot_longer(cols = c(mean_y.peak, mean_t.peak))

fig4 <- ggplot(df_all2, aes(y=value, x=varType, fill = name)) +
  ## add half-violin from {ggdist} package
  ggdist::stat_halfeye(
    ## custom bandwidth
    adjust = .5, 
    ## adjust height
    width = .6, 
    ## move geom to the right
    justification = -.2, 
    ## remove slab interval
    .width = 0, 
    point_colour = NA
  ) + 
  geom_boxplot(
    width = .25,
    ## remove outliers
    outlier.color = NA ## `outlier.shape = NA` or `outlier.alpha = 0` works as well
  ) +
  ylim(c(-10,10)) +
  facet_col("varGroup", scales = "free_x") +
  theme_bw()
fig4


ggsave2("fig4.png", plot = fig4, path = path_out, width = 10, height = 5)

