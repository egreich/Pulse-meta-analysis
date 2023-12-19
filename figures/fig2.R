### Stacked histogram

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
#df_all$varType <- factor(df_all$Sample.unit, levels = c("leaf", "individual", "plot/collar", "footprint"))


fig2 <- ggplot(df_all, aes(x=varType, fill=response_cat)) +
  geom_bar(position="fill") +
  scale_fill_brewer(palette = "PRGn") +
  facet_row("varGroup", scales="free_x") +
  theme_bw()
fig2

ggsave2("fig2.png", plot = fig2, path = path_out, width = 10, height = 5)



