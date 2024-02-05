### Density plots of pulse amounts, 
# by experimental/natural, water/carbon, sample units

library(tidyverse)
library(ggforce) # for facet grids
library(cowplot) # for ggsave2

path_out = "./figures/" # set save path

# Load output data
load("data_output/df_all.Rdata") # df_all

df_all2 <- df_all %>%
  mutate(varGroup = as.factor(varGroup), 
         Sample.unit = factor(Sample.unit, levels = c("leaf", "individual", "plot/collar", "footprint")), 
         Pulse.type = as.factor(Pulse.type), 
         response_cat = as.factor(response_cat),
         varGroup2 = case_when(varGroup == "carbon" ~ "C-related",
                               varGroup == "water" ~ "H[2]*O-related"),
         varType = factor(varType, levels = c("NPP", "GPP", "Anet",
                                              "ecosystemR", "belowgroundR",
                                              "ET", "T", "Gs", "PWP")))
# varType2 = case_when(varType == "Anet" ~ "A[net]",
#                      varType == "belowgroundR" ~ "R[below]",
#                      varType == "ecosystemR" ~ "R[eco]",
#                      varType == "Gs" ~ "g[s]",
#                      varType == "PWP" ~ "Psi[plant]",
#                      .default = as.character(varType)))
#df_all$varType <- factor(df_all$Sample.unit, levels = c("leaf", "individual", "plot/collar", "footprint"))
tot_df <- df_all2 |> 
  group_by(varType, varGroup2) |> 
  count()


# labs1 <- lapply(c("C-related", "H[2]O-related"), function(i) bquote(.(i)))

# By pulse type
figS2a <- df_all2 |> 
  ggplot() +
  geom_density(aes(x = Pulse.amount.mm,
                   fill = Pulse.type),
                 position = "identity",
                 alpha = 0.5) +
  scale_fill_discrete(labels = c("experimental", "natural")) +
  facet_row("varGroup2", scales = "free_x", space = "free",
            labeller = label_parsed) +
  theme_bw(base_size = 14) +
  theme(panel.grid = element_blank(),
        strip.background = element_blank(),
        axis.title.x = element_blank(),
        legend.title = element_blank(),
        legend.position = c(0.75, 0.75),
        legend.background = element_rect(fill = NA),
        legend.justification = 1)

# By sample unit
figS2b <- df_all2 |> 
  ggplot() +
  geom_density(aes(x = Pulse.amount.mm,
                     fill = Sample.unit),
                 position = "identity",
                 alpha = 0.75) +
  scale_fill_brewer(palette = "Paired",
                    direction = -1) +
  facet_row("varGroup2", scales = "free_x", space = "free",
            labeller = label_parsed) +
  theme_bw(base_size = 14) +
  theme(panel.grid = element_blank(),
        strip.background = element_blank(),
        axis.title.x = element_blank(),
        legend.title = element_blank(),
        legend.position = c(0.75, 0.75),
        legend.background = element_rect(fill = NA),
        legend.justification = 1)

# By Response category
figS2c <- df_all2 |> 
  ggplot() +
  geom_density(aes(x = Pulse.amount.mm,
                   fill = response_cat),
               position = "identity",
               alpha = 0.75) +
  scale_fill_brewer(palette = "PRGn",
                    labels = c("classic", "intermediate", "linear", "no pulse")) +
  
  facet_row("varGroup2", scales = "free_x", space = "free",
            labeller = label_parsed) +
  theme_bw(base_size = 14) +
  theme(panel.grid = element_blank(),
        strip.background = element_blank(),
        axis.title.x = element_blank(),
        legend.title = element_blank(),
        legend.position = c(0.75, 0.75),
        legend.background = element_rect(fill = NA),
        legend.justification = 1)

figS2 <- plot_grid(figS2a, figS2b, figS2c,
                   nrow = 3,
                   align = "v",
                   labels = "auto")

ggsave2("figS2.png", plot = figS2, 
        path = path_out, width = 8, height = 8)
