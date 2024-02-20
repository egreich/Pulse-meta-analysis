### Stacked histogram

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
tot_df <- df_all2 %>%
  group_by(varType, varGroup2) %>% 
  count()

labs <- c("NPP", "GPP", "A[net]", "R[eco]", "R[below]",
          "ET", "T", "g[s]", "Psi[plant]")
# labs1 <- lapply(c("C-related", "H[2]O-related"), function(i) bquote(.(i)))

fig2 <- ggplot(df_all2, aes(x = varType)) +
  geom_bar(position = "stack", 
           aes(fill = response_cat)) +
  geom_text(data = tot_df,
            aes(x = varType,  y = n, label = n),
            vjust = -0.2,
            size = 4) +
  scale_y_continuous("Number of pulses") +
  scale_x_discrete(labels = parse(text = labs), breaks = levels(df_all2$varType)) +
  scale_fill_brewer(palette = "PRGn",
                    labels = c("classic", "intermediate", "linear", "no pulse")) +
  facet_row("varGroup2", scales = "free_x", space = "free",
            labeller = label_parsed) +
  theme_bw(base_size = 14) +
  theme(panel.grid = element_blank(),
        strip.background = element_blank(),
        axis.title.x = element_blank(),
        legend.title = element_blank(),
        legend.position = c(0.1, 0.8),
        legend.background = element_rect(fill = NA))

ggsave2("fig2.png", plot = fig2, path = path_out, width = 8, height = 4)



