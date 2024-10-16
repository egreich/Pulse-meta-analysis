# Figure 4. Histogram? of t.peak and y.peaks for each variable

library(tidyverse)
library(ggforce) # for facet grids
library(cowplot) # for ggsave2
library(ggh4x)

path_out = "./figures/" # set save path

# Load output data
load("data_output/df_all.Rdata") # df_all

df_all2 <- df_all %>%
  mutate(varGroup = as.factor(varGroup), 
         Sample.unit = factor(Sample.unit, levels = c("leaf", "individual", "plot/collar", "footprint")), 
         Pulse.type = as.factor(Pulse.type), 
         response_cat = as.factor(response_cat),
         varGroup2 = case_when(varGroup == "carbon" ~ "Carbon-related",
                               varGroup == "water" ~ "Water-related"),
         varType = factor(varType, levels = c("NPP", "GPP", "Anet",
                                              "ecosystemR", "belowgroundR",
                                              "ET", "T", "Gs", "PWP"))) %>% 
  pivot_longer(cols = c(mean_y.peak, mean_t.peak, mean_mm),
               names_to = "param") %>%
  mutate(param = case_when(param == "mean_t.peak" ~ "t[peak]~(days)", 
                           param == "mean_y.peak" ~ "y[peak]",
                           param == "mean_mm" ~ "slope"),
         response_cat_name = case_when(response_cat == 1 ~ "classic", 
                                       response_cat == 2 ~ "intermediate",
                                       response_cat == 3 ~ "linear",
                                       response_cat == 4 ~ "no pulse"))


df_sum <- df_all2 %>%
  group_by(varGroup2, varType, param) %>% 
  summarize(param_m = mean(value, na.rm = TRUE),
            param_sd = sd(value, na.rm = TRUE))

labs <- c("NPP", "GPP", "A[net]", "R[eco]", "R[below]",
          "ET", "T", "g[s]", "Psi[plant]")

p <- df_all2 %>%
  ggplot() +
  geom_jitter(data = df_all2, 
              aes(x = varType, y = value,
                  color = response_cat_name),
              width = 0.1, alpha = 0.4) +
  geom_errorbar(data = df_sum,
                aes(x = varType, ymin = param_m - param_sd,
                    ymax = param_m + param_sd),
                width = 0.1) +
  geom_point(data = df_sum,
             aes(x = varType,y = param_m)) +
  # facet_wrap2(cols = vars(varType),
  #            rows = vars(param), scales = "free", axes = "all",
  #            labeller = label_parsed,
  #            switch = "y") +
  facet_grid2(vars(param),vars(varType), scales = "free",labeller = label_parsed, independent = "y", switch="y") +
  scale_x_discrete(labels = parse(text = labs), breaks = levels(df_all2$varType)) +
  scale_color_brewer(palette = "Dark2") +
  theme_bw(base_size = 14) +
  theme(panel.grid = element_blank(),
        strip.background = element_blank(),
        strip.placement = "outside",
        strip.text.x = element_blank(),
        axis.title = element_blank(),
        legend.title = element_blank(),
        legend.position = "top",
        legend.background = element_rect(fill = NA))


ggsave2("figS2.png", plot = p, path = path_out, width = 8, height = 4)

