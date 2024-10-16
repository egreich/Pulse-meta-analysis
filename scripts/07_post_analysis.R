### Post-analysis analysis
# glm code
library(tidyverse)
library(tibble)
library(cowplot)

# Load output data
load("data_output/df_all.Rdata") # df_all

## Question 1: Is there a pulse?
## Response variable of interest: response_cat (1-4)
## Predictor variables of interest: varGroup, Sample.unit, MAT.C.wc, MAP.mm.wc, Pulse.type, Pulse.amount.mm, preVAR

# Make data in the correct format
# scale(as.numeric(dataIN$VPD),center=TRUE,scale=TRUE)
# df_all <- df_all %>%
#   mutate(varGroup = as.factor(varGroup), Sample.unit = as.factor(Sample.unit),
#          Pulse.type = as.factor(Pulse.type),
#          response_cat = as.numeric(response_cat),
#          MAT.C.wc = scale(as.numeric(MAT.C.wc),center=TRUE,scale=TRUE),
#          MAP.mm.wc = scale(as.numeric(MAP.mm.wc),center=TRUE,scale=TRUE),
#          Pulse.amount.mm = scale(as.numeric(Pulse.amount.mm),center=TRUE,scale=TRUE),
#          preVar = scale(as.numeric(preVar),center=TRUE,scale=TRUE),
#          obs = scale(as.numeric(obs),center=TRUE,scale=TRUE))

df_all <- df_all %>%
  mutate(MAP.mm = ifelse(is.na(MAP.mm), MAP.mm.wc, MAP.mm)) %>%
  mutate(MAT.C = ifelse(is.na(MAT.C), MAT.C.wc, MAT.C)) %>%
  mutate(Sample.unit = ifelse(Sample.unit=="plot/collar", "plot/footprint", Sample.unit)) %>%
mutate(Sample.unit = ifelse(Sample.unit=="footprint", "plot/footprint", Sample.unit)) %>%
  mutate(varGroup = case_when(varGroup=="carbon"~"Carbon-related",
                              varGroup=="water"~"Water-related")) %>%
  mutate(varGroup = as.factor(varGroup), Sample.unit = as.factor(Sample.unit),
         Pulse.type = as.factor(Pulse.type),
         response_cat = as.numeric(response_cat))

df_all$Sample.unit <- factor(df_all$Sample.unit, levels = c("leaf", "individual", "plot/footprint"))

summary(lm(MAP.mm~MAT.C, data = df_all)) # MAP and MAT are very not correlated

## Some summary stats
df_all %>%
  count(response_cat)
# 1            1   226
# 2            2    53
# 3            3   148
# 4            4   160
df_all %>%
  count(mean_w==1)
#TRUE            198
df_all %>%
  count(mean_w==0)
#TRUE            308
df_all %>%
  count(mean_w!=0 & mean_w!=1)
#TRUE            81

df_all %>%
  group_by(varGroup) %>%
  count(response_cat)
# Groups:   varGroup [2]
# varGroup response_cat     n
# <fct>           <dbl> <int>
#   1 carbon              1   167
# 2 carbon              2    38
# 3 carbon              3    94
# 4 carbon              4    99
# 5 water               1    59
# 6 water               2    15
# 7 water               3    54
# 8 water               4    61
# Carbon 299/398 75%
# Water 128/189 68%

df_all %>%
  count(is.na(preSWC))
360/587 # 61% of SWC data is missing


df_all %>%
  group_by(varType) %>%
  summarise(scales = unique(Sample.unit))

## Question 1: Is there a pulse? (Q1 but cat responses are collapsed)
## Response variable of interest: response_cat (1-2)
## Predictor variables of interest: varGroup, Sample.unit, MAT.C.wc, MAP.mm.wc, Pulse.type, Pulse.amount.mm, preVAR

# Make data in the correct format
df_all1 <- df_all
df_all1$response_cat <- ifelse(df_all1$response_cat< 4, 1, 2)


q1_glm <- glm(as.factor(response_cat) ~ varGroup+Sample.unit+MAT.C+MAP.mm+Pulse.amount.mm+MAP.mm*MAT.C+MAP.mm*varGroup+Pulse.amount.mm*varGroup, data = df_all1, family = binomial) #Pulse.amount.mm*varGroup
sink("data_output/q1_glm.txt") # sink will save the summary output to a text file
summary(q1_glm)
sink() # end what we save to the text file

## Question 2: If there is a pulse, do t.peak (time to peak response) - NOTE: use 'lm" instead
## and y.peak (magnitude of peak response) depend on the variables of interest?
## Response of interest: y.peak and t.peak mean
## Predictor variables of interest: varGroup, Sample.unit, MAT.C.wc, MAP.mm.wc, Pulse.type, Pulse.amount.mm, preVAR

# Filter for all data that is at least somewhat Ricker-like
df_all2 <- df_all %>%
  filter(mean_w != 0)

q2.t_glm <- glm(mean_t.peak ~ varGroup+Sample.unit+MAT.C+MAP.mm+Pulse.amount.mm+MAP.mm*MAT.C+MAP.mm*varGroup+Pulse.amount.mm*varGroup, data = df_all2) #+Pulse.amount.mm*Sample.unit
sink("data_output/q2.t_glm.txt")
summary(q2.t_glm)
sink()

q2.y_glm <- glm(mean_y.peak ~ varGroup+Sample.unit+MAT.C+MAP.mm+Pulse.amount.mm+MAP.mm*MAT.C+MAP.mm*varGroup+Pulse.amount.mm*varGroup, data = df_all2) #+Pulse.amount.mm*Sample.unit
sink("data_output/q2.y_glm.txt")
summary(q2.y_glm)
sink()

## Question 3: If there is a linear pulse response, how steep is mm (slope, speed of response) depending on the variables of interest?
## Response of interest:  mm mean
## Predictor variables of interest: varGroup, Sample.unit, MAT.C.wc, MAP.mm.wc, Pulse.type, Pulse.amount.mm, preVAR
## intercept should be 0, maybe look into

# Filter for all pulses with at least some linear characteristics
df_all3 <- df_all %>%
  filter(mean_w != 1) %>%
  mutate(mean_mm = abs(mean_mm)) # take absolute value, because we only care about steepness

q3_glm <- glm(mean_mm ~ varGroup+Sample.unit+MAT.C+MAP.mm+Pulse.amount.mm+MAP.mm*MAT.C+MAP.mm*varGroup+Pulse.amount.mm*varGroup, data = df_all3)
sink("data_output/q3_glm.txt")
summary(q3_glm)
sink()


q3_glm_coeff <- summary(q3_glm)$coef[,1]


#### Save tables in convenient format for plotting

# Data for figure of heatmap for p-values

df_q1 <- as.data.frame(summary(q1_glm)$coefficients[,4]) %>%
  rownames_to_column()
df_q1 <- df_q1[-1,]
colnames(df_q1) <- c("pred", "q1")

df_q2.t <- as.data.frame(summary(q2.t_glm)$coefficients[,4]) %>%
  rownames_to_column()
df_q2.t <- df_q2.t[-1,]
colnames(df_q2.t) <- c("pred", "q2.t")

df_q2.y <- as.data.frame(summary(q2.y_glm )$coefficients[,4]) %>%
  rownames_to_column()
df_q2.y <- df_q2.y[-1,]
colnames(df_q2.y) <- c("pred", "q2.y")

df_q3 <- as.data.frame(summary(q3_glm )$coefficients[,4]) %>%
  rownames_to_column()
df_q3 <- df_q3[-1,]
colnames(df_q3) <- c("pred", "q3")


df_q <- full_join(df_q1, df_q2.t)
df_q <- full_join(df_q, df_q2.y)
df_q <- full_join(df_q, df_q3)

colnames(df_q) <- c("pred", "GLM for response or no response", "GLM for time of peak", "GLM for magnitude of peak", "GLM for speed of linear response")

rowp<- df_q$pred
df_q <- df_q %>%
  dplyr::select(-pred)
rownames(df_q) <- rowp
#[1] "varGroupWater-related"        "Sample.unitindividual"        "Sample.unitplot/collar"      
#[4] "Sample.unitfootprint"         "MAT.C"                        "MAP.mm"                      
#[7] "Pulse.typeNatural"            "Pulse.amount.mm"                      
#[10] "MAT.C:MAP.mm"                 "varGroupWater-related:MAP.mm" 
# "varGroupWater-related:Pulse.amount.mm"    
# [11] "Sample.unitindividual:Pulse.amount.mm"     "Sample.unitplot/footprint:Pulse.amount.mm"
rownames(df_q) <- c("(1) Carbon-related (2) water-related",
                    "Sample.unitindividual", 
                    "Sample.unitplot/footprint",
                    "MAT", 
                    "MAP",
                    "Pulse amount",
                    "MAP x MAT",
                    "MAP x carbon- or water-related",
                    "Pulse amount x carbon- or water-related")

write.csv(df_q, file = "data_output/p_mat.csv")


# Data for figure of heatmap for coefficients

df_q1 <- as.data.frame(summary(q1_glm)$coefficients[,c(1,4)])%>%
  rownames_to_column()
df_q1 <- df_q1[-1,]
colnames(df_q1) <- c("pred","est","q1")
df_q1$est <- ifelse(df_q1$q1>0.05, 0, df_q1$est)
df_q1 <- df_q1 %>%
  dplyr::select(c(1,2)) %>%
  rename(q1 = est)

df_q2.t <- as.data.frame(summary(q2.t_glm)$coefficients[,c(1,4)]) %>%
  rownames_to_column()
df_q2.t <- df_q2.t[-1,]
colnames(df_q2.t) <- c("pred","est", "q2.t")
df_q2.t$est <- ifelse(df_q2.t$q2.t>0.05, 0, df_q2.t$est)
df_q2.t <- df_q2.t %>%
  dplyr::select(c(1,2)) %>%
  rename(q2.t = est)

df_q2.y <- as.data.frame(summary(q2.y_glm )$coefficients[,c(1,4)]) %>%
  rownames_to_column()
df_q2.y <- df_q2.y[-1,]
colnames(df_q2.y) <- c("pred","est", "q2.y")
df_q2.y$est <- ifelse(df_q2.y$q2.y>0.05, 0, df_q2.y$est)
df_q2.y <- df_q2.y %>%
  dplyr::select(c(1,2)) %>%
  rename(q2.y = est)

df_q3 <- as.data.frame(summary(q3_glm )$coefficients[,c(1,4)]) %>%
  rownames_to_column()
df_q3 <- df_q3[-1,]
colnames(df_q3) <- c("pred","est", "q3")
df_q3$est <- ifelse(df_q3$q3>0.05, 0, df_q3$est)
df_q3 <- df_q3 %>%
  dplyr::select(c(1,2)) %>%
  rename(q3 = est)


df_q <- full_join(df_q1, df_q2.t)
df_q <- full_join(df_q, df_q2.y)
df_q <- full_join(df_q, df_q3)

colnames(df_q) <- c("pred", "GLM for response or no response", "GLM for time of peak", "GLM for magnitude of peak", "GLM for speed of linear response")

rowp<- df_q$pred
df_q <- df_q %>%
  dplyr::select(-pred)
rownames(df_q) <- rowp
rownames(df_q) <-  c("(1) Carbon-related (2) water-related",
                     "Sample.unitindividual", 
                     "Sample.unitplot/footprint",
                     "MAT", 
                     "MAP",
                     "Pulse amount",
                     "MAP x MAT",
                     "MAP x carbon- or water-related",
                     "Pulse amount x carbon- or water-related")

write.csv(df_q, file = "data_output/coeff_mat.csv")


# Basic Interaction Plot 
library(interactions)

p1 <- interact_plot(q2.t_glm, pred = MAP.mm, modx = varGroup, interval = T, legend.main = element_blank(), colors = "Dark2") +
  labs(x = "MAP (mm)", y = "Timing of peak (days)", title = "(B)") +
  #coord_cartesian(ylim = c(0, 14), xlim = c(-.3,1.3)) +
  theme_bw() +
  theme(text = element_text(size=14),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())
p1

p2 <- interact_plot(q2.y_glm, pred = MAP.mm, modx = varGroup, interval = T, legend.main = element_blank(), colors = "Dark2") +
  labs(x = "MAP (mm)", y = "Magnitude of peak", title = "(C)") +
  #coord_cartesian(ylim = c(0, 14), xlim = c(-.3,1.3)) +
  theme_bw() +
  theme(text = element_text(size=14))
p2
p3 <- interact_plot(q2.t_glm, pred = Pulse.amount.mm, modx = varGroup, interval = T, legend.main = element_blank(), colors = "Dark2") +
  labs(x = "Pulse amount (mm)", y = "Timing of peak (days)", title = "(A)") +
  #coord_cartesian(ylim = c(0, 14), xlim = c(-.3,1.3)) +
  theme_bw() +
  theme(text = element_text(size=14))
p3
p4 <- interact_plot(q2.t_glm, pred = MAT.C, modx = MAP.mm, interval = T, legend.main = element_blank(), colors = "Dark2") +
  labs(x = "MAT (C)", y = "Timing of peak (days)", title = "(A)") +
  #coord_cartesian(ylim = c(0, 14), xlim = c(-.3,1.3)) +
  theme_bw() +
  theme(text = element_text(size=14))
p4

library(gridExtra)
library(patchwork)
library(grid)
layout <- '
ABC
'
p <- wrap_plots(A = p3, B = p1, C = p2, design = layout)
p <- p + plot_layout(guides = "collect") & theme(legend.position = "bottom")
p1.2 <- grid.arrange(patchworkGrob(p))
path_out = "./figures/" # set save path
ggsave2("fig5.png", plot = p1.2, path = path_out, width = 8, height = 3)


