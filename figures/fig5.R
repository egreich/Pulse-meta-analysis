
library(ggpubr)
library(tidyverse)
library(cowplot)
library(ggh4x)

# p value heatmap
df_p <- read.csv("data_output/p_mat.csv")

rowp <- df_p$X
df_p <- df_p %>%
  dplyr::select(-X)
rownames(df_p) <- rowp
p <-as.matrix(df_p)
# par(mar=c(4.1, 9, 4.1, 4.1)) # adapt margins
# #p <- plot(as.pvalue(pval), las = 1, fmt.cell='%.2f', col=c("#fcffa4","#f98e09","#bc3754","#57106e","#000004"))
# plot(as.pvalue(p), las = 1, fmt.cell='%.2f', col=c("#fcffa4","#f98e09","#bc3754","#57106e","#000004"))

# coeff heatmap

df_q <- read.csv("data_output/coeff_mat.csv")

rowp <- df_q$X
df_q <- df_q %>%
  dplyr::select(-X)
rownames(df_q) <- rowp
q <-as.matrix(df_q)


# make non-significant coefficients 0
for(i in 1:nrow(p)){
  for(j in 1:ncol(p)){
    if(!is.na(p[i,j])){
      if(p[i,j]>0.05){ # if the p-value is not significant
        q[i,j]<-0 # then make the coefficient 0
      }
    }
  }
}


# Reorganize as a data frame and rename columns
df <- as.data.frame(q)
df <- df %>%
  rownames_to_column() %>%
  pivot_longer(c(2:6)) %>%
  rename(question = name, coefficient = rowname)

# Create labels
df <- df %>%
  mutate(label = ifelse(value>0, "positive", "negative")) %>%
  mutate(label = ifelse(value==0, "nonsignificant", label)) %>%
  mutate(value = ifelse(value==0, NA, value))

# Reorder coefficients and questions, rename to things that make sense
df <- df %>%
  mutate(coefficient.label = case_when(coefficient == "Sample.unitindividual" ~ "Spatial Scale Covariates",
                                       coefficient == "Sample.unitplot/collar" ~ "Spatial Scale Covariates",
                                       coefficient == "Sample.unitfootprint" ~ "Spatial Scale Covariates",
                                       coefficient == "preVar*MAP.mm.wc" ~ "Question 3: Interactions",
                                       coefficient == "MAP.mm.wc*Pulse.amount.mm" ~ "Question 3: Interactions",
                                       coefficient == "obs" ~ "Pulse-related Covariates",
                                       coefficient == "Pulse.typeNatural" ~ "Pulse-related Covariates",
                                       coefficient == "Pulse.amount.mm" ~ "Pulse-related Covariates",
                                       coefficient == "varGroupwater" ~ "Pulse-related Covariates",
                                       coefficient == "MAP.mm.wc" ~ "Site Covariates",
                                       coefficient == "MAT.C.wc" ~ "Site Covariates",
                                       coefficient == "preVar" ~ "Site Covariates"),
         question.label = case_when(question == "Response.type" ~ "Question 1",
                                    question == "Response.or.no.response" ~ "Question 1",
                                    question == "time.of.peak" ~ "Question 2",
                                    question == "magnitude.of.peak" ~ "Question 2",
                                    question == "speed.of.linear.response" ~ "Question 2"))

df$coefficient.label <- factor(df$coefficient.label, levels =  c("Question 3: Interactions","Pulse-related Covariates",
                                                           "Site Covariates","Spatial Scale Covariates"))
         
df$coefficient <- factor(df$coefficient, levels =  c("varGroupwater","Sample.unitindividual",
                                                      "Sample.unitplot/collar","Sample.unitfootprint",    
                                                      "MAT.C.wc","MAP.mm.wc",               
                                                      "Pulse.typeNatural","Pulse.amount.mm",         
                                                      "preVar","obs",                     
                                                      "preVar*MAP.mm.wc","MAP.mm.wc*Pulse.amount.mm"),
                            labels = c("Variable Group","Individual",
                                       "Plot/collar","Footprint",    
                                       "MAT","MAP",               
                                       "Pulse Type","Pulse Amount",         
                                       "Initial Conditions","Number of Observations",                     
                                       "MAP*Initial Conditions","MAP*Pulse Amount"))

df$question <- factor(df$question, levels =  c("Response.type","Response.or.no.response","time.of.peak","magnitude.of.peak","speed.of.linear.response"),
                         labels = c("Response type","Response or no response","Time of peak","Magnitude of peak","Speed of linear response"))

df$coefficient.label <- ifelse(is.na(df$coefficient.label), "Other",df$coefficient.label)
# Plot

df <- df %>% # filter out questions we're dropping
  filter(question != "Response type") # not identifiable anymore

p <- df %>%
  ggplot(aes(question, interaction(coefficient,coefficient.label, sep = "!"), fill = label)) +
  #geom_tile(aes(width=0.7, height=0.7, color = value), size = 2) +
  geom_tile() +
  #geom_text(aes(label=round(value, digits=4))) +
  scale_fill_manual(values = c("blue", "darkgray","red")) +
  facet_grid(coefficient.label~question.label, scales = "free", space = "free") +
  xlab("Response Variables") +
  #scale_x_discrete(guide = guide_axis_nested(delim = "!"), name = NULL) +
  scale_y_discrete(guide = guide_axis_nested(delim = "!"), name = NULL) +
  theme_bw() +
  theme(#panel.spacing = unit(0, "lines"), 
        strip.background = element_blank(),
        strip.placement = "outside",
        strip.text.y = element_blank(),
        text = element_text(size=14),
        legend.title = element_blank(),
        axis.text.x = element_text(size=14, angle = 90, vjust = 0.8, hjust = 1))
p

ggsave2("fig5.png", plot = p, path = "./figures/", width = 10, height = 6)


