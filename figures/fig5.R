
library('plot.matrix')
library(tidyverse)
library(cowplot)

# p value heatmap
df_p <- read.csv("data_output/p_mat.csv")

rowp <- df_p$X
df_p <- df_p %>%
  select(-X)
rownames(df_p) <- rowp
p <-as.matrix(df_p)
# par(mar=c(4.1, 9, 4.1, 4.1)) # adapt margins
# #p <- plot(as.pvalue(pval), las = 1, fmt.cell='%.2f', col=c("#fcffa4","#f98e09","#bc3754","#57106e","#000004"))
# plot(as.pvalue(p), las = 1, fmt.cell='%.2f', col=c("#fcffa4","#f98e09","#bc3754","#57106e","#000004"))

# coeff heatmap

df_q <- read.csv("data_output/coeff_mat.csv")

rowp <- df_q$X
df_q <- df_q %>%
  select(-X)
rownames(df_q) <- rowp
q <-as.matrix(df_q)


# make non-significant coefficients 0
for(i in 1:nrow(p)){
  for(j in 1:ncol(p)){
    if(!is.na(p[i,j])){
      if(p[i,j]>0.05){ # if the p-value is not significant
        q[i,j]<-NA # then make the coefficient 0
      }
    }
  }
}

# library(RColorBrewer)
# colors <- brewer.pal(n=11,"RdBu")
# 
# library('plot.matrix')
# png("fig5.png")
# par(mar=c(4.1, 9, 4.1, 4.1)) # adapt margins
# plot(q, las = 1, fmt.cell='%.2f', col=colors)
# dev.off
# plot(q, las = 1, fmt.cell='%.2f', col=c("#fcffa4","#f98e09","#bc3754","#57106e","#000004"))

df <- as.data.frame(q)
df <- df %>%
  rownames_to_column() %>%
  pivot_longer(c(2:6)) %>%
  rename(question = name, coefficient = rowname)
p <- df %>%
  ggplot(aes(question, coefficient, fill = ifelse(value>0, "positive", "negative"))) +
  #geom_tile(aes(width=0.7, height=0.7, color = value), size = 2) +
  geom_tile() +
  geom_text(aes(label=round(value, digits=3))) +
  #scale_color_viridis_c(option = "B") +
  scale_fill_manual(values = c("red", "blue")) +
  #scale_color_gradient2() +
  theme_bw() +
  theme(text = element_text(size=12),
        legend.text=element_blank(),
        axis.text.x = element_text(size = 12, angle = 90, vjust = 0.8, hjust = 1))
p
ggsave2("fig5.png", plot = p, path = "./figures/", width = 8, height = 5)


