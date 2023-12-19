
library('plot.matrix')

# p value heatmap
df_q <- read.csv("data_output/p_mat.csv")

rowp <- df_q$X
df_q <- df_q %>%
  select(-X)
rownames(df_q) <- rowp
q <-as.matrix(df_q)
par(mar=c(4.1, 9, 4.1, 4.1)) # adapt margins
#p <- plot(as.pvalue(pval), las = 1, fmt.cell='%.2f', col=c("#fcffa4","#f98e09","#bc3754","#57106e","#000004"))
plot(as.pvalue(q), las = 1, fmt.cell='%.2f', col=c("#fcffa4","#f98e09","#bc3754","#57106e","#000004"))


# coeff heatmap


df_q <- read.csv("data_output/coeff_mat.csv")

rowp <- df_q$X
df_q <- df_q %>%
  select(-X)
rownames(df_q) <- rowp
q <-as.matrix(df_q)

library('plot.matrix')
par(mar=c(4.1, 9, 4.1, 4.1)) # adapt margins
#p <- plot(as.pvalue(pval), las = 1, fmt.cell='%.2f', col=c("#fcffa4","#f98e09","#bc3754","#57106e","#000004"))
plot(q, las = 1, fmt.cell='%.2f', gray=T)



