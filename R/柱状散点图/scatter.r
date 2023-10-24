library(lmbio)
library(reshape)
library(ggplot2)
library(tidyr)
library(dplyr)
library(ggbeeswarm)
filename <- "Protein quantitation.xlsx"
sheet <- getsheetname(filename)
pos <- c(4,3)
mapname <- "scatter-WS"
# 富集前Normal
data1 <- readdata(filename, sheet[pos[1]])
predf <- melt(data1)
df1 <- na.omit(predf)
df_summary1 <- df1 %>%
  group_by(variable) %>%
  mutate(rank = rank(-value)) %>%
  ungroup()

# 然后将前50的值设为 "top50"
df_summary1$group[df_summary1$rank <= 50] <- "top50"
df_summary1$group[df_summary1$rank > 50] <- "untop"
df_summary1$value <- log2(df_summary1$value)

# 富集后Deep
data2 <- readdata(filename, sheet[pos[2]])
postdf <- melt(data2)
df2 <- na.omit(postdf)
df_summary2 <- df2 %>%
  group_by(variable) %>%
  mutate(rank = rank(-value)) %>%
  ungroup()

# 然后将前50的值设为 "top50"
df_summary2$group[df_summary2$rank <= 50] <- "top50"
df_summary2$group[df_summary2$rank > 50] <- "untop"
#非交集部分
inter <- intersect(df_summary1$Accession,df_summary2$Accession)
df_summary2$group[!(df_summary2$Accession %in% inter)]  <-  "none"

df_summary2$value <- log2(df_summary2$value)

#数据合并-将非交集部分提到最后，以防覆盖
df_summary <- rbind(df_summary1,df_summary2)
savexlsx(df_summary,"scatter-WS.xlsx")
df_summary <- df_summary %>% 
    arrange(factor(group, levels = c("untop","top50","none")))
p <- ggplot(df_summary, aes(variable, value)) + 
    geom_jitter(aes(fill = group,color=group) , position = position_jitter(0.3), shape=21, size = 2) +
    scale_fill_manual(values = c(untop="#CCCCCC",top50="#FF0000",none="#4A86E8")) +
    scale_color_manual(values = c(untop="#CCCCCC",top50="#FF0000",none="#4A86E8")) +
    theme_classic()+
    labs(x="",y="")+
    theme(panel.background=element_rect(fill = "white", colour = "black", size=0.25),
        axis.line=element_line(colour="black",size=0.25),
        axis.title=element_text(size=13,face="plain",color="black"),
        axis.text.x = element_text(size=12,face="plain",color="black",angle = 60,hjust = 1,vjust = 1),        
        axis.text.y = element_text(size=12,face="plain",color="black"),
        legend.position="none"
  )
ggplotsave(p,
    mapname = mapname,
    imagetype = c("png","pdf"),
    family = "sans",
    height = 6,
    width = 10)