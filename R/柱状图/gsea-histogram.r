#这是gsea柱状图
library(lmbio)
library(ggplot2)
inputfile <- 'gsea.gsea.substance.report-go.csv'
mapname <- 'histogram-go'
data <- readdata(inputfile)
data$Term <- gsub("\\(mmu.*\\)","",data$Term)
data$Term <- gsub("\\(GO.*\\)","",data$Term)
#此为排序由下调至上调排序
data <- data[order(data$NES,decreasing = F),]
fifterdata <- data[1:10,]
fifterdata <- fifterdata[order(fifterdata$NES,decreasing = T),]
fifterdata$Term <- factor(fifterdata$Term,levels = fifterdata$Term)
p <- ggplot(fifterdata, aes(x = Term, y = NES)) +
  geom_bar(stat = "identity", fill = "#0056a9",width = 0.9) +
  scale_y_continuous(position = "right",trans = "reverse",expand = c(0,0))+
  scale_x_discrete(expand = c(0,0))+
  coord_flip()+
  labs(y="Normalized Enrichment Score",x="")+
  theme_classic()+
  theme(plot.margin = unit(c(0.5, 0.8, 0.5, 0.5), "cm"))+
  theme(axis.text = element_text(color = "black",size = 12),
        axis.title.x = element_text(size = 13,hjust = -0.1),
        axis.ticks = element_line(size = 1.5,color = "black"),
        axis.ticks.length = unit(0.2, 'cm'),
        axis.line = element_line(size = 1.5))
ggplotsave(p,mapname = mapname,imagetype = c("png","pdf"),
           height = 2.76,
           width = 7.2) 
  
  
