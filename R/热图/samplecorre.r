library(lmbio)
library(reshape)
library(ggplot2)
library(tidyr)
library(dplyr)
library(cowplot)
filename <- "samplecorre.xlsx"
sheet <- getsheetname(filename)
pos <- c(2,1)
mapname <- "corre-CFS"
#富集前
prodata1 <- readdata(filename, sheet[pos[1]])[,-1]
promin1 <- min(as.matrix(prodata1),na.rm = T)
prodata1[is.na(prodata1)] <- promin1
procor1 <- cor(prodata1,method = "pearson")
procormat1 <- melt(procor1)
#富集后
prodata2 <- readdata(filename, sheet[pos[2]])[,-1]
promin2 <- min(as.matrix(prodata2),na.rm = T)
prodata2[is.na(prodata2)] <- promin2
procor2 <- cor(prodata2,method = "pearson")
procormat2 <- melt(procor2)
#颜色设置
color  <-  c("#440255","#38598A","#269189","#32AF7B","#6ECC58","#C2DD38","#F4E628")

#绘图
p1 <- ggplot(data = procormat1, aes(x=X1, y=X2, fill=value)) + 
      geom_tile()+
      labs(x="",y="")+
      scale_fill_gradientn(colors = colorRampPalette(color)(100))+
      #theme_minimal()+
      theme(
        panel.grid=element_blank(),
        panel.border = element_blank(),
        panel.background = element_rect(fill = NA),
        text=element_text(size=15),
        plot.margin = unit(c(2,2,0,2),"lines"),
        axis.ticks = element_blank(),
        axis.text.y=element_text(size=15,color="black"),
        axis.text.x=element_text(size=15,color="black",hjust = 1,vjust = 1,angle = 60),
        legend.position = "none",
        legend.title = element_blank())

p2 <- ggplot(data = procormat2, aes(x=X1, y=X2, fill=value)) + 
      geom_tile()+
      labs(x="",y="")+
      scale_y_discrete(position = "right",expand = c(0,0))+
      scale_fill_gradientn(colors = colorRampPalette(color)(100))+
      theme(
        panel.grid=element_blank(),
        panel.border = element_blank(),
        plot.margin = unit(c(2,2,0,2),"lines"),
        panel.background = element_rect(fill = NA),
        text=element_text(size=15),
        axis.ticks = element_blank(),
        axis.text.y=element_text(size=15,color="black"),
        axis.text.x=element_text(size=15,color="black",hjust = 1,vjust = 1,angle = 60),
        legend.position = "none",
        legend.title = element_blank())
legend <- get_legend(
    p1 +
    theme(legend.position = "bottom"))
combined_plot <- plot_grid(p1, p2, nrow = 1,ncol=2,rel_widths = c(2.5,2.5))
p <- plot_grid(combined_plot,legend,ncol=1,rel_heights = c(1, .1))
ggplotsave(p,
    mapname = mapname,
    imagetype = c("png","pdf"),
    family = "sans",
    height = 6,
    width = 14)
outdata <- list(as.data.frame(procor1),as.data.frame(procor2))
names(outdata) <- c(sheet[pos[1]],sheet[pos[2]])
savexlsx(outdata,paste0(mapname,".xlsx"),rowNames=T)
