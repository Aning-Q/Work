library(lmbio)
library(reshape)
library(ggplot2)
library(tidyr)
library(dplyr)
filename <- "Protein quantitation.xlsx"
sheet <- getsheetname(filename)
pos <- c(3,1)
mapname <- "line-Deep"
#蛋白
filename <- "Protein quantitation.xlsx"
prodata1 <- readdata(filename, sheet[pos[1]])
prodata2 <- readdata(filename, sheet[pos[2]])
#肽段
filename <- "Peptides.xlsx"
sheet <- getsheetname(filename)
pepdata1 <- readdata(filename, sheet[pos[1]])
pepdata2 <- readdata(filename, sheet[pos[2]])
                      
plotdata <- data.frame(pronum=c(colSums(!is.na(prodata1))[-1],colSums(!is.na(prodata2))[-1]),
                       pepnum=c(colSums(!is.na(pepdata1))[-1],colSums(!is.na(pepdata2))[-1]))

plotdata$name <- rownames(plotdata)
savexlsx(plotdata,paste0(mapname,".xlsx"))
#plotdata1 <- melt(plotdata,id="name")
plotdata$pepgp <- gsub("\\d+","-pep",plotdata$name)
plotdata$pepgp <- as.factor(plotdata$pepgp)
plotdata$progp <- gsub("\\d+","-pro",plotdata$name)
plotdata$progp <- as.factor(plotdata$progp)

name <- c("CFSN-pro","CFSD-pro","WSN-pro","WSD-pro",
          "CFSN-pep","CFSD-pep","WSN-pep","WSD-pep")

color <- c("#E16D74","#FFA566","#79BF74","#739FCA",
           "#814BB2","#794339","#102b6a","#1C7F93")
line <- c("solid","solid","solid","solid",
          "dashed","dashed","dashed","dashed")
shape <- c(15,15,15,15,
           17,17,17,17)
names(color) <- name
names(line) <- name
names(shape) <- name
#pronumdata$class <- factor(rownames(pronumdata),labels = rep(1:20))
p <- ggplot(plotdata,aes(x = factor(name,levels = name)))+
     geom_point(aes(y=pepnum,colour=pepgp,shape = pepgp),size = 4) +
     geom_line(aes(y=pepnum,group=pepgp,colour=pepgp,linetype=pepgp)) +
     
     #蛋白
     geom_point(aes(y=pronum*10,colour=progp,shape = progp),size = 4) +
     geom_line(aes(y=pronum*10,group=progp,colour=progp,linetype=progp)) +
     scale_y_continuous(limits = c(floor(min(plotdata$pepnum)/1000)*1000,ceiling(max(plotdata$pronum)/10)*100),sec.axis = sec_axis(~./10,
                                         name = ' ProteinGroups Frequence\n'))+
     scale_color_manual(values = color) +
     scale_linetype_manual(values = line) +
     scale_shape_manual(values = shape)+
     labs(x="",y="StrippedSequence Frequence\n")+
     theme_light()+
      theme(
        axis.ticks = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        plot.margin = unit(rep(2,4),"lines"),
        panel.border = element_blank(),
        axis.line.x = element_line(color = "#CCD6EB", linewidth = 1),
        axis.text.x=element_text(hjust = 1,vjust = 1,size=15,color="black",angle = 60),
        axis.text.y=element_text(vjust = 0.5,size=15,color="black"),
        legend.text=element_text(size=15),
        legend.position = "bottom",
        legend.title = element_blank(),
        title = element_text(size = 15,color="black"))

ggplotsave(p,
    mapname = mapname,
    imagetype = c("png","pdf"),
    family = "sans",
    height = 6,
    width = 12)

