####注意此代码需要手动调整通路及颜色！！！主要是通路名称无法固定：变量名是name和color
#setwd('/data/hstore3/shouhou/2023/202312/LM2021-11580-售后2')
pacman::p_load("lmbio","ComplexHeatmap","stringr","dplyr")
number <- 1
outname <- paste0(number,".heatmap")
y <- '30'
width <- 4.2
height <- 6
group <- readdata("data.xlsx",number)
group$Accession <- gsub(":.*","",group$`蛋白:基因`)
#data <- readdata("差异蛋白筛选结果.xlsx",3)
data <- readdata("预处理数据.xlsx",1)
sam <- readdata("sample_information.xlsx")
compare <- readdata("sample_information.xlsx",2)[1,1]
case <- gsub("/.*","",compare)
control <- gsub(".*/","",compare)
casesamp <- sam$样品编号[grepl(case,sam$样品分组)]
consamp <- sam$样品编号[grepl(control,sam$样品分组)]
anngroup <- c(rep(control,length(consamp)),rep(case,length(casesamp)))
heatdata <- left_join(group,data[c('Accession',consamp,casesamp,"FC")],by="Accession")
name_list <- list(c("Hepatic inflammation"="HI","Hepatic fibrosis"="HF","Hepatic glycogen synthesis"="HG"),
                  c("Cholesterol metabolism"="CM","Linoleic acid metabolism"="LAM","Bile acid metabolism"="BAM"),
                  c("Fatty acid metabolism"="FAM","Other lipid metabolism"="Other"))
name <- name_list[[number]]
# name <- c("HI","HF","HG")
# name <- c("CM","LAM","BAM")
# name <- c("FAM","Other")
# names(name) <- c("Hepatic inflammation","Hepatic fibrosis","Hepatic glycogen synthesis")
# names(name) <- c("Cholesterol metabolism","Linoleic acid metabolism","Bile acid metabolism")
# names(name) <- c("Fatty acid metabolism","Other lipid metabolism")

heatdata$ID <- str_replace_all(heatdata$Pathway,name)
heatdata$`log2FC` <- log2(heatdata$FC)
heatdata1 <- rbind(subset(heatdata,heatdata$ID==name[1]),
                   subset(heatdata,heatdata$ID==name[2])
                   ,subset(heatdata,heatdata$ID==name[3])
                   )
heatdf <- heatdata1[c(consamp,casesamp)]
rownames(heatdf) <- paste0(heatdata1$`蛋白:基因`,"-",heatdata1$ID) 
# color <- c("#5D9AD4","#BDBDBD")
color <- c("Hepatic inflammation"="#F5AE92","Hepatic fibrosis"="#A8CE97","Hepatic glycogen synthesis"="#FFDD7E",
           "Cholesterol metabolism"="#F5AE92","Linoleic acid metabolism"="#A8CE97","Bile acid metabolism"="#FFDD7E",
           "Fatty acid metabolism"="#5D9AD4","Other lipid metabolism"="#BDBDBD")
# names(color) <- names(name)
groupcol <- c("#023EFF","#FF7C00")
names(groupcol) <- c(case,control)
heatdata1$color <- str_replace_all(heatdata1$Pathway,color)
heatdata1$ID <- factor(heatdata1$ID,levels = name)
anno_right <-  HeatmapAnnotation(
  barplot = anno_barplot(
    heatdata1$`log2FC`, 
    bar_width = 0.75,
    axis = TRUE,
    gp = gpar(col = "black", fill = heatdata1$color), 
    border = FALSE,
    axis_param = list(labels_rot = 0,
                      #labels = paste0("Fold change/n",case,"/",control),
                      side = "top"),
    width = unit(2, "cm")
  ), show_annotation_name = FALSE,which = "row")

lgd <- Legend(at = c(0, 0.5, 1), direction = "horizontal",
  labels = c("low",  "high"),
  labels_gp = gpar(col = "red", font = 3)
)
anno_top <- HeatmapAnnotation(
                          foo = anno_empty(
                            border = FALSE, 
                            # 计算空白注释的宽度
                            height = unit(5, "mm"),
                            width = max_text_width(unlist(c(control,case))) + unit(2, "mm")),
                          bar=anngroup,#列注释
                          border = F,
                          show_legend = F,
                          show_annotation_name = F,
                          col = list(bar =groupcol))
anno_left <- HeatmapAnnotation(Function=heatdata1$Pathway,#列注释
                               border = F,
                               show_annotation_name = F,
                               width = unit(5, "mm"),
                               col = list(Function =color),
                           which = "row")
a <- pheatmap(heatdf,scale = "row")
heatdf <- as.data.frame(a@matrix)
ht_list <- Heatmap(heatdf,
                   name="Protein expression",
                   row_split = heatdata1$ID,
        row_gap = unit(4, "mm"),
        show_column_names = F,
        cluster_rows = F,cluster_columns = F,
        top_annotation = anno_top,
        left_annotation = anno_left,
        heatmap_width = unit(1, "npc"),
        heatmap_height = unit(1, "npc"),
        heatmap_legend_param = list(
          at = c(min(heatdf),  max(heatdf)),
          labels = c("Low",  "High"),
          legend_height = unit(4, "cm"),
          direction = "horizontal",
          title_position = "lefttop"
        ),
        rect_gp = gpar(col= "white",lwd = 2))+
  rowAnnotation(text = anno_text(heatdata1$Accession)) + 
  anno_right
pdf(paste0(outname,".pdf"), width = width, height = height)
par(mar=c(6,5,4,3) + 0.1)
#grid.newpage()
draw(ht_list, merge_legend = TRUE, 
     annotation_legend_side = "bottom",
     heatmap_legend_side = "bottom")
#list_components()
###以下为添加分组标签，需要手动调整！
decorate_annotation(
    "foo", 
    grid.text(
      c(control,case)[[1]], 
      x = unit(10, "mm"),
      y=unit(3.5,"mm"),
      gp = gpar(
        col = "black"
      ),
      just = "top"))+
decorate_annotation(
    "foo", 
    grid.text(
      c(control,case)[[2]], 
      x = unit(30, "mm"),
      y=unit(3.5,"mm"),
      gp = gpar(
        col = "black"
      ),
      just = "top"))+
  decorate_annotation(
    "barplot", 
    grid.text(
      paste0("Log2Foldchange\n",case,"/",control), 
      x = unit(10, "mm"),
      y=unit(y,"mm"),
      gp = gpar(fontsize=10,
        col = "black"
      ),
      just = "top"))
dev.off()
png(paste0(outname,".png"), width = width*80, height = height*80)
#grid.newpage()
draw(ht_list, merge_legend = TRUE, 
     annotation_legend_side = "bottom",
     heatmap_legend_side = "bottom")
#list_components()
###以下为添加分组标签，需要手动调整！
decorate_annotation(
    "foo", 
    grid.text(
      c(control,case)[[1]], 
      x = unit(10, "mm"),
      y=unit(3.5,"mm"),
      gp = gpar(
        col = "black"
      ),
      just = "top"))+
decorate_annotation(
    "foo", 
    grid.text(
      c(control,case)[[2]], 
      x = unit(30, "mm"),
      y=unit(3.5,"mm"),
      gp = gpar(
        col = "black"
      ),
      just = "top"))+
  decorate_annotation(
    "barplot", 
    grid.text(
      paste0("Log2Foldchange\n",case,"/",control), 
      x = unit(10, "mm"),
      y=unit(y,"mm"),
      gp = gpar(fontsize=10,
        col = "black"
      ),
      just = "top"))
dev.off()

###数据保存
out <- as.data.frame(cbind(rownames(heatdf),heatdf))
names(out)[1] <- "Accession"
out[-1] <- as.numeric(as.matrix(out[-1]))
savexlsx(list("输入数据"=heatdata1[c("Accession","蛋白:基因","Pathway",c(consamp,casesamp),"FC","log2FC")],
              "绘图数据"=out),paste0(outname,".xlsx"))
grid.newpage()
