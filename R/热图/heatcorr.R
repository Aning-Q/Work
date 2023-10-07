library(lmbio)
library(psych)
library(pheatmap)
library(reshape2)
#读取丰度信息
diff <-readdata("./差异蛋白筛选结果.xlsx",4)
rownames(diff) <- diff$Accession
diff1 <- t(diff[,-1])
#读取代谢物信息
data <-readdata("./肉品质参数-DLM202213989.xlsx",1)
rownames(data) <- data[,1]
data1 <- t(data[,-1])
cor <-corr.test(diff1, data1, method = "pearson",adjust="none")
cmt <-cor$r
pmt <- cor$p
#判断显著性
if (!is.null(pmt)){
  ssmt <- pmt< 0.01
  pmt[ssmt] <-'**'
  smt <- pmt >0.01& pmt <0.05
  pmt[smt] <- '*'
  pmt[!ssmt&!smt]<- ''
} else {
  pmt <- F
}
colors <- colorRampPalette(c("#090887", "white", "#B9393B"))(100)
#可视化
p <- pheatmap(cmt,scale = "none",cluster_row = F, cluster_col = F, border=NA,
         display_numbers = pmt,fontsize_number = 12, number_color = "black",
         fontsize_row = 12,fontsize_col = 15,angle_col = 45,
         cellwidth = 20, cellheight =20,color=colors)
ggplotsave(p,  
           mapname = paste0("Corre_heatmap"),  
           imagetype = c("png","pdf"),  
           family = "sans",  
           height = 10,  
           width = 12)
savexlsx(list("差异蛋白"=as.data.frame(diff1),"参数"=as.data.frame(data1),"绘图数据"=as.data.frame(cmt)),"Corre_heatmap.xlsx",row.names=T)
