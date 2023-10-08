# 这是一个修改示例
library(lmbio)
library(VennDiagram)
load("plot_pro.RData")
sheet <- getsheetname(dfname[type])
fill=c("#fee6ce","#ffffd6","#cce9cc","#e5deee")
names(fill) <- sheet
#调节哪两个韦恩图
position <- c(1,3)
mapname <- "venn_Deep"

da_cfs <- venlist[position]
futile.logger::flog.threshold(futile.logger::ERROR, name = "VennDiagramLogger") 
p_cfs <- venn.diagram(da_cfs,filename = NULL,
                  output=TRUE,
                  fill=fill[position],
                  margin=0.1,
                  col="transparent",
                  fontfamily = "ArialMT",
                  cat.cex=1.5,cat.dist = c(0.055, 0.085),cat.fontfamily ="ArialMT")
ggplotsave(plot = p_cfs,
           mapname = mapname,
           height = 6,
           width =6,
           savepath = "./",
           imagetype = c("png","pdf"),
           dpi = 300,
           family = "sans")
#拼接数据1,0
data1 <- Reduce(union, da_cfs)
data3 <- data.frame(ID = data1)
i <- 1
while (i <= length(da_cfs)) {
  # unlist(data[i])
  data2 <- data.frame(c(rep(0, length(data1))))
  data2[data3[, 1] %in% unlist(da_cfs[i]), ] <- 1
  names(data2) <- names(da_cfs[i])
  data3 <- data.frame(data3, data2, check.names = F)
  i <- i + 1
}
#匹配表达量信息
pep1 <- readdata(dfname[type],sheet[position[1]])
pep2 <- readdata(dfname[type],sheet[position[2]])
if(type=="pep"){
  names(pep1)[c(1,2,3)] <- c(paste0("ProteinGroups_",sheet[position[1]]),"ID",paste0("PeptidePosition_",sheet[position[1]]))
  names(pep2)[c(1,2,3)] <- c(paste0("ProteinGroups_",sheet[position[2]]),"ID",paste0("PeptidePosition_",sheet[position[2]]))
}else{
  names(pep1)[1] <- "ID"
  names(pep2)[1] <- "ID"
}
data4 <- merge(data3,pep1,by="ID",all.x = T)
data4 <- merge(data4,pep2,by="ID",all.x = T)
savexlsx(data4,paste0(mapname,".xlsx"))

#总表
data1 <- Reduce(union, venlist)
data3 <- data.frame(ID = data1)
i <- 1
while (i <= length(venlist)) {
  # unlist(data[i])
  data2 <- data.frame(c(rep(0, length(data1))))
  data2[data3[, 1] %in% unlist(venlist[i]), ] <- 1
  names(data2) <- names(venlist[i])
  data3 <- data.frame(data3, data2, check.names = F)
  i <- i + 1
}
ifelse(type=="pro",names(data3)[1] <-  "Accession")
for(j in 1:length(sheet)){
  pep1 <- readdata(dfname[type],sheet[[j]])
  if(type=="pep"){names(pep1)[c(1,2,3)] <- c(paste0("ProteinGroups_",sheet[[j]]),"ID",paste0("PeptidePosition_",sheet[[j]]))}
  data3 <- merge(data3,pep1,by="Accession",all.x = T)
}
savexlsx(data3,paste0(dfname[type],".xlsx"))
