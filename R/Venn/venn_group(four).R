pkgs <- c("eulerr", "ggplotify","openxlsx","VennDiagram","stringr","ggplot2")
pacman::p_load(pkgs, character.only = TRUE)
type <- "pro"
dfname <- c("pro"="Protein quantitation.xlsx","pep"="Peptides.xlsx")
colname <- c("pro"="Accession","pep"="StrippedSequence")
sheet <- getSheetNames(dfname[type])
venlist <- list()
for (i in 1:length(sheet)) {
  data <- read.xlsx(dfname[type],i)
  id <- unique(data[colname[type]][,1])
  venlist[[sheet[i]]] <- id
}
intersections <- list()
for (i in 1:(length(venlist) - 1)) {
  for (j in (i + 1):length(venlist)) {
    intersection <- length(intersect(venlist[[i]], venlist[[j]]))
    intersections[[paste(sheet[i], sheet[j], sep = "&")]] <- intersection
  }
}
for (i in 1:length(sheet)) {
  #venlist <- c(venlist,list(id))
  intersections[[sheet[i]]] <- length(venlist[[sheet[i]]])
}
intersections[[paste(sheet[1], sheet[2], sheet[3],sep = "&")]] <- length(intersect(intersect(venlist[[1]], venlist[[2]]), venlist[[3]]))
intersections[[paste(sheet[1], sheet[2], sheet[4],sep = "&")]] <- length(intersect(intersect(venlist[[1]], venlist[[2]]), venlist[[4]]))
intersections[[paste(sheet[1], sheet[3], sheet[4],sep = "&")]] <- length(intersect(intersect(venlist[[1]], venlist[[3]]), venlist[[4]]))
intersections[[paste(sheet[2], sheet[3], sheet[4],sep = "&")]] <- length(intersect(intersect(venlist[[2]], venlist[[3]]), venlist[[4]]))
intersections[[paste(sheet[2], sheet[3], sheet[4],sep = "&")]] <- length(intersect(intersect(venlist[[2]], venlist[[3]]), venlist[[4]]))
intersections[[paste(sheet[1],sheet[2], sheet[3], sheet[4],sep = "&")]] <- length(intersect(intersect(intersect(venlist[[1]], venlist[[2]]), venlist[[3]]),venlist[[4]]))

dat1 <- t(as.data.frame(intersections))
dat2 <- dat1[,1]
names(dat2) <- gsub("\\.","&",names(dat2))
#继续以上述4个分组为例，组间交集元素获得
#inter <- get.venn.partitions(venlist)
a <- euler(dat2)
b <- a[["original.values"]]
p <- plot(euler(dat2),
     fills = list(fill=c("#fee6ce","#ffffd6","#cce9cc","#e5deee"),
                  alpha=0.8),
     #quantities = c(152,155,518,173,33,41,NA,NA,8,702,90,43,176,151,1097),
     quantities = b,
     edges = list(col="white",alpha=0))
save(p,venlist,dfname,type,file = paste0("plot_",type,".RData"))
