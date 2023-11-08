#!/opt/conda/bin/Rscript
#' 工业部火山图
#'
#' @param inputpath 
#' @param inputfile 
#' @param imagetype 
#' @param height 
#' @param width 
#' @param dpi 
#' @param fontfamily 
#' @param x 
#' @param FC 
#' @param pvalue 
#' @param top 
#' @param num 
#' @param sigtype 
#' @param ... 
#'
#' @return
#' @export
#'
#' @examples
industvolplot <- function( inputpath="./",inputfile="预处理数据.xlsx", imagetype=c("pdf","png"), 
                           height=10, width=12, dpi=300, fontfamily="sans",x = 12,
                           FC=2, pvalue=0.05, top=F, num=5,sigtype="Gene Name",...){
  pacman::p_load("ggplot2","ggrepel","openxlsx","dplyr","readxl","lmbio")
  markdata <- "./RNA bind.xlsx"
  foldername <- paste0("volcano-FC=",FC,"&P=",pvalue)
  if (dir.exists(foldername)){
    list.files(foldername)
  }else{
    dir.create(foldername)
  }
  compare <- getSheetNames(inputfile)
  for (i in 1:length(compare)) {
    predata <- readdata(inputfile,sheet = i)
    names(predata) <- gsub("P-value|p-value","PValue",names(predata))
    names(predata)[names(predata) == 'FoldChange'] <- 'FC'
    dd <- predata[,c("Accession","Gene Name","PValue","FC")]
    data <- dd
    if(any(grepl("inf",data$FC))==TRUE & length(which(data$FC %in% 0)) != 0){
      d1 <- data[-which(data$FC %in% c("inf",0)),]
      mma <- max(as.numeric(max(d1$FC)),max(1/as.numeric(d1$FC)))*10+3000
      data[which(data$FC %in% "inf"),"FC"] <- mma
      data[which(data$FC %in% 0),"FC"] <- 1/mma
    }else if(any(grepl(Inf,data$FC))==TRUE & length(which(data$FC %in% 0)) != 0){
      d1 <- data[-which(data$FC %in% c(Inf,0)),]
      mma <- max(as.numeric(max(d1$FC)),max(1/as.numeric(d1$FC)))*10
      data[which(data$FC %in% Inf),"FC"] <- mma
      data[which(data$FC %in% 0),"FC"] <- 1/mma
    }
    data$PValue <- as.numeric(data$PValue)
    data$FC <- as.numeric(data$FC) 
    data$logFC <- log2(data$FC)
    data$sort[(-1*log10(data$PValue) < -1*log10(pvalue) | data$FC == "NA")|(log2(data$FC) < log2(FC)) & log2(data$FC) > -log2(FC)] <- "none-significan"
    data$sort[-1*log10(data$PValue) >= -1*log10(pvalue) & log2(data$FC) >= log2(FC)] <- "up-regulated"
    data$sort[-1*log10(data$PValue) >= -1*log10(pvalue) & log2(data$FC) <= -log2(FC)] <- "down-regulated"
    if(is.null(x)){
    mx <- ceiling(quantile(abs(data[which(data$sort %in% c("up-regulated","down-regulated")),"logFC"]),probs = c(0.999))[[1]]*1.12)
    }else{
      mx <- x
    }
    data$log10P <- -log10(data$PValue)
    my <- max(data$log10P)
    data2 <- data
    if(top){
      datat <- data2 %>% filter(sort=="up-regulated")
      datat <- datat[order(-datat$FC),]
      datat_up <- na.omit(datat[1:num,])
      datat <- data2 %>% filter(sort=="down-regulated")
      datat <- datat[order(datat$FC),]
      datat_down <- na.omit(datat[1:num,])
      datat_none <- NULL
    }else if(!(is.null(markdata))){
      markgene <- readdata(markdata)
      markgene1 <- merge(markgene[c("Accession")],data,by="Accession",by.x=T)
      datat_up <- markgene1 %>% filter(sort=="up-regulated")
      datat_down <- markgene1 %>% filter(sort=="down-regulated")
      #有有无蛋白
      if(ceiling(max(datat_up$FC))==ceiling(mma)){
        datat_up2 <- datat_up[datat_up$FC==max(datat_up$FC),]
        datat_up <- setdiff(datat_up,datat_up2)
        datat_down2 <- datat_down[datat_down$FC==min(datat_down$FC),]
        datat_down <- setdiff(datat_down,datat_down2)
      }
      datat_none <- NULL
    }
    else{
      datat_up <- NULL
      datat_down <- NULL
      datat_none <- NULL
    }
    if("sig" %in% names(predata)){
      label <- na.omit(predata$sig)
      signame <- ifelse(length(intersect(label,data2$`Gene Name`))!=0,"Gene Name","Accession")
      datat <- data2 %>% filter(sort=="up-regulated")
      sig_up <- datat[which(datat[,signame] %in% label) ,]
      datat <- data2 %>% filter(sort=="down-regulated")
      sig_down <- datat[which(datat[,signame] %in% label),]
      datat <- data2 %>% filter(sort=="none-significan")
      datat_none <- datat[which(datat[,signame] %in% label),]
    }else{
      sig_up <- NULL
      sig_down <- NULL
      datat_none <- NULL
    }
    datat_up <- rbind(datat_up,sig_up)
    datat_down <- rbind(datat_down,sig_down)
    if(exists("signame")){
      sigtype <- signame
    }else{
      signame <- sigtype
    }
    data2[data2[,signame] %in% datat_up[,signame],"addup"] <- data2[data2[,signame] %in% datat_up[,signame],sigtype]
    data2[data2[,signame] %in% datat_down[,signame],"adddown"] <- data2[data2[,signame] %in% datat_down[,signame],sigtype]
    data2[data2[,signame] %in% datat_none[,signame],"addnone"] <- data2[data2[,signame] %in% datat_none[,signame],sigtype]
    #有有无蛋白
    if(exists("datat_up2")){
      data2[data2[,signame] %in% datat_up2[,signame],"addup2"] <- data2[data2[,signame] %in% datat_up2[,signame],sigtype]
      data2[data2[,signame] %in% datat_down2[,signame],"adddown2"] <- data2[data2[,signame] %in% datat_down2[,signame],sigtype]
    }

    data2$sort <- factor(data2$sort,levels = c("none-significan","up-regulated","down-regulated"))
    data2 <- arrange(data2,sort)
    suma <- table(data2$sort)
    b <- mx
    #d <- ifelse(b >= 4,2,ifelse(b <= 1.5,0.5,1))
    d <- 5
    bb <- ifelse(b==1,1,ifelse(ceiling(b)%%2 ==0,ceiling(b),ceiling(b)-1))
    volplot <- ggplot(data2,aes(x=logFC,y=log10P,colour=sort))+
      geom_point(size=3)+ 
      scale_x_continuous(limits = c(-b, b),breaks = seq(-bb,bb,d))+
      geom_label_repel(
        aes(label= addup),size=5,force = 15,show.legend = F,nudge_x=b*0.2,direction="y",
        max.overlaps = getOption("ggrepel.max.overlaps", default = 100)
      )+
      geom_label_repel(
        aes(label= adddown),size=5,force = 15,show.legend = F,nudge_x=-b*0.2,direction="y",
        max.overlaps = getOption("ggrepel.max.overlaps", default = 100)
      )+
      geom_label_repel(
        aes(label= addnone),size=5,force = 15,show.legend = F,nudge_x=-b*0.2,direction="y",
        max.overlaps = getOption("ggrepel.max.overlaps", default = 100)
      )+
      #scale_color_manual(values = c("up-regulated" = "red","none-significan" = "gray50","down-regulated" = "blue",
       #                             "target-up"="red", "target-none"="gray50","target-down"="blue")
      #) +
      scale_color_manual(values = c("up-regulated" = "red","none-significan" = "gray50","down-regulated" = "blue"),
                         breaks = c("up-regulated","none-significan" ,"down-regulated"),
                         labels = c(paste0("up-regulated(",suma["up-regulated"][[1]],")"),
                                    paste0("none-significan(",suma["none-significan"][[1]],")"),
                                    paste0("down-regulated(",suma["down-regulated"][[1]],")")))+
      geom_vline(xintercept = c(-log2(FC),log2(FC)),lty = "dashed",col = "black",lwd = 0.9)+
      geom_hline(yintercept = -log10(pvalue),lty = "dashed",col = "black",lwd = 0.9) +
      annotate("text",x = 0.78*mx,y = -log10(pvalue)-0.15*(my/8.5),label = paste0("P-value=",pvalue),colour = "black",size = 6) +
      annotate("text",x = -log2(FC)-1.3*mx*0.105,y = my*0.92,label = paste0("FC=1/",FC),colour = "black",size = 6) +
      annotate("text",x = log2(FC)+1.22*mx*0.083,y = my*0.92,label = paste0("FC=",FC),colour = "black",size = 6) +
      #labs(x=paste0(" \nlog2 FC  (",gsub("-vs-","/",compare[i]),")"),y="-log10(P-value)\n ",title = "Volcano Plot")+
      labs(x=paste0(" \nlog2 FC"),y="-log10(P-value)\n ",title = "Volcano Plot")+
      theme_linedraw()+
      theme(panel.border = element_rect(fill=NA,color="black", size=1.5, linetype="solid"),
            panel.grid = element_blank(),
            legend.position = "right",#图例位置
            legend.text = element_text(size = 17),
            legend.title = element_blank(),
            legend.background = element_rect(color = "gray",linetype = 1,size = 0.5),#添加图例边框
            legend.key = element_blank(),#图标键填充为空
            legend.key.height = unit(22,"pt"),#增加图例标签之间的距离
            legend.key.width = unit(38,"pt"),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            axis.text = element_text(size = 18,colour = "black"),
            axis.title = element_text(size = 18,colour = "black"),
            plot.margin=unit(rep(1,4),'lines'),plot.title = element_text(hjust = 0.5,size = 20))
    if(exists("datat_up2")){
      volplot <- volplot+
      geom_label_repel(
                     aes(label= addup2),size=5,force = 15,show.legend = F,nudge_x=-bb*0.15,direction="y",
                     # box.padding=unit(0.35, "lines"), 
                     # #point.padding=unit(0.5, "lines"),
                     max.overlaps = getOption("ggrepel.max.overlaps", default = 100))+
      geom_label_repel(
                     aes(label= adddown2),size=5,force = 15,show.legend = F,nudge_x=bb*0.15,direction="y",
                     #box.padding=unit(0.35, "lines"),
                     #point.padding=unit(0.5, "lines"),
                     max.overlaps = getOption("ggrepel.max.overlaps", default = 100)
	    )
    }
    ggplotsave(plot = volplot,savepath=paste0(foldername,"/"),
               mapname =paste0("volcano-",gsub("/","_",compare[i])),
               width = width,
               height = height,
               imagetype = imagetype,
               family=fontfamily)
  }
}
if(this.path::is.main() & !is.null(whereami::thisfile_r())){
  options(warn=-1)
  suppressMessages(library("lmbio"))
  parser <- ArgumentParser()
  # 基本参数
  parser$add_argument("-i","--imagetype",default = c("pdf","png"), help = "图片格式")
  parser$add_argument("-fa","--fontfamily",default = "sans", help = "字体,默认为Arial")
  parser$add_argument("-wi","--width",default = 11, type= "double",help = "图片宽度，默认为11")
  parser$add_argument("-he","--height",default = 10, type= "double",help = "图片高度，默认为10")
  parser$add_argument("-d","--dpi",default = 300, type= "double",help = "分辨率，默认为300")
  
  # 此图参数
  parser$add_argument("-if","--inputfile", default = "预处理数据.xlsx",help = "输入数据名称，默认为预处理数据.xlsx")
  parser$add_argument("-t","--top", default = T, action = "store_true", help = "是否标记FCtop上下调蛋白/基因，默认为T")
  parser$add_argument("-f","--FC", default = 1.2, type= "double", help = "FC值，默认为1.2")
  parser$add_argument("-p","--pvalue", default = 0.05, type= "double", help = "pvalue值，默认为0.05")
  parser$add_argument("-x","--x", type= "double", help = "x轴范围，非必需")
  parser$add_argument("-n","--num", default = 5, type= "double", help = "标记top数目，默认为5")
  parser$add_argument("-st","--sigtype", default = "Gene Name",help = "标记展示形式，默认是基因名")
  args <- parser$parse_args()
  industvol <- do.call(industvolplot,args = args)
}
industvolplot()