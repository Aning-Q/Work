massplot <- function(savepath = "./",inputpath="./",inputfile = "iRT-Kit_QcData.xls",
                    imagetype = c("pdf","png") ,height = 6,width = 8,
                    dpi=300,fontfamily="sans",...){
  pacman::p_load(ggplot2)
  data <- readdata(paste0(inputpath,inputfile))
  data <- subset(data,data$PPMMS2MassShift!="NaN")
  df1 <- aggregate(data$PPMMS2MassShift, by=list(data$Run),FUN = quantile,
                   probs = c(0,0.25,0.5, 0.75,1))
  df1$mean <- aggregate(data$PPMMS2MassShift,by=list(data$Run),mean)
  df2 <- data.frame(Group=df1$Group.1,Q1=df1$x[,1],Q2=df1$x[,2],
                    median=df1$x[,3],Q3=df1$x[,4],Q4=df1$x[,5],
                    average=df1$mean[,2])
  p <- ggplot(df2)+
    geom_point(aes(x=Group,y=average),shape = 5, color = "red")+
    geom_ribbon(aes(x=1:nrow(df2),ymin=Q3, ymax=Q2), fill="grey",alpha=0.4) +
    geom_point(aes(x=Group,y=average),shape = 5, color = "red")+
    geom_line(aes(x=Group,y=median,group = 1),size=0.25)+
    geom_errorbar(aes(x=Group,ymin=Q4, ymax=Q1),width=0.1,linewidth=0.25)+
    theme_bw()+
    theme(panel.grid=element_blank(),
          plot.margin = unit(c(0.5, 0.8, 0.1, 0.8), "cm"))+
    labs(x='',y='PPMMS2MassShift')+
    scale_y_continuous(breaks = seq(floor(min(data$PPMMS2MassShift)), 
                                    ceiling(max(data$PPMMS2MassShift)), by = 1))
  if(length(df2$Group)>100){p <- p+theme(axis.text.x = element_text(angle = 60,size = 8,hjust = 1))}
  ggplotsave(plot = p,savepath=savepath,
             mapname = 'iRT-all-mass accuracy',
             width = 15,
             height = height,
             imagetype = imagetype,
             family=fontfamily,...)
  
}
if(this.path::is.main() & !is.null(whereami::thisfile_r())){
  options(warn=-1)
  suppressMessages(library("lmbio"))
  parser <- ArgumentParser()
  # 基本参数
  parser$add_argument("-i","--imagetype",default = c("png","pdf"), help = "图片格式")
  parser$add_argument("-fa","--fontfamily",default = "sans", help = "字体,默认为Arial")
  parser$add_argument("-wi","--width",default = 8, type= "double",help = "图片宽度")
  parser$add_argument("-he","--height",default = 6, type= "double",help = "图片高度")
  parser$add_argument("-d","--dpi",default = 300, type= "double",help = "分辨率")
  
  # 此图参数
  parser$add_argument("-s","--savepath",default = "./", help = "结果保存路径,默认./")
  parser$add_argument("-ip","--inputpath", default="./", help="分析路径")
  parser$add_argument("-if","--inputfile",default = "iRT-Kit_QcData.xls", help = "输入数据文件")
  args <- parser$parse_args()
  mass_plot <- do.call(massplot,args = args)
}
