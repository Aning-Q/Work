library(lmbio)
library(ggplot2) # 绘图
sheet <- getsheetname("Molecular_weight.xlsx")
k <- 2
data <- readdata("Molecular_weight.xlsx",k)
df <- data
name <- colnames(data)[-1]
for (i in 1:length(name)) {
  df1 <-  data[c('range',name[i])]
  #df1 <-  aggregate(df[name[i]], by = list(df$range), FUN = sum)
  df1[paste0('per',i)]  <-  df1[name[i]] / sum(df1[name[i]])
  df1[paste0('per.y',i)] <- 0
  for (j in seq(nrow(df1), 1)) {
    if (j == nrow(df1)) {
      df1[paste0('per.y',i)][j,] <- df1[paste0('per',i)][j,] / 2
    }else{
      df1[paste0('per.y',i)][j,] <- sum(df1[paste0('per',i)][(j + 1):nrow(df1),]) + df1[paste0('per',i)][j,] / 2
    }
  }
  df1[paste0('label',i)]  <-  paste(round(df1[,paste0('per',i)]*100, 2),'%', sep = '')
  #df <- merge(df, df1[,c(1,3,4,5)], by.x = 'range', by.y = 'range')
  df <- cbind(df, df1[,c(3,4,5)])
}
df$x <- name[1]
df$y <- name[2]
df$z <- "A"
df$range <- factor(df$range, levels=df$range)
col1 <- c("#7f96ff","#f2b5d4","#3dccc7","#e4c1f9","#ffa69e","#ffd6a5")
col <- c("#EEBD6C","#D94E32","#834127","#EDDDC2","#4DAB90","#8EB79E")
p<- ggplot(df) +
  geom_bar(aes(z, per2), 
           stat = 'identity', width = .8, color = 'white',fill = 'white') +
  geom_text(aes(0.65, 0.65, 
                label = paste0("内圈:",name[1],"\n\n","外圈:",name[2])),
            size =5, color = 'black')+
  geom_bar(aes(x, 
               per1, 
               fill = range),
           stat = 'identity', width = 0.6,color = 'white')+
  geom_text(aes(2, as.numeric(per.y1), 
                label = label1),
            size =3, color = 'black')+
  geom_bar(aes(y, per2, fill = range), 
           stat = 'identity', width = 0.6, color = 'white') +
  # 添加标签
  geom_text(aes(3, as.numeric(per.y2),label = label2),
            size = 3, color = 'black') +
  
  scale_y_continuous(labels = scales::percent) +
  coord_polar(theta = "y") + # 转换坐标轴
  theme_void() +
  scale_fill_manual(values = col)+
  #labs(tag = paste0("内圈:",name[1],"\n\n","外圈:",name[1])) +
  theme(plot.margin = unit(c(-0.5, 1, -0.5, -2), "cm"),
        legend.title = element_blank(),
        legend.text = element_text(size = 10))
savexlsx(df[c("range",name[1],name[2],"per1","label1","per2","label2")],paste0("Circle_",sheet[k],".xlsx"))
ggplotsave(p,  
           mapname = paste0("Circle_",sheet[k]),  
           imagetype = c("png","pdf"),  
           family = "sans",  
           height = 8,  
           width = 10)
