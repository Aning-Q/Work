library(lmbio)
library(ggplot2) # 绘图
sheet <- getsheetname("circle.xlsx")
k <- 1
data <- readdata("circle.xlsx",k)
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

dd <- rbind(
      #data.frame(range=df$range,group="A",per=1,pernum=1,label="A"),
      data.frame(range=df$range,group=2,per=df$per1,pernum = df$per.y1,label=df$label1),
      data.frame(range=df$range,group=3,per=df$per2,pernum=df$per.y2,label=df$label2))

dd$range <- factor(dd$range, levels=unique(dd$range))
#dd$group <- factor(dd$group,levels = c('A',name[1],name[2]))
col1 <- c("#7f96ff","#f2b5d4","#3dccc7","#e4c1f9","#ffa69e","#ffd6a5")
col <- c("#EEBD6C","#D94E32","#834127","#EDDDC2","#4DAB90","#8EB79E")
p<- ggplot(dd) +
  geom_bar(aes("1", 1/nrow(dd)), 
           stat = 'identity', width = .8, color = 'white',fill = 'white') +
  geom_text(aes(0.65, 0.65, 
                label = paste0("内圈:",name[1],"\n\n","外圈:",name[2])),
            size =5, color = 'black')+
  geom_bar(aes(group, 
               per, 
               fill = range),
           stat = 'identity', width = 0.6,color = 'white')+
  geom_text(aes(group, as.numeric(pernum), 
                label = label),
            size =3, color = 'black') +
  
  scale_y_continuous(labels = scales::percent) +
  coord_polar(theta = "y") + # 转换坐标轴
  theme_void() +
  scale_fill_manual(values = col)+
  #labs(tag = paste0("内圈:",name[1],"\n\n","外圈:",name[1])) +
  theme(plot.margin = unit(c(-0.5, 1, -0.5, -2), "cm"),
        legend.title = element_blank(),
        legend.text = element_text(size = 10))
savexlsx(df[c("range",name[1],name[2],"per1","per2")],paste0("Circle_",sheet[k],".xlsx"))
ggplotsave(p,  
           mapname = paste0("Circle_",sheet[k]),  
           imagetype = c("png","pdf"),  
           family = "sans",  
           height = 8,  
           width = 10)
