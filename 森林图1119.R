library(readxl)
forest1119<-read_excel("D:/学习工作/工作/森林图1119.xlsx",na="NA")
forest1119$circle=as.character(forest1119$circle)
forest1119$I[forest1119$Order == 19] <- "NA"
forest1119$I[forest1119$Order == 29] <- "NA"

library(ggplot2)
library(ggpubr)
c1  <-ggplot(forest1119) +
  geom_hline(yintercept = 66, color="black")+
  theme_linedraw() +geom_text(aes(label =Variable, x = hjust, y=Order, fontface=font), size=3.5,hjust=0, family="serif") +
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    panel.border = element_blank(),
    plot.margin= grid::unit(c(0, 0, 0, 0), "in")
  ) +
  ylim(0, 67)  +
  xlim(0,1) +
  xlab(NULL)  +
  ylab(NULL)  +
  theme(plot.title = element_text(size =12, face = "bold" ,family="serif"))
c1

c2  <-ggplot(forest1119) +
  geom_hline(yintercept = 66, color="black")+
  theme_linedraw() +
  
  geom_text(aes(label =Stu, x = hjust, y=Order, fontface=font), size=3.5,hjust=0, family="serif") +
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    panel.border = element_blank(),
    plot.margin= grid::unit(c(0, 0, 0, 0), "in")
  ) +
  ylim(0, 67)  +
  xlim(0,1) +
  xlab(NULL)  +
  ylab(NULL)  +
  theme(plot.title = element_text(size =12, face = "bold" ,family="serif"))
c2

c3  <-ggplot(forest1119) +
  theme_linedraw() +
  geom_hline(yintercept = 66, color="black")+
  
  geom_text(aes(label =Pa, x = hjust, y=Order, fontface=font), size=3.5,hjust=0, family="serif") +
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    panel.border = element_blank(),
    plot.margin= grid::unit(c(0, 0, 0, 0), "in")
  ) +
  ylim(0, 67)  +
  xlim(0,1) +
  xlab(NULL)  +
  ylab(NULL)  +
  theme(plot.title = element_text(size =12, face = "bold" ,family="serif"))
c3

c4  <-ggplot(forest1119) +
  theme_linedraw() +
  geom_hline(yintercept = 66, color="black")+
  geom_text(aes(label =P, x = hjust, y=Order, fontface=font), size=3.5,hjust=0, family="serif") +
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    panel.border = element_blank(),
    plot.margin= grid::unit(c(0, 0, 0, 0), "in")
  ) +
  ylim(0, 67)  +
  xlim(0,1) +
  xlab(NULL)  +
  ylab(NULL)  +
  theme(plot.title = element_text(size =12, face = "bold" ,family="serif"))
c4

c5 <-  ggplot(forest1119, aes(x =Prevalence, xmin = LowerCI, xmax = UpperCI, y = Order, shape=circle)) +
  theme_bw() +
  

  geom_segment(aes(x = 0 , y = 66, xend =0, yend = -Inf),size=0.3) +
  geom_segment(aes(x = 0.425 , y = 66, xend = 0.425, yend = 1),linetype="dashed",colour="grey")+

  geom_hline(yintercept = 66, color="black")+
  #geom_vline(xintercept = 0.425, linetype="dashed",color="grey60") +
  #geom_vline(xintercept = 0, color="black")+
  geom_errorbarh(height = 0, color = 'black') + 
  
  geom_point(fill="black", color="black", size=2.5) +
  scale_shape_manual(values = c(15,23)) +
  theme(
    axis.text.y = element_blank(),
    axis.title.y = element_blank(),
    axis.ticks.y = element_blank(),
    legend.position="none",
    panel.grid=element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.border = element_blank(),
    axis.text.x=element_text(size=12, colour="black", family="serif"),
    axis.line.x=element_line(color="black"), 
    plot.margin= grid::unit(c(0, 0, 0, 0), "in")
  )  +
  xlim(0,1)+
  ylim(0, 67) +
  xlab(NULL)  +
  ylab(NULL) #+
#scale_x_continuous(limits=c(0, round(max(result2$CI95, na.rm = TRUE))), breaks=seq(0,round(max(result2$CI95, na.rm = TRUE)),0.5))  
c5

c6  <-ggplot(forest1119) +
  theme_linedraw() +
  geom_hline(yintercept = 66, color="black")+
  geom_text(aes(label =I, x = hjust, y=Order, fontface=font), size=3.5,hjust=0, family="serif") +
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    panel.border = element_blank(),
    plot.margin= grid::unit(c(0, 0, 0, 0), "in")
  ) +
  ylim(0, 67)  +
  xlim(0,1) +
  xlab(NULL)  +
  ylab(NULL)  +
  theme(plot.title = element_text(size =12, face = "bold" ,family="serif"))
c6

library(patchwork)
forest_plot1 <- c1+ c3 + c4 + c5+  plot_layout(ncol=4, widths=c(0.5,0.15,0.21,0.6))
#forest_plot1 <- c1 + c2 + c3 + c4 + c5 + c6 +  plot_layout(ncol=6, widths=c(0.5,0.13,0.15,0.21,0.6,0.15))
forest_plot1 
ggsave(forest_plot1 ,
       width = 14,height =10,
       filename =  paste0("forest.pdf"))
