# FigS7A
library(tidyverse)
library(ggsignif)

feast = read.table(file = "Final data/FigS7A.txt",sep='\t',header=T)
colnames(feast)
feast.p = feast %>% pivot_longer(cols = -c(m,Region))

compared = list(c("V4","V9"))
feast.p$g = paste(feast.p$name,feast.p$Region,sep=".")
p = ggplot(data = feast.p,aes(x=name,y=value,fill=g)) +
    geom_boxplot(alpha=0.75,outlier.size = 1, 
                 outlier.color = "grey", 
                 outlier.fill = "grey")+
  labs(x = "Source-sink",y = "Explained percentage(%)")
  
p

p.values <- sapply(split(feast.p, feast.p$name), function(x){wilcox.test(value~Region, x)$p.value})
labels <- symnum(p.values, corr = FALSE, cutpoints = c(0,  .001,.01,.05, 1), symbols = c("***","**","*","n.s."))
y.values <- sapply(split(feast.p, feast.p$name), function(x){max(sapply(split(x, x$Region), function(xx){boxplot(x$value, plot=F)$stats[5, ]}))})+2

p <- p + geom_signif(
                     y_position = y.values, 
                     xmin = levels(factor(feast.p$name)), 
                     xmax = levels(factor(feast.p$name)), 
                     annotations = labels,tip_length = 0,
                     textsize = 9,color="tomato3")+
  scale_fill_manual(values = c(rep(c("#339933","#84c184"),4), #moss
                               rep(c("#990000","#c16666"),4), #sediment
                               rep(c("#CC9900","#e0c166"),4), #soil
                               rep(c("#663366","#a384a3"),4), #tree
                               rep(c("#99CCFF","#c1e0ff"),4)))+ # water
  scale_color_manual(values = c(rep(c("#339933","#84c184"),4), #moss
                                rep(c("#990000","#c16666"),4), #sediment
                                rep(c("#CC9900","#e0c166"),4), #soil
                                rep(c("#663366","#a384a3"),4), #tree
                                rep(c("#99CCFF","#c1e0ff"),4)))+ # water
  theme_bw()+
  theme(axis.text = element_text(size = 30),axis.title = element_text(size = 35),
        legend.text = element_text(size = 30),legend.title = element_text(size = 35),
        axis.text.x = element_text(angle = 45, hjust = 1))+
  theme(axis.ticks = element_line(size = 0.4))+  
  theme(axis.ticks.length = unit(0.6,"lines"))+  
  theme(panel.grid=element_blank())+  
  theme(legend.position="NULL")


ggsave("figs7a.pdf",plot = p,width = 600,height = 300,units = "mm")
getwd()
