## FigS1 rarefaction curve

library(ggplot2)
library(reshape2)
library(ggalt)
library(vegan)
library(patchwork)

# v4
otu = read.table(file = "Final data/18SV4.norm.txt",sep='\t',header=T,row.names = 1)
g = read.table(file = "Final data/group.v4.txt",sep="\t",header=T)

#v9
otu = read.csv(file = "Final data/18SV9.norm.csv",header=T,row.names = 1)
g = read.table(file = "Final data/group.v9.txt",sep="\t",header=T)

## 1 single habitat
x = t(otu)


## 2 five habitats-----
nn = unique(g$habitat)
otu.sum = c()
for (i in 1:5){ # i = 1
  mat = match(g[g$habitat==nn[i],]$sample,colnames(otu))
  otu.sum = cbind(otu.sum,rowSums(otu[,mat]))
}
colnames(otu.sum) = nn
otu.sum = as.data.frame(otu.sum)
x = t(otu.sum)
##-----


step = 1000 # single habitat
# step = 50000 # five habitats
  read = max(rowSums(x))
  all.alpha = c()
  for (i in seq(1,read,step)){  
    rare.otu = rrarefy(x,i)  
    
    alpha.div <- function(x){
      result <- rowSums(x > 0)
    }
    rare.alpha <- alpha.div(rare.otu)
    all.alpha <- rbind(all.alpha,rare.alpha)
    print(i)
  }
  all.alpha <-  as.data.frame(all.alpha)
  all.alpha$id <- seq(1,read,step)
  rare.alpha <- all.alpha
  
  gg.rare = melt(rare.alpha,
                 id.vars = "id")
  
  #- do not do this for five habitats
  mat = match(gg.rare$variable,g$sample)
  rare = cbind(gg.rare,g[mat,])
  #-
  
  p1 = ggplot(rare,aes(id,value,group = sample,color = habitat))+
    stat_smooth(geom="line",alpha=.5,size=1.2)+
    labs(x = 'Sequence number', y = "Richness", color = NULL)+
    scale_fill_manual(values = c("#339933","#990000","#CC9900","#663366","#99CCFF"))+
    scale_color_manual(values = c("#339933","#990000","#CC9900","#663366","#99CCFF"))+
    theme_bw()+
    theme(plot.title = element_text(hjust = 0.5,size = 15),
          axis.text = element_text(size = 30,colour = "black"),axis.title = element_text(size = 35),
          legend.text = element_text(size = 30,colour = "black"),legend.title = element_text(size = 35))+
    theme(axis.ticks = element_line(size = 0.6))+ 
    theme(axis.ticks.length = unit(0.6,"lines"))+  
    theme(panel.grid=element_blank()) 
p1

p2 = ggplot(gg.rare,aes(id,value,group = variable,color = variable))+
  stat_smooth(geom="line",alpha=.5,size=1.2)+
  labs(x = 'Sequence number', y = "Richness", color = NULL)+
  scale_fill_manual(values = c("#339933","#990000","#CC9900","#663366","#99CCFF"))+
  scale_color_manual(values = c("#339933","#990000","#CC9900","#663366","#99CCFF"))+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5,size = 15),
        axis.text = element_text(size = 30,colour = "black"),axis.title = element_text(size = 35),
        legend.text = element_text(size = 30,colour = "black"),legend.title = element_text(size = 35))+
  theme(axis.ticks = element_line(size = 0.6))+ 
  theme(axis.ticks.length = unit(0.6,"lines"))+ 
  theme(panel.grid=element_blank()) 
p2

#-
v4.p = p1
v4.rare = rare

v4.sum.p = p2
v4.rare.sum = rare

v9.p = p1
v9.rare = rare

v9.sum.p = p2
v9.rare.sum = rare
#-

(v4.p | v4.sum.p) / (v9.p | v9.sum.p)

ggsave("v4.p.pdf",plot = v4.p,width = 500,height = 300,units = "mm")
ggsave("v4.sum.p.pdf",plot = v4.sum.p,width = 500,height = 300,units = "mm")
ggsave("v9.p.pdf",plot = v9.p,width = 500,height = 300,units = "mm")
ggsave("v9.sum.p.pdf",plot = v9.sum.p,width = 500,height = 300,units = "mm")
