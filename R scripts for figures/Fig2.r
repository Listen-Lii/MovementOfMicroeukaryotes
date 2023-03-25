## Fig2

library(vegan)
# Fig2AB NMDS 

otu.18.v4 = read.table(file = "Final data/18SV4.norm.txt",sep="\t",row.names=1,header=T,check.names=F)
g.18.v4 = read.table(file = "Final data/group.v4.txt",sep="\t",row.names=1,header=T,check.names=F)

otu.18.v9 = read.csv(file = "Final data/18SV9.norm.csv",row.names=1,header=T,check.names=F)
g.18.v9 = read.table(file = "Final data/group.v9.txt",sep="\t",row.names=1,header=T,check.names=F)

# 1
otu.18.v4 = decostand(otu.18.v4,method = "hellinger")
x = otu.18.v4
group = g.18.v4

# 2
otu.18.v9 = decostand(otu.18.v9,method = "hellinger")
x = otu.18.v9
group = g.18.v9

# v4 and v9 run separately
nmds <- function(x,group,dis="bray",try=100){

  if (dis=="bray"){
    mds<-metaMDS(t(x), distance="bray", k=2, trymax=100) ;mds
  } else if(dis=="jaccard"){
    x = decostand(x,"pa")
    mds<-metaMDS(t(x), distance="jaccard", k=2, trymax=100) ;mds
  }
  dis <<- dis 
  out = mds$points
  out12 = out[,1:2]
  stress = round(mds$stress,4);stress
  stress<<-stress
  
  rowgroup = rownames(group)
  rowsam = rownames(out12)
  mat = match(rowgroup,rowsam)
  sam = out12[mat,] 
  
  data.plot = cbind(sam,group)
}
data.plot = nmds(x,group,dis="bray",try=100)

plot_theme = theme(panel.background=element_blank(),
                   panel.grid=element_blank(),
                   axis.line.x=element_line(size=.5, colour="black"),
                   axis.line.y=element_line(size=.5, colour="black"),
                   axis.ticks=element_line(color="black"),
                   axis.text=element_text(color="black", size=10),
                   legend.position="right",
                   legend.background=element_blank(),
                   legend.key=element_blank(),
                   legend.text= element_text(size=20),
                   text=element_text(family="sans", size=20)
)

p1 = ggplot(data.plot,aes(MDS1,MDS2))+
  geom_point(aes(colour = habitat),alpha=1,size = 2)+
  xlab("MDS1")+ylab("MDS2")+ labs(title = paste(dis,"\nstress =",stress,sep=" "))+
  plot_theme + theme(plot.title=element_text(hjust=0.5))+theme_bw()+
  theme(axis.text= element_text(size=16, color="black", family  = "serif", face= "bold", vjust=0.5, hjust=0.5))+
  theme(axis.title = element_text(size=16, color="black", family  = "serif",face= "bold", vjust=0.5, hjust=0.5))+
  theme(legend.text = element_text(colour = 'black', size = 16,  family  = "serif",face = 'bold'))+
  theme(legend.title=element_blank())+
  theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())
print(p1)


# Fig2C
library(tidyverse)
library(NST)

# v4
m4=read.csv("Final data/parkv4.moss.csv",header=T,row.names=1) %>%
  t() %>% vegdist(method = "bray") %>% dist.3col()
s4=read.csv("Final data/parkv4.soil.csv",header=T,row.names=1) %>%
  t() %>% vegdist(method = "bray") %>% dist.3col()
se4=read.csv("Final data/parkv4.sediment.csv",header=T,row.names=1) %>%
  t() %>% vegdist(method = "bray") %>% dist.3col()
t4=read.csv("Final data/parkv4.tree.csv",header=T,row.names=1) %>%
  t() %>% vegdist(method = "bray") %>% dist.3col()
w4=read.csv("Final data/parkv4.water.csv",header=T,row.names=1) %>%
  t() %>% vegdist(method = "bray") %>% dist.3col()

# v9
m9=read.table("Final data/parkv9.moss.txt",header=T,row.names=1,sep="\t") %>%
  t() %>% vegdist(method = "bray") %>% dist.3col()
s9=read.table("Final data/parkv9.soil.txt",header=T,row.names=1,sep="\t") %>%
  t() %>% vegdist(method = "bray") %>% dist.3col()
se9=read.table("Final data/parkv9.sediment.txt",header=T,row.names=1,sep="\t") %>%
  t() %>% vegdist(method = "bray") %>% dist.3col()
t9=read.table("Final data/parkv9.tree.txt",header=T,row.names=1,sep="\t") %>%
  t() %>% vegdist(method = "bray") %>% dist.3col()
w9=read.table("Final data/parkv9.water.txt",header=T,row.names=1,sep="\t") %>%
  t() %>% vegdist(method = "bray") %>% dist.3col()

m = cbind(m4[,1:2],v4 = m4$dis,v9 = m9$dis)
s = cbind(s4[,1:2],v4 = s4$dis,v9 = s9$dis)
se = cbind(se4[,1:2],v4 = se4$dis,v9 = se9$dis)
t = cbind(t4[,1:2],v4 = t4$dis,v9 = t9$dis)
w = cbind(w4[,1:2],v4 = w4$dis,v9 = w9$dis)

theme = theme(plot.title = element_text(hjust = 0.5,size = 15),
        axis.text = element_text(size = 48),axis.title = element_text(size = 65),
        legend.text = element_text(size = 48),legend.title = element_text(size = 65),
        legend.position="NULL")+
  theme(axis.ticks = element_line(size = 0.2))+  
  theme(axis.ticks.length = unit(1.8,"lines"))+  
  theme(panel.grid=element_blank())+  
  theme(legend.position="NULL")

(p.m = ggplot(data=m,aes(x=v4,y=v9))+
      geom_point(color = "#00D200",size=2)+
      geom_abline(intercept = 0, slope = 1,size=0.2)+
      labs(x = "Dissimilarity (V4)",y = "Dissimilarity (V9)")+
      scale_y_continuous(limits = c(0,1),breaks=seq(0,1,0.2))+
      scale_x_continuous(limits = c(0,1),breaks=seq(0,1,0.2))+
      theme_bw()+theme)
(p.s = ggplot(data=s,aes(x=v4,y=v9))+
    geom_point(color = "#F1C427",size=2)+
    geom_abline(intercept = 0, slope = 1,size=0.2)+
    labs(x = "Dissimilarity (V4)",y = "Dissimilarity (V9)")+
    scale_y_continuous(limits = c(0,1),breaks=seq(0,1,0.2))+
    scale_x_continuous(limits = c(0,1),breaks=seq(0,1,0.2))+
    theme_bw()+theme)
(p.se = ggplot(data=se,aes(x=v4,y=v9))+
    geom_point(color = "#BD5F00",size=2)+
    geom_abline(intercept = 0, slope = 1,size=0.2)+
    labs(x = "Dissimilarity (V4)",y = "Dissimilarity (V9)")+
    scale_y_continuous(limits = c(0,1),breaks=seq(0,1,0.2))+
    scale_x_continuous(limits = c(0,1),breaks=seq(0,1,0.2))+
    theme_bw()+theme)
(p.t = ggplot(data=t,aes(x=v4,y=v9))+
    geom_point(color = "#808080",size=2)+
    geom_abline(intercept = 0, slope = 1,size=0.2)+
    labs(x = "Dissimilarity (V4)",y = "Dissimilarity (V9)")+
    scale_y_continuous(limits = c(0,1),breaks=seq(0,1,0.2))+
    scale_x_continuous(limits = c(0,1),breaks=seq(0,1,0.2))+
    theme_bw()+theme)
(p.w = ggplot(data=w,aes(x=v4,y=v9))+
    geom_point(color = "#0080FF",size=2)+
    geom_abline(intercept = 0, slope = 1,size=0.2)+
    labs(x = "Dissimilarity (V4)",y = "Dissimilarity (V9)")+
    scale_y_continuous(limits = c(0,1),breaks=seq(0,1,0.2))+
    scale_x_continuous(limits = c(0,1),breaks=seq(0,1,0.2))+
    theme_bw()+theme)

ggsave("s.pdf",plot = p.s,width = 200,height = 200,units = "mm")
ggsave("se.pdf",plot = p.se,width = 200,height = 200,units = "mm")
ggsave("m.pdf",plot = p.m,width = 200,height = 200,units = "mm")
ggsave("w.pdf",plot = p.w,width = 200,height = 200,units = "mm")
ggsave("t.pdf",plot = p.t,width = 200,height = 200,units = "mm")

