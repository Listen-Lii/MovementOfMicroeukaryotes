# Fig S2 ABCDEF

library(ggsci)
library(vegan)
library(ggplot2)
library(tidyverse)
library(eoffice)
library(patchwork)
library(scales)
plot_theme = theme(plot.title = element_text(hjust = 0,size = 30),
                   axis.text = element_text(size = 30,color="black"),axis.title = element_text(size = 35,color="black"),
                   legend.text = element_text(size = 20,color="black"),legend.title = element_text(size = 20,color="black"),
                   axis.text.x = element_text(angle = 0, hjust = 0))+
  theme(axis.ticks = element_line(size = 0.5))+ 
  theme(axis.ticks.length = unit(0.8,"lines"))+ 
  theme(panel.grid=element_blank())+ 
  theme(legend.position="right")

## FigS2 AB
# v4
m=read.csv("Final data/parkv4.moss.csv",header=T,row.names=1)
s=read.csv("Final data/parkv4.soil.csv",header=T,row.names=1)
se=read.csv("Final data/parkv4.sediment.csv",header=T,row.names=1)
t=read.csv("Final data/parkv4.tree.csv",header=T,row.names=1)
w=read.csv("Final data/parkv4.water.csv",header=T,row.names=1)

# v9
m=read.table("Final data/parkv9.moss.txt",header=T,row.names=1,sep="\t")
s=read.table("Final data/parkv9.soil.txt",header=T,row.names=1,sep="\t")
se=read.table("Final data/parkv9.sediment.txt",header=T,row.names=1,sep="\t")
t=read.table("Final data/parkv9.tree.txt",header=T,row.names=1,sep="\t")
w=read.table("Final data/parkv9.water.txt",header=T,row.names=1,sep="\t")

v4 = list(m = ifelse(rowSums(m)>0,rownames(m),NULL),
          s = ifelse(rowSums(s)>0,rownames(s),NULL),
          se = ifelse(rowSums(se)>0,rownames(se),NULL),
          t = ifelse(rowSums(t)>0,rownames(t),NULL),
          w = ifelse(rowSums(w)>0,rownames(w),NULL))


v9 = list(m = ifelse(rowSums(m)>0,rownames(m),NULL),
          s = ifelse(rowSums(s)>0,rownames(s),NULL),
          se = ifelse(rowSums(se)>0,rownames(se),NULL),
          t = ifelse(rowSums(t)>0,rownames(t),NULL),
          w = ifelse(rowSums(w)>0,rownames(w),NULL))

library(VennDiagram)
library(RColorBrewer)
venn.diagram(v4,filename = "v4.venn.png",height = 5400,width = 5400,
             resolution = 600,imagetype = "png",units = "px",
             lwd = 2,lty = 1,fill = brewer.pal(length(v4),"Set2"),cex = 1.3,
             cat.cex = 2,alpha = 0.8,margin = 0.05,fontface = 2,
             cat.fontface = 2, print.mode = c("raw","percent"))
venn.diagram(v9,filename = "v9.venn.png",height = 5400,width = 5400,
             resolution = 600,imagetype = "png",units = "px",
             lwd = 2,lty = 1,fill = brewer.pal(length(v4),"Set2"),cex = 1.3,
             cat.cex = 2,alpha = 0.8,margin = 0.05,fontface = 2,
             cat.fontface = 2, print.mode = c("raw","percent"))

## Fig S2C
#v4
v4 <- read.table(file = "Final data/v4.txt", sep='\t',header=T,row.names = 1)
g.v4 = read.table(file = "Final data/group.v4.txt",sep="\t",header=T)

#v9
v9 <- read.table(file = "Final data/v9.txt", sep='\t',header=T,row.names = 1)
g.v9 = read.table(file = "Final data/group.v9.txt",sep="\t",header=T)


v4 = v4[,c(2,9:98)]
v9 = v9[,c(2,9:98)]

v4.pa = decostand(v4[,2:91],"pa")
v9.pa = decostand(v9[,2:91],"pa")

v4.pa$D = v4$D;v9.pa$D = v9$D

v4.r = v4.pa %>% group_by(D) %>% 
    summarise(across(everything(), ~ sum(., is.na(.), 0)))
v4.r$D[1] = "Unassigned"
v4.rs = data.frame(n.v4 =v4.r$D, v4 = rowSums(v4.r[,-1]))

v9.r = v9.pa %>% group_by(D) %>% 
  summarise(across(everything(), ~ sum(., is.na(.), 0)))
v9.r$D[1] = "Unassigned"
v9.rs = data.frame(n.v9 =v9.r$D, v9 = rowSums(v9.r[,-1]))

rs = cbind(v4.rs,v9 = v9.rs[,2])

fun.count = rs
link.count = fun.count %>% arrange(by=desc(n.v4)) %>%
  mutate(V4 = cumsum(v4),V9 = cumsum(v9))

fun.count = reshape2::melt(fun.count,value.name = "count",variable.name="group")

p1 = ggplot(fun.count,aes(x=group,y=count,fill=n.v4))+
  geom_bar(stat="identity",width=.5)+
  geom_segment(data=link.count,aes(x=1.25,xend=1.75,y=V4,yend=V9))+
  guides(fill=guide_legend(reverse=F)) +
  labs(x = NULL,y = "Taxa numbers")+
  scale_fill_manual(values = pal_npg("nrc", alpha = 1)(10))+
  scale_color_manual(values = pal_npg("nrc", alpha = 1)(10))+
  theme_bw()+plot_theme
p1


## Fig S2D
v4.sum = data.frame(D =v4[,1], sum = rowSums(v4[,-1]))
v9.sum = data.frame(D =v9[,1], sum = rowSums(v9[,-1]))

fun.v4.abun = v4.sum %>% group_by(D) %>% summarize(abundance.v4=sum(sum))
fun.v4.abun$D[1] = "Unassigned"
fun.v4.abun$t.V4 = "t.V4"
names(fun.v4.abun)[1] = "D.v4"
fun.v9.abun = v9.sum %>% group_by(D) %>% summarize(abundance.v9=sum(sum))
fun.v9.abun$D[1] = "Unassigned"
fun.v9.abun$t.V9 = "t.V9"
names(fun.v9.abun)[1] = "D.v9"
fun.abun = cbind(fun.v4.abun,fun.v9.abun)

link.abun = fun.abun %>% arrange(by=desc(D.v4)) %>%
  mutate(V4 = cumsum(abundance.v4),V9 = cumsum(abundance.v9))

fun.abun = reshape2::melt(fun.abun,value.name = "abun",variable.name="group")

p2 = ggplot(fun.abun,aes(x=group,y=abun,fill=D.v4))+
  geom_bar(stat="identity",width=.5)+
  geom_segment(data=link.abun,aes(x=1.25,xend=1.75,y=V4,yend=V9))+
  guides(fill=guide_legend(reverse=F)) +
  labs(x = NULL,y = "Abundance")+
  scale_fill_manual(values = pal_npg("nrc", alpha = 1)(10))+
  scale_color_manual(values = pal_npg("nrc", alpha = 1)(10))+
  theme_bw()+plot_theme
p2

getwd()
ggsave("1.all.count.pdf",plot = p1,width = 300,height = 150,units = "mm")
ggsave("2.all.abundance.pdf",plot = p2,width = 300,height = 150,units = "mm")


## Fig S2EF
v4 <- read.table(file = "Final data/v4.domain.txt", sep='\t',header=T,row.names = 1)
v9 <- read.table(file = "Final data/v9.domain.txt", sep='\t',header=T,row.names = 1)
g.v4 = read.table(file = "Final data/group.v4.txt",sep="\t",header=T)
g.v9 = read.table(file = "Final data/group.v9.txt",sep="\t",header=T)

g.v4 = g.v4[match(colnames(v4),g.v4$sample),]
m.v4 = rowSums(v4[,g.v4$habitat == "Moss"])
se.v4 = rowSums(v4[,g.v4$habitat == "Sediment"])
so.v4 = rowSums(v4[,g.v4$habitat == "Soil"])
t.v4 = rowSums(v4[,g.v4$habitat == "Tree hole"])
w.v4 = rowSums(v4[,g.v4$habitat == "Water"])
res.v4 = as.data.frame(cbind(m.v4,se.v4,so.v4,t.v4,w.v4))
res.v4$d = rownames(res.v4)
p.v4 = res.v4 %>% pivot_longer(cols=-d)

g.v9 = g.v9[match(colnames(v9),g.v9$sample),]
m.v9 = rowSums(v9[,g.v9$habitat == "Moss"])
se.v9 = rowSums(v9[,g.v9$habitat == "Sediment"])
so.v9 = rowSums(v9[,g.v9$habitat == "Soil"])
t.v9 = rowSums(v9[,g.v9$habitat == "Tree hole"])
w.v9 = rowSums(v9[,g.v9$habitat == "Water"])
res.v9 = as.data.frame(cbind(m.v9,se.v9,so.v9,t.v9,w.v9))
res.v9$d = rownames(res.v9)
p.v9 = res.v9 %>% pivot_longer(cols=-d)

lab = c("Moss","Sediment","Soil","Tree hole","Water")

p3 = ggplot(p.v4, aes(name,value,fill=d)) +
  geom_bar(stat="identity",width=.5)+
  guides(fill=guide_legend(reverse=F)) +
  labs(x = NULL,y = "Taxa percentage(%)")+
  scale_fill_manual(values = pal_npg("nrc", alpha = 1)(10))+
  scale_color_manual(values = pal_npg("nrc", alpha = 1)(10))+
  scale_x_discrete(labels  = lab)+  
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5,size = 15),
        axis.text = element_text(size = 30),axis.title = element_text(size = 35),
        legend.text = element_text(size = 20),legend.title = element_text(size = 25),
        legend.position="NULL",axis.text.x = element_text(angle = 0, hjust = 0.5))+
  theme(axis.ticks = element_line(size = 0.4))+  
  theme(axis.ticks.length = unit(0.6,"lines"))+  
  theme(panel.grid=element_blank())+  
  theme(legend.position="right")
p3

p4 = ggplot(p.v9, aes(name,value,fill=d)) +
  geom_bar(stat="identity",width=.5)+
  guides(fill=guide_legend(reverse=F)) +
  labs(x = NULL,y = "Taxa percentage(%)")+
  scale_fill_manual(values = pal_npg("nrc", alpha = 1)(10))+
  scale_color_manual(values = pal_npg("nrc", alpha = 1)(10))+
  scale_x_discrete(labels  = lab)+  
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5,size = 15),
        axis.text = element_text(size = 30),axis.title = element_text(size = 35),
        legend.text = element_text(size = 20),legend.title = element_text(size = 25),
        legend.position="NULL",axis.text.x = element_text(angle = 0, hjust = 0.5))+
  theme(axis.ticks = element_line(size = 0.4))+  
  theme(axis.ticks.length = unit(0.6,"lines"))+  
  theme(panel.grid=element_blank())+  
  theme(legend.position="right")
p4

getwd()
ggsave("3.v4.domain.pdf",plot = p3,width = 400,height = 150,units = "mm")
ggsave("4.v9.domain.pdf",plot = p4,width = 400,height = 150,units = "mm")

