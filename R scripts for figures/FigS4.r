## FigS4 dissimilarity and betapart

library(tidyverse)
library(ggsignif)
library(vegan)
library(NST)

# FigS4 AB dissimilarity
a = read.csv(file = "Final data/parkv4.moss.csv",header=T,row.names = 1)
a.dis = vegdist(t(a),method = "bray")
m = dist.3col(a.dis)

a = read.csv(file = "Final data/parkv4.sediment.csv",header=T,row.names = 1)
a.dis = vegdist(t(a),method = "bray")
se = dist.3col(a.dis)

a = read.csv(file = "Final data/parkv4.soil.csv",header=T,row.names = 1)
a.dis = vegdist(t(a),method = "bray")
s = dist.3col(a.dis)

a = read.csv(file = "Final data/parkv4.tree.csv",header=T,row.names = 1)
a.dis = vegdist(t(a),method = "bray")
t = dist.3col(a.dis)

a = read.csv(file = "Final data/parkv4.water.csv",header=T,row.names = 1)
a.dis = vegdist(t(a),method = "bray")
w = dist.3col(a.dis)

v4.r = cbind(m$dis,se$dis,s$dis,t$dis,w$dis)
colnames(v4.r) = c("Moss","Sediment","Soil","Tree hole","Water")
v4.r = as.data.frame(v4.r)

##
a = read.table(file = "Final data/parkv9.moss.txt",sep="\t",header=T,row.names = 1)
a.dis = vegdist(t(a),method = "bray")
m = dist.3col(a.dis)

a = read.table(file = "Final data/parkv9.sediment.txt",sep="\t",header=T,row.names = 1)
a.dis = vegdist(t(a),method = "bray")
se = dist.3col(a.dis)

a = read.table(file = "Final data/parkv9.soil.txt",sep="\t",header=T,row.names = 1)
a.dis = vegdist(t(a),method = "bray")
s = dist.3col(a.dis)

a = read.table(file = "Final data/parkv9.tree.txt",sep="\t",header=T,row.names = 1)
a.dis = vegdist(t(a),method = "bray")
t = dist.3col(a.dis)

a = read.table(file = "Final data/parkv9.water.txt",sep="\t",header=T,row.names = 1)
a.dis = vegdist(t(a),method = "bray")
w = dist.3col(a.dis)

v9.r = cbind(m$dis,se$dis,s$dis,t$dis,w$dis)
colnames(v9.r) = c("Moss","Sediment","Soil","Tree hole","Water")
v9.r = as.data.frame(v9.r)

v4.r$Region = "V4";v9.r$Region = "V9"


all.r = rbind(v4.r,v9.r)

all.r.p = all.r %>% pivot_longer(cols = -Region)

p3=ggplot(data = all.r.p, aes(x = name, y = value,fill = Region)) + 
  geom_boxplot(outlier.size = 1, 
               outlier.color = "grey", 
               outlier.fill = "grey")+ 
  labs(x="Habitat", y="Dissimilarity") +
  scale_fill_manual(values = c("#99CCFF","#339933"))+
  scale_color_manual(values = c("#99CCFF","#339933"))+theme_bw()+
  theme(plot.title = element_text(hjust = 0.5,size = 15),
        axis.text = element_text(size = 18),axis.title = element_text(size = 18),
        legend.text = element_text(size = 18),legend.title = element_text(size = 20))
p3

p.v4=ggplot(data = v4.r.p, aes(x = name, y = value,fill = name,color = name)) + 
  geom_boxplot(alpha=.5,outlier.alpha=.3)+ 
  labs(x="Habitat", y="Bray-Curtis Dissimilarity") +
   scale_fill_manual(values = c("#66CC33","#996600","#FFCC33","#666666","#0099FF"))+
   scale_color_manual(values = c("#66CC33","#996600","#FFCC33","#666666","#0099FF"))+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5,size = 25),
        axis.text = element_text(size = 30),axis.title = element_text(size = 35),
        legend.text = element_text(size = 30),legend.title = element_text(size = 35),
        legend.position = "none")+
  scale_y_continuous(limits = c(0,1),breaks = seq(0,1,0.25))
p.v4

v9.r.p = v9.r %>% pivot_longer(cols = -Region)
p.v9=ggplot(data = v9.r.p, aes(x = name, y = value,fill = name,color = name)) + 
  geom_boxplot(alpha=.5,outlier.alpha=.3)+ 
  labs(x="Habitat", y="Bray-Curtis Dissimilarity") +
   scale_fill_manual(values = c("#66CC33","#996600","#FFCC33","#666666","#0099FF"))+
   scale_color_manual(values = c("#66CC33","#996600","#FFCC33","#666666","#0099FF"))+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5,size = 25),
        axis.text = element_text(size = 30),axis.title = element_text(size = 35),
        legend.text = element_text(size = 30),legend.title = element_text(size = 35),
        legend.position = "none")+
  scale_y_continuous(limits = c(0,1),breaks = seq(0,1,0.25))
p.v9

Total <- ggpubr::ggarrange(p.v4,p.v9,ncol = 2,nrow = 1,align = "hv")
Total

library(patchwork)
p = (p.v4 | p.v9) + plot_layout(guides='collect') &
  theme(legend.position="NULL")
p
getwd()
ggsave("figs4AB.pdf",plot = Total,width = 500,height = 200,units = "mm")

# FigS4 CD betapart
library(betapart)
library(NST)
library(tidyverse)

otu.18.v4 = read.table(file = "Final data/18SV4.norm.txt",sep="\t",row.names=1,header=T,check.names=F)
g.18.v4 = read.table(file = "Final data/group.v4.txt",sep="\t",header=T,check.names=F)
title = "18S.v4"

otu.18.v9 = read.csv(file = "Final data/18SV9.norm.csv",row.names=1,header=T,check.names=F)
g.18.v9 = read.table(file = "Final data/group.v9.txt",sep="\t",header=T,check.names=F)
title = "18S.v9"

otu.16 = ifelse(otu.16>0,1,0)
otu.18.v4 = ifelse(otu.18.v4>0,1,0)
otu.18.v9 = ifelse(otu.18.v9>0,1,0)

b.16<-beta.pair(t(otu.16))
b.18.v4<-beta.pair(t(otu.18.v4))
b.18.v9<-beta.pair(t(otu.18.v9))


gc()
r.16.repl = dist.3col(b.16$beta.sim)
r.16.nest = dist.3col(b.16$beta.sne)
r.16.tota = dist.3col(b.16$beta.sor)

r.18.v4.repl = dist.3col(b.18.v4$beta.sim)
r.18.v4.nest = dist.3col(b.18.v4$beta.sne)
r.18.v4.tota = dist.3col(b.18.v4$beta.sor)

r.18.v9.repl = dist.3col(b.18.v9$beta.sim)
r.18.v9.nest = dist.3col(b.18.v9$beta.sne)
r.18.v9.tota = dist.3col(b.18.v9$beta.sor)

gc()
r.16.repl$name1 = g.16[match(r.16.repl$name1,g.16$sample),]$habitat
r.16.repl$name2 = g.16[match(r.16.repl$name2,g.16$sample),]$habitat
r.16.nest$name1 = g.16[match(r.16.nest$name1,g.16$sample),]$habitat
r.16.nest$name2 = g.16[match(r.16.nest$name2,g.16$sample),]$habitat
r.16.tota$name1 = g.16[match(r.16.tota$name1,g.16$sample),]$habitat
r.16.tota$name2 = g.16[match(r.16.tota$name2,g.16$sample),]$habitat


r.18.v4.repl$name1 = g.18.v4[match(r.18.v4.repl$name1,g.18.v4$sample),]$habitat
r.18.v4.repl$name2 = g.18.v4[match(r.18.v4.repl$name2,g.18.v4$sample),]$habitat
r.18.v4.nest$name1 = g.18.v4[match(r.18.v4.nest$name1,g.18.v4$sample),]$habitat
r.18.v4.nest$name2 = g.18.v4[match(r.18.v4.nest$name2,g.18.v4$sample),]$habitat
r.18.v4.tota$name1 = g.18.v4[match(r.18.v4.tota$name1,g.18.v4$sample),]$habitat
r.18.v4.tota$name2 = g.18.v4[match(r.18.v4.tota$name2,g.18.v4$sample),]$habitat


r.18.v9.repl$name1 = g.18.v9[match(r.18.v9.repl$name1,g.18.v9$sample),]$habitat
r.18.v9.repl$name2 = g.18.v9[match(r.18.v9.repl$name2,g.18.v9$sample),]$habitat
r.18.v9.nest$name1 = g.18.v9[match(r.18.v9.nest$name1,g.18.v9$sample),]$habitat
r.18.v9.nest$name2 = g.18.v9[match(r.18.v9.nest$name2,g.18.v9$sample),]$habitat
r.18.v9.tota$name1 = g.18.v9[match(r.18.v9.tota$name1,g.18.v9$sample),]$habitat
r.18.v9.tota$name2 = g.18.v9[match(r.18.v9.tota$name2,g.18.v9$sample),]$habitat


gc()
r.16 = cbind(r.16.repl[,1:2],replace = r.16.repl[,3]/r.16.tota[,3])
r.16$nest = 1-r.16$replace

r.18.v4 = cbind(r.18.v4.repl[,1:2],replace = r.18.v4.repl[,3]/r.18.v4.tota[,3])
r.18.v4$nest = 1-r.18.v4$replace

r.18.v9 = cbind(r.18.v9.repl[,1:2],replace = r.18.v9.repl[,3]/r.18.v9.tota[,3])
r.18.v9$nest = 1-r.18.v9$replace

r.16 = r.16[r.16$name1 != r.16$name2,]
r.18.v4 = r.18.v4[r.18.v4$name1 != r.18.v4$name2,]
r.18.v9 = r.18.v9[r.18.v9$name1 != r.18.v9$name2,]

res.16 = r.16 %>% group_by(name1,name2) %>% 
  summarise(replace.m = mean(replace)) %>% 
  mutate(nest.m = 1-replace.m);res.16

res.18.v4 = r.18.v4 %>% group_by(name1,name2) %>% 
  summarise(replace.m = mean(replace)) %>% 
  mutate(nest.m = 1-replace.m);res.18.v4

res.18.v9 = r.18.v9 %>% group_by(name1,name2) %>% 
  summarise(replace.m = mean(replace)) %>% 
  mutate(nest.m = 1-replace.m);res.18.v9

# output, Remove duplicate results
getwd()
write.csv(res.18.v4, file = "betapart.18.v4.csv")
write.csv(res.18.v9, file = "betapart.18.v9.csv")

#### and then input
r.v4 = read.table(file = "betapart.18.v4.txt",header=T,sep="\t")
r.v9 = read.table(file = "betapart.18.v9.txt",header=T,sep="\t")

r.v4$g = paste(r.v4$name1," and ",r.v4$name2,sep="")
r.v9$g = paste(r.v9$name1," and ",r.v9$name2,sep="")

r.v4$nest.per = paste("(",round(r.v4$nest.m *100,2),"%)", sep = '')
r.v9$nest.per = paste("(",round(r.v9$nest.m *100,2),"%)", sep = '')

p.v4 = r.v4 %>% pivot_longer(cols = -c(name1,name2,g,nest.per))
p.v9 = r.v9 %>% pivot_longer(cols = -c(name1,name2,g,nest.per))

library(ggmap)
v4 = ggplot(data = p.v4,aes(x=g,y=value,fill=name,group=g))+
  geom_bar(stat = 'identity', position = 'stack', width = 1)+
  coord_polar(theta = 'y')+ labs(x = '', y = '', title = '')+
  geom_text(aes(y = value/2 + c(0, cumsum(value)[-length(value)]/100), 
                x = cumsum(value), label = nest.per)) +
  theme_nothing() 
v4
v9 = ggplot(data = p.v9,aes(x=g,y=value,fill=name,group=g))+
  geom_bar(stat = 'identity', position = 'stack', width = 1)+
  coord_polar(theta = 'y')+ labs(x = '', y = '', title = '')+
  geom_text(aes(y = value/2 + c(0, cumsum(value)[-length(value)]/100), 
                x = cumsum(value), label = nest.per)) +
  theme_nothing() 
v9
