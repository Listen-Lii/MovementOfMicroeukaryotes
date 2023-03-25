## Fig S3

library(ggsci)
library(vegan)
library(ggplot2)
library(tidyverse)
library(eoffice)
library(patchwork)
plot_theme = theme(plot.title = element_text(hjust = 0,size = 30),
                   axis.text = element_text(size = 30,color="black"),axis.title = element_text(size = 35,color="black"),
                   legend.text = element_text(size = 20,color="black"),legend.title = element_text(size = 20,color="black"),
                   axis.text.x = element_text(angle = 0, hjust = 0))+
  theme(axis.ticks = element_line(size = 0.5))+  
  theme(axis.ticks.length = unit(0.8,"lines"))+  
  theme(panel.grid=element_blank())+  
  theme(legend.position="right")


#v4
otu.v4 <- read.table(file = "Final data/18SV4.norm.txt", sep='\t',header=T,row.names = 1)
colSums(otu.v4)
g.v4 = read.table(file = "Final data/group.v4.txt",sep="\t",header=T)

#v9
otu.v9 <- read.csv(file = "Final data/18SV9.norm.csv", header=T,row.names = 1)
colSums(otu.v9)
g.v9 = read.table(file = "Final data/group.v9.txt",sep="\t",header=T)


fun.v4 = metadata <- read.csv(file = "FUNGuild_18S PR2_V4.csv", header=T, sep = ",", row.names = 1)
fun.v9 = metadata <- read.csv(file = "FUNGuild_18S PR2_V9.csv", header=T, sep = ",", row.names = 1)

fun.v4$t = "V4"
fun.v9$t = "V9"

fun.v4$abundance = rowSums(otu.v4[match(rownames(fun.v4),rownames(otu.v4)),])
fun.v9$abundance = rowSums(otu.v9[match(rownames(fun.v9),rownames(otu.v9)),])


# Fig S3A
fun.count = data.frame(v4 = table(fun.v4[,"trophicMode"]),
                       v9 = table(fun.v9[,"trophicMode"]))
link.count = fun.count %>% arrange(by=desc(v4.Var1)) %>%
  mutate(V4 = cumsum(v4.Freq),V9 = cumsum(v9.Freq))

fun.count = reshape2::melt(fun.count,value.name = "count",variable.name="group")

p1 = ggplot(fun.count,aes(x=group,y=count,fill=v4.Var1))+
  geom_bar(stat="identity",width=.5)+
  geom_segment(data=link.count,aes(x=1.25,xend=1.75,y=V4,yend=V9))+
  guides(fill=guide_legend(reverse=F)) +
  labs(x = NULL,y = "Taxa numbers")+
  scale_fill_manual(values = pal_npg("nrc", alpha = 1)(10))+
  scale_color_manual(values = pal_npg("nrc", alpha = 1)(10))+
  theme_bw()+plot_theme
p1

# Fig S3B
fun.v4.abun = fun.v4 %>% group_by(trophicMode) %>% summarize(abundance=sum(abundance))
fun.v4.abun$t = "V4"
colnames(fun.v4.abun) = c("trophicMode.v4","abun.v4","t.v4" )
fun.v9.abun = fun.v9 %>% group_by(trophicMode) %>% summarize(abundance=sum(abundance))
fun.v9.abun$t = "V9"
colnames(fun.v9.abun) = c("trophicMode.v9","abun.v9","t.v9" )
fun.abun = cbind(fun.v4.abun,fun.v9.abun)

link.abun = fun.abun %>% arrange(by=desc(trophicMode.v4)) %>%
  mutate(V4 = cumsum(abun.v4),V9 = cumsum(abun.v9))

fun.abun = reshape2::melt(fun.abun,value.name = "abun",variable.name="group")

p2 = ggplot(fun.abun,aes(x=group,y=abun,fill=trophicMode.v4))+
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


# v4
g.v4 = g.v4[match(colnames(otu.v4),g.v4$sample),]
m.v4= otu.v4[,g.v4$habitat=="Moss"]
s.v4= otu.v4[,g.v4$habitat=="Soil"]
se.v4= otu.v4[,g.v4$habitat=="Sediment"]
t.v4= otu.v4[,g.v4$habitat=="Tree hole"]
w.v4= otu.v4[,g.v4$habitat=="Water"]

# v9
g.v9 = g.v9[match(colnames(otu.v9),g.v9$sample),]
m.v9= otu.v9[,g.v9$habitat=="Moss"]
s.v9= otu.v9[,g.v9$habitat=="Soil"]
se.v9= otu.v9[,g.v9$habitat=="Sediment"]
t.v9= otu.v9[,g.v9$habitat=="Tree hole"]
w.v9= otu.v9[,g.v9$habitat=="Water"]

gr = c("Moss","Soil","Sediment","Treehole","Water")
v4.otu = list(m.v4,s.v4,se.v4,t.v4,w.v4)
v9.otu = list(m.v9,s.v9,se.v9,t.v9,w.v9)

fun.count.all = c()
link.count.all = c()
fun.abun.all = c()
link.abun.all = c()
for (i in 1:5){ 
  input.otu.v4 = v4.otu[[i]]
  input.funguild.v4 = fun.v4
  input.otu.v9 = v9.otu[[i]]
  input.funguild.v9 = fun.v9
  gn = gr[i]
  
  input.funguild.v4$abundance = rowSums(input.otu.v4[match(rownames(input.funguild.v4),rownames(input.otu.v4)),])
  input.funguild.v9$abundance = rowSums(input.otu.v9[match(rownames(input.funguild.v9),rownames(input.otu.v9)),])
  
  input.funguild.v4 = input.funguild.v4[input.funguild.v4$abundance>0,]
  input.funguild.v9 = input.funguild.v9[input.funguild.v9$abundance>0,]
  
  fun.count = data.frame(v4 = table(input.funguild.v4[,"trophicMode"]),
                         v9 = table(input.funguild.v9[,"trophicMode"]))
  link.count = fun.count %>% arrange(by=desc(v4.Var1)) %>%
    mutate(V4 = cumsum(v4.Freq),V9 = cumsum(v9.Freq))
  
  fun.count$g = gn
  link.count$g = gn
  
  fun.count.all = rbind(fun.count.all,fun.count)
  link.count.all = rbind(link.count.all,link.count)
  
  fun.v4.abun = input.funguild.v4 %>% group_by(trophicMode) %>% summarize(abundance=sum(abundance))
  fun.v4.abun$t = "V4"
  colnames(fun.v4.abun) = c("trophicMode.v4","abun.v4","t.v4" )
  fun.v9.abun = input.funguild.v9 %>% group_by(trophicMode) %>% summarize(abundance=sum(abundance))
  fun.v9.abun$t = "V9"
  colnames(fun.v9.abun) = c("trophicMode.v9","abun.v9","t.v9" )
  fun.abun = cbind(fun.v4.abun,fun.v9.abun)
  
  link.abun = fun.abun %>% arrange(by=desc(trophicMode.v4)) %>%
    mutate(V4 = cumsum(abun.v4),V9 = cumsum(abun.v9))
  
  fun.abun$g = gn
  link.abun$g = gn
  
  fun.abun.all = rbind(fun.abun.all,fun.abun)
  link.abun.all = rbind(link.abun.all,link.abun)
}

fun.count.all2 = reshape2::melt(fun.count.all,value.name = "count",variable.name="region")
fun.abun.all2 = reshape2::melt(fun.abun.all,value.name = "abun",variable.name="region")

# Fig S3C
p3 = ggplot(fun.count.all2,
            aes(x=region,y=count,fill=v4.Var1))+
  geom_bar(stat="identity",position = "stack",width=.5) +
  facet_wrap(.~g,nrow=1) +
  geom_segment(data=link.count.all,aes(x=1.25,xend=1.75,y=V4,yend=V9))+
  guides(fill=guide_legend(reverse=F)) +
  labs(x = NULL,y = "Taxa numbers")+
  scale_fill_manual(values = pal_npg("nrc", alpha = 1)(10))+
  scale_color_manual(values = pal_npg("nrc", alpha = 1)(10))+
  theme_bw()+plot_theme+
  theme(strip.text.x = element_text(size = 20, colour = "black")) 
p3

# Fig S3D
p4 = ggplot(fun.abun.all2,
            aes(x=region,y=abun,fill=trophicMode.v4))+
  geom_bar(stat="identity",position = "stack",width=.5) +
  facet_wrap(.~g,nrow=1) +
  geom_segment(data=link.abun.all,aes(x=1.25,xend=1.75,y=V4,yend=V9))+
  guides(fill=guide_legend(reverse=F)) +
  labs(x = NULL,y = "Taxa numbers")+
  scale_fill_manual(values = pal_npg("nrc", alpha = 1)(10))+
  scale_color_manual(values = pal_npg("nrc", alpha = 1)(10))+
  theme_bw()+plot_theme+
  theme(strip.text.x = element_text(size = 20, colour = "black")) 
p4

getwd()
ggsave("3.habitat.count.pdf",plot = p3,width = 400,height = 150,units = "mm")
ggsave("4.habitat.abundance.pdf",plot = p4,width = 400,height = 150,units = "mm")
