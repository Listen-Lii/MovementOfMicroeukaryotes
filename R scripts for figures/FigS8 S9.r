# FigS9 and FigS8

# Take V4 as an example. V9 is the same

library(tidyverse)
library(ggsci)
library(ggplot2)
library(scales)
library(EcolUtils)


tax = read.csv(file = "Final data/v4.txt", header=T,sep="\t")
otu.all <- read.table(file = "Final data/18SV4.norm.txt", sep='\t',header=T,row.names = 1)
colSums(otu.all)
g = read.table(file = "Final data/group.v4.txt",sep="\t",header=T)

# m.s
metadata <- read.csv(file = "Final data/FEAST/V4/group/group_mosstos.csv", header=T, sep = ",", row.names = 1)
otus <- read.csv(file = "Final data/FEAST/V4/input/moss_soil.csv", header=T, sep = ",", row.names = 1)

# m.se
metadata <- read.csv(file = "Final data/FEAST/V4/group/group_mtose.csv", header=T, sep = ",", row.names = 1)
otus <- read.csv(file = "Final data/FEAST/V4/input/moss_sediment.csv", header=T, sep = ",", row.names = 1)

# m.w
metadata <- read.csv(file = "Final data/FEAST/V4/group/group_mtow.csv", header=T, sep = ",", row.names = 1)
otus <- read.csv(file = "Final data/FEAST/V4/input/moss_water.csv", header=T, sep = ",", row.names = 1)

# m.t
metadata <- read.csv(file = "Final data/FEAST/V4/group/group_treetom.csv", header=T, sep = ",", row.names = 1)
otus <- read.csv(file = "Final data/FEAST/V4/input/tree_moss.csv", header=T, sep = ",", row.names = 1)

# se.s
metadata <- read.csv(file = "Final data/FEAST/V4/group/group_setos.csv", header=T, sep = ",", row.names = 1)
otus <- read.csv(file = "Final data/FEAST/V4/input/soil_sediment.csv", header=T, sep = ",", row.names = 1)

# se.t
metadata <- read.csv(file = "Final data/FEAST/V4/group/group_treetose.csv", header=T, sep = ",", row.names = 1)
otus <- read.csv(file = "Final data/FEAST/V4/input/sedimenttree.csv", header=T, sep = ",", row.names = 1)

# se.w
metadata <- read.csv(file = "Final data/FEAST/V4/group/group_setow.csv", header=T, sep = ",", row.names = 1)
otus <- read.csv(file = "Final data/FEAST/V4/input/water_sediment.csv", header=T, sep = ",", row.names = 1)

# s.t
metadata <- read.csv(file = "Final data/FEAST/V4/group/group_stotree.csv", header=T, sep = ",", row.names = 1)
otus <- read.csv(file = "Final data/FEAST/V4/input/tree_soil.csv", header=T, sep = ",", row.names = 1)

# s.w
metadata <- read.csv(file = "Final data/FEAST/V4/group/group_soiltow.csv", header=T, sep = ",", row.names = 1)
otus <- read.csv(file = "Final data/FEAST/V4/input/water_soil.csv", header=T, sep = ",", row.names = 1)

# t.w
metadata <- read.csv(file = "Final data/FEAST/V4/group/group_treetow.csv", header=T, sep = ",", row.names = 1)
otus <- read.csv(file = "Final data/FEAST/V4/input/water_tree.csv", header=T, sep = ",", row.names = 1)

if(TRUE){
otus = otus[rowSums(otus)>0,]
a = intersect(colnames(otus),rownames(metadata))

otus2 = otus[,a]
g = metadata[a,]


otus3 = c()
for (k in 1:nrow(otus2)){ 
  g1 = otus2[k,][g$SourceSink=="Source"];n1 = length(g1)
  g2 = otus2[k,][g$SourceSink=="Sink"];n2 = length(g2)
  if( (sum(g1>0) >= n1/2 ) & (sum(g2>0) >= n2/2 ) ){
    otus3 = rbind(otus3, otus2[k,])
  }
}

res = c()
for(i in 1:nrow(otus3)){  
  aa = data.frame(t(otus3[i,]) )
  colnames(aa) = "d"
  aa$g = g$SourceSink
  ra = wilcox.test(aa[aa$g=="Source",1],aa[aa$g=="Sink",1])
  
  r = c(ra$p.value,ra$estimate)
  res = rbind(res,r)
}

res = as.data.frame(res)
colnames(res) = "p"
res$otu = rownames(otus3)
res = na.omit(res)

aa = match(res$otu,tax[,1])
tax2 = tax[aa,]
res2 = cbind(res,tax2[,2:9])
table(res2$D)

res3 = res2[res2$p > 0.05,];
r = as.data.frame(table(res3$D))
per = nrow(res3)/nrow(res2)

# niche width
sour = otus3[,match(rownames(g[g$SourceSink=="Source",]),colnames(otus3))]
sink = otus3[,match(rownames(g[g$SourceSink=="Sink",]),colnames(otus3))]

sour.niche <- spec.gen(t(sour), niche.width.method = 'levins', perm.method = 'quasiswap', n = 99, probs = c(0.025, 0.975))
sink.niche <- spec.gen(t(sink), niche.width.method = 'levins', perm.method = 'quasiswap', n = 99, probs = c(0.025, 0.975))
nich = data.frame(source = sour.niche$observed,sink = sink.niche$observed)
}

#- run the above code 10 times and get each result
m.s = res3;per.m.s = per;niche.m.s = nich
m.se = res3;per.m.se = per;niche.m.se = nich
m.w = res3;per.m.w = per;niche.m.w = nich
m.t = res3;per.m.t = per;niche.m.t = nich

se.s = res3;per.se.s = per;niche.se.s = nich
se.t = res3;per.se.t = per;niche.se.t = nich
se.w = res3;per.se.w = per;niche.se.w = nich

s.t = res3;per.s.t = per;niche.s.t = nich
s.w = res3;per.s.w = per;niche.s.w = nich

t.w = res3;per.t.w = per;niche.t.w = nich
#-

m.s$g = "m.s"
m.se$g =  "m.se"
m.w$g =  "m.w"
m.t$g =  "m.t"
se.s$g =  "s.se"
se.t$g =  "se.t"
se.w$g =  "se.w"
s.t$g =  "s.t"
s.w$g =  "s.w"
t.w$g =  "t.w"

all = rbind(select(m.s,c(p,otu,D,g)),
select(m.se,c(p,otu,D,g)),
select(m.w,c(p,otu,D,g)),
select(m.t,c(p,otu,D,g)),
select(se.s,c(p,otu,D,g)),
select(se.t,c(p,otu,D,g)),
select(se.w,c(p,otu,D,g)),
select(s.t,c(p,otu,D,g)),
select(s.w,c(p,otu,D,g)),
select(t.w,c(p,otu,D,g)))

lab = c("Moss.Soil","Moss.Sediment","Moss.Tree hole","Moss.Water",
        "Soil.Sediment","Soil.Tree hole","Soil.Water",
        "Sediment.Tree hole","Sediment.Water","Tree hole.Water")
all$D = ifelse(all$D == "","Unidentified",all$D)

##FigS9 AC

p1 = ggplot(all, aes(g,fill=D)) +
  geom_bar(stat="count",width=.5) +
  guides(fill=guide_legend(reverse=F)) +
  labs(x = NULL,y = "Taxa numbers")+
  scale_fill_manual(values = pal_npg("nrc", alpha = 1)(10))+
  scale_color_manual(values = pal_npg("nrc", alpha = 1)(10))+
  scale_x_discrete(labels  = lab)+  
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5,size = 15),
        axis.text = element_text(size = 30),axis.title = element_text(size = 35),
        legend.text = element_text(size = 30),legend.title = element_text(size = 35),
        legend.position="NULL",axis.text.x = element_text(angle = 45, hjust = 1))+
  theme(axis.ticks = element_line(size = 0.4))+  
  theme(axis.ticks.length = unit(0.6,"lines"))+  
  theme(panel.grid=element_blank())+  
  theme(legend.position="NULL")
p1

##FigS9 BD
p2 = ggplot(all, aes(g,fill=D)) +
  geom_bar(position = "fill",width=.5) +
  guides(fill=guide_legend(reverse=F)) +
  scale_y_continuous(labels = percent)+  
  labs(x = NULL,y = "Taxa percentage(%)")+
  scale_fill_manual(values = pal_npg("nrc", alpha = 1)(10))+
  scale_color_manual(values = pal_npg("nrc", alpha = 1)(10))+
  scale_x_discrete(labels  = lab)+  
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5,size = 15),
        axis.text = element_text(size = 30),axis.title = element_text(size = 35),
        legend.text = element_text(size = 30),legend.title = element_text(size = 35),
        legend.position="NULL",axis.text.x = element_text(angle = 45, hjust = 1))+
  theme(axis.ticks = element_line(size = 0.4))+  
  theme(axis.ticks.length = unit(0.6,"lines"))+  
  theme(panel.grid=element_blank())+  
  theme(legend.position="NULL")
p2

##FigS9 EF
g = read.table(file = "Final data/group.v4.txt",sep="\t",header=T)

# m.s
N1 = "Moss";N2 = "Soil";N3 = "m.s"
a = g[g$habitat==N1|g$habitat==N2,] 
m.otu = otu.all[,intersect(a$sample,colnames(otu.all))] 
m = intersect(rownames(m.otu),all[all$g == N3,]$otu) 
m.otu2 =  m.otu[m,]
m.all = all[all$g == N3,] 
u = unique(m.all$D)

m.r = c()
for(i in 1:length(u)){ 
  ot = m.all[m.all$D == u[i],]$otu
  m.otu.per = m.otu2[intersect(ot,rownames(m.otu2)),]
  m.r.per = colSums(m.otu.per)
  m.r = rbind(m.r,m.r.per)
}
m.r  = as.data.frame(m.r)
m.r$Domain = u
m.r.t = m.r %>% pivot_longer(cols = -Domain)
m.g = g[match(unique(m.r.t$name),g$sample),] 


m.s.r = cbind(m.r.t,g[match(m.r.t$name,g$sample),])
m.s.r$g = paste(N1,N2,sep=".")
m.s.r$g2 = ifelse(m.s.r$habitat==N1,1,2) 


# m.se
N1 = "Moss";N2 = "Sediment";N3 = "m.se"
a = g[g$habitat==N1|g$habitat==N2,] 
m.otu = otu.all[,intersect(a$sample,colnames(otu.all))] 
m = intersect(rownames(m.otu),all[all$g == N3,]$otu) 
m.otu2 =  m.otu[m,]
m.all = all[all$g == N3,] 
u = unique(m.all$D)

m.r = c()
for(i in 1:length(u)){ 
  ot = m.all[m.all$D == u[i],]$otu
  m.otu.per = m.otu2[intersect(ot,rownames(m.otu2)),]
  m.r.per = colSums(m.otu.per)
  m.r = rbind(m.r,m.r.per)
}
m.r  = as.data.frame(m.r)
m.r$Domain = u
m.r.t = m.r %>% pivot_longer(cols = -Domain)
m.g = g[match(unique(m.r.t$name),g$sample),] 


m.se.r = cbind(m.r.t,g[match(m.r.t$name,g$sample),])
m.se.r$g = paste(N1,N2,sep=".")
m.se.r$g2 = ifelse(m.se.r$habitat==N1,1,2) 

# m.w
N1 = "Moss";N2 = "Water";N3 = "m.w"
a = g[g$habitat==N1|g$habitat==N2,] 
m.otu = otu.all[,intersect(a$sample,colnames(otu.all))] 
m = intersect(rownames(m.otu),all[all$g == N3,]$otu) 
m.otu2 =  m.otu[m,]
m.all = all[all$g == N3,] 
u = unique(m.all$D)

m.r = c()
for(i in 1:length(u)){ 
  ot = m.all[m.all$D == u[i],]$otu
  m.otu.per = m.otu2[intersect(ot,rownames(m.otu2)),]
  m.r.per = colSums(m.otu.per)
  m.r = rbind(m.r,m.r.per)
}
m.r  = as.data.frame(m.r)
m.r$Domain = u
m.r.t = m.r %>% pivot_longer(cols = -Domain)
m.g = g[match(unique(m.r.t$name),g$sample),] 


m.w.r = cbind(m.r.t,g[match(m.r.t$name,g$sample),])
m.w.r$g = paste(N1,N2,sep=".")
m.w.r$g2 = ifelse(m.w.r$habitat==N1,1,2) 


# m.t
N1 = "Moss";N2 = "Tree hole";N3 = "m.t"
a = g[g$habitat==N1|g$habitat==N2,] 
m.otu = otu.all[,intersect(a$sample,colnames(otu.all))] 
m = intersect(rownames(m.otu),all[all$g == N3,]$otu) 
m.otu2 =  m.otu[m,]
m.all = all[all$g == N3,] 
u = unique(m.all$D)

m.r = c()
for(i in 1:length(u)){ 
  ot = m.all[m.all$D == u[i],]$otu
  m.otu.per = m.otu2[intersect(ot,rownames(m.otu2)),]
  m.r.per = colSums(m.otu.per)
  m.r = rbind(m.r,m.r.per)
}
m.r  = as.data.frame(m.r)
m.r$Domain = u
m.r.t = m.r %>% pivot_longer(cols = -Domain)
m.g = g[match(unique(m.r.t$name),g$sample),] 


m.t.r = cbind(m.r.t,g[match(m.r.t$name,g$sample),])
m.t.r$g = paste(N1,N2,sep=".")
m.t.r$g2 = ifelse(m.t.r$habitat==N1,1,2) 

# se.s  -- s.se
N1 = "Soil";N2 = "Sediment";N3 = "s.se"
a = g[g$habitat==N1|g$habitat==N2,] 
m.otu = otu.all[,intersect(a$sample,colnames(otu.all))] 
m = intersect(rownames(m.otu),all[all$g == N3,]$otu) 
m.otu2 =  m.otu[m,]
m.all = all[all$g == N3,] 
u = unique(m.all$D)

m.r = c()
for(i in 1:length(u)){ 
  ot = m.all[m.all$D == u[i],]$otu
  m.otu.per = m.otu2[intersect(ot,rownames(m.otu2)),]
  m.r.per = colSums(m.otu.per)
  m.r = rbind(m.r,m.r.per)
}
m.r  = as.data.frame(m.r)
m.r$Domain = u
m.r.t = m.r %>% pivot_longer(cols = -Domain)
m.g = g[match(unique(m.r.t$name),g$sample),] 


s.se.r = cbind(m.r.t,g[match(m.r.t$name,g$sample),])
s.se.r$g = paste(N1,N2,sep=".")
s.se.r$g2 = ifelse(s.se.r$habitat==N1,1,2)

# se.w
N1 = "Sediment";N2 = "Water";N3 = "se.w"
a = g[g$habitat==N1|g$habitat==N2,] 
m.otu = otu.all[,intersect(a$sample,colnames(otu.all))] 
m = intersect(rownames(m.otu),all[all$g == N3,]$otu) 
m.otu2 =  m.otu[m,]
m.all = all[all$g == N3,] 
u = unique(m.all$D)

m.r = c()
for(i in 1:length(u)){ 
  ot = m.all[m.all$D == u[i],]$otu
  m.otu.per = m.otu2[intersect(ot,rownames(m.otu2)),]
  m.r.per = colSums(m.otu.per)
  m.r = rbind(m.r,m.r.per)
}
m.r  = as.data.frame(m.r)
m.r$Domain = u
m.r.t = m.r %>% pivot_longer(cols = -Domain)
m.g = g[match(unique(m.r.t$name),g$sample),] 


se.w.r = cbind(m.r.t,g[match(m.r.t$name,g$sample),])
se.w.r$g = paste(N1,N2,sep=".")
se.w.r$g2 = ifelse(se.w.r$habitat==N1,1,2)


# se.t
N1 = "Sediment";N2 = "Tree hole";N3 = "se.t"
a = g[g$habitat==N1|g$habitat==N2,] 
m.otu = otu.all[,intersect(a$sample,colnames(otu.all))] 
m = intersect(rownames(m.otu),all[all$g == N3,]$otu) 
m.otu2 =  m.otu[m,]
m.all = all[all$g == N3,] 
u = unique(m.all$D)

m.r = c()
for(i in 1:length(u)){ 
  ot = m.all[m.all$D == u[i],]$otu
  m.otu.per = m.otu2[intersect(ot,rownames(m.otu2)),]
  m.r.per = colSums(m.otu.per)
  m.r = rbind(m.r,m.r.per)
}
m.r  = as.data.frame(m.r)
m.r$Domain = u
m.r.t = m.r %>% pivot_longer(cols = -Domain)
m.g = g[match(unique(m.r.t$name),g$sample),] 


se.t.r = cbind(m.r.t,g[match(m.r.t$name,g$sample),])
se.t.r$g = paste(N1,N2,sep=".")
se.t.r$g2 = ifelse(se.t.r$habitat==N1,1,2)

# s.w
N1 = "Soil";N2 = "Water";N3 = "s.w"
a = g[g$habitat==N1|g$habitat==N2,] 
m.otu = otu.all[,intersect(a$sample,colnames(otu.all))] 
m = intersect(rownames(m.otu),all[all$g == N3,]$otu) 
m.otu2 =  m.otu[m,]
m.all = all[all$g == N3,] 
u = unique(m.all$D)

m.r = c()
for(i in 1:length(u)){ 
  ot = m.all[m.all$D == u[i],]$otu
  m.otu.per = m.otu2[intersect(ot,rownames(m.otu2)),]
  m.r.per = colSums(m.otu.per)
  m.r = rbind(m.r,m.r.per)
}
m.r  = as.data.frame(m.r)
m.r$Domain = u
m.r.t = m.r %>% pivot_longer(cols = -Domain)
m.g = g[match(unique(m.r.t$name),g$sample),] 


s.w.r = cbind(m.r.t,g[match(m.r.t$name,g$sample),])
s.w.r$g = paste(N1,N2,sep=".")
s.w.r$g2 = ifelse(s.w.r$habitat==N1,1,2)

# s.t
N1 = "Soil";N2 = "Tree hole";N3 = "s.t"
a = g[g$habitat==N1|g$habitat==N2,] 
m.otu = otu.all[,intersect(a$sample,colnames(otu.all))] 
m = intersect(rownames(m.otu),all[all$g == N3,]$otu) 
m.otu2 =  m.otu[m,]
m.all = all[all$g == N3,] 
u = unique(m.all$D)

m.r = c()
for(i in 1:length(u)){ 
  ot = m.all[m.all$D == u[i],]$otu
  m.otu.per = m.otu2[intersect(ot,rownames(m.otu2)),]
  m.r.per = colSums(m.otu.per)
  m.r = rbind(m.r,m.r.per)
}
m.r  = as.data.frame(m.r)
m.r$Domain = u
m.r.t = m.r %>% pivot_longer(cols = -Domain)
m.g = g[match(unique(m.r.t$name),g$sample),] 


s.t.r = cbind(m.r.t,g[match(m.r.t$name,g$sample),])
s.t.r$g = paste(N1,N2,sep=".")
s.t.r$g2 = ifelse(s.t.r$habitat==N1,1,2)

# t.w
N1 = "Tree hole";N2 = "Water";N3 = "t.w"
a = g[g$habitat==N1|g$habitat==N2,] 
m.otu = otu.all[,intersect(a$sample,colnames(otu.all))] 
m = intersect(rownames(m.otu),all[all$g == N3,]$otu) 
m.otu2 =  m.otu[m,]
m.all = all[all$g == N3,] 
u = unique(m.all$D)

m.r = c()
for(i in 1:length(u)){ 
  ot = m.all[m.all$D == u[i],]$otu
  m.otu.per = m.otu2[intersect(ot,rownames(m.otu2)),]
  m.r.per = colSums(m.otu.per)
  m.r = rbind(m.r,m.r.per)
}
m.r  = as.data.frame(m.r)
m.r$Domain = u
m.r.t = m.r %>% pivot_longer(cols = -Domain)
m.g = g[match(unique(m.r.t$name),g$sample),] 


t.w.r = cbind(m.r.t,g[match(m.r.t$name,g$sample),])
t.w.r$g = paste(N1,N2,sep=".")
t.w.r$g2 = ifelse(t.w.r$habitat==N1,1,2)

all.per = rbind(m.s.r,m.se.r,m.t.r,m.w.r,
                se.t.r,se.w.r,
                s.se.r,s.t.r,s.w.r,
                t.w.r)


all.per$g = factor(all.per$g,levels = lab)

p3 = ggplot(all.per,aes(x = g2, y = value/1000,fill=Domain,color=Domain))+
  geom_bar(stat="identity",position = "stack",width=.75) +
  facet_wrap(.~g,nrow=1)+
  labs(x = "Habitats for eukaryotic intermigration",y = "Average abundance (× 10^3)")+
  scale_fill_manual(values = pal_npg("nrc", alpha = 1)(10))+
  scale_color_manual(values = pal_npg("nrc", alpha = 1)(10))+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5,size = 15),
        axis.text = element_text(size = 30,color="black"),axis.title = element_text(size = 35),
        legend.text = element_text(size = 20,color="black"),legend.title = element_text(size = 25),
        axis.text.x = element_text(angle = 45, hjust = 1))+
  theme(axis.ticks = element_line(size = 0.4))+  
  theme(axis.ticks.length = unit(0.6,"lines"))+  
  theme(panel.grid=element_blank())+  
  theme(legend.position="NULL") 

p3


library(patchwork)
layout <- "
1111122222
333333333#
"
p = (p1 | p2)/(p3) + 
    theme(legend.position="right")

p = p1 + p2 + p3 + plot_layout(design = layout)
p
getwd()
ggsave("v4.pdf",plot = p,width = 600,height = 400,units = "mm")



## FigS8
niche.m.s2 = niche.m.s %>% rename(Moss=source,Soil=sink) %>% 
  mutate(h = "m.s") %>% pivot_longer(col=-h)
niche.m.se2 = niche.m.se %>% rename(Moss=source,Sediment=sink) %>% 
  mutate(h = "m.se") %>% pivot_longer(col=-h)
niche.m.t2 = niche.m.t %>% rename(Moss=source,Treehole=sink) %>% 
  mutate(h = "m.t") %>% pivot_longer(col=-h)
niche.m.w2 = niche.m.w %>% rename(Moss=source,Water=sink) %>% 
  mutate(h = "m.w") %>% pivot_longer(col=-h)
niche.s.se2 = niche.se.s %>% rename(Sediment=source,Soil=sink) %>% 
  mutate(h = "s.se") %>% pivot_longer(col=-h)
niche.se.t2 = niche.se.t %>% rename(Sediment=source,Treehole=sink) %>% 
  mutate(h = "se.t") %>% pivot_longer(col=-h)
niche.se.w2 = niche.se.w %>% rename(Sediment=source,Water=sink) %>% 
  mutate(h = "se.w") %>% pivot_longer(col=-h)
niche.s.t2 = niche.s.t %>% rename(Soil=source,Treehole=sink) %>% 
  mutate(h = "s.t") %>% pivot_longer(col=-h)
niche.s.w2 = niche.s.w %>% rename(Soil=source,Water=sink) %>% 
  mutate(h = "s.w") %>% pivot_longer(col=-h)
niche.t.w2 = niche.t.w %>% rename(Treehole=source,Water=sink) %>% 
  mutate(h = "t.w") %>% pivot_longer(col=-h)

p4 = ggplot()+
  geom_boxplot(data=niche.m.s2,aes(x=h,y=value,group=name,fill = h,color=h))+
  geom_boxplot(data=niche.m.se2,aes(x=h,y=value,group=name,fill = h,color=h))+
  geom_boxplot(data=niche.m.t2,aes(x=h,y=value,group=name,fill = h,color=h))+
  geom_boxplot(data=niche.m.w2,aes(x=h,y=value,group=name,fill = h,color=h))+
  geom_boxplot(data=niche.s.se2,aes(x=h,y=value,group=name,fill = h,color=h))+
  geom_boxplot(data=niche.s.t2,aes(x=h,y=value,group=name,fill = h,color=h))+
  geom_boxplot(data=niche.s.w2,aes(x=h,y=value,group=name,fill = h,color=h))+
  geom_boxplot(data=niche.se.t2,aes(x=h,y=value,group=name,fill = h,color=h))+
  geom_boxplot(data=niche.se.w2,aes(x=h,y=value,group=name,fill = h,color=h))+
  geom_boxplot(data=niche.t.w2,aes(x=h,y=value,group=name,fill = h,color=h))+
  labs(x = "Shared species in habitat pair",y = "Niche breadth")+
  scale_fill_manual(values = pal_npg("nrc", alpha = 1)(10))+
  scale_color_manual(values = pal_npg("nrc", alpha = 1)(10))+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5,size = 15),
        axis.text = element_text(size = 30,color="black"),axis.title = element_text(size = 35),
        legend.text = element_text(size = 20,color="black"),legend.title = element_text(size = 25),
        axis.text.x = element_text(angle = 45, hjust = 1))+
  theme(axis.ticks = element_line(size = 0.4))+  
  theme(axis.ticks.length = unit(0.6,"lines"))+  
  theme(panel.grid=element_blank())+  
  theme(legend.position="NULL") 

p4

getwd()
ggsave("figs8.pdf",plot = p4,width = 400,height = 200,units = "mm")
