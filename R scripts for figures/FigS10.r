# Fig S10
# Take V4 as an example. V9 is the same.

library(tidyverse)
library(ggsci)
library(ggplot2)
library(scales)

fun.v4 = metadata <- read.csv(file = "Final data/FUNGuild_18S PR2_V4.csv", header=T, sep = ",", row.names = 1)
fun.v4$t = "V4"

#otu.all
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
  
  #res$otu = paste("Z",res$otu,sep="")
  aa = na.omit(match(res$otu,rownames(fun.v4)))
  tax2 = fun.v4[aa,]
  res2 = res[na.omit(match(rownames(tax2),res$otu)),]  
  res2 = cbind(res2,tax2[,c("trophicMode","t")])

  res3 = res2[res2$p > 0.05,];
}

#- run the above code for 10 times to get each result
m.s = res3
m.se = res3
m.w = res3
m.t = res3

se.s = res3
se.t = res3
se.w = res3

s.t = res3
s.w = res3

t.w = res3
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

all = rbind(m.s, m.se, m.w, m.t,
            se.s,se.t, se.w,
            s.t,s.w,t.w)

lab = c("Moss.Soil","Moss.Sediment","Moss.Tree hole","Moss.Water",
        "Soil.Sediment","Soil.Tree hole","Soil.Water",
        "Sediment.Tree hole","Sediment.Water","Tree hole.Water")

##FigS10 AC

p1 = ggplot(all, aes(g,fill=trophicMode)) +
  geom_bar(stat="count",width=.5) +
  guides(fill=guide_legend(reverse=F)) +
  labs(x = NULL,y = "Taxa numbers")+
  scale_fill_manual(values = pal_npg("nrc", alpha = 1)(10))+
  scale_color_manual(values = pal_npg("nrc", alpha = 1)(10))+
  scale_x_discrete(labels  = lab)+  
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5,size = 15),
        axis.text = element_text(size = 30,color="black"),axis.title = element_text(size = 35),
        legend.text = element_text(size = 20,color="black"),legend.title = element_text(size = 25),
        legend.position="NULL",axis.text.x = element_text(angle = 45, hjust = 1))+
  theme(axis.ticks = element_line(size = 0.4))+ 
  theme(axis.ticks.length = unit(0.6,"lines"))+  
  theme(panel.grid=element_blank())+  
  theme(legend.position="NULL")  
p1

##FigS10 BD  
p2 = ggplot(all, aes(g,fill=trophicMode)) +
  geom_bar(position = "fill",width=.5) +
  guides(fill=guide_legend(reverse=F)) +
  scale_y_continuous(labels = percent)+  
  labs(x = NULL,y = "Taxa percentage(%)")+
  scale_fill_manual(values = pal_npg("nrc", alpha = 1)(10))+
  scale_color_manual(values = pal_npg("nrc", alpha = 1)(10))+
  scale_x_discrete(labels  = lab)+  
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5,size = 15),
        axis.text = element_text(size = 30,color="black"),axis.title = element_text(size = 35),
        legend.text = element_text(size = 20,color="black"),legend.title = element_text(size = 25),
        legend.position="NULL",axis.text.x = element_text(angle = 45, hjust = 1))+
  theme(axis.ticks = element_line(size = 0.4))+  
  theme(axis.ticks.length = unit(0.6,"lines"))+  
  theme(panel.grid=element_blank())+  
  theme(legend.position="NULL")
p2
