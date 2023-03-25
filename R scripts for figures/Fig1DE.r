# Fig1DE Richness and phylogenetic diversity

library(ggsignif) 
library(viridis)
library(tidyverse)

# v4
otu = read.table(file = "Final data/18SV4.norm.txt",sep='\t',header=T,row.names = 1)
pd = read.table(file = "Final data/PD.v4.txt",sep='\t',header=T,row.names = 1)
g = read.table(file = "Final data/group.v4.txt",sep="\t",header=T)

#v9
otu = read.csv(file = "Final data/18SV9.norm.csv",header=T,row.names = 1)
pd = read.table(file = "Final data/PD.v9.txt",sep='\t',header=T,row.names = 1)
g = read.table(file = "Final data/group.v9.txt",sep="\t",header=T)

library(vegan)

#- v4 and v9 run separately
x = otu
x[is.na(x)] = 0
x = t(x)

Shannon <- diversity(x)
Inv_Simpson <- diversity(x, "inv")
S <- specnumber(x)
Pielou_evenness <- Shannon/log(S)
Simpson_evenness <- Inv_Simpson/S
Observed_richness <- colSums(t(x)>0)
report1 = cbind(Shannon, Inv_Simpson,Observed_richness, Pielou_evenness, Simpson_evenness)

report1 = report1[order(rownames(report1)),]
pd = pd[order(rownames(pd)),]
g = g[order(g$sample),]
r = cbind(report1,pd,g)
#-

r.v4 = r
r.v9 = r

r.v4$Region = "V4";r.v9$Region = "V9"
all = rbind(r.v4,r.v9)


rich.v4 = ggplot(data = r.v4,aes(x = habitat,y = SR,fill = habitat,color=habitat))+
  geom_boxplot(alpha=.5,outlier.alpha = .1)+ 
  labs(x = "18S V4 rRNA gene",y = "Richness")+
  scale_fill_manual(values = c("#339933","#990000","#CC9900","#663366","#99CCFF"))+
  scale_color_manual(values = c("#339933","#990000","#CC9900","#663366","#99CCFF"))+
  scale_y_continuous(limits=c(2000, 5000))+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5,size = 15),
        axis.text = element_text(size = 22),axis.title = element_text(size = 25),
        legend.text = element_text(size = 22),legend.title = element_text(size = 25))
rich.v4
rich.v9 = ggplot(data = r.v9,aes(x = habitat,y = SR,fill = habitat,color=habitat))+
  geom_boxplot(alpha=.5,outlier.alpha = .1)+
  labs(x = "18S V9 rRNA gene",y = "Richness")+
  scale_fill_manual(values = c("#339933","#990000","#CC9900","#663366","#99CCFF"))+
  scale_color_manual(values = c("#339933","#990000","#CC9900","#663366","#99CCFF"))+
  scale_y_continuous(limits=c(2000, 5000))+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5,size = 15),
        axis.text = element_text(size = 22),axis.title = element_text(size = 25),
        legend.text = element_text(size = 22),legend.title = element_text(size = 25))
rich.v9


library(patchwork)
p = (rich.v4 | rich.v9) + plot_layout(guides='collect') &
  theme(legend.position="NULL")
p
getwd()
ggsave("richness.pdf",plot = p,width = 500,height = 200,units = "mm")


pd.v4 = ggplot(data = r.v4,aes(x = habitat,y = PD,fill = habitat,color=habitat))+
  geom_boxplot(alpha=.5,outlier.alpha = .1)+ 
  labs(x = "18S V4 rRNA gene",y = "Phylogenetic diversity")+
  scale_fill_manual(values = c("#339933","#990000","#CC9900","#663366","#99CCFF"))+
  scale_color_manual(values = c("#339933","#990000","#CC9900","#663366","#99CCFF"))+
  scale_y_continuous(limits=c(100, 300))+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5,size = 15),
        axis.text = element_text(size = 22),axis.title = element_text(size = 25),
        legend.text = element_text(size = 22),legend.title = element_text(size = 25))
pd.v4
pd.v9 = ggplot(data = r.v9,aes(x = habitat,y = PD,fill = habitat,color=habitat))+
  geom_boxplot(alpha=.5,outlier.alpha = .1)+
  labs(x = "18S V9 rRNA gene",y = "Phylogenetic diversity")+
  scale_fill_manual(values = c("#339933","#990000","#CC9900","#663366","#99CCFF"))+
  scale_color_manual(values = c("#339933","#990000","#CC9900","#663366","#99CCFF"))+
  scale_y_continuous(limits=c(100, 300))+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5,size = 15),
        axis.text = element_text(size = 22),axis.title = element_text(size = 25),
        legend.text = element_text(size = 22),legend.title = element_text(size = 25))
pd.v9


library(patchwork)
p = (pd.v4 | pd.v9) + plot_layout(guides='collect') &
  theme(legend.position="NULL")
p
getwd()
ggsave("pd.pdf",plot = p,width = 500,height = 200,units = "mm")
