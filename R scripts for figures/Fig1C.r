# Fig 1C ggtree cluster

library(vegan)
library(tidyverse)
library(ggplot2) 
library(ggtree) 
library(treeio)
library(tidytree)
library(ggtreeExtra)

v4 = read.table(file = "Final data/V4.txt",sep='\t',header=T,row.names = 1)
g4 = read.table(file = "Final data/group.v4.txt",sep='\t',header=T)

v9 = read.table(file = "Final data/V9.txt",sep='\t',header=T,row.names = 1)
g9 = read.table(file = "Final data/group.v9.txt",sep='\t',header=T)


v4domain = c()
v4n = unique(v4$D)
for(i in 1:length(v4n)){ 
  v4domain = rbind(v4domain,colSums(v4[v4$D == v4n[i],9:98]))
}
v4domain = as.data.frame(v4domain)
rownames(v4domain) = v4n

v9domain = c()
v9n = unique(v9$D)
for(i in 1:length(v9n)){ 
  v9domain = rbind(v9domain,colSums(v9[v9$D == v9n[i],9:98]))
}
v9domain = as.data.frame(v9domain)
rownames(v9domain) = v9n


colnames(v4domain) = paste(colnames(v4domain),".v4",sep="")
colnames(v9domain) = paste(colnames(v9domain),".v9",sep="")

v4domain$n = rownames(v4domain)
v9domain$n = rownames(v9domain)

domain = full_join(v4domain, v9domain, by = "n")
rownames(domain) = domain$n
domain = domain %>% select(-n)
rownames(domain)[4] = "Unidentified"  

d.domain = vegdist(t(domain),method = "bray")
f.domain = hclust(d.domain,method="ward.D2");
plot(f.domain,hang=-1,main="ward.D2",sub="",xlab = "domain")

tree.domain = as.phylo(f.domain)

g4$label = paste(g4$sample,".v4",sep="")
g9$label = paste(g9$sample,".v9",sep="")

g4$l = "v4"
g9$l = "v9"

g.all = rbind(g4,g9)

a = g.all$label
g.all$label = g.all$sample
g.all$sample = a
names(g.all)[4] = "lab" 

groupInfo.domain <- split(g.all$sample, g.all$habitat) 
tree.domain.g <- groupOTU(tree.domain, groupInfo.domain)

habitat.col = c("#339933", "#990000","#CC9900", "#663366","#99CCFF" )
p.domain = ggtree(tree.domain.g, layout="fan", ladderize = T, size=0.5,
                  branch.length = "none",aes(color=group)) + 
  scale_color_manual(values =habitat.col )+
  theme(legend.position = "right")+ 
  new_scale_colour()
p.domain


p.domain = p.domain %<+% g.all 
p.domain = p.domain + geom_tippoint(mapping=aes(colour=l),
                   size=3,stroke=0,alpha=1) +
         scale_colour_manual(name="Region",values=c("#9ac9db","#f8ac8c"),
                    guide=guide_legend(keywidth=0.3,keyheight=0.3,ncol=1,
                       override.aes=list(size=2,alpha=1),order=1)) +
  theme(legend.title=element_text(size=8),legend.text=element_text(size=6),
        legend.spacing.y = unit(0.02, "cm"))+ 
  new_scale_colour()

p.domain

park.col = c("#8ECFC9","#FFBE7A","#FA7F6F","#82B0D2","#BEB8DC","#E7DAD2")
p.domain = p.domain + geom_fruit(geom=geom_star,
                  mapping=aes(fill=park), starshape=1,colour=NA,
                  size=2,starstroke=0,pwidth=0.1,offset=0.05,
                  inherit.aes = FALSE) +
            scale_fill_manual(name="Park",values=park.col,
            guide=guide_legend(keywidth=0.3,keyheight=0.3,order=3),
            na.translate=FALSE)
p.domain
ggsave("fig1c.pdf",plot = p.domain,width = 280,height = 280,units = "mm")

# region > habitat > park
adonis(d.domain~l* park * habitat,data=g.all)

