## Fig 3CD

# Run FigS10.r first before run this code to get Fig 3CD.

g = read.table(file = "Final data/group.v4.txt",sep="\t",header=T)

# m.s
N1 = "Moss";N2 = "Soil";N3 = "m.s"
a = g[g$habitat==N1|g$habitat==N2,] 
m.otu = otu.all[,intersect(a$sample,colnames(otu.all))] 
m = intersect(rownames(m.otu),all[all$g == N3,]$otu) 
m.otu2 =  m.otu[m,]
m.all = all[all$g == N3,] 
u = unique(m.all$trophicMode)

m.r = c()
for(i in 1:length(u)){ 
  ot = m.all[m.all$trophicMode == u[i],]$otu
  m.otu.per = m.otu2[intersect(ot,rownames(m.otu2)),]
  m.r.per = colSums(m.otu.per)
  m.r = rbind(m.r,m.r.per)
}
m.r  = as.data.frame(m.r)
m.r$trophicMode = u
m.r.t = m.r %>% pivot_longer(cols = -trophicMode)
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
u = unique(m.all$trophicMode)

m.r = c()
for(i in 1:length(u)){ 
  ot = m.all[m.all$trophicMode == u[i],]$otu
  m.otu.per = m.otu2[intersect(ot,rownames(m.otu2)),]
  m.r.per = colSums(m.otu.per)
  m.r = rbind(m.r,m.r.per)
}
m.r  = as.data.frame(m.r)
m.r$trophicMode = u
m.r.t = m.r %>% pivot_longer(cols = -trophicMode)
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
u = unique(m.all$trophicMode)

m.r = c()
for(i in 1:length(u)){ 
  ot = m.all[m.all$trophicMode == u[i],]$otu
  m.otu.per = m.otu2[intersect(ot,rownames(m.otu2)),]
  m.r.per = colSums(m.otu.per)
  m.r = rbind(m.r,m.r.per)
}
m.r  = as.data.frame(m.r)
m.r$trophicMode = u
m.r.t = m.r %>% pivot_longer(cols = -trophicMode)
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
u = unique(m.all$trophicMode)

m.r = c()
for(i in 1:length(u)){ 
  ot = m.all[m.all$trophicMode == u[i],]$otu
  m.otu.per = m.otu2[intersect(ot,rownames(m.otu2)),]
  m.r.per = colSums(m.otu.per)
  m.r = rbind(m.r,m.r.per)
}
m.r  = as.data.frame(m.r)
m.r$trophicMode = u
m.r.t = m.r %>% pivot_longer(cols = -trophicMode)
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
u = unique(m.all$trophicMode)

m.r = c()
for(i in 1:length(u)){ 
  ot = m.all[m.all$trophicMode == u[i],]$otu
  m.otu.per = m.otu2[intersect(ot,rownames(m.otu2)),]
  m.r.per = colSums(m.otu.per)
  m.r = rbind(m.r,m.r.per)
}
m.r  = as.data.frame(m.r)
m.r$trophicMode = u
m.r.t = m.r %>% pivot_longer(cols = -trophicMode)
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
u = unique(m.all$trophicMode)

m.r = c()
for(i in 1:length(u)){ 
  ot = m.all[m.all$trophicMode == u[i],]$otu
  m.otu.per = m.otu2[intersect(ot,rownames(m.otu2)),]
  m.r.per = colSums(m.otu.per)
  m.r = rbind(m.r,m.r.per)
}
m.r  = as.data.frame(m.r)
m.r$trophicMode = u
m.r.t = m.r %>% pivot_longer(cols = -trophicMode)
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
u = unique(m.all$trophicMode)

m.r = c()
for(i in 1:length(u)){ 
  ot = m.all[m.all$trophicMode == u[i],]$otu
  m.otu.per = m.otu2[intersect(ot,rownames(m.otu2)),]
  m.r.per = colSums(m.otu.per)
  m.r = rbind(m.r,m.r.per)
}
m.r  = as.data.frame(m.r)
m.r$trophicMode = u
m.r.t = m.r %>% pivot_longer(cols = -trophicMode)
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
u = unique(m.all$trophicMode)

m.r = c()
for(i in 1:length(u)){ 
  ot = m.all[m.all$trophicMode == u[i],]$otu
  m.otu.per = m.otu2[intersect(ot,rownames(m.otu2)),]
  m.r.per = colSums(m.otu.per)
  m.r = rbind(m.r,m.r.per)
}
m.r  = as.data.frame(m.r)
m.r$trophicMode = u
m.r.t = m.r %>% pivot_longer(cols = -trophicMode)
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
u = unique(m.all$trophicMode)

m.r = c()
for(i in 1:length(u)){ 
  ot = m.all[m.all$trophicMode == u[i],]$otu
  m.otu.per = m.otu2[intersect(ot,rownames(m.otu2)),]
  m.r.per = colSums(m.otu.per)
  m.r = rbind(m.r,m.r.per)
}
m.r  = as.data.frame(m.r)
m.r$trophicMode = u
m.r.t = m.r %>% pivot_longer(cols = -trophicMode)
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
u = unique(m.all$trophicMode)

m.r = c()
for(i in 1:length(u)){ 
  ot = m.all[m.all$trophicMode == u[i],]$otu
  m.otu.per = m.otu2[intersect(ot,rownames(m.otu2)),]
  m.r.per = colSums(m.otu.per)
  m.r = rbind(m.r,m.r.per)
}
m.r  = as.data.frame(m.r)
m.r$trophicMode = u
m.r.t = m.r %>% pivot_longer(cols = -trophicMode)
m.g = g[match(unique(m.r.t$name),g$sample),] 


t.w.r = cbind(m.r.t,g[match(m.r.t$name,g$sample),])
t.w.r$g = paste(N1,N2,sep=".")
t.w.r$g2 = ifelse(t.w.r$habitat==N1,1,2)


all.per = rbind(m.s.r,m.se.r,m.t.r,m.w.r,
                se.t.r,se.w.r,
                s.se.r,s.t.r,s.w.r,
                t.w.r)

all.per$g = factor(all.per$g,levels = lab)


p3 = ggplot(all.per,aes(x = g2, y = value/1000,fill=trophicMode,color=trophicMode))+
  geom_bar(stat="identity",position = "stack",width=.75) +
  facet_wrap(.~g,nrow=1)+
  labs(x = "Habitats for fungal intermigration",y = "Average abundance (× 10^3)")+
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
