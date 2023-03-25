# Figure 1F niche breadth

library(EcolUtils)
library(vegan)
library(tidyverse)
library(ggsignif)

gc()
v4.moss = read.csv(file = "Final data/parkv4.moss.csv",header=T,row.names = 1)
v4.se = read.csv(file = "Final data/parkv4.sediment.csv",header=T,row.names = 1)
v4.s = read.csv(file = "Final data/parkv4.soil.csv",header=T,row.names = 1)
v4.tr = read.csv(file = "Final data/parkv4.tree.csv",header=T,row.names = 1)
v4.w = read.csv(file = "Final data/parkv4.water.csv",header=T,row.names = 1)

v9.moss = read.csv(file = "Final data/parkv9.moss.csv",header=T,row.names = 1)
v9.se = read.csv(file = "Final data/parkv9.sediment.csv",header=T,row.names = 1)
v9.s = read.csv(file = "Final data/parkv9.soil.csv",header=T,row.names = 1)
v9.tr = read.csv(file = "Final data/parkv9.tree.csv",header=T,row.names = 1)
v9.w = read.csv(file = "Final data/parkv9.water.csv",header=T,row.names = 1)


v4.moss.n <- spec.gen(t(v4.moss), niche.width.method = 'levins', perm.method = 'quasiswap', n = 99, probs = c(0.025, 0.975))
v4.se.n <- spec.gen(t(v4.se), niche.width.method = 'levins', perm.method = 'quasiswap', n = 99, probs = c(0.025, 0.975))
v4.s.n <- spec.gen(t(v4.s), niche.width.method = 'levins', perm.method = 'quasiswap', n = 99, probs = c(0.025, 0.975))
v4.tr.n <- spec.gen(t(v4.tr), niche.width.method = 'levins', perm.method = 'quasiswap', n = 99, probs = c(0.025, 0.975))
v4.w.n <- spec.gen(t(v4.w), niche.width.method = 'levins', perm.method = 'quasiswap', n = 99, probs = c(0.025, 0.975))

v9.moss.n <- spec.gen(t(v9.moss), niche.width.method = 'levins', perm.method = 'quasiswap', n = 99, probs = c(0.025, 0.975))
v9.se.n <- spec.gen(t(v9.se), niche.width.method = 'levins', perm.method = 'quasiswap', n = 99, probs = c(0.025, 0.975))
v9.s.n <- spec.gen(t(v9.s), niche.width.method = 'levins', perm.method = 'quasiswap', n = 99, probs = c(0.025, 0.975))
v9.tr.n <- spec.gen(t(v9.tr), niche.width.method = 'levins', perm.method = 'quasiswap', n = 99, probs = c(0.025, 0.975))
v9.w.n <- spec.gen(t(v9.w), niche.width.method = 'levins', perm.method = 'quasiswap', n = 99, probs = c(0.025, 0.975))


v4.n = list(v4.moss.n$observed,v4.se.n$observed,
v4.s.n$observed,v4.tr.n$observed, v4.w.n$observed)

v4 <- do.call(cbind.data.frame,
              lapply(lapply(v4.n, unlist), `length<-`,
                            max(lengths(v4.n))))
colnames(v4) = c("Moss","Sediment","Soil","Tree hole","Water")
v4$Region = "V4"
v4.p = v4 %>% pivot_longer(cols = -Region) %>% na.omit()


v9.n = list(v9.moss.n$observed, v9.se.n$observed, 
v9.s.n$observed, v9.tr.n$observed,v9.w.n$observed)
v9 <- do.call(cbind.data.frame,
              lapply(lapply(v9.n, unlist), `length<-`,
                     max(lengths(v9.n))))
colnames(v9) = c("Moss","Sediment","Soil","Tree hole","Water")
v9$Region = "V9"
v9.p = v9 %>% pivot_longer(cols = -Region) %>% na.omit()

p.v4 = ggplot(data = v4.p,aes(x = name,y = value,fill = name,color=name))+
  geom_boxplot(alpha=.5,outlier.alpha = .1)+ 
  labs(x = "18S V4 rRNA gene",y = "Niche breadth")+
  scale_fill_manual(values = c("#339933","#990000","#CC9900","#663366","#99CCFF"))+
  scale_color_manual(values = c("#339933","#990000","#CC9900","#663366","#99CCFF"))+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5,size = 15),
        axis.text = element_text(size = 22),axis.title = element_text(size = 25),
        legend.text = element_text(size = 22),legend.title = element_text(size = 25))
p.v4
p.v9 = ggplot(data = v9.p,aes(x = name,y = value,fill = name,color=name))+
  geom_boxplot(alpha=.5,outlier.alpha = .1)+
  labs(x = "18S V9 rRNA gene",y = "Niche breadth")+
  scale_fill_manual(values = c("#339933","#990000","#CC9900","#663366","#99CCFF"))+
  scale_color_manual(values = c("#339933","#990000","#CC9900","#663366","#99CCFF"))+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5,size = 15),
        axis.text = element_text(size = 22),axis.title = element_text(size = 25),
        legend.text = element_text(size = 22),legend.title = element_text(size = 25))
p.v9


library(patchwork)
p = (p.v4 | p.v9) + plot_layout(guides='collect') &
  theme(legend.position="NULL")
p

ggsave("fig1f.pdf",plot = p,width = 500,height = 200,units = "mm")

