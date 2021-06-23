library(ggalluvial)
library(ggplot2)
library(tidyverse)
library(factoextra)
library(rmarkdown)
x<- read.csv('./dat/female_pop_2050_Western_Africa.csv', header = T)


x$stat<- factor(x$stat, levels = c(    "L95" ,"L80","median",    "U80",    "U95"))
# Flowplot/ Alluvial Plot
ggplot(x, aes(x=stat, y=value, stratum = reorder(name,value), alluvium = name,
              fill = reorder(name,value), label = name))+scale_x_discrete(expand = c(.1, .1)) +
  geom_flow() +
  geom_stratum(alpha = .5) +
  geom_text(stat = "stratum", size = 2) +
  theme(legend.position = "bottom")+ labs(fill="Countries")+xlab(NULL)+ylab("Projected Population")+
  scale_fill_viridis_d()
#ggsave("plot2.png",height=8,width=7,units="in",dpi=720)


# Remove Nigeria
ggplot(x[which(x$name!="Nigeria"),], aes(x=stat, y=value, stratum = reorder(name,value), alluvium = name,
              fill = reorder(name,value), label = name))+scale_x_discrete(expand = c(.1, .1)) +
  geom_flow() +
  geom_stratum(alpha = .5) +
  geom_text(stat = "stratum", size = 2) +
  theme(legend.position = "bottom")+ labs(fill="Countries")+xlab(NULL)+ylab("Projected Population")+
  scale_fill_viridis_d()
  

# Make Ranks within as lines
x1<- x %>%
  group_by(stat) %>%
  mutate(my_ranks = rank(-value, ties.method = "first"))

ggplot(x1, aes(x=stat, y=my_ranks))+
  geom_point(aes(size=log10(value), colour=reorder(name,my_ranks)))+
  geom_line(aes(group=name, colour=reorder(name,my_ranks)))+
  labs(colour="Countries")+xlab(NULL)+ylab("Estimated ranks by statistic")+
  scale_fill_viridis_d()+ labs(size="Projected Population (log 10 scaled)")+
  guides(color = guide_legend(order=1),
         size = guide_legend(order=2))+theme(legend.position = "bottom",legend.box="vertical", legend.margin=margin())+
  guides(colour=guide_legend(nrow=4,byrow=TRUE))+
  scale_y_continuous(breaks = seq(1:16))

#ggsave("plot1.png",height=8,width=7,units="in",dpi=720)
# Remove Nigeria
ggplot(x1[which(x1$name!="Nigeria"),], aes(x=stat, y=my_ranks))+
  geom_point(aes(size=log10(value), colour=reorder(name,my_ranks)))+
  geom_line(aes(group=name, colour=reorder(name,my_ranks)))+
  labs(colour="Countries")+xlab(NULL)+ylab("Estimated ranks by statistic")+
  scale_fill_viridis_d()+ labs(size="Projected Population (log 10 scaled)")+
  guides(color = guide_legend(order=1),
         size = guide_legend(order=2))+theme(legend.position = "bottom",legend.box="vertical", legend.margin=margin())+
  guides(colour=guide_legend(nrow=2,byrow=TRUE))+
  scale_y_continuous(breaks = seq(1:16))
# PCA Plots
x_wide<- x1 %>% pivot_wider(id_cols =name,
                            names_from=stat, values_from=c(my_ranks,value))

x_w<-column_to_rownames(x_wide, var = "name") 


# PCR based on values similar
res.pca <- prcomp(x_w[c("value_L95"       ,"value_L80"       ,"value_median"    ,"value_U80"       ,"value_U95")], scale = TRUE)
fviz_eig(res.pca)
fviz_pca_ind(res.pca,
             col.ind = "cos2", # Color by the quality of representation
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE,# Avoid text overlapping
             pointsize = x_w$value_median
)


fviz_pca_var(res.pca,
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)

fviz_pca_biplot(res.pca, repel = TRUE,
                col.var = "#2E9FDF", # Variables color
                col.ind = "#696969"  # Individual countries color
)
