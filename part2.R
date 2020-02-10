## DataViz 2.0 Workshop
## Part 2

gene_loc <- read.table("~/Desktop/tidyverse-genomics-master/data/GSE69360.gene-locations.txt",
                  header = T)


scatter<-
  ggplot(gene_loc ,aes(x = End-Start, y = Length, group=Chr, color=Chr)) +
  geom_point()
scatter

### it is hard to visualize all the data. Let pretend we are only interested
### in a small gorup of chromosomes
### Let's subset the data and add some variables

target <- c("chrX","chrY", "chrM", "chr17")
gene_loc2<-filter(gene_loc, Chr %in% target)

log_EndStart<-log10(gene_loc2$End-gene_loc2$Start)
log_length<-log10(gene_loc2$Length)
gene_loc2$log_length<-log_length
gene_loc2$log_EndStart<-log_EndStart
head(gene_loc2)

## Now lets check the new scatter plot... mmm still no the best
scatter<-
  ggplot(gene_loc2 ,aes(x = End-Start, y = Length, group=Chr, color=Chr)) +
  geom_point()
scatter

## the gray background is anoying ... remove it!
scatter<-
  ggplot(gene_loc2 ,aes(x = End-Start, y = Length, group=Chr, color=Chr)) +
  geom_point() +
  theme_bw()
scatter

### recap, try a different geometry .. by yourself!
scatter2<-
  ggplot(gene_loc2 ,aes(x = Chr, y = Length, group=Chr, color=Chr)) +
  geom_boxplot() +
  theme_bw()
scatter2

## adjust the axes
scatter<-
  ggplot(gene_loc2 ,aes(x = End-Start, y = Length, group=Chr, color=Chr)) +
  geom_point() +
  theme_bw() +
  xlim(0, 2500)+ ylim(0, 10000)
scatter

### where are the green dots?
scatter3<-
  ggplot(gene_loc2 ,aes(x = End-Start, y = Length, group=Chr, color=Chr)) +
  geom_point(alpha = 0.7, size =0.5) +
  theme_bw() +
  xlim(0, 2500)+ ylim(0, 10000)
scatter3

## Did it change? compare the plots side-by-side
ggarrange(scatter, scatter3,
          labels = c("A", "B"),
          ncol = 2, nrow = 1)

## transformed the axes.. Thats better.. isny it?
trans_scatter <- scatter +
  scale_x_log10("End-Start") +
  scale_y_log10("Gene length") +
  theme_minimal()
trans_scatter

## You want to add the regression lines.. So, lets do a multiple regression
scatter1<- ggplot(gene_loc2, aes(x = log_EndStart, y = log_length, color=Chr)) +
  geom_point() +
  theme_bw() +
  geom_smooth(method=lm,  se=FALSE)
scatter1

## We can see the lines clearly. Can you thing in a solution?
scatter2<- ggplot(gene_loc2, aes(x = log_EndStart, y = log_length, color=Chr)) +
  geom_point(size =1, alpha = 0.2) +
  geom_smooth(method=lm, se=FALSE) +
  theme_bw()
scatter2

##Can you put them together in the same graph to compare?
ggarrange(scatter1, scatter2,
          labels = c("A", "B"),
          ncol = 2, nrow = 1)


## Now, lets add some numerical values to the graph. Like R^2
scatter<-ggplot(gene_loc2, aes(x = log_EndStart, y = log_length, color = Chr))+
  geom_point() +
  theme_bw() +
  geom_smooth(method = lm, se = FALSE)+
  ggpubr::stat_cor()
scatter

## Now, lets add some numerical values to the graph. Linear quation
scatter<-ggplot(gene_loc2, aes(x = log_EndStart, y = log_length, color = Chr))+
  geom_point() +
  geom_smooth(method = lm, se = FALSE)+
  ggpubr::stat_regline_equation()
scatter

## Your boss wants to see the lines in different plots!
## multiple regression with equation and r2 different plots

ml_scatter<- ggscatter(gene_loc2, x = "log_EndStart", y = "log_length",
  color = "Chr", palette = "jco",
  add = "reg.line") +
  facet_wrap(~Chr) +
  stat_cor(label.y = 4.4) +
  stat_regline_equation(label.y = 4.2)
ml_scatter

## labeling a point in a scatterplot..

scatter<-
  ggplot(gene_loc2 ,aes(x = End-Start, y = Length, group=Chr, color=Chr)) +
  geom_point()
scatter

## that gene in the corner looks interesting!! What gene is it?

scatter<-
  ggplot(gene_loc2, aes(x = End-Start, y = Length, group=Chr, color=Chr)) +
  geom_point()+
  geom_text(label=gene_loc2$Geneid, size = 2, color="black")
scatter

## second example!! Laballing point and adding confidence interval to a regresion.

a<-gene_loc %>%
  group_by(Chr) %>%
  summarize(meanLength = mean(Length), numGenes = n())
head(a)

scatter2<-ggplot(a, aes(x = numGenes, y = meanLength)) +
  geom_point()+
  theme_bw()
scatter2

## which chromosome is represented by which point?

scatter2<-ggplot(a, aes(x = numGenes, y = meanLength)) +
  geom_point()+
  theme_bw()+
  geom_text(label=a$Chr, size = 2, color="black")
scatter2

## geom_text_repel is a better function!
scatter2<-ggplot(a, aes(x = numGenes, y = meanLength)) +
  geom_point()+
  theme_bw()+
  geom_text_repel(aes(label = Chr), color="red", segment.color="blue")
scatter2

## add cofidence interval
scatter2<-ggplot(a, aes(x = numGenes, y = meanLength)) +
  geom_point()+
  theme_bw()+
  geom_text_repel(aes(label = Chr), color="red", segment.color="blue")+
  geom_smooth(method = loess, color = "lightblue", alpha = 0.1)
scatter2



