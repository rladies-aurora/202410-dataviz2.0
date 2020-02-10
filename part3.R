## DataViz 2.0
## Part 3: heatmaps and more!

## When to make a heatmap?
## When you have any two-dimensional matrix that you'd like to visualize!

## Reading in a subset of the gene count data | Chr22
highvar_genes <- read_tsv("data/chr22-gene-logcpm.txt")

highvar_genes_df <- highvar_genes %>%
  select(Geneid, Sample, Logcpm) %>%
  spread(Sample, Logcpm) %>%
  column_to_rownames(var="Geneid") %>%
  as.data.frame()

## using pheatmap
library(pheatmap); library(RColorBrewer)
pheatmap(highvar_genes_df, cluster_rows=F, cluster_cols=F)
pheatmap(highvar_genes_df)
pheatmap(highvar_genes_df,
         color=brewer.pal(n=9, name="YlGnBu"), # changing cell colors
         border_color="grey90") # changing border colors

## OTHER WAYS to generate heatmaps
## Using heatmap3
# library(heatmap3)
heatmap3(highvar_genes_df)
heatmap3(highvar_genes_df,
         col=brewer.pal(n=9, name="YlGn")) # go green!

## Heatmap using ggplot and geom_tile
## works with tidy data format
highvar_genes$Sample <- factor(highvar_genes$Sample)

ggplot(data=highvar_genes,
       aes(x=Sample, y=Geneid, fill=Logcpm)) +
  geom_tile(size=0.3) + ## to create heatmap
  scale_fill_gradient(low="white", high="darkred") + # color spec
  #scale_fill_distiller(palette = "YlGnBu") + # if using RColorBrewer
  scale_x_discrete(position="top") + # legend position
  theme_minimal() + #coord_flip() +
  theme(axis.text.x=element_text(angle=90,hjust=0,vjust=0.5))

# can use hclust for clustering
# can use ggdendro to add dendrograms