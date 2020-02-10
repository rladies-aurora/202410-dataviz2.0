#Dataviz2.0 - part4
#install.packages("ggsci")
#install.packages("ggsci")
#install.packages("tidyverse")
#install.packages("ggpubr")
#install.packages("ggrepel")
#install.packages("cowplot")


library("ggsci")
library(tidyverse)
library(ggpubr)
library(ggrepel)
library(cowplot)
library(ggrepel)

#Create the data
#We'll use the plots you've already created in part3: box1, ml_scatter, scatter2

## Data Import
gene_loc <- read.table("GSE69360.gene-locations.txt",
                       header = T)

## Plotting the data
scatter <- ggplot(gene_loc, aes(x=End-Start, y=Length, group=Chr, color=Chr)) +
  geom_point()
scatter

### It is hard to visualize the entire data.
### Let's pretend we are only interested in a small set of chromosomes.
### Let's subset the data and add a few variables!

target <- c("chrX", "chrY", "chrM", "chr17")
gene_loc2 <- filter(gene_loc, Chr %in% target)

log_EndStart <- log10(gene_loc2$End-gene_loc2$Start)
log_length <- log10(gene_loc2$Length)
gene_loc2$log_length <- log_length
gene_loc2$log_EndStart <- log_EndStart
head(gene_loc2)

###################################################

##Combine boxplot and regression plot previously created
### Boxplot
box1 <- ggplot(gene_loc2, aes(x = Chr, y = Length, 
                              group=Chr, color=Chr)) +
  geom_boxplot() +
  xlab("Chr")+
  theme_bw()

box1 <- box1 + scale_color_jco()+
  theme(legend.position = "none")                 # remove legend            
#box1 + scale_fill_discrete(name = "Chromosome")  #rename legend

## Scatterplot + regression line
ml_scatter <- ggscatter(gene_loc2, x = "log_EndStart", y = "log_length",
                        color = "Chr", palette = "jco",
                        add = "reg.line", add.params = list(color = "black"),    # customize regression line
                        fullrange - TRUE) +   
  facet_wrap(~Chr) +
  stat_cor(label.y = 4.4) +
  stat_regline_equation(label.y = 4.2)
ml_scatter

# arrange facet grid as 1x4
ml_scatter <- ml_scatter + facet_grid(cols = vars(Chr))

#Arranging boxplot and scatterplot on one page
## we’ll use the function ggarrange()[in ggpubr], which is a wrapper around the function plot_grid() [in cowplot package]. 
## Compared to the standard function plot_grid(), ggarange() can arrange multiple ggplots over multiple pages.
## use either

fig <- ggarrange(ml_scatter, box1 + rremove ("x.text"),
                  labels = c("A", "B"),
                  ncol =1, nrow = 2,
                 common.legend = TRUE)
fig


#Annotate the arranged figure

annotate_figure(fig,
                             top = text_grob("Visualizing gene length", color = "red", face = "bold", size = 14),  #grob -- graphical object
                             bottom = text_grob("Data source: \n gse69360 data set", color = "blue",         #customize text 
                                                hjust = 1, x = 1, face = "italic", size = 10),             # horizontal justification: 0= left align, 1= right align
                             left = text_grob("Figure arranged using ggpubr", color = "green", rot = 90),   # rot = angle to rotate the text
                             right = "I'm done, thanks :-)!",
                             fig.lab = "Figure 1", fig.lab.face = "bold"
)


#Adding descriptive text
#Text to be added
text <- paste("gse69360 data set is a resource of ribosomal",
              "RNA-depleted RNA-Seq data from different normal",
              "adults and fetal human tissues. The dataset was first",
              "published in https://www.nature.com/articles/sdata201563", sep = " ")
text.p <- ggparagraph(text = text, face = "italic", size = 11, color = "black")

final_plt <- ggarrange(ml_scatter, box1, text.p + rremove ("x.text"),
                 labels = c("A", "B"),
                 ncol =1, nrow = 3,
                 common.legend = TRUE)
final_plt

#Export plot
ggexport(final_plt, filename = "gene_lth_viz.pdf", width = 200, height = 200)


# Place scatterplot together with density plot
# Scatter plot colored by groups ("Chr")
sp <- gene_loc2 %>%
  ggscatter("log_EndStart", "log_length", 
            color = "Chr", palette = "jco",
            size = 1, alpha = 0.2)+
  border()                                         
# Marginal density plot of x (top panel) and y (right panel)
xplot <- gene_loc2 %>%
  ggdensity("log_EndStart", fill = "Chr", palette = "jco")

yplot <- gene_loc2 %>%
  ggdensity("log_length", fill = "Chr", palette = "jco")+
  rotate()
# Cleaning the plots
yplot <- yplot + clean_theme() 
xplot <- xplot + clean_theme()
# Arranging the plot
fig_2 <- ggarrange(xplot, NULL, sp, yplot, 
          ncol = 2, nrow = 2,  align = "hv", 
          widths = c(2, 1), heights = c(1, 2),
          common.legend = TRUE)

# Annotating the plot
scat_density_plt <- annotate_figure(fig_2,
                             top = text_grob("Scatter plot with marginal density", color = "red", face = "bold", size = 14),  #grob -- graphical object
                             bottom = text_grob("Data source: \n gse69360 data set", color = "blue",         #customize text 
                                                hjust = 1, x = 1, face = "italic", size = 10),             # horizontal justification: 0= left align, 1= right align
                             left = text_grob("Figure arranged using ggpubr", color = "green", rot = 90),   # rot = angle to rotate the text
                             right = "I'm done, thanks :-)!",
                             fig.lab = "Figure 1", fig.lab.face = "bold"
)

scat_density_plt

################################
#Insert a table into a plot
#Regression plot summary data with labels & confidence interval
scatter2 <- ggplot(a, aes(x = numGenes, y = meanLength)) +
  geom_point()+
  theme_bw()+
  geom_text_repel(aes(label = Chr), color="red", segment.color="blue")+
  geom_smooth(method = loess, color = "lightblue", alpha = 0.1)
scatter2


# Compute descriptive statistics by groups
stable <- gene_loc2 %>%
  desc_statby(measure.var = "Length",             #Column containing variable to be sumarized
              grps = "Chr")                       # Group
stable <- stable[, c("Chr", "length", "mean", "sd")]
# Summary table plot, medium blue theme
stable.p <- ggtexttable(stable, rows = NULL, 
                        theme = ttheme("mBlue"))

# Arranging the plot
fig_3 <- ggarrange(scatter2, stable.p, text.p + rremove ("x.text"),
                   ncol =1, nrow = 3,
                   heights = c(1, 0.5, 0.2),
                   common.legend = TRUE)
fig_3

#Place the table within the plot using annotate_custom in ggplot
fig_4 <- scatter2 + annotation_custom(ggplotGrob(stable.p),
                                      xmin = 2000, ymax = 1500,
                                      xmax = 5000)

# Annotating the plot
final_scatter2 <- annotate_figure(fig_4,
                                  top = text_grob("Scatter plot with summary table", color = "red", face = "bold", size = 14),  #grob -- graphical object
                                  bottom = text_grob("Data source: \n gse69360 data set", color = "blue",         #customize text 
                                                     hjust = 1, x = 1, face = "italic", size = 10),             # horizontal justification: 0= left align, 1= right align
                                  left = text_grob("Figure arranged using ggpubr", color = "green", rot = 90),   # rot = angle to rotate the text
                                  right = "I'm done, thanks :-)!",
                                  fig.lab = "Figure 1", fig.lab.face = "bold"
)

final_scatter2

#Export plot
ggexport(final_scatter2, filename = "mean_length.pdf", width = 200, height = 200)

