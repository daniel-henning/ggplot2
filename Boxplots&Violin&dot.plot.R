#### Loading required packages
library(ggplot2)

###################################################################
# geom_boxplot()
ggplot(data = iris) +
  geom_boxplot(aes(x=Species, y=Sepal.Length, fill = Species), width = .5, outlier.shape = NA) + 
  #geom_jitter(shape=16, position=position_jitter(0.2)) + 
  guides(fill = guide_legend(title = "Species"))

###################################################################
# geom_violin()
ggplot(data = iris) +
  geom_violin(aes(x=Species, y=Sepal.Length, fill = Species), width = .5, outlier.shape = NA) + 
  #geom_jitter(shape=16, position=position_jitter(0.2)) + 
  guides(fill = guide_legend(title = "Species"))

###################################################################
# geom_jitter()
ggplot(data = iris) +
  geom_jitter(aes(x=Species, y=Sepal.Length, color = Species), outlier.shape = NA, hape=16, position=position_jitter(0.2)) + 
  guides(fill = guide_legend(title = "Species"))

###################################################################
# geom_dotplot()
ggplot(data = iris) +
    geom_dotplot(aes(y = Sepal.Length, x = Species, fill = Species),
                 binaxis = "y",         # which axis to bin along
                 binwidth = 0.05,       # Minimal difference considered diffeerent
                 stackdir = "center"    # Centered
                 )


# Additing median line
ggplot(data = iris, aes(y = Sepal.Length, x = Species)) + # Move y and x here so than they can be used in stat_*
    geom_dotplot(aes(fill = Species),   # Use fill = Species here not in ggplot()
                 binaxis = "y",         # which axis to bin along
                 binwidth = 0.1,        # Minimal difference considered diffeerent
                 stackdir = "center"    # Centered
                 ) +
    stat_summary(fun.y = median, fun.ymin = median, fun.ymax = median,
                 geom = "crossbar", width = 0.5)
