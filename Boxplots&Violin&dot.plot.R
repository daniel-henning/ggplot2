#### Loading required packages
ibrary(ggplot2)



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
