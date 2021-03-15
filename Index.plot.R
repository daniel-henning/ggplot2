###########################################################################
# Index (or shift) plot of gene expression or specific variables for multi group.
index.dotplot <- function(df, vars, group, log2 = FALSE){
  # vars: the variable name, such as “BRCA1” in an expression matrix.
  # group: the factors group names, such as "corhorts" in differential tumor panel.
  df <- df[,c(vars, group)]
  if(log2){df[,vars] <- log2(df[,vars] + 1)}
  df$index <- NA
  for (c in unique(df[,group])) {
    idx <- which(df[,group] == c)
    if(!exists("median.val")){
      median.val <- median(df[,vars][idx])
    }else{median.val <- c(median.val, median(df[,vars][idx])) }
    df$index[idx][order(df[,vars][idx])] <- seq(along = df[,vars][idx])
  }; names(median.val) <- unique(df[,group])
  #df <- df[order(df[,"APM"], df$index),]
  df[,group] <- factor(df[,group], levels = names(sort(median.val)))
  p <- ggplot(df, aes(x = index, y = get(vars))) +
    geom_point(aes(color = get(group))) +
    theme_classic() +
    facet_grid(cols = vars(get(group)), scales = "free") +
    theme(legend.position = 'none',
          #axis.title.x = element_blank(),
          axis.text.x  = element_blank(),
          axis.line.x  = element_line(colour = "black"),
          axis.ticks.x = element_blank(),
          #panel.grid.major = element_blank(),
          #panel.grid.minor = element_blank(),
          #panel.background = element_rect(fill = NA),
          panel.border = element_blank(),
          strip.background.x = element_blank()) +
    xlab(group) + ylab(vars)
  return(p)
}


