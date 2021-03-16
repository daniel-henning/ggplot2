###########################################################################
scatter.plot <- function(df, x, y, .scale = FALSE, group = NULL, palette = NULL, label.x = NULL, label.y = NULL){
  library(ggpubr)
  library(ggfortify)
  z.score = function(x){return((x - mean(x))/sd(x))}
  # normalization of varies x and y.
  if(.scale){df[,x] <- z.score(df[,x]); df[,y] <- z.score(df[,y])}
  # if group of samples was/was not applied.
  if(is.null(group)){
    sp <- ggscatter(df, x, y,
                    shape = 21, size = 2, # Points color, shape and size
                    add = "reg.line",  # Add regressin line
                    add.params = list(color = "blue", fill = "lightgray"), # Customize reg. line
                    conf.int = TRUE, # Add confidence interval
                    cor.coef = TRUE)  # Add correlation coefficient. see ?stat_cor)
  }else{
    sp <- ggscatter(df, x, y, color = group, add = "reg.line", conf.int = TRUE, palette = palette) +
      stat_cor(aes(color = get(group)), label.x = label.x, label.y = label.y)
  }
  return(sp)
}


