###################################################################################
####           compare continuous  variable by classified variable             ####
###################################################################################
contiV2class <- function(df, x = "Class1", y = "Class2"){
  library(plyr)
  library(scales)
  library(ggpubr)
  dt <- ddply(group_by(count(df, c(x,y)),get(x)), x, summarise, 
              class = get(y), count = freq, Percent = count/sum(count), 
              Percentage = paste0(formatC(count*100/sum(count), digits = 2), "%"))
  
  fisher.res <- fisher.test(table(df[,c(x, y)]))
  
  ggplot(data=dt, aes(x = get(x), y = Percent, fill = class)) +   
    geom_bar(position=position_fill(reverse=FALSE), stat = "identity", width=0.7) +
    ggtitle(paste0("Fisher test p.value = ", round(fisher.res$p.value, digits = 4))) + 
    labs(x = x, y = "Percent", fill = y) +
    scale_fill_brewer(palette="Dark2") +
    geom_text(aes(label = paste0(Percentage,"(", count,")")),position = position_stack(vjust = 0.5)) + 
    theme_pubr()
  
}

####################################################################################
biBarPlot <- function(df, x, y, fill = NA, title = NULL, xlab = NULL, ylab = NULL){
  easypackages::libraries(c("ggplot2"))
  commapos <- function(x, ...){format(abs(x), big.mark = ",", trim = TRUE,scientific = FALSE, ...)}
  if(!is.null(fill)){
    ggplot(data=dane,aes(x = x,y = y, fill = get(fill))) +
      geom_bar(stat="identity") +
      #labs(title = title, x = xlab, y = ylab)  +
      guides(fill = guide_legend(title = fill)) +
    scale_y_continuous(labels = commapos) +
      coord_flip()
  }else{
    ggplot(data=dane,aes(x = x,y = y)) +
      geom_bar(stat="identity") +
      #labs(title = title, x = xlab, y = ylab) +
    scale_y_continuous(labels = commapos) +
      coord_flip()
  }
}
