####################################################################
####  t-sne plot of a given data with label or without label    ####
####################################################################
tsne.plot <- function(train, dims = 2, perplexity = 30, max_iter = 1000, eta = 200, check_dup = TRUE, palette = "Dark2"){
  # using tsne
  require(ggplot2)
  require(RColorBrewer)
  require(Rtsne)
  set.seed(1234) # for reproducibility
  if (length(grep('label', colnames(train))) == 1){label.status <- TRUE}else{label.status <- FALSE}
  if (label.status){
    tsne <- Rtsne(train[,-which(colnames(train) == 'label')],
                  dims = 2, perplexity = perplexity, check_duplicates = check_dup,
                  verbose=TRUE, max_iter = max_iter, eta = eta)
    tsne.data <- as.data.frame(tsne$Y)
    colnames(tsne.data) <- c("Dimension1", "Dimension2")
    tsne.data$label <- train$label
    ggplot(data = tsne.data, aes(x = Dimension1, y = Dimension2, color = label)) +
      geom_point() + scale_color_brewer(palette = palette)
  }else{
    tsne <- Rtsne(train,dims = 2, perplexity = perplexity, check_duplicates = check_dup,
                  verbose=TRUE, max_iter = max_iter, eta = eta)
    tsne.data <- as.data.frame(tsne$Y)
    colnames(tsne.data) <- c("Dimension1", "Dimension2")
    tsne.data$label <- train$label
    ggplot(data = tsne.data, aes(x = Dimension1, y = Dimension2)) +
      geom_point() + scale_color_brewer(palette = palette)
  }
  
}
