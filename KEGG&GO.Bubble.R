##############################################################
####define a function to draw bubble plot of kegg or go.  ####
##############################################################
KGBubble <- function(pathway = pathway, 
                     outdir = outdir, 
                     x.label = "GeneRatio",
                     KEGG = TRUE,
                     qvalue = FALSE, 
                     l.col = 'blue',
                     h.col = 'red', 
                     width = 10, 
                     height = 6){
  if(!require("ggplot2")){BiocManager::install('ggplot2')}
  ## TRANSFORM THE GENERATIO IN KEGG OR GO RESILTS TO FLOAT FORMAT.
  GR = c(rep(0,dim(pathway)[1]))
  for (i in 1:dim(pathway)[1]) {
    gn <- as.integer(strsplit(as.character(pathway$GeneRatio[i]),'/')[[1]][1])
    bn <- as.integer(strsplit(as.character(pathway$GeneRatio[i]),'/')[[1]][2])
    GR[i] <-  gn/bn
  }
  pathway$GeneRatio <- GR
  ####USING PVALUE OR QVALUE TO DRAW PLOTS.
  ####
  if (qvalue){
    if (KEGG){
      ggplot(pathway,aes(x=GeneRatio,y=reorder(Description,GeneRatio))) +
        geom_point(aes(size=Count,color=-1*log10(pvalue))) +
        scale_colour_gradient(low = l.col,high = h.col) +
        labs(color=expression(-log[10](pvalue)),
             size="Gene number",
             x="GeneRatio",
             y="Pathway name",
             title="Pathway enrichment") +
        theme_bw()+
        theme(axis.text.y = element_text(size = rel(1.3)),
              axis.title.x = element_text(size=rel(1.3)),
              axis.title.y = element_blank())
      
      ## SAVING IMAGE.
      outputFile = paste(outdir,"KEGG_enrichMent_bubble_plot.pdf", sep='/')
      ggsave(outputFile, device = "pdf", width = width, height = height, units = "cm")
    }else{
      ggplot(pathway,aes(x=GeneRatio,y=reorder(Description,GeneRatio))) +
        geom_point(aes(size=Count,color=-1*log10(pvalue))) +
        scale_colour_gradient(low = l.col,high = h.col) +
        labs(color=expression(-log[10](pvalue)),
             size="Gene number",
             x="GeneRatio",
             y="Pathway name",
             title="Pathway enrichment") +
        theme_bw()+
        theme(axis.text.y = element_text(size = rel(1.3)),
              axis.title.x = element_text(size=rel(1.3)),
              axis.title.y = element_blank())
      ## SAVING IMAGE.
      outputFile = paste(outdir, "GO_enrichMent_bubble_plot.pdf", sep='/')
      ggsave(outputFile, device = "pdf", width = width, height = height, units = "cm")
      
      ####
      df <- data.frame(Description = pathway$Description,qvalue = pathway$qvalue,GO_term = pathway$GO_term)
      df$qvalue <- -log2(with(df, qvalue))
      df <- df[with(df,order(qvalue,decreasing = TRUE)),]
      #df <- df[1:50,]
      p <- ggplot(df,aes(x = interaction(Description,GO_term),y = qvalue,fill = GO_term))+ geom_bar(stat = "identity")+coord_flip()
      p <- p+xlab("GO Term Description (top50)")+ylab("P-value (-log2)")+labs(fill="GO_term")+
        theme(axis.text.x=element_text(size=10,colour="black", angle = 45),
              axis.text.y=element_text(colour = "black", vjust = 0.5))
      outputFile = paste(outdir, "GO_enrichment_bar_plot.pdf", sep='//')
      ggsave(outputFile,plot = p, device = "pdf", width = width, height = height, units = "cm")
    }
  }else{
    if (KEGG){
      ggplot(pathway,aes(x=GeneRatio,y=reorder(Description,GeneRatio))) +
        geom_point(aes(size=Count,color=-1*log10(pvalue))) +
        scale_colour_gradient(low = l.col,high = h.col) +
        labs(color=expression(-log[10](pvalue)),
             size="Gene number",
             x="GeneRatio",
             y="Pathway name",
             title="Pathway enrichment") +
        theme_bw()+
        theme(axis.text.y = element_text(size = rel(1.3)),
              axis.title.x = element_text(size=rel(1.3)),
              axis.title.y = element_blank())
      ## SAVE IAMGE.
      outputFile = paste(outdir,"KEGG_enrichMent_bubble_plot.pdf", sep='/')
      ggsave(outputFile, device = "pdf", width = width, height = height, units = "cm")
    }else{
      ggplot(pathway,aes(x=GeneRatio,y=reorder(Description,GeneRatio))) +
        geom_point(aes(size=Count,color=-1*log10(pvalue))) +
        scale_colour_gradient(low = l.col,high = h.col) +
        labs(color=expression(-log[10](pvalue)),
             size="Gene number",
             x="GeneRatio",
             y="Pathway name",
             title="Pathway enrichment") +
        theme_bw()+
        theme(axis.text.y = element_text(size = rel(1.3)),
              axis.title.x = element_text(size=rel(1.3)),
              axis.title.y = element_blank())
      ## SAVE IMAGE.
      outputFile = paste(outdir, "GO_enrichMent_bubble_plot.pdf", sep='/')
      ggsave(outputFile, device = "pdf", width = width, height = height, units = "cm")
      ####
      df <- data.frame(x = pathway$Description,y = pathway$pvalue,z = pathway$GO_term)
      df$y <- -log2(with(df, y))
      df <- df[with(df,order(y,decreasing = TRUE)),]
      #df <- df[1:50,]
      p<-ggplot(df,aes(x = interaction(x,z),y = y,fill = z))+ geom_bar(stat = "identity")+coord_flip()
      p+xlab("GO Term Description (top50)")+ylab("P-value (-log2)")+labs(fill="GO_term")+
        theme(axis.text.x=element_text(size=10,colour="black"),
              axis.text.y=element_text(colour = "black",vjust=0.5))
      outputFile = paste(outdir, "GO_enrichment_bar_plot.pdf", sep='//')
      ggsave(outputFile, device = "pdf", width = width, height = height, units = "cm")
    }
  }
}
