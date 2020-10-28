
########################################################################
#                                                                      #
# AUTHOR: Daniel Gaspar Goncalves                                      #
# git: https://github.com/d-gaspar/R-functions                         #
# version: 0.0.1                                                       #
#                                                                      #
########################################################################

# library(ggplot2)

########################################################################

dgg_confusion_matrix = function(df, xlab, ylab, subtitle=NULL){
  
  cat(paste(
    "dgg_confusion_graph",
    "   AUTHOR: Daniel Gaspar Goncalves",
    "   git: https://github.com/d-gaspar/R-functions",
    "\n",
    sep="\n"))
  
  ####################################################################
  
  colnames(df)[1:2] = c("xlab", "ylab")
  
  df = as.data.frame(table(df))
  df$Perc = numeric(4)
  
  # perc
  for(i in levels(df[,1])){
    for(j in levels(df[,2])){
      df[df[,1]==i & df[,2]==j,"Perc"] = df[df[,1]==i & df[,2]==j,"Freq"] / sum(df[df[,1]==i,"Freq"])
    }
  }
  df$Perc = round(df$Perc*100, 2)
  df$Text = paste0(as.character(df$Freq), "\n(", as.character(df$Perc), "%)")
  
  p = ggplot(data=df, mapping=aes(x=xlab, y=ylab)) +
    geom_tile(aes(fill=Freq), color="white") +
    geom_text(aes(label=Text), vjust=0.5, size=10) +
    scale_fill_gradient(low="#f0f0f0", high="#636363") +
    theme(
      legend.position="none",
      text = element_text(size=20),
      panel.background = NULL,
      plot.subtitle = element_text(size=14)
    ) +
    labs(
      title = "Confusion Matrix",
      subtitle = subtitle,
      x = xlab,
      y = ylab
    )

  ####################################################################
  
  return(p)
  
}
