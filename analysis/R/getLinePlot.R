#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------

getLinePlot = function(df, measure = "mean", put.annotations = TRUE) {

  # omit missing values
  df = na.omit(df)
  rownames(df) = NULL
  df$data.id = as.numeric(as.factor(as.character(df$dataset)))
  df$data.id = as.factor(df$data.id)

  df.melted = melt(df, id.vars = c(1, 2, ncol(df)))
  g = ggplot(df.melted, aes(x = data.id, y = value, color = variable, group = variable,
    linetype = variable))
  g = g + geom_line()

  if(measure == "mean") {
    g = g + ylab("(balanced) accuracy)") 
  } else {
    g = g + ylab("max (balanced) accuracy)") 
  }

  g = g + xlab("dataset id")
  g = g + scale_color_manual(values = custom.line.colors)
  g = g + theme_bw()
  g = g + scale_y_continuous(breaks = c(0,0.2,0.4,0.6,0.8,1))
  g = g + scale_linetype_manual(values = custom.line.types) 
  # g = g + theme(legend.position = c(.06,.49))
  g = g + theme(legend.background = element_rect(colour = "black"))
  g = g + theme(legend.title = element_blank(), legend.text = element_text(size = 8))
  g = g + theme(legend.key.height = unit(0.3, "cm"), legend.key.width = unit(0.4, "cm"))
  g = g + theme(axis.text=element_text(size=6.5))
 
  # add annotations
  if(put.annotations) {
    for(i in 1:nrow(df)) {
      g = g + annotate("text", x = i, y=0.1, label = df[i, "Best"], size = 2, 
        angle = 90, hjust = 0)
    }
  }
  return(g)
}

#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------
