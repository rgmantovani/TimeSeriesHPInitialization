#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------

getRuntimePlot = function(df) {

  temp = na.omit(df)
  rownames(temp) = NULL
  temp$data.id = as.numeric(as.factor(as.character(temp$dataset)))
  temp$data.id = as.factor(temp$data.id)
  df.melted = melt(temp, id.vars = c(1, 2, ncol(temp)))

  g = ggplot(df.melted, aes(x = data.id, y = log(value), color = variable, group = variable,
    linetype = variable))
  g = g + geom_line()
  g = g + ylab(" time in ms (log scale) ") 
  g = g + xlab("dataset id")
  g = g + scale_color_manual(values = custom.line.colors)
  g = g + theme_bw()
  g = g + scale_linetype_manual(values = custom.line.types) 
  g = g + theme(legend.background = element_rect(colour = "black"))
  g = g + theme(legend.title = element_blank(), legend.text = element_text(size = 8))
  g = g + theme(legend.key.height = unit(0.3, "cm"), legend.key.width = unit(0.4, "cm"))
  g = g + theme(axis.text=element_text(size=6.5))
 
  return(g)
}

#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------
