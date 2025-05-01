forest_plot = function(data, title){
  p <- ggplot(data, aes(x = OR, y = Index)) + 
    geom_vline(aes(xintercept = 1), size = .25, linetype = "dashed") + 
    geom_errorbarh(aes(xmax = highCI, xmin = lowCI), size = .5, height = 
                     .5, color = "gray50") +
    geom_point(size = 3.5, color = "orange") +
    theme_bw()+
    theme(panel.grid.minor = element_blank()) +
    ylab("") +
    xlab("Odds ratio") +
    ggtitle(title) + scale_x_continuous(trans='log10')
  p
}

forest_plot_tab = function(data, title){
  p <- ggplot(data, aes(x = OR, y = Index)) + 
    geom_vline(aes(xintercept = 1), size = .25, linetype = "dashed") + 
    geom_errorbarh(aes(xmax = highCI, xmin = lowCI), size = .5, height = 
                     .5, color = "gray50") +
    geom_point(size = 3.5, color = "orange") +
    theme_bw()+
    theme(panel.grid.minor = element_blank()) +
    ylab("") +
    xlab("Odds ratio") +
    ggtitle(title) + scale_x_continuous(trans='log10')
  table_base <- ggplot(data, aes(y=rev(forest$index))) +
    ylab(NULL) + xlab(NULL) + 
    theme(plot.title = element_text(hjust = 0.5, size=10), 
          axis.text.x = element_text(color="white", hjust = -0.5 ,margin = margin(t = 20, r = 0, b = 0, l = 0)), ## This is used to help with alignment
          axis.line = element_blank(),
          axis.text.y = element_blank(), 
          axis.ticks = element_blank(),
          axis.title.y = element_blank(), 
          legend.position = "none",
          panel.background = element_blank(), 
          panel.border = element_blank(), 
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(), 
          plot.background = element_blank())
  
  
  tab1 <- table_base +
    geom_text(aes(y = Index, x = 1, label = .), size = 3) + 
    ggtitle(".")
  
  tab2 <- table_base +
    geom_text(aes(y = Index, x = 1, label = ncontrols), size = 3) + 
    ggtitle("controls(%)")
  
  tab3 <- table_base +
    geom_text(aes(y = Index, x = 1, label = ncases), size = 3) + 
    ggtitle("heterozygotes(%)")
  
  tab4 <- table_base +
    geom_text(aes(y =Index, x = 1, label = formatC(as.numeric(p_value), format = "e", digits = 2)), size = 3) + 
    ggtitle("p-value")
  
  tab5 <- table_base +
    geom_text(aes(y =Index, x = 1, label = OR_CI), size = 3) + 
    ggtitle("OR [95% CI]")
  
  
  tab1 + p+ tab2+tab3 + tab5+tab4+ plot_layout(widths=c(15, 15,6,6,6,6,6))
}