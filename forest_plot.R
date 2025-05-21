forest_plot_simple = function(data, title, colname){
  p <- ggplot(data, aes(x = OR, y = {{colname}}, col = study, fill = study)) + 
    geom_vline(aes(xintercept = 1), size = .25, linetype = "dashed") + 
    geom_errorbarh(aes(xmax = highCI, xmin = lowCI), size = .5, height = 
                     .5, color = "gray50", position=position_dodge(width = 1)) +
    geom_point(size = 3.5, position=position_dodge(width = 1)) +
    theme_bw()+
    theme(panel.grid.major.y = element_line(color = "grey90", size = 0.5),
          panel.grid.minor = element_blank(),
          panel.background = element_blank())+
    ylab("") +
    xlab("Odds ratio") +
    scale_color_manual(values = c("dodgerblue3", "gold"))+
    ggtitle(title) + scale_x_continuous(trans='log10')
  p
}

forest_plot_tab = function(data, title, colname){
  data <- data %>%
    arrange({{colname}}, study) %>%
    mutate(
      row_id = -as.numeric(factor({{colname}}, levels = unique({{colname}}))),
      dodge = ifelse(study == "ghs", -0.2, 0.2),
      y_pos = row_id + dodge,
      y_label = reorder({{colname}}, row_id)
    )
  
  bg_rects <- data %>%
    distinct(row_id) %>%
    filter(row_id %% 2 == 0) %>%
    mutate(ymin = row_id - 0.5, ymax = row_id + 0.5)
  
  p <- ggplot(data) +
    geom_rect(data = bg_rects,
              aes(ymin = ymin, ymax = ymax),
              xmin = -Inf, xmax = Inf,
              fill = "grey95",
              inherit.aes = FALSE) +
    geom_vline(xintercept = 1, linetype = "dashed") +
    geom_errorbarh(aes(xmin = lowCI, xmax = highCI, y = y_pos),
                   height = 0.2, color = "gray40") +
    geom_point(aes(x = OR, y = y_pos, color = study), size = 3) +
    scale_y_continuous(
      breaks = unique(data$row_id),
      labels = unique(data$y_label),
      expand = expansion(mult = c(0.01, 0.01))
    ) +
    scale_x_continuous(trans = 'log10') +
    scale_color_manual(values = c("dodgerblue3", "gold")) +
    theme_minimal() +
    theme(panel.grid.major.y = element_blank(),
          legend.position = "right") +
    xlab("Odds ratio") +
    ylab("") +
    ggtitle(title)
  
  # Table base with dodged y_pos
  table_base <- ggplot(data, aes(y = y_pos)) +
    ylab(NULL) + xlab(NULL) + 
    theme(plot.title = element_text(hjust = 0.5, size = 10), 
          axis.text.x = element_text(color = "white", hjust = -0.5, margin = margin(t = 20)),
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
    geom_text(aes(x = 1, label = study), size = 3) + 
    ggtitle("Study")
  
  tab2 <- table_base +
    geom_text(aes(x = 1, label = ncontrols), size = 3) + 
    ggtitle("controls(%)")
  
  tab3 <- table_base +
    geom_text(aes(x = 1, label = ncases), size = 3) + 
    ggtitle("heterozygotes(%)")
  
  tab4 <- table_base +
    geom_text(aes(x = 1, label = formatC(as.numeric(p_value), format = "e", digits = 2)), size = 3) + 
    ggtitle("p-value")
  
  tab5 <- table_base +
    geom_text(aes(x = 1, label = OR_CI), size = 3) + 
    ggtitle("OR [95% CI]")
  
  # Combine everything
  p + tab1 + tab2 + tab3 + tab5 + tab4 + plot_layout(widths = c(15, 6, 6, 6, 6, 6, 6))
}


forest_plot = function(data, title, colname){
  
  data <- data %>%
    arrange({{colname}}, study) %>%
    mutate(row_id = -as.numeric(factor({{colname}}, levels = unique({{colname}}))),
           dodge = ifelse(study == "ghs", -0.2, 0.2),
           y_pos = row_id + dodge,
           y_label = reorder({{colname}}, row_id))
  
  bg_rects <- data %>%
    distinct(row_id) %>%
    filter(row_id %% 2 == 0) %>%
    mutate(ymin = row_id - 0.5, ymax = row_id + 0.5)
  
  ggplot(data) +
    # Background shading
    geom_rect(data = bg_rects,
              aes(ymin = ymin, ymax = ymax),
              xmin = -Inf, xmax = Inf,
              fill = "grey95",
              inherit.aes = FALSE) +
    
    # Reference line
    geom_vline(xintercept = 1, linetype = "dashed") +
    
    # Error bars
    geom_errorbarh(aes(xmin = lowCI, xmax = highCI, y = y_pos),
                   height = 0.2, color = "gray40") +
    
    # Points
    geom_point(aes(x = OR, y = y_pos, color = study), size = 3) +
    
    # Axis setup
    scale_y_continuous(
      breaks = unique(data$row_id),
      labels = unique(data$y_label),
      expand = expansion(mult = c(0.01, 0.01))
    ) +
    scale_x_continuous(trans='log10')+
    
    scale_color_manual(values = c("dodgerblue3", "gold")) +
    
    theme_minimal() +
    theme(panel.grid.major.y = element_blank(),
          legend.position = "right") +
    xlab("Odds ratio") +
    ylab("") +
    ggtitle(title)
}

table_plot = function(data, title) {
  data%>%
    dplyr::arrange(Index)%>%
    mutate(case = label_cases,
           control = label_controls,
           label = paste0(Index, ":", icd10),
           p_value = format(round(as.numeric(p_value), 3), scientific=TRUE))%>%
    select(label, study, control, case, p_value)%>%
    kableExtra::kable(col.names = c("ICD10 Code", "Study", "Controls, n(%)", "Cases, n(%)", "P-Value"))%>%
    collapse_rows(columns = 1:2, valign = "top")%>%
    kable_paper(full_width = F)
}

forest_plot_tab2 <- function(data, title) {
  data$Index <- factor(data$Index, levels=rev(data$Index))
  
  p <- ggplot(data, aes(x = OR, y = Index, color=study)) + 
    geom_vline(aes(xintercept = 1), size = .25, linetype = "dashed") + 
    geom_errorbarh(aes(xmax = highCI, xmin = lowCI), size = .5, height = 
                     .5, color = "gray50") +
    geom_point(size = 3.5) + scale_color_manual(values = c("ukbb"="deepskyblue", "ghs"="orange"))+
    theme_bw()+
    theme(panel.grid.minor = element_blank()) +
    ylab("") +
    xlab("Odds ratio") +
    ggtitle(title) + scale_x_continuous(trans='log10') + scale_y_discrete(position="right")
  p
  
  
  table_base <- ggplot(data, aes(y=rev(data$Index))) +
    ylab(NULL) + xlab(NULL) + 
    theme(plot.title = element_text(hjust = 0.5, size=12), 
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
    labs(title = "space") +
    geom_text(aes(y = Index, x = 1,label = OR), size = 4) +
    ggtitle("OR")
  
  tab2 <- table_base +
    geom_text(aes(y = Index, x = 1, label = label_controls), size = 3) + 
    ggtitle("controls(%)")
  
  tab3 <- table_base +
    geom_text(aes(y = Index, x = 1, label = label_cases), size = 3) + 
    ggtitle("heterozygotes(%)")
  
  tab4 <- table_base +
    geom_text(aes(y =Index, x = 1, label = p_value), size = 3) + 
    ggtitle("p-value")
  
  tab5 <- table_base +
    geom_text(aes(y =Index, x = 1, label = OR_CI), size = 3) + 
    ggtitle("OR [95% CI]")
  
  tab6 <- table_base +
    geom_text(aes(y =Index, x = 1, label = paste0(Index, ": ", icd10)), size = 3) + 
    ggtitle("ICD10")
  
  tab2+tab3 + tab5+tab4+p + tab6 + plot_layout(widths=c(2,2,2,2,10,10))
  
  tab6 + p +tab2 + tab3+tab5+tab4 + plot_layout(widths=c(10,10,3,3,3,2))
  
}

format_data = function(data){
  data = data %>%
    arrange(Index)%>%
    mutate(Index = factor(Index, levels=rev(Index)),
           OR = as.numeric(OR),
           lowCI = as.numeric(lowCI),
           highCI = as.numeric(highCI),
           p_value = formatC(as.numeric(p_value), format = "e", digits = 2),
           pcases = as.numeric(pcases),
           pcontrols = as.numeric(pcontrols),
           label_controls = ifelse(!is.na(ncontrols), paste0(ncontrols, " (", round(pcontrols, 2), ")"), "NA"),
           label_cases = ifelse(!is.na(ncases), paste0(ncases, " (", round(pcases, 2), ")"), "NA"),
           OR_CI = case_when(
             !is.na(OR) & is.numeric(OR) ~ paste0(round(as.numeric(OR), 2), "[", round(as.numeric(lowCI), 2), "-", round(as.numeric(highCI), 2), "]"),
             TRUE ~ "NA " # Use NA_character_ to explicitly specify NA for character columns
           ))
}


forest_plot_single = function(data, title){
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
  
  
  table_base <- ggplot(data, aes(y=rev(data$index))) +
    ylab(NULL) + xlab(NULL) + 
    theme(plot.title = element_text(hjust = 0.5, size=12), 
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
    geom_text(aes(y = Index, x = 1, label = icd10), size = 4) + 
    ggtitle(".")
  
  tab2 <- table_base +
    geom_text(aes(y = Index, x = 1, label = ncontrols), size = 4) + 
    ggtitle("controls(%)")
  
  tab3 <- table_base +
    geom_text(aes(y = Index, x = 1, label = ncases), size = 4) + 
    ggtitle("heterozygotes(%)")
  
  tab4 <- table_base +
    geom_text(aes(y = Index, x = 1, label = formatC(as.numeric(p_value), format = "e", digits = 2)), size = 4) + 
    ggtitle("p-value")
  
  tab5 <- table_base +
    geom_text(aes(y = Index, x = 1, label = OR_CI), size = 4) + 
    ggtitle("OR [95% CI]")
  
  
  tab1 + p+ tab2+tab3 + tab5+tab4+ plot_layout(widths=c(5, 10,3,2,4,2,2))
}
