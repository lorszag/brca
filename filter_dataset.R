filter_dataset = function(data){
  data$AD <- gsub('[^0-9.,]', '', data$AD)
  data$GQ <- gsub('[^0-9.,]', '', data$GQ)
  data$DP <- gsub('[^0-9.,]', '', data$DP)
  
  data_filtered = data %>% 
    filter(as.numeric(GQ)>30)%>%
    mutate(AD_2 = sapply(strsplit(AD, ","), function(y) ifelse(length(y) > 1, y[2], NA)),
           filter = as.numeric(AD_2)/as.numeric(DP))%>%
    filter(filter>=.3 & filter <=.7)
}