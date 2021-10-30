
descriptive_plot <- function(data,all){
  
  if(all=="All"){
  data %>% group_by(agediff5) %>% 
    count(fevtreat,wt=perweight) %>% pivot_wider(values_from=n,names_from=fevtreat) %>% 
    mutate(percent=`Received treatment`/(`No treatment`+`Received treatment`)*100) %>% 
    ggplot(aes(agediff5,percent)) + geom_bar(stat="identity")
  } else {
    data %>% filter(country==input$country) %>% group_by(agediff5) %>% 
      count(fevtreat,wt=perweight) %>% pivot_wider(values_from=n,names_from=fevtreat) %>% 
      mutate(percent=`Received treatment`/(`No treatment`+`Received treatment`)*100) %>% 
      ggplot(aes(agediff5,percent)) + geom_bar(stat="identity")
  }
}