miss.plot<-function(data, only.missing=TRUE, plot.color="coral1"){
  
  library(ggplot2)
  library(dplyr)
  #Calculate percent missing data for every column in input data frame
  miss_pct<-purrr::map_dbl(data, function(x){round((sum(is.na(x)) / length(x)) * 100, 1)})
  
  #Subset miss_pct>0
  if(only.missing){
    miss_pct<-miss_pct[miss_pct > 0]
  }
  
  missing.df<-data.frame(pct.missing=miss_pct, var=names(miss_pct), row.names=NULL)
  
  
  #Plot percent of missing data for variables, in descending order
  missing.df %>%
    ggplot(aes(x=reorder(var, -pct.missing), y=pct.missing)) + 
    geom_bar(stat='identity', fill=plot.color) +
    labs(x='', y='% missing', title='Percent missing data by feature') +
    theme(axis.text.x=element_text(angle=90, hjust=1, vjust=0.5))
  
  #Same thing in tabular form
  return (dplyr::arrange(missing.df, desc(pct.missing)))
}
