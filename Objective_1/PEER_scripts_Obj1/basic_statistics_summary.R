# Author: Martin Guacaneme
# Date: 15/o6/2020

library(ggplot2)

data_tour = function(){
  my_data = readline(prompt="Enter file name: ")
  my_data = read.csv(my_data, header = TRUE) 
  df = data.frame(my_data)
  
  get_summary = function(df, my_var){
    my_summary = summary(df[my_var])
    print(
      assign(
        paste("plot_", my_var, sep = ""), 
        ggplot(data = df, aes_string(x = my_var)) + geom_histogram()
        )
      )
    return(my_summary)
  }
  
  print("The variables in your data frame with NA counts are:")
  print(colSums(is.na(df)))
  print("The following are nummerical variables")
  
  for(i in names(df)){
    j = df[i][1:1,]
    if(class(j) == class(1L) | class(j) == class(1)){
      print(i)
    }
  }
  continue = readline(prompt="Do you want to know the summary of any of the previous var (y/n): ")
  
  while(continue == "y"){
    my_var = readline(prompt="Enter variable name: ") 
    print(get_summary(df, my_var))
    #print(sd(df[my_var][1], na.rm = T))
    continue = readline(prompt="Do you want to know the summary of any of the previous var (y/n): ")
  }
}

data_tour()

#to remove all variables run the line below
#rm(list=ls()) 


