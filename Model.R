library(readr)
library(plotly)
library(caret)
library(e1071)
library(dplyr)
library(h2o)        # Awesome ML Library
library(timetk)     # Toolkit for working with time series in R
library(tidyquant)
library(randomForest)
#install.packages("devtools")
packageVersion('plotly')
#devtools::install_github("tidyverse/ggplot2")
#devtools::install_github('hadley/ggplot2')
library(ggplot2)
library(caTools)
cities<-c('Chicago','Joilet','Yonkers')
filenames<-c('DatasetChicago.csv','DatasetJoilet.csv','DatasetYonkers.csv')
for(i in c(1:3)){
  current_file<-filenames[i]
  city<-cities[i]
  
  resp<-readline(paste0("Continue for next City ",city," ?"))
  message("For City:",paste0(city))
  dataframe<-read.csv(file=paste0(getwd(),"/",current_file),header = FALSE)
  colnames(dataframe) <- c("Year",'Population','Temperature')
  dataframe
  set.seed(101)
  sample = sample.split(dataframe$Year, SplitRatio = .70)
  train = subset(dataframe, sample == TRUE)
  test  = subset(dataframe, sample == FALSE)
  
  #init the h2o cluster
  h2o.init()
  train_h2o<-as.h2o(train) # convert training data as h2o dataframe
  test_h2o<-as.h2o(test)  # convert test data as h2o dataframe
  y <- 'Temperature'   #dependent variable
  x <- setdiff(names(train_h2o), y) #independent variables
  
  #=============AUTOML ==========================================================================
  
  #h2o automl
  automl_models_h2o <- h2o.automl(
    x = x,
    y = y,
    training_frame = train_h2o,
    leaderboard_frame = test_h2o,#decides the leaderboard by this data
    max_runtime_secs = 100  #max runtime for the model to be trained or tuned
  )
  
  #predict the whole dataset after the model is created
  dataframe_h2o<-as.h2o(dataframe)
  #winner model
  automl_leader<-automl_models_h2o@leader
  pred_h2o <- h2o.predict(automl_leader, newdata = dataframe_h2o)

  #winner algorithm
  automl_leader@algorithm
  #performance matrix
  pref<-h2o.performance(automl_leader, newdata = test_h2o)
  pref
  #Add the prediction to the dataframe to be ploted
  dataframe_pred<-dataframe
  dataframe_pred['Prediction']<-as.data.frame(pred_h2o)
  
  city_line<-plot_ly(dataframe_pred,x = ~ Year,name = 'Population',type='line') %>%
    add_trace(y = ~ Temperature,name = 'Temperature',mode = 'lines') %>%
    #add_trace(y = ~ Population,name = 'Population',mode = 'bar') %>%
    add_trace(y = ~ Prediction,name = 'Predicted Temp',mode = 'lines') %>%
    
    layout(title = "Population vs Temperature",xaxis = list(title = "Population vs Temperature",rangeslider = list(type = "date")),
           yaxis = list(title = ""))
    
  city_line  
  
  city_line_1<-plot_ly(dataframe_pred,x = ~ Year,y= ~ Population,name = 'Population',mode='lines') %>%
              layout(title = "Population vs Temperature",xaxis = list(title = "Year",rangeslider = list(type = "date")),
         yaxis = list(title = "Population"))
  
  city_line_2<-plot_ly(dataframe_pred,x = ~ Year) %>%
      add_trace(y= ~ Temperature,name = 'Temperature',mode="lines") %>%
      add_trace(y= ~ Prediction,name = 'Predicted Temperature',mode = 'lines') %>%
      layout(title = "Population vs Temperature",xaxis = list(title = "Year",rangeslider = list(type = "date")))
  
  city_line_n<-subplot(city_line_1,city_line_2,nrows=2,shareX = TRUE)
  city_line_n
  
  
  city_box<-plot_ly(dataframe_pred,x = ~Year,y = ~Population,color = ~Temperature,type = "box") %>%
            layout(boxmode = "group",title = "Boxplot of Population by Temperature distribution", xaxis= list(title = "Year",rangeslider = list(type = "date")))
    
  city_box
  
  city_bar<-plot_ly(dataframe_pred,x = ~ Year,y = ~ Temperature,name = 'Temperature',type='bar') %>%
    #add_trace(y =  ~ Population,name = 'Population') %>%
    add_trace(y = ~ Prediction,name = 'Predicted Temperature') %>%
    
    layout(title = "Population vs Temperature",yaxis = list(title = 'Population vs Temperature '), barmode = 'group',xaxis=list('Year',rangeslider=list(type="Year")))
  city_bar  
  
  city_bar_1<-plot_ly(dataframe_pred,x = ~ Year,y = ~ Temperature,name = 'Temperature',type='bar') %>%
    add_trace(y =  ~ Population,name = 'Population') %>%
    add_trace(y = ~ Prediction,name = 'Predicted Temperature') %>%
    layout(title = "Population vs Temperature",yaxis = list(title = 'Population vs Temperature '), barmode = 'group',xaxis=list('Year',rangeslider=list(type="Year")))
  city_bar_1
  city_bar_2<-plot_ly(dataframe_pred,x = ~ Year,y = ~ Temperature,name = 'Temperature',type='bar') %>%
    #add_trace(y =  ~ Population,name = 'Population') %>%
    add_trace(y = ~ Prediction,name = 'Predicted Temperature') %>%
    layout(title = "Population vs Temperature",yaxis = list(title = 'Population vs Temperature '), barmode = 'group',xaxis=list('Year',rangeslider=list(type="date")))
  city_bar_2  
  city_bar_n<-subplot(city_bar_1,city_bar_2,nrows=2,shareX = TRUE)
  
  city_bar_n
  #=======================================================================================
  

  
  #=======================================================================================
  #h2o randomforest
  randomForest_models_h2o <- h2o.randomForest(
    x = x,
    y = y,
    training_frame = train_h2o
  )
  dataframe_h2o<-as.h2o(dataframe)
  pred_h2o <- h2o.predict(randomForest_models_h2o, newdata = dataframe_h2o)
  pref<-h2o.performance(randomForest_models_h2o, newdata = test_h2o)
  pref
  #Plot of aggregation by 
  dataframe_pred<-dataframe
  dataframe_pred['Prediction']<-as.data.frame(pred_h2o)
  
  city_line<-plot_ly(dataframe_pred,x = ~ Year,name = 'Population') %>%
    add_trace(y = ~ Temperature,name = 'Temperature',mode = 'lines') %>%
    #add_trace(y = ~ Population,name = 'Population',mode = 'bar') %>%
    add_trace(y = ~ Prediction,name = 'Predicted Temp',mode = 'lines') %>%
    
    layout(title = "Population vs Temperature",xaxis = list(title = "Population vs Temperature",rangeslider = list(type = "date")),
           yaxis = list(title = ""))
  
  city_line  
  
  city_line_1<-plot_ly(dataframe_pred,x = ~ Year,y= ~ Population,name = 'Population',mode='lines') %>%
    layout(title = "Population vs Temperature",xaxis = list(title = "Year",rangeslider = list(type = "date")),
           yaxis = list(title = "Population"))
  
  city_line_2<-plot_ly(dataframe_pred,x = ~ Year) %>%
    add_trace(y= ~ Temperature,name = 'Temperature',mode="lines") %>%
    add_trace(y= ~ Prediction,name = 'Predicted Temperature',mode = 'lines') %>%
    layout(title = "Population vs Temperature",xaxis = list(title = "Year",rangeslider = list(type = "date")))
  
  city_line_n<-subplot(city_line_1,city_line_2,nrows=2,shareX = TRUE)
  city_line_n
  
  
  city_box<-plot_ly(dataframe_pred,x = ~Year,y = ~Population,color = ~Temperature,type = "box") %>%
    layout(boxmode = "group",title = "Boxplot of Population by Temperature distribution", xaxis= list(title = "Year",rangeslider = list(type = "date")))
  
  city_box
  
  city_bar<-plot_ly(dataframe_pred,x = ~ Year,y = ~ Temperature,name = 'Temperature',type='bar') %>%
    #add_trace(y =  ~ Population,name = 'Population') %>%
    add_trace(y = ~ Prediction,name = 'Predicted Temperature') %>%
    
    layout(title = "Population vs Temperature",yaxis = list(title = 'Population vs Temperature '), barmode = 'group',xaxis=list('Year',rangeslider=list(type="Year")))
  city_bar  
  
  city_bar_1<-plot_ly(dataframe_pred,x = ~ Year,y = ~ Temperature,name = 'Temperature',type='bar') %>%
    add_trace(y =  ~ Population,name = 'Population') %>%
    add_trace(y = ~ Prediction,name = 'Predicted Temperature') %>%
    layout(title = "Population vs Temperature",yaxis = list(title = 'Population vs Temperature '), barmode = 'group',xaxis=list('Year',rangeslider=list(type="Year")))
  city_bar_1
  city_bar_2<-plot_ly(dataframe_pred,x = ~ Year,y = ~ Temperature,name = 'Temperature',type='bar') %>%
    #add_trace(y =  ~ Population,name = 'Population') %>%
    add_trace(y = ~ Prediction,name = 'Predicted Temperature') %>%
    layout(title = "Population vs Temperature",yaxis = list(title = 'Population vs Temperature '), barmode = 'group',xaxis=list('Year',rangeslider=list(type="date")))
  city_bar_2  
  city_bar_n<-subplot(city_bar_1,city_bar_2,nrows=2,shareX = TRUE)
  
  city_bar_n
  
  #=======================================================================================
  
  
  
  
  #======h2o deeplearning=================================================================================
  
  #h2o deeplearning
  deeplearning_model_h2o <- h2o.deeplearning(
    x = x,
    y = y,
    training_frame = train_h2o
  )
  dataframe_h2o<-as.h2o(dataframe)
  pred_h2o <- h2o.predict(deeplearning_model_h2o, newdata = dataframe_h2o)
  pref<-h2o.performance(deeplearning_model_h2o, newdata = test_h2o)
  pref
  #Plot of aggregation by 
  dataframe_pred<-dataframe
  dataframe_pred['Prediction']<-as.data.frame(pred_h2o)
  
  city_line<-plot_ly(dataframe_pred,x = ~ Year,name = 'Population') %>%
    add_trace(y = ~ Temperature,name = 'Temperature',mode = 'lines') %>%
    #add_trace(y = ~ Population,name = 'Population',mode = 'bar') %>%
    add_trace(y = ~ Prediction,name = 'Predicted Temp',mode = 'lines') %>%
    
    layout(title = "Population vs Temperature",xaxis = list(title = "Population vs Temperature",rangeslider = list(type = "date")),
           yaxis = list(title = ""))
  
  city_line  
  
  city_line_1<-plot_ly(dataframe_pred,x = ~ Year,y= ~ Population,name = 'Population',mode='lines') %>%
    layout(title = "Population vs Temperature",xaxis = list(title = "Year",rangeslider = list(type = "date")),
           yaxis = list(title = "Population"))
  
  city_line_2<-plot_ly(dataframe_pred,x = ~ Year) %>%
    add_trace(y= ~ Temperature,name = 'Temperature',mode="lines") %>%
    add_trace(y= ~ Prediction,name = 'Predicted Temperature',mode = 'lines') %>%
    layout(title = "Population vs Temperature",xaxis = list(title = "Year",rangeslider = list(type = "date")))
  
  city_line_n<-subplot(city_line_1,city_line_2,nrows=2,shareX = TRUE)
  city_line_n
  
  
  city_box<-plot_ly(dataframe_pred,x = ~Year,y = ~Population,color = ~Temperature,type = "box") %>%
    layout(boxmode = "group",title = "Boxplot of Population by Temperature distribution", xaxis= list(title = "Year",rangeslider = list(type = "date")))
  
  city_box
  
  city_bar<-plot_ly(dataframe_pred,x = ~ Year,y = ~ Temperature,name = 'Temperature',type='bar') %>%
    #add_trace(y =  ~ Population,name = 'Population') %>%
    add_trace(y = ~ Prediction,name = 'Predicted Temperature') %>%
    
    layout(title = "Population vs Temperature",yaxis = list(title = 'Population vs Temperature '), barmode = 'group',xaxis=list('Year',rangeslider=list(type="Year")))
  city_bar  
  
  city_bar_1<-plot_ly(dataframe_pred,x = ~ Year,y = ~ Temperature,name = 'Temperature',type='bar') %>%
    add_trace(y =  ~ Population,name = 'Population') %>%
    add_trace(y = ~ Prediction,name = 'Predicted Temperature') %>%
    layout(title = "Population vs Temperature",yaxis = list(title = 'Population vs Temperature '), barmode = 'group',xaxis=list('Year',rangeslider=list(type="Year")))
  city_bar_1
  city_bar_2<-plot_ly(dataframe_pred,x = ~ Year,y = ~ Temperature,name = 'Temperature',type='bar') %>%
    #add_trace(y =  ~ Population,name = 'Population') %>%
    add_trace(y = ~ Prediction,name = 'Predicted Temperature') %>%
    layout(title = "Population vs Temperature",yaxis = list(title = 'Population vs Temperature '), barmode = 'group',xaxis=list('Year',rangeslider=list(type="date")))
  city_bar_2  
  city_bar_n<-subplot(city_bar_1,city_bar_2,nrows=2,shareX = TRUE)
  
  city_bar_n
  
  #=======================================================================================

}

