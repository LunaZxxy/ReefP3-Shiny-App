library(ggiraph)
library(plotly)

# Define server logic required to draw a histogram ----
server <- function(input, output) {
  
  # Plot of the new cases with selected countries ----
  # This expression that generates a plot is wrapped in a call
  # to renderPlot to indicate that:
  #
  # 1. It is "reactive" and therefore should be automatically
  #    re-executed when inputs (input$bins) change
  # 2. Its output type is a plot

  output$reef_plotly <- renderPlotly({
    #filter
    input_position <- input$position
    input_year <- input$year
    input_label=data_label[data_label$label==input_position,]
    input_label=input_label[year(mdy(input_label$date))==input_year,]
    input_label=input_label[order(input_label$date),]
    
    plot_ly(input_label, x = ~date, y = ~average_bleaching, type = 'scatter', mode = 'markers')%>%
    layout(title = paste("Average Bleaching in:", input_position,input_year)) %>%
    add_lines(y=input$threshold)  
    
  })
  
  output$map<-renderLeaflet({
    #filter
    input_position <- input$position
    input_year <- input$year
    input_map <-input$map_type
    input_factor <- input$factor_map
    input_label<- data_label[data_label$label==input_position,]
    input_label<- input_label[year(mdy(input_label$date))==input_year,]
    input_label<- input_label[order(input_label$date),]
    factor <- change_temp_factor(input_label,input_factor)
    
    #creat colour
    cPal <- colorNumeric(palette = c("blue","green"),domain = factor)
    
    leaflet(input_label,options = leafletOptions(
      minZoom = 4, maxZoom = 18, zoomControl = FALSE
    )) %>% addTiles() %>% addCircleMarkers(
      fillColor = ~cPal(factor),
      color =  ~ifelse(average_bleaching >= input$threshold, "red", "green"), 
      fillOpacity = 0.5,
      label = ~average_bleaching,
      labelOptions = labelOptions(noHide = F, 
                                  direction = 'bottom',
                                  style = list(
                                    'font-size' = '14px'
                                  ))) %>%
      addProviderTiles(change_map_type(input_map))%>%  
      
      addLegend("bottomright", pal = cPal, 
                values = ~factor,title = )
  
    })  
  
  output$table_test <- DT::renderDataTable({
    #filter
    input_position <- input$position
    input_year <- input$year
    input_label=data_label[data_label$label==input_position,]
    input_label=data_label[year(mdy(data_label$date))==input_year,]
    input_label=input_label[order(input_label$date),]
    
    DT::datatable(input_label)
  })
  
  output$table <- DT::renderDataTable({
    DT::datatable(data_label)
  })
  
  output$importance_plot <- renderPlot({
    #filter
    input_position <- input$position_importacne
    input_label_filtered=data_label[data_label$label==input_position,]%>%dplyr::select(temp_factor)
    
    #importance
    set.seed(35)
    ind = sample(2,nrow(input_label_filtered),replace = TRUE,prob = c(0.7,0.3))
    trainset = input_label_filtered[ind == 1,]
    testset = input_label_filtered[ind == 2,]
    control = trainControl(method = "repeatedcv",number = 10,repeats = 3)
    model = train(average_bleaching~.,data = trainset,method = "rpart",preProcess = "scale" ,trControl = control)
    importance = varImp(model,scale = FALSE)
    
    #plot
    importance$importance %>% 
      rownames_to_column() %>%
      arrange(Overall) %>%
      mutate(rowname = forcats::fct_inorder(rowname)) %>%
      ggplot()+
      geom_col(aes(x = rowname, y = Overall))+
      coord_flip()+
      theme_bw()+ ggtitle(paste("importance in:", input_position)) 
  })
  
  
  output$heat_map <- renderPlotly({
    data<-na.omit(df)
    pairs <- data%>% 
      # turn location into factor, since it is treated as a factor
      mutate(location = as.factor(location)) 
    a=ggcorrplot(cor(pairs[, 1:15]))
    plotly::ggplotly(a)
  })
  
  
  output$average_bleaching_date <- renderPlotly({
    df_time <- data_location %>% dplyr::select(c("average_bleaching","date",
                                                 "location"))  %>% drop_na()
    df_time$date <- as.Date(df_time$date,"%d/%m/%Y")
    
    a=ggplot(data=df_time,aes(x=date, y=average_bleaching, group=location,color=location))+
      geom_line()
    plotly::ggplotly(a)
  })
  
  output$average_bleaching_distribution <- renderPlotly({
    df_time <- data_location %>% dplyr::select(c("average_bleaching","date",
                                                 "location"))  %>% drop_na()
    df_time$date <- as.Date(df_time$date,"%d/%m/%Y")
    
    a=ggplot(df_time, aes(x=average_bleaching, colour=location)) + geom_density() 
    plotly::ggplotly(a)
  })
  
  output$position_Check <- renderPlotly({
    a=ggboxplot(df, x = "location", y = "ssta_dhw", 
                color = "location", palette = c("#00AFBB", "#E7B800", "#FC4E07"),
                order = c("Northern", "Inner", "Southern"),
                ylab = "ssta dhw", xlab = "Location of  GBR")
    
    plotly::ggplotly(a)
  })
  
  output$position_Check_a <- renderPlotly({
    
  a=ggboxplot(df, x = "location", y = "average_bleaching", 
            color = "location", palette = c("#00AFBB", "#E7B800", "#FC4E07"),
            order = c("Northern", "Inner", "Southern"),
            ylab = "average bleaching", xlab = "Location of  GBR")
    
    plotly::ggplotly(a)
  })
  
  
  output$Kruskal_Test <- renderPrint({
    print(kruskal.test(as.formula(paste(input$position_Kruskal,"~location",sep="")),data=df))
  })
  
  
  output$lm_fit <- renderPrint({
    lmmo<-lm(as.formula(paste(input$position_Kruskal,"~location",sep="")),data=df)
    print(summary(lmmo))
  })
  
  output$acc_all <- renderPlotly({
    baseline_accuracy <- melt(baseline_accuracy, id="type")
    gbdt_accuracy <-  melt(gbdt_accuracy, id="type")
    rf_accuracy <- melt(rf_accuracy, id="type")
    rpart_accuracy <-  melt(rpart_accuracy, id="type")
    xgb_accuracy <- melt(xgb_accuracy, id="type")
    acc <- rbind(baseline_accuracy,
                 gbdt_accuracy,
                 rf_accuracy,
                 rpart_accuracy,
                 xgb_accuracy)
    
    p <- ggplot(acc,aes(variable,value,fill=type)) +
      geom_bar(stat = 'identity',position="dodge")
    plotly::ggplotly(p)

  })  
  
  
  output$Stability_all <- renderPlot({
    table_gbdt$type = 'gbdt'
    table_rf$type = 'rf'
    table_rpart$type = 'rpart'
    table_xgb$type = 'xgb'
    table_gbdt$cv <- c(1:10)
    table_rf$cv <- c(1:10)
    table_rpart$cv <- c(1:10)
    table_xgb$cv <- c(1:10)
    stab <- rbind(table_gbdt,table_rf,table_rpart,table_xgb)
    p1 <- ggplot(stab, aes(x=cv, y=RMSE, fill=type)) + 
      geom_boxplot(outlier.size = 0.5, size = 0.5) 
    #plotly::ggplotly(p1)
    p1
  })  
  
  output$Scalability_all <- renderPlotly({
    df_times <- data.frame(
      size = c(1:9),
      gbdt = as.numeric(gbdt_times),
      rf = as.numeric(rf_times),
      rpart = as.numeric(rpart_times),
      xgb = as.numeric(xgb_times)
      
    )
    
    times <- melt(df_times, id="size")
    p2 <- ggplot(data=times,aes(x=size, y=value, group=variable,color=variable))+
      geom_line()
    plotly::ggplotly(p2)
    
  }) 
  
  output$modelgbdt_importance <- renderPlot({
    dfht<-summary(modelgbdt,n.trees=bestntrees,plotit = F)
    ggplot(
      aes(x=reorder(var,rel.inf),y=rel.inf),data=dfht)+
      geom_bar(stat="identity")+
      coord_flip()
  }) 

  output$modelrpart_importance <- renderPlot({
    dfht<-data.frame(imp=modelrpart$variable.importance)
    ggplot(
      aes(x=reorder(rownames(dfht),imp),y=imp),data=dfht)+
      geom_bar(stat="identity")+
      coord_flip()+
      labs(x="varinames")
  }) 
 
  output$xgb_importance <- renderPlot({
    xgb.plot.importance(variimp)
  })   
  
  output$rf_importance <- renderPlot({
    varImpPlot(modelrf)
  })
  
}