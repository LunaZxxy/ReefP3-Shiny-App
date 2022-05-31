library(shinythemes)
library(ggiraph)
library(plotly)
library(shinyjqui)

ui = tagList(
  tags$head(
    tags$style("label{font-family: BentonSans Book;}")
  ),
    navbarPage(
      #collapsible = TRUE, 
      inverse = TRUE, 
      #theme = shinytheme("spacelab"), 
      # theme =  shinytheme("cerulean"), 
      "Coral Bleaching in the GBR",
      # 

      
      
      
      # page 1
      tabPanel("Introduction", 
               icon=icon("home"),
               
               fluidPage(
               h4("What is coral bleaching?"),
               
               "A healthy coral reef thrives off the symbiotic relationship with microscopic algae known as zooxanthellae, ",
               "in which they provide the coral with nutrients in exchange for a sheltered environment. ",
               "The symbiosis between the two organisms is only possible in water temperatures of 23-29 degrees celsius. When temperatures exceed this range,", 
               "the coral expels the zooxanthellae, causing them to lose their vibrant colours and become 'bleached'. ",
               "If the temperatures fall back into the desired range, they can regain the zooxanthellae and survive. ",
               "However, if temperature remains above the bleaching threshold for more than 4 weeks it will bleach.  ",
               "Prolonged periods of bleaching causes the corals to starve and die. ",
               
               br(),
               br(),
               br(),
               
               
               
               fluidRow(
                 column(1, offset = 0, 
                        img(src = "bl.png", height = 148, width = 240), 
                        br()),
                 #column(1, offset = 5, 
                 #       img(src = "coralbleaching.jpg", height = 128, width = 192), 
                 #       br())
                 
                 ),
               
               fluidRow(
                 column(10, offset = 0, 
                        helpText("reef bleaching", 
                                 style = "font-size:115%;font-style:italic;" ), 
                        br())),
               
               
               
               
               h4("Aim for this project"),
               "Since the severity and extent of bleaching is tightly correlated with local heat exposure, ",
               "differences in temperature between regions can cause differences in coral bleaching between regions (Hughes et al., 2018). ",
               "These differences were evident in the 2016 mass coral bleaching events in the Great Barrier Reef, ",
               "where the southern regions of the reef experienced temperatures closer to average and so only experienced minor bleaching (Hughes et al., 2018). " ,
               "Likewise, coral mass mortality was higher in northern regions during the 2017 coral bleaching event due to differences in local weather patterns ",
               "and a low number of storms which increased surface heating (Smith et al., 2020). ",
               "Thus, we are interested in investigating how differences in temperature can drive the differences in coral bleaching across different regions of the Great Barrier Reef.",
               br(),
               
               
               #img(src = "bl.png", height = 128, width = 192),
               #h4("How we collected our dataset?"),

               
               )
               ),
      
      
      
      
   
      # page 2 
      tabPanel("Reef Visualisation",
               sidebarPanel(width = 3,
                 h4("Select Visualisation"),
                 selectizeInput(inputId = "position",
                                "Filter by position in GBR",
                                choices = unique(data_label$label),
                                selected = "Northern"),
                 selectizeInput(inputId = "year",
                                "Filter by year",
                                choices = sort(unique(year(mdy(data_label$date)))),
                                selected = min(sort(unique(year(mdy(data_label$date)))))),
                sliderInput("threshold", "Set a bleaching threshold (%)", value = 25, min = 0, max = 100),
                selectizeInput(inputId = "map_type",
                               "Select map theme",
                               choices = map_theme_choose,
                               selected = "Stamen.TonerLabels"),
                selectizeInput(inputId = "factor_map",
                               "Select factor",
                               choices = factor_li,
                               selected = "SSTA(Sea Surface Temperature Anomaly)"),
                
                "if no graph been shows, it is due to a lack of dataset", 
                 
                 br(),
                 br(),
                 img(src = "reefff.jpg", height = 64, width = 96),
                 br(),
                 "reef blaching", 
                br(),
                 span("The map is powered by Leaflet, it requires Internet connection", style = "color:red")  
               ),
               mainPanel(
                 tabsetPanel(
                   tabPanel("map",icon = icon("map"),jqui_resizable(leafletOutput("map"))),
                   tabPanel("scatter plot",icon =icon("bar-chart-o"),plotlyOutput("reef_plotly")),
                   tabPanel("table",icon = icon("table"),DT::dataTableOutput("table_test"),),
                 )
               )
      ),

      #page 3
      navbarMenu("Correlation",
                
                 
                 tabPanel(
                   "Qualitative",
                   sidebarLayout(position = "right",
                                 sidebarPanel(
                                   "The full information is shown here in the correlation matrix. ",
                                   "The colour of each grid represents the correlation for rows and columns, ",
                                   "with the redder the colour the stronger the positive correlation and the bluer the negative correlation.",
                                   "It can be seen that, there  is a strong positive correlation with ssta,ssta_frequency, ssta_dhw",
                                   "For other variables, some are positively correlated like ssta_dhw&tsa_dhw, some are negatively correlated like depth&windspeed.",
                                   "More detial will be shows in the next section.",
                                   width = 3
                                 ),
                                 mainPanel(
                                   plotlyOutput("heat_map")
                                 )),
                 ),
            
                 
                 tabPanel("Quantitative", 
                          sidebarPanel(width = 3,
                            selectizeInput(inputId = "position_Kruskal",
                                           "Filter by variable",
                                           choices = unique(colnames(df)),
                                           selected = "average_bleaching"),
                          "p valve in Kruskal Test shows the if these two variable are dependent, the smaller number it has, the releationship they have is strong",
                          br(),
                          " r_square shows if they are correlated, the larger the number is, the releationship they have is strong",
                          ),
                          mainPanel(
                            tabPanel("importance plot",verbatimTextOutput("Kruskal_Test")),
                            tabPanel("importance plot",verbatimTextOutput("lm_fit"))
                          ),
                          
                 ),
                 
                 
                 tabPanel("Overall region",
                          fluidPage(
                            fluidRow(
                              column(3, offset = 2, br(), br(), 
                                     h3("Average Bleaching change with date", 
                                        align = "center", style = "opacity: 0.75;"), br(),),
                              column(6, plotlyOutput(outputId = "average_bleaching_date", height = "400px")),
                              column(1)),
                          ),
                          
                          fluidRow(
                            column(3, offset = 2, br(), br(),
                                   h3("Distribution of average bleaching in Northern, Inner, Southern", 
                                      align = "center", style = "opacity: 0.75;"), br(),),
                            column(6, plotlyOutput(outputId = "average_bleaching_distribution", height = "400px")),
                            column(1)),
                          
                          
                          
                          fluidRow(
                            column(3, offset = 2, br(), br(),
                                   h3("GBR position Check", 
                                      align = "center", style = "opacity: 0.75;"), br(),),
                            column(6, plotlyOutput(outputId = "position_Check", height = "400px")),
                            column(1)),
                          
                          fluidRow(
                            column(3, offset = 2, br(), br(),
                                   h3("GBR position Check", 
                                      align = "center", style = "opacity: 0.75;"), br(),),
                            column(6, plotlyOutput(outputId = "position_Check_a", height = "400px")),
                            column(1)),
                          
                          
                 ),
                 
                 
                 
      ),
      
    
            
      # 
      navbarMenu("Model - Average Bleaching",
        tabPanel("Methodology", 
                 fluidPage(
              "There are 4 machine learning models in our project: GBDT, XGBoost, Random Forest, and Decision Tree. ", 
              "All models except XGboost showed that sea surface temperature anomaly in degree heating weeks was the most important predictor for coral bleaching. ",
              "In XGBoost, sea surface temperature anomaly frequency was the most important predictor followed by degree heating weeks.",
              "In each of the models, the location had an impact on average bleaching but was close to or was the least important predictor. ",
              br(),
              br(),
              "We also compared the performance of the models from 4 aspects:",
              br(),
              "Accuracy : RMSE, MAE, R square, and a baseline model are built to evaluate the accuracy of the models, which is based on the mean values of each factor.",
              br(),
              "Stability : by using cross-validation 10 flods",
              br(),
              
              "Scalability : by increasing the size of the data",
              br(),
              "And Interpretability ",
              
                 )
                
                 
                 
                 ),
        tabPanel("Significance of variables",
                 fluidPage(
                 fluidRow(
                   column(3, offset = 2, br(), br(), 
                          h3("Random Forest", 
                             align = "center", style = "opacity: 0.75;"), br(),
                          h5("random forest is a machine learning algorithm that's used to solve regression and classification problems.", 
                             align = "center", style = "opacity: 0.75;")
                          ),
                   column(6, plotOutput(outputId = "rf_importance", height = "400px")),
                   column(1)),
                 
                 fluidRow(
                   column(3, offset = 2, br(), br(), 
                          h3("xgb", 
                             align = "center", style = "opacity: 0.75;"), br(),
                          h5("XGBoost is a decision-tree-based ensemble Machine Learning algorithm that uses a gradient boosting framework.", 
                             align = "center", style = "opacity: 0.75;")),
                   column(6, plotOutput(outputId = "xgb_importance", height = "400px")),
                   column(1)),
                 
                 fluidRow(
                   column(3, offset = 2, br(), br(), 
                          h3("rpart", 
                             align = "center", style = "opacity: 0.75;"), br(),
                          h5("rpart is a Tree-based models can be used for both classification and regression", 
                             align = "center", style = "opacity: 0.75;")
                          ),
                   column(6, plotOutput(outputId = "modelrpart_importance", height = "400px")),
                   column(1)),
                 
                 fluidRow(
                   column(3, offset = 2, br(), br(), 
                          h3("gbdt", 
                             align = "center", style = "opacity: 0.75;"), br(),
                          h5("gbdt is a gradient lifting decision tree with excellent automatic feature combination ability and efficient operation.", 
                             align = "center", style = "opacity: 0.75;")
                          ),
                   column(6, plotOutput(outputId = "modelgbdt_importance", height = "400px")),
                   column(1)),
        
                 )
                 
                 ),
        
        
        tabPanel("Performance",
                 fluidPage(
                   fluidRow(
                     column(3, offset = 2, br(), br(), 
                            h3("Accuracy",
                               align = "center", style = "opacity: 0.75;"), br(),
                            h5("accuracy is the fraction of predictions model got right.", 
                               "we use RMSE, MAE, R to estimate",
                               align = "center", style = "opacity: 0.75;"),
                            
                            ),
            
                     
                     column(6, plotlyOutput(outputId = "acc_all", height = "400px")),
                     column(1)),
                   
                   fluidRow(
                     column(3, offset = 2, br(), br(), 
                            h3("Stability", 
                               align = "center", style = "opacity: 0.75;"), br(),
                            h5("It means the performance of algorithm perturbed by small changes to its inputs.", 
                               "we use 10 CVs to check the performance of each algorithm",
                               align = "center", style = "opacity: 0.75;"),
                            ),
                     column(6, plotOutput(outputId = "Stability_all", height = "400px")),
                     column(1)),
                   
                   fluidRow(
                     column(3, offset = 2, br(), br(), 
                            h3("Scalability", 
                               align = "center", style = "opacity: 0.75;"), br(),
                            h5("It means if the algorithm can deal with large amount of data, with the increase of X label, the data is geting large", 
                               align = "center", style = "opacity: 0.75;"),
                            
                            ),
                     column(6, plotlyOutput(outputId = "Scalability_all", height = "400px")),
                     column(1)),
                   
                   fluidRow(
                     column(3, offset = 2, br(), br(), 
                            h3("Interpretibility", 
                               align = "center", style = "opacity: 0.75;"), br(),),
                     column(6,
                            
                            "We determine a model s interpretability by the extent to which we can explain its algorithm and how it reaches a prediction.", 
                            "Therefore, it is important to consider the complexity of the model and also the background of our target audience in our comparison.",
                            "The gbm and xgboost models are gradient boosted models. ",
                            "Their algorithms are similar both combine weak models to produce a stronger prediction model.",
                            "While these models outperform the random forest model, we consider both models to be difficult to interpret as their algorithms are complex",
                            "The rparts package produces a decision tree model. This model is easier to interpret as its algorithm is simpler than the gbm and xgboost models. ",   
                              
                            
                            
                            ),
                     column(1)),
                   
                   
                       )
        ),

    ),
    
    
    
    #

 
    #
      tabPanel("Model - location", 
               sidebarPanel(width = 3,
                 selectizeInput(inputId = "position_importacne",
                                "Filter by position in GBR",
                                choices = unique(data_label$label),
                                selected = "Northern"),
                 "rank the factor importance affect average bleaching in different region, this result is based on rpart"
               ),
               mainPanel(
                 tabPanel("importance plot",plotOutput("importance_plot")),
               ),
               
             ),
    
      #
      navbarMenu("More",
                 tabPanel("Dataset",
                    DT::dataTableOutput("table"),
                 ),
                 tabPanel("About",
                  h3("Dataset"),
                  "For our dataset we have merged two different sets of data to get all the material we need, ",
                  "so we did not have to compromise on any of the variables we could not find in only one dataset. ",
                  "Our first one comes from the Integrated Marine Observing System (IMOS), where they record the air-sea flux. To do this they observe wind, ",
                  "humidity, air and wind temperature, precipitation and pressure." ,
                  "And this generates a daily file of 1 minute averages. Our second dataset comes from the Biological and Chemical Oceanography Data Management Office (BCO-DMO)." ,
                  "This includes the number of corals showing bleaching. It uses the Coral Reef Temperature Anomaly Database from the National Oceanic and Atmospheric Administration, ",
                  "to predict the abundance and severity of coral bleaching worldwide ",
                  "(we have taken just the values that are from the Great Barrier Reef).",
                  br(),
                  'The main dataset in',
                  a('www.nodc.noaa.gov/sog/cortad/'),
                  a('https://www.bco-dmo.org/dataset/773466'),  
                  br(),
  
                  "The weather dataset in",
                  a('https://catalogue-imos.aodn.org.au/geonetwork/srv/eng/catalog.search#/metadata/9436802f-ed93-4660-be50-e77c3c07bd09'),
                  h3("References"),

                  "Hughes, T.P., Kerry, J.T. and Simpson, T., 2018.",
                  "Large scale bleaching of corals on the Great Barrier Reef",
                  br(),
                  br(),
                  "Smith, G.A., 2020. Seasonal climate summary for the southern hemisphere (autumn 2017): ",
                  "the Great Barrier Reef experiences coral bleaching during El Nino Southern Oscillation neutral conditions",
                  "Journal of Southern Hemisphere Earth Systems Science, 69(1), pp.310 330.",
                  br(),
                  br(),
                  "J. H. Friedman. Stochastic gradient boosting. Computational Statistics and Data Analysis,", 
                  "38(4):367 378, 2002.Greedy function approximation: A gradient boosting machine.Jerome H. Friedman",
                  a("https://christophm.github.io/interpretable-ml-book/interpretability.html"),
                  br(),
                  ) 
                )          

    )

)
