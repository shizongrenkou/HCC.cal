##AFP only calculator
setwd("~/Documents/统计分析文件/R 建模/shiny/my own")

library(randomForestSRC)
library(pec)
library(MASS)
library(abind)
library(face)
library(tidyverse)
library(scales)

##define function cond.prod.pec
# cond.prob.pec = function(model, newdata, Tstart, Tpred){
#   risk.Tstart = as.numeric(predictSurvProb(model,newdata=newdata,times=Tstart))
#   risk.Tpred = as.numeric(predictSurvProb(model,newdata=newdata,times=Tpred))
#   return(risk.Tpred/risk.Tstart)
# }

##read in the prediction model and the dataframe needed
rsf.fit.afp= readRDS("rsf-afp.rds")
rsf.fit= readRDS("rsf-all.rds")

df_needed= read.csv(file = "df_needed.csv", header = T, stringsAsFactors = F)

afp_df= data.frame("subj"=df_needed$ID, "argvals"=df_needed$time, "y"=df_needed$AFP)

tbil_df= data.frame("subj"=df_needed$ID, "argvals"=df_needed$time, "y"=df_needed$TBIL) 
dbil_df= data.frame("subj"=df_needed$ID, "argvals"=df_needed$time, "y"=df_needed$DBIL) 
alt_df= data.frame("subj"=df_needed$ID, "argvals"=df_needed$time, "y"=df_needed$ALT) 
ast_df= data.frame("subj"=df_needed$ID, "argvals"=df_needed$time, "y"=df_needed$AST) 
che2_df= data.frame("subj"=df_needed$ID, "argvals"=df_needed$time, "y"=df_needed$CHE2)
alp_df= data.frame("subj"=df_needed$ID, "argvals"=df_needed$time, "y"=df_needed$ALP)
ggt_df= data.frame("subj"=df_needed$ID, "argvals"=df_needed$time, "y"=df_needed$GGT) 
tp_df= data.frame("subj"=df_needed$ID, "argvals"=df_needed$time, "y"=df_needed$TP) 
alb_df= data.frame("subj"=df_needed$ID, "argvals"=df_needed$time, "y"=df_needed$ALB) 


library(shiny)
#####user interface####

ui <- navbarPage("HCC Calculator",
  
    # First navigation tab
    tabPanel("Comprehensive Calculator",
    fluidPage(
                 
    # Side bar
    sidebarPanel(
    # Example CSV Section
    HTML("<h4>Example CSV</h4>"),
    downloadButton("ComprehensiveExample", "Download Example CSV"),
    HTML("<br>"),  # Add a line break for some spacing
    
    # File Upload Section
    HTML("<h4 style='margin-top:20px;'>Upload Your CSV</h4>"),  # Added some top margin for spacing
    fileInput("comprehensiveFileUpload", NULL, accept = ".csv"),  # Removed the label here as it's provided by the h4 tag above
    
    # Input Box for Prediction Time
    numericInput(
      inputId = "comprehensivePredWindow",
      label = "Prediction Window (in days):",
      value = 0,  # Default value, adjust as needed
      min = 0    # Minimum value, adjust as needed
    ),
    
    # # Select Box for Prediction Window
    # numericInput(
    #   inputId = "comprehensivePredWindow",
    #   label = "Prediction Window:",
    #   value = 0,  # Default value, adjust as needed
    #   min = 0    # Minimum value, adjust as needed
    # ),
    # 
    # ActionButton for Submit
    actionButton("comprehensiveSubmitBtn", "Submit")
  ),
  
  
  # Display Results
  mainPanel(
    tags$label(h3('Output')),
    
    # Conditional Message
    conditionalPanel(
      condition = "input.comprehensiveSubmitBtn === 0",  # Message displays when button not pressed
      textOutput("comprehensiveWaitMessage")
    ),
    
    #Output table
    tableOutput("comprehensiveCancerProbTable") 
  )
)
),


# Second navigation tab
tabPanel("AFP-only Calculator",
         
         fluidPage(
           # Side bar
           sidebarPanel(
             # Example CSV Section
             HTML("<h4>Example CSV</h4>"),
             downloadButton("downloadExample", "Download Example CSV"),
             HTML("<br>"),  # Add a line break for some spacing
             
             # File Upload Section
             HTML("<h4 style='margin-top:20px;'>Upload Your CSV</h4>"),  # Added some top margin for spacing
             fileInput("fileUpload", NULL, accept = ".csv"),  # Removed the label here as it's provided by the h4 tag above
             
             # Input Box for Start Time
             numericInput(
               inputId = "predWindow",
               label = "Prediction Window:",
               value = 0,  # Default value, adjust as needed
               min = 0    # Minimum value, adjust as needed
             ),
             
             # Select Box for Prediction Window
             # selectInput(
             #   inputId = "predWindow",
             #   label = "Prediction Window:",
             #   choices = c("1 year (365 days)" = 365, 
             #               "2 years (730 days)" = 730, 
             #               "3 years (1095 days)" = 1095),
             #   selected = 365  # Default selection
             # ),
             # 
             # ActionButton for Submit
             actionButton("submitBtn", "Submit")
           ),
           
           
           # Display Results
           mainPanel(
             tags$label(h3('Output')),
             
             # Conditional Message
             conditionalPanel(
               condition = "input.submitBtn === 0",  # Message displays when button not pressed
               textOutput("waitMessage")
             ),
             
             #Output table
             tableOutput("cancerProbTable") 
           ) 
           
         )
  
),


# Third navigation tab
tabPanel("About", 
         titlePanel("About the Calculator"), 
         div(includeMarkdown("about_cal.md"), 
             align="justify")
)

)



#####server####
server <- function(input, output, session) {
  
  # --------------------- AFP-only Calculator ---------------------
  
  # Handle Example File Download
  output$downloadExample <- downloadHandler(
    filename = function() "afp_only_example.csv",
    content = function(file) {
      file.copy("example_afp.csv", file)
    }
  )
  
  #Message for user
  output$waitMessage <- renderText({
    "The calculation might take a while."
  })
  
  
  observeEvent(input$submitBtn, {
    
    if (is.null(input$fileUpload)) {
      showNotification("Please upload a CSV file!", type = "error")
      return()
    }
    
  
  # Process the Uploaded File
  processedData <- reactive({
    inFile <- input$fileUpload
    if (is.null(inFile)) return(NULL)
    
    # Read the uploaded file
    test.long <- read.csv(inFile$datapath)
    
    # Check if the CSV has the correct number of columns (for example, 3)
    if (ncol(test.long) != 3) {
      validate(
        need(FALSE,
             "The format of the uploaded CSV is incorrect.")
      )
    }
    
    sortnames <- c("ID","time") 
    test.long <- test.long[do.call("order", test.long[sortnames]),]
    
   
    
    # Check for missing baseline values for each ID
    missing_baseline <- test.long %>%
      group_by(ID) %>%
      summarize(has_baseline = any(time == 0), .groups = 'drop') %>%
      filter(!has_baseline)
    
    # If any ID is missing baseline value, return a warning message
    if (nrow(missing_baseline) > 0) {
      missing_ids <- paste(missing_baseline$ID, collapse = ", ")
      validate(
        need(FALSE,
             paste("Missing baseline values for the following IDs: ", missing_ids))
      )
    }
    
    # Create a dataframe like the baseline dataframe
    latest_time <- test.long %>%
      group_by(ID) %>%
      summarize(
        latest_time = max(time),
        .groups = 'drop'  
      )
    test.long.info=data.frame("ID" = latest_time$ID, status = 0, survtime = latest_time$latest_time)
    
    ##check for if the time of pred window is smaller than last measurements of all patients
    pred_window <- as.numeric(input$predWindow)
    
    if(any(latest_time$latest_time > pred_window)) {
      validate(
        need(FALSE, "Prediction window is smaller than the last measurement for some patients. 
                     Please adjust the prediction window.")
      )
    }
  
    
    # Process the uploaded file as per your steps
    afp_df_test <- data.frame("subj" = test.long$ID, "argvals" = test.long$time, "y" = test.long$AFP)
    
    # Perform further processing and return the result
    faces.afp.test <- face.sparse(
      afp_df, 
      knots = 7,
      newdata = afp_df_test,
      calculate.scores = TRUE
    )
    
    b <- faces.afp.test$rand_eff$scores 
    test.surv.afp=cbind(test.long.info,b)
    
    # Return the processed data
    return(test.surv.afp)
  })
  
  
  ##Make Predictions
  predictions <- reactive({
    
    test.surv.afp <- processedData()
    # Check if the data is available
    if (is.null(test.surv.afp)) return(NULL)
    
    test.surv.afp1= test.surv.afp[,-1]
    
    # Access the start time and predictive window
    #start_time <- as.numeric(input$startTime)
    pred_window <- as.numeric(input$predWindow)
    
    # Calculate the survival probability of having no cancer
    # survival_prob <- cond.prob.pec(rsf.fit.afp, test.surv.afp1, start_time, pred_window)
    survival_prob = as.numeric (predictSurvProb(rsf.fit.afp,newdata=test.surv.afp1,times=pred_window))
    cancer_prob = 1- survival_prob                          
    cancer_prob_percent <- percent(cancer_prob, accuracy = 0.01)
   
     cancer_prob_df <- data.frame(
      ID = test.surv.afp$ID,
      Cancer_Probability = cancer_prob_percent
    )
    
     # Create a data frame instead of using cbind directly
    # cancer_prob_df <- data.frame(
    #   ID = test.surv.afp$ID,
    #   Cancer_Probability = sprintf("%.2f%%", (1 - survival_prob) * 100)  # Named the column as "Cancer_Probability"
    # )

    return(cancer_prob_df)
  })
  

  output$cancerProbTable <- renderTable({
    predictions()  # Renders the data frame created in predictions reactive
  }, rownames = TRUE) 
  
  })
  
  
  
  # --------------------- Comprehensive Calculator ---------------------
  
  # Handle Example File Download
  output$ComprehensiveExample <- downloadHandler(
    filename = function() "comprehensive_example.csv",
    content = function(file) {
      file.copy("example_compre.csv", file)
    }
  )
  
  #Message for user
  output$comprehensiveWaitMessage <- renderText({
    "The calculation might take a while."
  })
  
  
  observeEvent(input$comprehensiveSubmitBtn, {
    
    if (is.null(input$comprehensiveFileUpload)) {
      showNotification("Please upload a CSV file!", type = "error")
      return()
    }
  
    # Process the Uploaded File
    comprehensiveProcessedData <- reactive({
      inFile <- input$comprehensiveFileUpload
      if (is.null(inFile)) return(NULL)
      
      # Read the uploaded file
      test.long <- read.csv(inFile$datapath)
      
      # Check if the CSV has the correct number of columns (for example, 3)
      if (ncol(test.long) != 14) {
        validate(
          need(FALSE,
               "The format of the uploaded CSV is incorrect.")
        )
      }
      
      sortnames <- c("ID","time") 
      test.long <- test.long[do.call("order", test.long[sortnames]),]
      
      
      
      # Check for missing baseline values for each ID
      missing_baseline <- test.long %>%
        group_by(ID) %>%
        summarize(has_baseline = any(time == 0), .groups = 'drop') %>%
        filter(!has_baseline)
      
      # If any ID is missing baseline value, return a warning message
      if (nrow(missing_baseline) > 0) {
        missing_ids <- paste(missing_baseline$ID, collapse = ", ")
        validate(
          need(FALSE,
               paste("Missing baseline values for the following IDs: ", missing_ids))
        )
      }
      
      # Create a dataframe like the baseline dataframe
      latest_time <- test.long %>%
        group_by(ID) %>%
        summarize(
          latest_time = max(time),
          gender= max(gender),
          age= min(age_at_baseline),
          .groups = 'drop'  
        )
      
      test.long.info=data.frame("ID" = latest_time$ID, status = 0, survtime = latest_time$latest_time, 
                                age_at_baseline= latest_time$age, gender= latest_time$gender)
      
      
      ##check for if the time of pred window is smaller than last measurements of all patients
      pred_window1 = as.numeric(input$comprehensivePredWindow)
      
      if(any(latest_time$latest_time > pred_window1)) {
        validate(
          need(FALSE, "Prediction window is smaller than the last measurement for some patients. 
                     Please adjust the prediction window.")
        )
      }
      
      # Process the uploaded file as per your steps
      afp_df_test = data.frame("subj" = test.long$ID, "argvals" = test.long$time, "y" = test.long$AFP)
      
      tbil_df_test= data.frame("subj"=test.long$ID, "argvals"=test.long$time, "y"=test.long$total_bilirubin) 
      dbil_df_test= data.frame("subj"=test.long$ID, "argvals"=test.long$time, "y"=test.long$direct_bilirubin) 
      alt_df_test= data.frame("subj"=test.long$ID, "argvals"=test.long$time, "y"=test.long$ALT) 
      ast_df_test= data.frame("subj"=test.long$ID, "argvals"=test.long$time, "y"=test.long$AST) 
      che2_df_test= data.frame("subj"=test.long$ID, "argvals"=test.long$time, "y"=test.long$cholinesterase)
      alp_df_test= data.frame("subj"=test.long$ID, "argvals"=test.long$time, "y"=test.long$ALP)
      ggt_df_test= data.frame("subj"=test.long$ID, "argvals"=test.long$time, "y"=test.long$GGT) 
      tp_df_test= data.frame("subj"=test.long$ID, "argvals"=test.long$time, "y"=test.long$total_protein) 
      alb_df_test= data.frame("subj"=test.long$ID, "argvals"=test.long$time, "y"=test.long$albumin) 
      
      
      # Perform further processing and return the result
      faces.afp.test <- face.sparse(afp_df, knots = 7,newdata = afp_df_test,calculate.scores = TRUE)
       b <- faces.afp.test$rand_eff$scores 
       
       faces.tbil.test= face.sparse(tbil_df, knots=7,newdata = tbil_df_test,calculate.scores = TRUE)
       b1=faces.tbil.test$rand_eff$scores
       
       faces.dbil.test= face.sparse(dbil_df, knots=7,newdata = dbil_df_test,calculate.scores = TRUE)
       b2=faces.dbil.test$rand_eff$scores
       
       faces.alt.test= face.sparse(alt_df, knots=7,newdata = alt_df_test,calculate.scores = TRUE)
       b3=faces.alt.test$rand_eff$scores
       
       faces.ast.test= face.sparse(ast_df, knots=7,newdata = ast_df_test,calculate.scores = TRUE)
       b4=faces.ast.test$rand_eff$scores
       
       faces.che2.test= face.sparse(che2_df, knots=7,newdata = che2_df_test,calculate.scores = TRUE)
       b5=faces.che2.test$rand_eff$scores
       
       faces.alp.test= face.sparse(alp_df, knots=7,newdata = alp_df_test,calculate.scores = TRUE)
       b6=faces.alp.test$rand_eff$scores
       
       faces.ggt.test= face.sparse(ggt_df, knots=7,newdata = ggt_df_test,calculate.scores = TRUE)
       b7=faces.ggt.test$rand_eff$scores
       
       faces.tp.test= face.sparse(tp_df, knots=7,newdata = tp_df_test,calculate.scores = TRUE)
       b8=faces.tp.test$rand_eff$scores
       
       faces.alb.test= face.sparse(alb_df, knots=7,newdata = alb_df_test,calculate.scores = TRUE)
       b9=faces.alb.test$rand_eff$scores
       
       test.scores.combine= cbind(b,b1,b2,b3,b4,b5,b6,b7,b8,b9)
       
      test.surv=cbind(test.long.info,test.scores.combine)
      
      # Return the processed data
      return(test.surv)
    })
  
    
    ##Make Predictions
    comprehensivePredictions <- reactive({
      
      test.surv <- comprehensiveProcessedData()
      # Check if the data is available
      if (is.null(test.surv)) return(NULL)
      
      test.surv1= test.surv[,-1]
      
      # Access the start time and predictive window
      pred_window1 = as.numeric(input$comprehensivePredWindow)
      #print(pred_window1)
      
      # Calculate the survival probability of having no cancer
      # survival_prob <- cond.prob.pec(rsf.fit, test.surv1, start_time1, pred_window1)
      survival_prob1 <- as.numeric(predictSurvProb(rsf.fit, newdata= test.surv1, times= pred_window1))
      
      cancer_prob1 = 1 - survival_prob1
      cancer_prob_percent1 = percent(cancer_prob1, accuracy = 0.01)
      
      cancer_prob_df_com = data.frame(
        ID = test.surv$ID,
        Cancer_Probability = cancer_prob_percent1
      )
      
      # # Create a data frame instead of using cbind directly
      # cancer_prob_df_com <- data.frame(
      #   ID = test.surv$ID,
      #   Cancer_Probability = sprintf("%.2f%%", (1 - survival_prob1) * 100)  # Named the column as "Cancer_Probability"
      # )
      return(cancer_prob_df_com)
    })
    
    
    # output$comprehensiveCancerProbTable <- renderTable({
    #   comprehensivePredictions()  # Renders the data frame created in predictions reactive
    # }, rownames = TRUE) 
  

    
    output$comprehensiveCancerProbTable <- renderTable({
      pred_data <- comprehensivePredictions()  # Get the result of comprehensivePredictions()
      
      # Check if pred_data is NULL
      if (is.null(pred_data)) {
        stop("Error: comprehensivePredictions() returned NULL.")
      }
      
      return(pred_data)  # Return the data frame for rendering
    }, rownames = TRUE)
  
})

}


shinyApp(ui, server)




