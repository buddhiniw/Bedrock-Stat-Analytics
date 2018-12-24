library(shiny)
library(caret) # Package to traint the model
library(glmnet) # Package to fit ridge/lasso/elastic net models
library(DMwR)
library(boot) # Package to do bootstrap error estimates
library(elasticnet)
library(dplyr)
library(corrplot)
library(officer)
library(rmarkdown)
library(latexpdf)
shinyServer(function(input, output, session) {
  
  current_user_status <- reactiveValues()
  current_user_status$logged <- FALSE
  current_user_status$current_user <- NULL
  current_user_status$access <- NULL
  
  
  output$ui_page_1 <- renderUI({
    
    if(current_user_status$logged == TRUE){
      if("access_to_page_1" %in% current_user_status$access){
        tagList(
          verticalLayout(
            fileInput("file1", "Choose CSV File",
                      multiple = FALSE,
                      accept = c("text/csv",
                                 "text/comma-separated-values,text/plain",
                                 ".csv"))
            
          )
        )  
      } else {
        tagList(
          div("No access to this part.", style="color:purple")
        )
      }
      
    } else {
      tagList(
        div("Please log in", style="color:red")
      )
    }
    
  })
  
  output$ui_page_2 <- renderUI({
    
    if(current_user_status$logged == TRUE){
      if("access_to_page_1" %in% current_user_status$access){
        tagList(
          verticalLayout(
            downloadButton("report","Download Report"),
            div("To save as pdf: After downloading, open file in browser click print and save as pdf",style="color:red")
            )
        )  
      } else {
        tagList(
          div("No access to this part.", style="color:purple")
        )
      }
      
    } else {
      tagList(
        div("Please log in", style="color:red")
      )
    }
    
  })

  
  output$data_1_for_authorized_user <- renderTable({
    head(iris)
  })
  

  
  
  observeEvent(input$button_login, {
    
    if(input$user!="" && input$password!="" && input$user %in% user_db$id && input$password == user_db$password[user_db$id == input$user]){
      current_user_status$logged <- TRUE
      current_user_status$current_user <- input$user
      current_user_status$access <- user_db[user_db$id == current_user_status$current_user,
                                            c("access_to_page_1", "access_to_page_2")] %>%
                                    unlist %>% {.==1} %>% names(.)[.]
      
      output$verification_result <- renderText({
        "Login succeeded"
      })
      
    } else {
      current_user_status$logged <- FALSE
      current_user_status$current_user <- NULL
      
      output$verification_result <- renderText({
        "Login failed"
      })
    }
  })
  
  output$contents <- renderDataTable({
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, head of that data file by default,
    # or all rows if selected, will be shown.
    
    req(input$file1)
    
    # when reading semicolon separated files,
    # having a comma separator causes `read.csv` to error
    tryCatch(
      {
        dataIn <- read.csv(input$file1$datapath,
                           header = T)
        df <- dataIn[1:(nrow(dataIn)-1),]
        
      },
      error = function(e) {
        # return a safeError if a parsing error occurs
        stop(safeError(e))
      }
    )
    return(df)
  })
  
  output$statistics <- renderDataTable({
    req(input$file1)
    tryCatch(
      {
        dataIn <- read.csv(input$file1$datapath,
                           header = T)
        statistics_table <- summary(dataIn)
        statistics_table <- statistics_table[-c(2,5),]
        
      },
      error = function(e) {
        # return a safeError if a parsing error occurs
        stop(safeError(e))
      }
    )
    
    return(statistics_table)
    
  })
  
  output$correlation <- renderPlot({
    
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, head of that data file by default,
    # or all rows if selected, will be shown.
    
    req(input$file1)
    
    # when reading semicolon separated files,
    # having a comma separator causes `read.csv` to error
    tryCatch(
      {
        dataIn <- read.csv(input$file1$datapath,
                           header = T)
        indx <- sapply(dataIn, is.factor)
        dataIn[indx] <- lapply(dataIn[indx], function(x) as.numeric(x))
        dataIn.x <- dataIn[,-1]
        cat.vars <- apply(dataIn.x,2,function(x) { all(x %in% 0:1) })
        dataIn.continuous <- dataIn.x[!cat.vars]
        dataIn.catrgorical <- dataIn.x[cat.vars]
        dataIn.continuous.scaled <-scale(dataIn.continuous,center=TRUE, scale=TRUE)
        x.scaled <- cbind(as.data.frame(dataIn.continuous.scaled),dataIn.catrgorical)
        dataIn.y <- dataIn[1:(nrow(dataIn)-1),1,drop=FALSE]
        dataIn.y.scaled <- scale(dataIn.y,center=TRUE, scale=TRUE)
        y.scaled <- as.data.frame(dataIn.y.scaled)
        x <- as.matrix(x.scaled[1:(nrow(x.scaled)-1),])
        y <- as.matrix(y.scaled)
        dataIn.scaled.test <- x.scaled[nrow(x.scaled),]
        x.test <-as.matrix(dataIn.scaled.test)
        
      },
      error = function(e) {
        # return a safeError if a parsing error occurs
        stop(safeError(e))
      }
    )
    corrplot::corrplot(cov(as.matrix(x.scaled)),method = "number")
  })
  
  output$cross_validation <- renderPlot({
    req(input$file1)
    
    tryCatch(
      {
        dataIn <- read.csv(input$file1$datapath,header = T)
        indx <- sapply(dataIn, is.factor)
        dataIn[indx] <- lapply(dataIn[indx], function(x) as.numeric(x))
        dataIn.x <- dataIn[,-1]
        cat.vars <- apply(dataIn.x,2,function(x) { all(x %in% 0:1) })
        dataIn.continuous <- dataIn.x[!cat.vars]
        dataIn.catrgorical <- dataIn.x[cat.vars]
        dataIn.continuous.scaled <-scale(dataIn.continuous,center=TRUE, scale=TRUE)
        x.scaled <- cbind(as.data.frame(dataIn.continuous.scaled),dataIn.catrgorical)
        dataIn.y <- dataIn[1:(nrow(dataIn)-1),1,drop=FALSE]
        dataIn.y.scaled <- scale(dataIn.y,center=TRUE, scale=TRUE)
        y.scaled <- as.data.frame(dataIn.y.scaled)
        x <- as.matrix(x.scaled[1:(nrow(x.scaled)-1),])
        y <- as.matrix(y.scaled)
        dataIn.scaled.test <- x.scaled[nrow(x.scaled),]
        x.test <-as.matrix(dataIn.scaled.test)
        lambda.grid <- c(0,0.01,0.1,1,10,100)
        #lambda.grid <- seq(0,10,by = 1)
        s.grid <- seq(0,1,by=0.05)
        train.control = trainControl(method = "LOOCV")
        search.grid <- expand.grid(.fraction = s.grid, .lambda = lambda.grid)
        dataIn.scaled <- cbind(y,x)
        dataIn.scaled <- na.omit(dataIn.scaled)
        set.seed(42)
        
        train.enet = train(
          x.scaled[1:(nrow(x.scaled)-1),], y.scaled[[1]],
          method = "enet",
          metric = "RMSE",
          tuneGrid = search.grid,
          normalize = FALSE,
          intercept = FALSE,
          trControl = train.control
        )
        
        
      },
      error = function(e) {
        # return a safeError if a parsing error occurs
        stop(safeError(e))
      }
    )
    
    plot(train.enet)
    
    
  })
  
  output$variable_importance <- renderPlot({
    req(input$file1)
    
    tryCatch(
      {
        dataIn <- read.csv(input$file1$datapath,header = T)
        indx <- sapply(dataIn, is.factor)
        dataIn[indx] <- lapply(dataIn[indx], function(x) as.numeric(x))
        dataIn.x <- dataIn[,-1]
        cat.vars <- apply(dataIn.x,2,function(x) { all(x %in% 0:1) })
        dataIn.continuous <- dataIn.x[!cat.vars]
        dataIn.catrgorical <- dataIn.x[cat.vars]
        dataIn.continuous.scaled <-scale(dataIn.continuous,center=TRUE, scale=TRUE)
        x.scaled <- cbind(as.data.frame(dataIn.continuous.scaled),dataIn.catrgorical)
        dataIn.y <- dataIn[1:(nrow(dataIn)-1),1,drop=FALSE]
        dataIn.y.scaled <- scale(dataIn.y,center=TRUE, scale=TRUE)
        y.scaled <- as.data.frame(dataIn.y.scaled)
        x <- as.matrix(x.scaled[1:(nrow(x.scaled)-1),])
        y <- as.matrix(y.scaled)
        dataIn.scaled.test <- x.scaled[nrow(x.scaled),]
        x.test <-as.matrix(dataIn.scaled.test)
        lambda.grid <- c(0,0.01,0.1,1,10,100)
        #lambda.grid <- seq(0,10,by = 1)
        s.grid <- seq(0,1,by=0.05)
        train.control = trainControl(method = "LOOCV")
        search.grid <- expand.grid(.fraction = s.grid, .lambda = lambda.grid)
        dataIn.scaled <- cbind(y,x)
        dataIn.scaled <- na.omit(dataIn.scaled)
        set.seed(42)
        
        train.enet = train(
          x.scaled[1:(nrow(x.scaled)-1),], y.scaled[[1]],
          method = "enet",
          metric = "RMSE",
          tuneGrid = search.grid,
          normalize = FALSE,
          intercept = FALSE,
          trControl = train.control
        )
        
        best.fraction <- train.enet$bestTune$fraction
        best.lambda <- train.enet$bestTune$lambda
        best = which(rownames(train.enet$results) == rownames(train.enet$bestTune))
        best.result = train.enet$results[best, ]
        rownames(best.result) = NULL
        prediction.error <- best.result$RMSE
        prediction.rsquared <-best.result$Rsquared
        final.enet.model <- train.enet$finalModel
        beta.hat.enet.scaled <- predict.enet(final.enet.model,
                                             s=best.fraction,
                                             type="coefficient",
                                             mode="fraction")
        y.hat.enet.scaled <- predict.enet(final.enet.model,
                                          as.data.frame(x.test),
                                          s=best.fraction,
                                          type="fit",
                                          mode="fraction")
        y.hat.enet.unscaled <- unscale(y.hat.enet.scaled$fit,dataIn.y.scaled)
        
      },
      error = function(e) {
        # return a safeError if a parsing error occurs
        stop(safeError(e))
      }
    )
    
    plot(varImp(train.enet))
    
  })
  
  
  output$sm_coefficients <- renderDataTable({
    req(input$file1)
    
    tryCatch(
      {
        dataIn <- read.csv(input$file1$datapath,header = T)
        indx <- sapply(dataIn, is.factor)
        dataIn[indx] <- lapply(dataIn[indx], function(x) as.numeric(x))
        dataIn.x <- dataIn[,-1]
        cat.vars <- apply(dataIn.x,2,function(x) { all(x %in% 0:1) })
        dataIn.continuous <- dataIn.x[!cat.vars]
        dataIn.catrgorical <- dataIn.x[cat.vars]
        dataIn.continuous.scaled <-scale(dataIn.continuous,center=TRUE, scale=TRUE)
        x.scaled <- cbind(as.data.frame(dataIn.continuous.scaled),dataIn.catrgorical)
        dataIn.y <- dataIn[1:(nrow(dataIn)-1),1,drop=FALSE]
        dataIn.y.scaled <- scale(dataIn.y,center=TRUE, scale=TRUE)
        y.scaled <- as.data.frame(dataIn.y.scaled)
        x <- as.matrix(x.scaled[1:(nrow(x.scaled)-1),])
        y <- as.matrix(y.scaled)
        dataIn.scaled.test <- x.scaled[nrow(x.scaled),]
        x.test <-as.matrix(dataIn.scaled.test)
        lambda.grid <- c(0,0.01,0.1,1,10,100)
        #lambda.grid <- seq(0,10,by = 1)
        s.grid <- seq(0,1,by=0.05)
        train.control = trainControl(method = "LOOCV")
        search.grid <- expand.grid(.fraction = s.grid, .lambda = lambda.grid)
        dataIn.scaled <- cbind(y,x)
        dataIn.scaled <- na.omit(dataIn.scaled)
        set.seed(42)
        
        train.enet = train(
          x.scaled[1:(nrow(x.scaled)-1),], y.scaled[[1]],
          method = "enet",
          metric = "RMSE",
          tuneGrid = search.grid,
          normalize = FALSE,
          intercept = FALSE,
          trControl = train.control
        )
        
        best.fraction <- train.enet$bestTune$fraction
        best.lambda <- train.enet$bestTune$lambda
        best = which(rownames(train.enet$results) == rownames(train.enet$bestTune))
        best.result = train.enet$results[best, ]
        rownames(best.result) = NULL
        prediction.error <- best.result$RMSE
        prediction.rsquared <-best.result$Rsquared
        final.enet.model <- train.enet$finalModel
        beta.hat.enet.scaled <- predict.enet(final.enet.model,
                                             s=best.fraction,
                                             type="coefficient",
                                             mode="fraction")
        y.hat.enet.scaled <- predict.enet(final.enet.model,
                                          as.data.frame(x.test),
                                          s=best.fraction,
                                          type="fit",
                                          mode="fraction")
        y.hat.enet.unscaled <- unscale(y.hat.enet.scaled$fit,dataIn.y.scaled)
        data <- as.matrix(beta.hat.enet.scaled$coefficients)
        data <- formatC(data, digits = 6, format = "f", flag = "0")
        str(data)
        print(data[,1])
        print(row.names(data))
        k <- row.names(data)
        data <- data.frame(data)
        data <- cbind(Row.Names= row.names(data),data)
        colnames(data)<-c('Variable','Coefficient')
      },
      error = function(e) {
        # return a safeError if a parsing error occurs
        stop(safeError(e))
      }
    )
    return(data)
    
    
  })
  
  
  output$prediction <- renderDataTable({
    req(input$file1)
    
    tryCatch(
      {
        dataIn <- read.csv(input$file1$datapath,header = T)
        indx <- sapply(dataIn, is.factor)
        dataIn[indx] <- lapply(dataIn[indx], function(x) as.numeric(x))
        dataIn.x <- dataIn[,-1]
        cat.vars <- apply(dataIn.x,2,function(x) { all(x %in% 0:1) })
        dataIn.continuous <- dataIn.x[!cat.vars]
        dataIn.catrgorical <- dataIn.x[cat.vars]
        dataIn.continuous.scaled <-scale(dataIn.continuous,center=TRUE, scale=TRUE)
        x.scaled <- cbind(as.data.frame(dataIn.continuous.scaled),dataIn.catrgorical)
        dataIn.y <- dataIn[1:(nrow(dataIn)-1),1,drop=FALSE]
        dataIn.y.scaled <- scale(dataIn.y,center=TRUE, scale=TRUE)
        y.scaled <- as.data.frame(dataIn.y.scaled)
        x <- as.matrix(x.scaled[1:(nrow(x.scaled)-1),])
        y <- as.matrix(y.scaled)
        dataIn.scaled.test <- x.scaled[nrow(x.scaled),]
        x.test <-as.matrix(dataIn.scaled.test)
        lambda.grid <- c(0,0.01,0.1,1,10,100)
        #lambda.grid <- seq(0,10,by = 1)
        s.grid <- seq(0,1,by=0.05)
        train.control = trainControl(method = "LOOCV")
        search.grid <- expand.grid(.fraction = s.grid, .lambda = lambda.grid)
        dataIn.scaled <- cbind(y,x)
        dataIn.scaled <- na.omit(dataIn.scaled)
        set.seed(42)
        
        train.enet = train(
          x.scaled[1:(nrow(x.scaled)-1),], y.scaled[[1]],
          method = "enet",
          metric = "RMSE",
          tuneGrid = search.grid,
          normalize = FALSE,
          intercept = FALSE,
          trControl = train.control
        )
        
        best.fraction <- train.enet$bestTune$fraction
        best.lambda <- train.enet$bestTune$lambda
        best = which(rownames(train.enet$results) == rownames(train.enet$bestTune))
        best.result = train.enet$results[best, ]
        rownames(best.result) = NULL
        prediction.error <- best.result$RMSE
        prediction.rsquared <-best.result$Rsquared
        final.enet.model <- train.enet$finalModel
        beta.hat.enet.scaled <- predict.enet(final.enet.model,
                                             s=best.fraction,
                                             type="coefficient",
                                             mode="fraction")
        y.hat.enet.scaled <- predict.enet(final.enet.model,
                                          as.data.frame(x.test),
                                          s=best.fraction,
                                          type="fit",
                                          mode="fraction")
        y.hat.enet.unscaled <- unscale(y.hat.enet.scaled$fit,dataIn.y.scaled)
        
        data <- as.matrix(cbind(y.hat.enet.unscaled,prediction.error,prediction.rsquared))
        data <- formatC(data, digits = 3, format = "f", flag = "0")
        k <- row.names(data)
        data <- data.frame(data)
        data <- cbind(Row.Names= row.names(data),data)
        colnames(data)<-c("","Predicted Value", "Prediction Error", "R2")
      },
      error = function(e) {
        # return a safeError if a parsing error occurs
        stop(safeError(e))
      }
    )
    return(data)
    
    
  })
  
  output$report <- downloadHandler(
    filename = "report.html",
    content = function(file) {
      # Copy the report file to a temporary directory before processing it, in
      # case we don't have write permissions to the current working dir (which
      # can happen when deployed).
      tempReport <- file.path(tempdir(), "report.Rmd")
      file.copy("report.Rmd", tempReport, overwrite = TRUE)
      #print(getwd())
      # Set up parameters to pass to Rmd document
      dataIn <- read.csv(input$file1$datapath,
                         header = T)
      params <- list(dat_data = dataIn)
      
      # Knit the document, passing in the `params` list, and eval it in a
      # child of the global environment (this isolates the code in the document
      # from the code in this app).
      rmarkdown::render(tempReport, output_file = file,
                        params = params,
                        envir = new.env(parent = globalenv())
      )
      
      
    }
  )
  
  
  
  
})