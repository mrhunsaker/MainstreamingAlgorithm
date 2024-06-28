#    Mainstreaming INCLUDE Algorithm Shiny App
#    Copyright (C) 2024, Michael Ryan Hunsaker, M.Ed., Ph.D.
#
#   This program is free software: you can redistribute it and/or modify
#    it under the terms of the GNU General Public License as published by
#    the Free Software Foundation, either version 3 of the License, or
#    (at your option) any later version.
#
#    This program is distributed in the hope that it will be useful,
#    but WITHOUT ANY WARRANTY; without even the implied warranty of
#    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#    GNU General Public License for more details.
#
#    You should have received a copy of the GNU General Public License
#    along with this program.  If not, see <http://www.gnu.org/licenses/>.
#########################################################################

packages <- c("shiny", "shinyjs", "gplots", "RColorBrewer", "rpart", "rpart.plot", "e1071", "gridExtra", "shinythemes")
install_if_missing <- function(packages) {
  # Check if each package is installed, install if not
  for (pkg in packages) {
    if (!(pkg %in% installed.packages())) {
      install.packages(pkg, dependencies = TRUE, repos = "https://cloud.r-project.org")
    }
  }
}

install_if_missing(packages)

# Load required libraries
library(shiny)
library(gplots)
library(RColorBrewer)
library(rpart)
library(rpart.plot)
library(e1071)
library(gridExtra)
library(shinyjs)
library(shinythemes)

ui <- fluidPage(
  theme = shinythemes::shinytheme("sandstone"),
  useShinyjs(),
  tags$head(
    tags$style(HTML("
      .full-height-plot {
        height: calc(100vh - 150px);
      }
      .full-height-plot > div {
        height: 100% !important;
      }
    "))
  ),
  titlePanel("Mainstreaming Decision Tree"),
  sidebarLayout(
    sidebarPanel(
      fileInput("file1", "Choose CSV File", accept = ".csv"),
      actionButton("analyzeButton", "Analyze Data"),
      uiOutput("errorMessage"),
      hr(), # Add a horizontal line for visual separation
      h4("Download Options:"),
      disabled(downloadButton("downloadHeatmap", "Download Heatmap")),
      br(), br(), # Add some vertical space
      disabled(downloadButton("downloadTree1", "Download Tree 1")),
      br(), br(),
      disabled(downloadButton("downloadTree2", "Download Tree 2")),
      br(), br(),
      disabled(downloadButton("downloadTree3", "Download Tree 3")),
      br(), br(),
      disabled(downloadButton("downloadSVM", "Download SVM Results"))
    ),
    mainPanel(
      tabsetPanel(
        tabPanel(
          "Heatmap",
          div(
            class = "full-height-plot",
            plotOutput("heatmap", height = "100%")
          )
        ),
        tabPanel(
          "Decision Trees",
          div(
            style = "height: 100vh; overflow-y: auto;",
            htmlOutput("scrollTextTop"),
            div(
              plotOutput("treeOutput1", height = "100vh")
            ),
            div(
              plotOutput("treeOutput2", height = "100vh")
            ),
            plotOutput("treeOutput3", height = "100vh")
          )
        ),
        tabPanel(
          "SVM Analysis",
          tags$head(
            tags$style(HTML("
      .svm-results {
        width: 100%;
        max-width: 100%;
        overflow-x: auto;
      }
      .svm-results pre {
        white-space: pre-wrap;
        word-wrap: break-word;
      }
      .svm-predictions {
        width: 100%;
        max-width: 100%;
        overflow-x: auto;
      }
      .svm-predictions .table {
        width: 100%;
        max-width: 100%;
      }
      .svm-accuracy {
        font-weight: bold;
        margin-bottom: 10px;
        font-size: 1.2em;
      }
    "))
          ),
          h3("SVM Results"),
          lapply(c(30, 20, 10, 5, 3), function(cv) {
            tagList(
              h4(paste("Cross-validation:", cv)),
              div(class = "svm-accuracy", textOutput(paste0("svmAccuracy", cv))),
              div(class = "svm-results", verbatimTextOutput(paste0("svmResults", cv))),
              h4("Prediction Results:"),
              div(class = "svm-predictions", tableOutput(paste0("svmPredictions", cv))),
              hr()
            )
          })
        )
      )
    )
  )
)
server <- function(input, output, session) {
  uploadedData <- reactive({
    req(input$file1) # Ensure a file is uploaded
    return(input$file1$datapath)
  })

  output$errorMessage <- renderUI({
    return(NULL)
  })

  analyze_data <- function(data_file_path) {
    Data <- read.csv(data_file_path, header = TRUE)

    SXData <- subset(Data, select = c(
      Name, Outcome, FSIQ, Basic_Reading_Skills,
      Reading_Comp, Math_Calc, Math_Reasoning,
      Written_Lang, Adaptive, SocioEmotional,
      CBM_Math, CBM_Reading
    ))

    SXclusterdata <- subset(SXData, select = c(
      FSIQ, Basic_Reading_Skills, Reading_Comp,
      Math_Calc, Math_Reasoning, Written_Lang,
      Adaptive, SocioEmotional, CBM_Math, CBM_Reading
    ))

    SXClusteringdata <- scale(SXclusterdata)
    SXClusteringdata <- as.matrix(SXClusteringdata)

    mypalette <- colorRampPalette(c("#e66101", "#fdb863", "#b2abd2", "#4e3c99"))(n = 256)

    # Create a color vector for Outcome
    outcome_colors <- c("GenEd" = "purple", "Mainstream" = "orange", "Inclusion" = "lightsalmon")
    outcome_col <- outcome_colors[SXData$Outcome]

    return(list(data = SXClusteringdata, palette = mypalette, row_labels = SXData$Name, outcome_col = outcome_col))
  }

  perform_decision_tree_analysis <- function(data_file_path) {
    Data <- read.csv(data_file_path, header = TRUE)

    fit1 <- rpart(Outcome ~ Adaptive + FSIQ + CBM_Math + CBM_Reading + Basic_Reading_Skills + Reading_Comp + Math_Reasoning + Math_Calc + Written_Lang + SocioEmotional, data = na.omit(Data), cost = c(3, 1, 2, 2, 2, 2, 2, 2, 2, 1), method = "class", control = rpart.control(minsplit = 1, minbucket = 1, cp = -1))


    fit2 <- rpart(Outcome ~ Adaptive + FSIQ + WJIII + CBM + SocioEmotional, data = na.omit(Data), method = "class", cost = c(3, 1, 2, 2, 1), control = rpart.control(minsplit = 1, minbucket = 1, cp = -1, mincriterion = 0.5))


    fit3 <- rpart(Outcome ~ Adaptive + FSIQ + SocioEmotional, data = na.omit(Data), method = "class", cost = c(3, 1, 1), control = rpart.control(minsplit = 1, minbucket = 1, cp = -1))

    return(list(fit1 = fit1, fit2 = fit2, fit3 = fit3))
  }

  perform_svm_analysis <- function(data) {
    data$Outcome <- as.factor(data$Outcome)
    results <- list()
    cross_vals <- c(30, 20, 10, 5, 3)

    original_names <- data$Name
    original_outcomes <- data$Outcome

    for (cross_val in cross_vals) {
      output <- capture.output({
        cat("K means cross validation -", cross_val, "v", 61 - cross_val, "\n")
        svm_fit <- svm(Outcome ~ Adaptive + FSIQ + WJIII + CBM + SocioEmotional,
                       data = na.omit(data), kernel = "linear", cross = cross_val,
                       probability = TRUE, type = "C-classification")
        print(svm_fit)
        cat("\nSummary:\n")
        svm_summary <- summary(svm_fit)
        print(svm_summary)

        # Extract total accuracy from the summary
        total_accuracy <- svm_summary$tot.accuracy

        predictions <- predict(svm_fit, na.omit(data))

        prediction_df <- data.frame(
          Name = original_names[!is.na(data$Outcome)],
          Actual_Outcome = original_outcomes[!is.na(data$Outcome)],
          Prediction = predictions,
          Match = predictions == original_outcomes[!is.na(data$Outcome)]
        )

        cat("\nPredictions:\n")
        print(prediction_df)
        cat("\nTotal Accuracy: ", round(total_accuracy, 2), "%\n")
      })

      results[[paste0("cross_", cross_val)]] <- list(
        output = paste(output, collapse = "\n"),
        predictions = prediction_df,
        accuracy = round(total_accuracy, 2)
      )
    }

    return(results)
  }

  analysisResults <- reactiveVal(NULL)

  observeEvent(input$analyzeButton, {
    if (is.null(input$file1)) {
      output$errorMessage <- renderUI({
        div(
          style = "color: red; font-weight: bold; margin-top: 10px;",
          "Error: Please upload a CSV file before analyzing."
        )
      })
      return() # Exit the observer early
    }

    # Clear any previous error message
    output$errorMessage <- renderUI({
      return(NULL)
    })

    results <- list(
      heatmap = analyze_data(uploadedData()),
      trees = perform_decision_tree_analysis(uploadedData()),
      svm = perform_svm_analysis(read.csv(uploadedData()))
    )
    analysisResults(results)

    output$heatmap <- renderPlot(
      {
        heatmap.2(results$heatmap$data,
          main = "Student Placement Heatmap (Z-Scores)",
          col = results$heatmap$palette,
          scale = "none",
          rowsep = 1:nrow(results$heatmap$data),
          colsep = 1:ncol(results$heatmap$data),
          sepcol = "white",
          sepwidth = c(0.015, 0.025),
          trace = "none",
          labRow = results$heatmap$row_labels,
          margins = c(8, 8),
          cexRow = 0.75,
          cexCol = 1,
          keysize = 1,
          lhei = c(1, 4),
          lwid = c(1, 4),
          dendrogram = "row",
        )
      },
      height = function() {
        session$clientData$output_heatmap_height
      })

    output$scrollTextTop <- renderUI({
      HTML("<div style='text-align: center; padding: 10px 0; font-weight: bold; font-size: 2em;'>Scroll Down to See All 3 Plots</div>")
    })

    output$treeOutput1 <- renderPlot(
      {
        req(analysisResults())
        rpart.plot(analysisResults()$trees$fit1, type = 0, extra = 100, branch.lty = 1, shadow.col = "gray", nn = TRUE, under = TRUE, tweak = 1.2, main = "Decision Tree (Academic Testing Separated)")
      },
      height = function() session$clientData$output_treeOutput1_width * 0.7
    )

    output$treeOutput2 <- renderPlot(
      {
        req(analysisResults())
        rpart.plot(analysisResults()$trees$fit2, type = 0, extra = 100, branch.lty = 1, shadow.col = "gray", nn = TRUE, under = TRUE, tweak = 1.2, main = "Decision Tree (Academic Testing Present)")
      },
      height = function() session$clientData$output_treeOutput2_width * 0.7
    )

    output$treeOutput3 <- renderPlot(
      {
        req(analysisResults())
        rpart.plot(analysisResults()$trees$fit3, type = 0, extra = 100, branch.lty = 1, shadow.col = "gray", nn = TRUE, under = TRUE, tweak = 1.2, main = "Decision Tree (Academic Testing Absent)")
      },
      height = function() session$clientData$output_treeOutput3_width * 0.7
    )


    lapply(c(30, 20, 10, 5, 3), function(cv) {
      output[[paste0("svmResults", cv)]] <- renderPrint({
        req(analysisResults())
        cat(analysisResults()$svm[[paste0("cross_", cv)]]$output)
      })

      output[[paste0("svmPredictions", cv)]] <- renderTable({
        req(analysisResults())
        analysisResults()$svm[[paste0("cross_", cv)]]$predictions
      }, width = "100%")

      output[[paste0("svmAccuracy", cv)]] <- renderText({
        req(analysisResults())
        paste("Total Accuracy:", analysisResults()$svm[[paste0("cross_", cv)]]$accuracy, "%")
      })
    })

    shinyjs::enable("downloadHeatmap")
    shinyjs::enable("downloadTree1")
    shinyjs::enable("downloadTree2")
    shinyjs::enable("downloadTree3")
    shinyjs::enable("downloadSVM")
  })
  # Download handlers
  output$downloadHeatmap <- downloadHandler(
    filename = function() {
      paste("heatmap-", Sys.Date(), ".pdf", sep = "")
    },
    content = function(file) {
      pdf(file, width = 11, height = 8.5)
      heatmap_data <- analysisResults()$heatmap
      heatmap.2(heatmap_data$data,
                main = "Student Placement Heatmap (Z-Scores)",
                col = results$heatmap$palette,
                scale = "none",
                rowsep = 1:nrow(results$heatmap$data),
                colsep = 1:ncol(results$heatmap$data),
                sepcol = "white",
                sepwidth = c(0.015, 0.025),
                trace = "none",
                labRow = results$heatmap$row_labels,
                margins = c(10, 10),
                cexRow = 0.75,
                cexCol = 1,
                keysize = 1,
                lhei = c(1, 4),
                lwid = c(1, 4),
                dendrogram = "row",
      )
      dev.off()
    }
  )

  output$downloadTree1 <- downloadHandler(
    filename = function() {
      paste("tree1-", Sys.Date(), ".pdf", sep = "")
    },
    content = function(file) {
      pdf(file, width = 11, height = 8.5)
      par(mar = c(1, 1, 1, 1))
      rpart.plot(analysisResults()$trees$fit1, type = 0, extra = 100, branch.lty = 1, shadow.col = "gray", nn = TRUE, under = TRUE, tweak = 0.75, main = "Decision Tree (Academic Testing Separated)")
      dev.off()
    }
  )

  output$downloadTree2 <- downloadHandler(
    filename = function() {
      paste("tree2-", Sys.Date(), ".pdf", sep = "")
    },
    content = function(file) {
      pdf(file, width = 11, height = 8.5)
      par(mar = c(1, 1, 1, 1))
      rpart.plot(analysisResults()$trees$fit2, type = 0, extra = 100, branch.lty = 1, shadow.col = "gray", nn = TRUE, under = TRUE, tweak = 0.75, main = "Decision Tree (Academic Testing Present)")
      dev.off()
    }
  )

  output$downloadTree3 <- downloadHandler(
    filename = function() {
      paste("tree3-", Sys.Date(), ".pdf", sep = "")
    },
    content = function(file) {
      pdf(file, width = 11, height = 8.5)
      par(mar = c(1, 1, 1, 1))
      rpart.plot(analysisResults()$trees$fit3, type = 0, extra = 100, branch.lty = 1, shadow.col = "gray", nn = TRUE, under = TRUE, tweak = 0.75, main = "Decision Tree (Academic Testing Absent)")

      dev.off()
    }
  )

  output$downloadSVM <- downloadHandler(
    filename = function() {
      paste("svm-results-", Sys.Date(), ".txt", sep = "")
    },
    content = function(file) {
      results <- analysisResults()$svm
      writeLines(unlist(results), file)
    }
  )
}
shinyApp(ui = ui, server = server)
