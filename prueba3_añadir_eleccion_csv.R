library(shiny)
library(shinydashboard)
library(shinythemes)
library(DT)
library(formattable)

load('modelo_ordinal.rda')

ui <- fluidPage(
  dashboardPage(skin="blue",
                dashboardHeader(title=tags$em("Shiny prediction app", style="text-align:center;color:#006600;font-size:100%"),titleWidth = 800),

                dashboardSidebar(width = 250,
                                 sidebarMenu(
                                   br(),
                                   menuItem(tags$em("Introduction",style="font-size:120%"),icon=icon("intro"),tabName="intro"),
                                   menuItem(tags$em("Upload Test Data",style="font-size:120%"),icon=icon("upload"),tabName="data"),
                                   menuItem(tags$em("Download Predictions",style="font-size:120%"),icon=icon("download"),tabName="download")


                                 )
                ),

                dashboardBody(
                  tabItems(
                    tabItem(tabName="intro",


                            br(),
                            br(),
                            br(),
                            br(),
                            tags$h4("Bladder cancer (BC) is among the most frequent malignancies worldwide.
                                    BC is responsible for 3% of all malignant tumors in adults and is the most lethal urological malignancy.
                                    Indeed, according to the GLOBOCAN 2018 study, BC accounts for more than 540,000
                                    new cases worldwide and almost 200,000 deaths annually (2018_Global cancer statistics 2018: GLOBOCAN estimates of incidence and mortality worldwide for 36 cancers in 185 countries; 2019_ Cancer statistics, 2019).
                                    The contribution of BC to medical expenditure is significant. By the end of the decade,
                                    the disease is expected to account for >3% of all cancer-related medical expenses.
                                    In fact, BC is the tumor with the highest monitoring costs and the most expensive cancer to treat,
                                    with the cost of the muscle-invasive subtype approaching $150,000 per capita
                                    (2014_The economics of bladder cancer: costs and considerations of caring for this disease; 2019_ Epidemiology,
                                    aetiology and screening of bladder cancer).", style="font-size:150%"),


                            br(),

                            tags$h4("We propose an application which any researcher all around the world can use.
                                    We have fitted a statistical model using 7 miRNAs
                                    (miR-192-5p, miR-362-3p, miR-191-5p, miR-93-5p, miR-200c-3p, miR-21-5p, miR-221-3p)
                                    able to stratify BC patients and healthy subjects.
                                    More information about methodological approach you can consult the article: ", style="font-size:150%"),

                            tags$h4("If you have used BladdimiR in a scientific publication, we would appreciate citations to the following paper:", style="font-size:150%"),

                            br(),
                            br(),
                            br(),
                            br()

                    ),

                    tabItem(tabName="data",


                            br(),
                            br(),
                            br(),
                            br(),
                            tags$h4("With this app, you can upload your data and get back predictions.
                                  The model is a Regularized Ordinal Regression that predicts the probability of .
                                    ", style="font-size:150%"),


                            br(),

                            tags$h4("To predict using this model, upload test data in csv format (you can change the code to read other data types) by using the button below.", style="font-size:150%"),

                            tags$h4("Then, go to the", tags$span("Download Predictions", style="font-weight:bold"),
                                    tags$span("section in the sidebar to  download the predictions."), style="font-size:150%"),

                            br(),
                            br(),
                            br(),
                            br(),
                            # Sidebar panel for inputs ----
                            sidebarPanel(

                              # Input: Select a file ----
                              fileInput("file1", "Choose CSV File",
                                        multiple = TRUE,
                                        accept = c("text/csv",
                                                   "text/comma-separated-values,text/plain",
                                                   ".csv")),

                              # Horizontal line ----
                              tags$hr(),

                              # Input: Checkbox if file has header ----
                              checkboxInput("header", "Header", TRUE),

                              # Input: Select separator ----
                              radioButtons("sep", "Separator",
                                           choices = c(Comma = ",",
                                                       Semicolon = ";",
                                                       Tab = "\t"),
                                           selected = ","),

                               # Input: Select dec ----
                              radioButtons("dec", "Decimal",
                                           choices = c("." = '.',
                                                       "," = ","),
                                           selected = '.'),

                              # Horizontal line ----
                              tags$hr(),

                              # Input: Select quotes ----
                              radioButtons("quote", "Quote",
                                           choices = c(None = "",
                                                       "Double Quote" = '"',
                                                       "Single Quote" = "'"),
                                           selected = '"'),

                              # Horizontal line ----
                              tags$hr()

                            ),

                            fluidRow(
                              column(width = 7,
                              uiOutput("sample_input_data_heading"),
                              dataTableOutput("sample_input_data")
                            ))

                    ),


                    tabItem(tabName="download",
                            fluidRow(
                              br(),
                              br(),
                              br(),
                              br(),
                              column(width = 8,
                                     tags$h4("After you upload a test dataset, you can download the predictions in csv format by
                                    clicking the button below.",
                                             style="font-size:200%"),
                                     br(),
                                     br()
                              )),
                            fluidRow(

                              column(width = 4,
                                     downloadButton("downloadData", em('Download Predictions',style="text-align:center;color:blue;font-size:150%"))

                              ),
                              column(width = 7,
                                     uiOutput("sample_prediction_heading"),
                                     dataTableOutput("sample_predictions")
                              )

                            ))
                  )))
)

server <- function(input, output) {

  options(shiny.maxRequestSize = 800*1024^2)   # This is a number which specifies the maximum web request size,
  # which serves as a size limit for file uploads.
  # If unset, the maximum request size defaults to 5MB.
  # The value I have put here is 800MB


  output$sample_input_data_heading = renderUI({   # show only if data has been uploaded
    inFile <- input$file1

    if (is.null(inFile)){
      return(NULL)
    }else{
      tags$h4('Sample data')
    }
  })

  output$sample_input_data = renderDataTable({    # show sample of uploaded data
    inFile <- input$file1

    if (is.null(inFile)){
      return(NULL)
    }else{
      tryCatch({ read.csv(input$file1$datapath, sep = input$sep, dec = input$dec, quote = input$quote, na.strings = c("NA","","-"," "))},
                             error = function(cond){
                               return("Please, check your CSV options")
                             })
    }
  })



  predictions <- reactive({

    inFile <- input$file1

    if (is.null(inFile)){
      return(NULL)
    }else{
      withProgress(message = 'Predictions in progress. Please wait ...', {
        imput_data =  read.csv(input$file1$datapath, sep = input$sep, dec = input$dec, quote = input$quote, na.strings = c("NA","","-"," "))
        row_data <- imput_data

        colnames(imput_data)[which(grepl("^[A-Za-z]+$", names(imput_data), perl = TRUE))] <- "Subject"
        colnames(imput_data)[which(grepl("29|norm", names(imput_data)))] <- "normalizer"
        colnames(imput_data)[which(grepl("R21|r21|\\.21|_21|-21", names(imput_data)))] <- "mir_21_5p"

        names(imput_data) <- ifelse(grepl("221", names(imput_data)), "mir_221_3p",
                                ifelse(grepl("192", names(imput_data)), "mir_192_5p",
                                  ifelse(grepl("191", names(imput_data)), "mir_191_5p",
                                               ifelse(grepl("93", names(imput_data)), "mir_93_5p",
                                                            ifelse(grepl("362", names(imput_data)), "mir_362_3p",
                                                                         ifelse(grepl("200", names(imput_data)), "mir_200c_3p",names(imput_data)))))))

        imput_data[,which(grepl("mir", names(imput_data)))] <- imput_data[which(grepl("mir", names(imput_data)))]  - imput_data$normalizer

        coefs <- glmnet.fit$beta[,24]
        intercepts <- coefs[c("cp1", "cp2", "cp3")]
        LP1 = -4.061045 + intercepts[1] + coefs["hsa.miR.221.3p.1"]*imput_data[, "mir_221_3p"] +
          coefs["hsa.miR.93.5p.1"]*imput_data[, "mir_93_5p"] +
          coefs["hsa.miR.362.3p.1"]*imput_data[, "mir_362_3p"] +
          coefs["hsa.miR.191.5p.1"]*imput_data[, "mir_191_5p"] +
          coefs["hsa.miR.200c.3p.1"]*imput_data[, "mir_200c_3p"] +
          coefs["hsa.miR.192.5p.1"]*imput_data[, "mir_192_5p"] +
          coefs["hsa.miR.21.5p.1"]*imput_data[, "mir_21_5p"]

        LP2 = -4.061045 + intercepts[2] + coefs["hsa.miR.221.3p.1"]*imput_data[, "mir_221_3p"] +
          coefs["hsa.miR.93.5p.1"]*imput_data[, "mir_93_5p"] +
          coefs["hsa.miR.362.3p.1"]*imput_data[, "mir_362_3p"] +
          coefs["hsa.miR.191.5p.1"]*imput_data[, "mir_191_5p"] +
          coefs["hsa.miR.200c.3p.1"]*imput_data[, "mir_200c_3p"] +
          coefs["hsa.miR.192.5p.1"]*imput_data[, "mir_192_5p"] +
          coefs["hsa.miR.21.5p.1"]*imput_data[, "mir_21_5p"]

        LP3 = -4.061045 + intercepts[3] + coefs["hsa.miR.221.3p.1"]*imput_data[, "mir_221_3p"] +
          coefs["hsa.miR.93.5p.1"]*imput_data[, "mir_93_5p"] +
          coefs["hsa.miR.362.3p.1"]*imput_data[, "mir_362_3p"] +
          coefs["hsa.miR.191.5p.1"]*imput_data[, "mir_191_5p"] +
          coefs["hsa.miR.200c.3p.1"]*imput_data[, "mir_200c_3p"] +
          coefs["hsa.miR.192.5p.1"]*imput_data[, "mir_192_5p"] +
          coefs["hsa.miR.21.5p.1"]*imput_data[, "mir_21_5p"]


        cond1 <- exp(LP1)/(1+exp(LP1))
        cond2 <- exp(LP2)/(1+exp(LP2))
        cond3 <- exp(LP3)/(1+exp(LP3))

        P4 = cond3
        P3 = cond2*(1-cond3)
        P2 = cond1*(1-cond2)*(1-cond3)
        P1 = 1 - (P2+P3+P4)

        predicciones <- data.frame(Control = P1, TaG1 = P2, TaT1G3 = P3, T2GX= P4)
        return(cbind(Subject = row_data$Subject,predicciones))

      })
    }
  })


  output$sample_prediction_heading = renderUI({  # show only if data has been uploaded
    inFile <- input$file1

    if (is.null(inFile)){
      return(NULL)
    }else{
      tags$h4('Sample predictions')
    }
  })

  output$sample_predictions = renderDataTable({   # the last 6 rows to show
    pred = predictions()
    pred2 <- formattable(pred, align =c("l","c","c","c","r"), list(
      'Subject' = formatter("span", style = ~ style(color = "grey",font.weight = "bold")),
      `Control`= color_tile("#DeF7E9", "#71CA97"),
      `TaG1`= color_tile("#DeF7E9", "#71CA97"),
      `TaT1G3`= color_tile("#DeF7E9", "#71CA97"),
      `T2GX`= color_tile("#DeF7E9", "#71CA97")
    ))
    as.datatable(pred2)

  })


  # Downloadable csv of predictions ----

  output$downloadData <- downloadHandler(
    filename = function() {
      paste("input_data_with_predictions", ".csv", sep = "")
    },
    content = function(file) {
      write.csv(predictions(),file,sep = input$sep, dec = input$dec , row.names = FALSE)
    })

}

shinyApp(ui = ui, server = server)



