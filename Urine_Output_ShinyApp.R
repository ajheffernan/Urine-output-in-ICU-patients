library(shiny)
library(caret)
load("XGBoost_ICU_ANZICS_DeathPred.rda") 

# Define UI
ui <- shinyUI(fluidPage(
  
  # Define custom styles
  tags$head(tags$style(
    HTML('
         .title_panel{
         font-size: 40px;
         line-height: 40px;
         font-weight: 900
         }
         .header-row{
         padding-left: 25px;
         padding-bottom: 8px;
         background-color: #1bb193;
         color:#ffffff;
         font-family: "Calibri";
         }
         .content-row{
         border: 1px silver solid;
         margin-left: 8px;
         margin-right: 8px;
         margin-top: -5px;
         padding: 15px;
         }
         .content-title{
         font-weight: 1000;
         margin-left: 8px;
         margin-top: 25px;
         }
         .result-button, .result-button:focus{
         font-weight: 600;
         background-color: #1bb193;
         color: white;
         border-color: #1bb193;  
         margin-top: 30px;
         margin-left: 23px;
         font-size: 25px;
         }
         .result-button:hover{
         background-color: #117d67;
         border-color: #117d67;  
         color: white;
         }
         .button-wrapper{
         text-align: center;
         }
         .checkbox-header{
         font-weight: 600;
         }
         .result-panel{
         border: 1px #117d67 solid;
         background-color: #117d67;
         margin-left: 8px;
         margin-right: 8px;
         margin-top: 10px;
         padding: 15px;
         color: #ffffff;  
         font-size: 20px;
         }
         .result-header{
         font-weight: 700;
         font-size: 32px;
         }
         body, label, input, button, select{ 
         font-family: "Calibri";
         color: #595959;
         }
         ')
    )),
  
  fluidRow(class = "header-row",
           h1(class = "title_panel", "Mortality Probability Calculator")
  ),
  
  fluidRow(
    actionButton(class = "result-button", "submit", "Calculate Probability")
  ),
  
  htmlOutput("results_panel"),
  
  h4(class = "content-title", "General Information"),
  fluidRow(class = "content-row",
           column(3,
                  radioButtons(inputId = "sex", label = "Sex", choices = c("Male", "Female"), selected = "Male", inline = TRUE),
                  radioButtons(inputId = "indigenous", label = "Aboriginal/torres straight islander", choices = c("Yes", "No"), selected = "No", inline = TRUE)
           ),
           column(3,
                  radioButtons(inputId = "elect", label = "Elective admission", choices = c("Yes", "No"), selected = "No", inline = TRUE),
                  selectInput(inputId = "thrombpro", label = "Thrombpro", choices = c("No", "Yes", "Contraindicated", "Not indicated"))
           ),
           column(3,
                  sliderInput(inputId = "age", label = "Age", min = 16, max = 104, value = 60)
           ),
           column(3,
                  sliderInput(inputId = "weight", label = "Weight (Kg)", min = 20, max = 318, value = 80, step = 0.01)
           )
  ),
  
  h4(class = "content-title", "Medical Information"),
  fluidRow(
    column(7,       
           fluidRow(class = "content-row",
                    column(4,
                           checkboxGroupInput("diseases", h5(class = "checkbox-header", "Diagnosed Diseases"), choices = list("Lymphoma" = 1, "Metastatic cancer" = 2, "Leukaemia" = 3, "Immunological disorder" = 4, "AIDS" = 5, "Chronic respiratory condition" = 6, "Chronic cardiovascular condition" = 7, "Chronic liver condition" = 8, "Unknown diagnosis" = 9))
                    ),
                    column(5,
                           checkboxGroupInput("special_conditions", h5(class = "checkbox-header", "Special Conditions"), choices = list("Cardiac arrest within 24h of admission" = 1, "Respiratory arrest within 24h of admission" = 2, "Hepatic failure with encephalopathy" = 3, "Intubated" = 4))
                    ),
                    column(3,
                           checkboxGroupInput("other", h5(class = "checkbox-header", "Other"), choices = list("Trauma" = 1, "Metabolic/toxicological" = 2, "Sepsis" = 3))
                           
                    )
           )
    ),
    column(5,
           fluidRow(class = "content-row",
                    column(6,
                           checkboxGroupInput("medicines", h5(class = "checkbox-header", "Medical Admission Diagnosis"), choices = list("Respiratory" = 1, "Cardiology" = 2, "Gastrointestinal" = 3, "Neurological" = 4, "Genitourinary/renal" = 5, "Musculoskeletal" = 6, "Haematology" = 7, "Immunosuppressive therapy" = 8))
                    ),
                    column(6,
                           checkboxGroupInput("surgeries", h5(class = "checkbox-header", "Surgical Admission Diagnosis"), choices = list("Respiratory" = 1, "Cardiology" = 2, "Gastrointestinal" = 3, "Neurological" = 4, "Genitourinary/renal" = 5, "Musculoskeletal" = 6, "Obstetrics/gynaecology" = 7))
                    )  
           )
    )
  ),
  
  h4(class = "content-title", "Measurements"),
  fluidRow(class = "content-row",
           column(3,
                  sliderInput(inputId = "hr_anz", label = "Worst  heart rate per minute", min = 0, max = 300, value = 150, step = 0.01),
                  sliderInput(inputId = "rr_anz", label = "Worst respiratory rate per minute", min = 0, max = 80, value = 40, step = 0.01),
                  sliderInput(inputId = "hct_anz", label = "Worst haematocrit", min = 0.05, max = 0.75, value = 0.4, step = 0.01),
                  sliderInput(inputId = "temp_anz", label = "Worst temperature within the first 24h (C)", min = 25, max = 46, value = 35, step = 0.01),
                  sliderInput(inputId = "uo_wt_hr", label = "Urine output over 24h per Kg / 24", min = 0, max = 20.9, value = 10.5, step = 0.000001)
           ),
           column(3,
                  sliderInput(inputId = "map_anz", label = "Worst  pressure (mmHg)", min = 0, max = 281, value = 140, step = 0.01),
                  sliderInput(inputId = "pao2_anz", label = HTML(paste0("Worst arterial O",tags$sub("2"), " partial pressure (mmHg)")), min = 15, max = 715, value = 385, step = 0.01),
                  sliderInput(inputId = "paco2_anz", label = HTML(paste0("Worst arterial CO",tags$sub("2"), " partial pressure (mmHg)")), min = 5, max = 249, value = 126, step = 0.01),
                  sliderInput(inputId = "ph_anz", label = "Worst arterial pH", min = 6.5, max = 8.5, value = 7.5, step = 0.01),
                  sliderInput(inputId = "wcc_anz", label = HTML(paste0("Worst white cell count (x10",tags$sup("9"), "/L)")), min = 0, max = 300, value = 150, step = 0.01)
           ),
           column(3,
                  sliderInput(inputId = "na_anz", label = "Worst sodium (mmol/L)", min = 100, max = 215, value = 157, step = 0.01),
                  sliderInput(inputId = "k_ap2", label = "Worst potassium (mmol/L)", min = 0.05, max = 12, value = 6, step = 0.01),
                  sliderInput(inputId = "hco3_ap2", label = "Worst bicarbonate (mmol/L)", min = 2, max = 60, value = 31, step = 0.01),
                  sliderInput(inputId = "glucose_anz", label = "Worst glucose (mmol/L)", min = 0, max = 90, value = 45, step = 0.01),
                  sliderInput(inputId = "creat_anz", label = "Worst creatinine (micromol/L)", min = 10, max = 2500, value = 1260, step = 0.01)
           ),
           column(3,
                  sliderInput(inputId = "bili_anz", label = "Worst unconjugated bilirubin (micromol/L)", min = 1, max = 1129, value = 565, step = 0.01),
                  sliderInput(inputId = "albumin_anz", label = "Worst albumin (g/L)", min = 5, max = 65, value = 35, step = 0.01),
                  selectInput(inputId = "gcsmotor", label = "Worst GCS motor score", choices = c("1", "2", "3", "4", "5", "6")),
                  selectInput(inputId = "gcseye", label = "Worst GCS eye score", choices = c("1", "2", "3", "4")),
                  selectInput(inputId = "gcsverb", label = "Worst GCS verbal score", choices = c("1", "2", "3", "4", "5"))
           )
  ),
  
  br()
    ))


# Define server logic to prepare user inputs in the required format
server <- shinyServer(function(input, output, session) {
  
  observeEvent(input$submit, {
    
    user_inputs <- list()
    
    # General information
    user_inputs[["sex"]] <- if (input$sex == "Male") 0 else 1
    user_inputs[["indigenous"]] <- if (input$indigenous == "Yes") 1 else 0
    user_inputs[["elect"]] <- if (input$elect == "Yes") 1 else 0
    
    if(input$thrombpro == "Yes")
      user_inputs[["thrombpro"]] <- 1
    else if(input$thrombpro == "No")
      user_inputs[["thrombpro"]] <- 2
    else if(input[["thrombpro"]] == "Contraindicated")
      user_inputs[["thrombpro"]] <- 3
    else
      user_inputs[["thrombpro"]] <- 4
    
    user_inputs[["age"]] <- input$age
    user_inputs[["weight"]] <- input$weight
    
    # Mediacal information
    # Diseases
    user_inputs[["lymphoma"]] <- if (1 %in% input$diseases) 1 else 0
    user_inputs[["metast"]] <- if (2 %in% input$diseases) 1 else 0
    user_inputs[["leukaem"]] <- if (3 %in% input$diseases) 1 else 0
    user_inputs[["immundis"]] <- if (4 %in% input$diseases) 1 else 0
    user_inputs[["aids"]] <- if (5 %in% input$diseases) 1 else 0
    user_inputs[["chr_resp"]] <- if (6 %in% input$diseases) 1 else 0
    user_inputs[["chr_cvs"]] <- if (7 %in% input$diseases) 1 else 0
    user_inputs[["chr_liv"]] <- if (8 %in% input$diseases) 1 else 0
    user_inputs[["ap3diag.0"]] <- if (9 %in% input$diseases) 1 else 0
    
    # Special conditions
    user_inputs[["cardarrest"]] <- if (1 %in% input$special_conditions) 1 else 0
    user_inputs[["resparrest"]] <- if (2 %in% input$special_conditions) 1 else 0
    user_inputs[["hepfail"]] <- if (3 %in% input$special_conditions) 1 else 0
    user_inputs[["intubated"]] <- if (4 %in% input$special_conditions) 1 else 0
    
    # Other
    user_inputs[["ap3diag.1"]] <- if (1 %in% input$other) 1 else 0
    user_inputs[["ap3diag.7"]] <- if (2 %in% input$other) 1 else 0
    user_inputs[["ap3diag.17"]] <- if (3 %in% input$other) 1 else 0
    
    # Medicines
    user_inputs[["ap3diag.2"]] <- if (1 %in% input$medicines) 1 else 0
    user_inputs[["ap3diag.4"]] <- if (2 %in% input$medicines) 1 else 0
    user_inputs[["ap3diag.5"]] <- if (3 %in% input$medicines) 1 else 0
    user_inputs[["ap3diag.6"]] <- if (4 %in% input$medicines) 1 else 0
    user_inputs[["ap3diag.9"]] <- if (5 %in% input$medicines) 1 else 0
    user_inputs[["ap3diag.10"]] <- if (6 %in% input$medicines) 1 else 0
    user_inputs[["ap3diag.8"]] <- if (7 %in% input$medicines) 1 else 0
    user_inputs[["immunrx"]] <- if (8 %in% input$medicines) 1 else 0
    
    # Surgeries
    user_inputs[["ap3diag.12"]] <- if (1 %in% input$surgeries) 1 else 0
    user_inputs[["ap3diag.11"]] <- if (2 %in% input$surgeries) 1 else 0
    user_inputs[["ap3diag.3"]] <- if (3 %in% input$surgeries) 1 else 0
    user_inputs[["ap3diag.13"]] <- if (4 %in% input$surgeries) 1 else 0
    user_inputs[["ap3diag.14"]] <- if (5 %in% input$surgeries) 1 else 0
    user_inputs[["ap3diag.16"]] <- if (6 %in% input$surgeries) 1 else 0
    user_inputs[["ap3diag.15"]] <- if (7 %in% input$surgeries) 1 else 0
    
    # Measurements
    user_inputs[["uo_wt_hr"]] <- input$uo_wt_hr
    user_inputs[["temp_anz"]] <- input$temp_anz
    user_inputs[["map_anz"]] <- input$map_anz
    user_inputs[["hr_anz"]] <- input$hr_anz
    user_inputs[["rr_anz"]] <- input$rr_anz
    user_inputs[["pao2_anz"]] <- input$pao2_anz
    user_inputs[["paco2_anz"]] <- input$paco2_anz
    user_inputs[["ph_anz"]] <- input$ph_anz
    user_inputs[["hco3_ap2"]] <- input$hco3_ap2
    user_inputs[["na_anz"]] <- input$na_anz
    user_inputs[["k_ap2"]] <- input$k_ap2
    user_inputs[["creat_anz"]] <- input$creat_anz
    user_inputs[["hct_anz"]] <- input$hct_anz
    user_inputs[["wcc_anz"]] <- input$wcc_anz
    user_inputs[["albumin_anz"]] <- input$albumin_anz
    user_inputs[["bili_anz"]] <- input$bili_anz
    user_inputs[["glucose_anz"]] <- input$glucose_anz
    
    user_inputs[["gcsmotor.1"]] <- 0
    user_inputs[["gcsmotor.2"]] <- 0
    user_inputs[["gcsmotor.3"]] <- 0
    user_inputs[["gcsmotor.4"]] <- 0
    user_inputs[["gcsmotor.5"]] <- 0
    user_inputs[["gcsmotor.6"]] <- 0
    
    if(input$gcsmotor == "1")
      user_inputs[["gcsmotor.1"]] <- 1
    else if(input$gcsmotor == "2")
      user_inputs[["gcsmotor.2"]] <- 1
    else if(input$gcsmotor == "3")
      user_inputs[["gcsmotor.3"]] <- 1
    else if(input$gcsmotor == "4")
      user_inputs[["gcsmotor.4"]] <- 1
    else if(input$gcsmotor == "5")
      user_inputs[["gcsmotor.5"]] <- 1
    else if(input$gcsmotor == "6")
      user_inputs[["gcsmotor.6"]] <- 1
    
    user_inputs[["gcseye.1"]] <- 0
    user_inputs[["gcseye.2"]] <- 0
    user_inputs[["gcseye.3"]] <- 0
    user_inputs[["gcseye.4"]] <- 0
    
    if(input$gcseye == "1")
      user_inputs[["gcseye.1"]] <- 1
    else if(input$gcseye == "2")
      user_inputs[["gcseye.2"]] <- 1
    else if(input$gcseye == "3")
      user_inputs[["gcseye.3"]] <- 1
    else if(input$gcseye == "4")
      user_inputs[["gcseye.4"]] <- 1
    
    user_inputs[["gcsverb.1"]] <- 0
    user_inputs[["gcsverb.2"]] <- 0
    user_inputs[["gcsverb.3"]] <- 0
    user_inputs[["gcsverb.4"]] <- 0
    user_inputs[["gcsverb.5"]] <- 0
    
    if(input$gcsverb == "1")
      user_inputs[["gcsverb.1"]] <- 1
    else if(input$gcsverb == "2")
      user_inputs[["gcsverb.2"]] <- 1
    else if(input$gcsverb == "3")
      user_inputs[["gcsverb.3"]] <- 1
    else if(input$gcsverb == "4")
      user_inputs[["gcsverb.4"]] <- 1
    else if(input$gcsverb == "5")
      user_inputs[["gcsverb.5"]] <- 1
    
    output$probability <- renderText({ 
      calculate_probability(user_inputs)
    })
    
    output$results_panel <- renderUI(
      wellPanel(
        class = "result-panel",
        h4(class = "result-header", "Result:"),
        # If required, define any additional information that you need to show other than the probability here
        textOutput("probability")
      )
    )
  })
})

# Call XGBoost model with user inputs
calculate_probability <- function(inputs){
  df <- data.frame(matrix(unlist(inputs), nrow=1, ncol=length(inputs), byrow=F))
  colnames(df)<-names(inputs)
  df$resparrest=as.factor(df$resparrest)
  df$indigenous=as.factor(df$indigenous)
  df$thrombpro=as.factor(df$thrombpro)
  df$sex=as.factor(df$sex)
  df$chr_resp=as.factor(df$chr_resp)
  df$chr_cvs=as.factor(df$chr_cvs)
  df$chr_liv=as.factor(df$chr_liv)
  df$lymphoma=as.factor(df$lymphoma)
  df$metast=as.factor(df$metast)
  df$leukaem=as.factor(df$leukaem)
  df$immundis=as.factor(df$immundis)
  df$immunrx=as.factor(df$immunrx)
  df$aids=as.factor(df$aids)
  df$hepfail=as.factor(df$hepfail)
  df$intubated=as.factor(df$intubated)
  df$cardarrest=as.factor(df$cardarrest)
  df$ap3diag.0=as.factor(df$ap3diag.0)
  df$ap3diag.1=as.factor(df$ap3diag.1)
  df$ap3diag.2=as.factor(df$ap3diag.2)
  df$ap3diag.3=as.factor(df$ap3diag.3)
  df$ap3diag.4=as.factor(df$ap3diag.4)
  df$ap3diag.5=as.factor(df$ap3diag.5)
  df$ap3diag.6=as.factor(df$ap3diag.6)
  df$ap3diag.7=as.factor(df$ap3diag.7)
  df$ap3diag.8=as.factor(df$ap3diag.8)
  df$ap3diag.9=as.factor(df$ap3diag.9)
  df$ap3diag.10=as.factor(df$ap3diag.10)
  df$ap3diag.11=as.factor(df$ap3diag.11)
  df$ap3diag.12=as.factor(df$ap3diag.12)
  df$ap3diag.13=as.factor(df$ap3diag.13)
  df$ap3diag.14=as.factor(df$ap3diag.14)
  df$ap3diag.15=as.factor(df$ap3diag.15)
  df$ap3diag.16=as.factor(df$ap3diag.16)
  df$ap3diag.17=as.factor(df$ap3diag.17)
  df$gcsmotor.1=as.factor(df$gcsmotor.1)
  df$gcsmotor.2=as.factor(df$gcsmotor.2)
  df$gcsmotor.3=as.factor(df$gcsmotor.3)
  df$gcsmotor.4=as.factor(df$gcsmotor.4)
  df$gcsmotor.5=as.factor(df$gcsmotor.5)
  df$gcsmotor.6=as.factor(df$gcsmotor.6)
  df$gcseye.1=as.factor(df$gcseye.1)
  df$gcseye.2=as.factor(df$gcseye.2)
  df$gcseye.3=as.factor(df$gcseye.3)
  df$gcseye.4=as.factor(df$gcseye.4)
  df$gcsverb.1=as.factor(df$gcsverb.1)
  df$gcsverb.2=as.factor(df$gcsverb.2)
  df$gcsverb.3=as.factor(df$gcsverb.3)
  df$gcsverb.4=as.factor(df$gcsverb.4)
  df$gcsverb.5=as.factor(df$gcsverb.5)
  Hosp_Death=predict(XGBoost_death_search_smote, newdata=df, type="prob")
  Probdied=round(Hosp_Death$died*100, digits=3)
  paste(Probdied,"%")
}


# Create Shiny app ----
shinyApp(ui = ui, server = server)

