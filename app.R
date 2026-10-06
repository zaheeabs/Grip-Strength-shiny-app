# library(shiny)
library(bslib)

dominant_model <- readRDS("dominant_model.rds")
nondominant_model <- readRDS("nondominant_model.rds")
combined_model <- readRDS("combined_model.rds")

grip_data <- readRDS("grip_data.rds")  

ui <- page_fillable(
  theme = bs_theme(
    version = 5,
    bootswatch = "flatly",
    primary = "#0F6E56"
  ),
  
  card(
    card_header(h3("Grip strength calculator")),
    card_body(
      layout_column_wrap(
        width = 1/2,
        numericInput("age", "Age", value = 25, min = 16, max = 90),
        selectInput("sex", "Sex", choices = c("Male", "Female")),
        numericInput("weight", "Weight (kg)", value = 70),
        numericInput("height", "Height (cm)", value = 175)
      ),
      actionButton("calculate", "Calculate", class = "btn-primary w-100 mt-2"),
      
      layout_column_wrap(
        width = 1/3,
        class = "mt-4",
        value_box(title = "Dominant", value = textOutput("dom_out")),
        value_box(title = "Non-dominant", value = textOutput("non_out")),
        value_box(title = "Combined", value = textOutput("comb_out"))
      ),
      
      div(
        class = "mt-4",
        p(class = "text-muted small mb-1", "Percentile rank"),
        div(style = "height:10px;background:#eee;border-radius:6px;overflow:hidden;",
            div(id = "bar", style = "height:100%;width:0%;background:#0F6E56;")
        ),
        textOutput("pct_out")
      ),
      
      tags$script("Shiny.addCustomMessageHandler('setBarWidth', function(pct) {
      document.getElementById('bar').style.width = pct + '%';
      });")
    )
  )
)

server <- function(input, output, session) {
  
  prediction <- eventReactive(input$calculate, {
    new_person <- data.frame(
      Age = input$age,
      Sex = factor(input$sex, levels = c("Male","Female")),
      Weight = input$weight,
      Height = input$height
    )
    
    list(
      dom = predict(dominant_model, new_person),
      non = predict(nondominant_model, new_person),
      comb = predict(combined_model, new_person)
    )
  })
  
  output$dom_out  <- renderText(paste(round(prediction()$dom, 1), "kg"))
  output$non_out  <- renderText(paste(round(prediction()$non, 1), "kg"))
  output$comb_out <- renderText(paste(round(prediction()$comb, 1), "kg"))
  
  output$pct_out <- renderText({
    pct <- round(ecdf(grip_data$Combined)(prediction()$comb) * 100, 0)
    session$sendCustomMessage("setBarWidth", pct)
    paste0(pct, "th percentile")
  })
}

shinyApp(ui, server)