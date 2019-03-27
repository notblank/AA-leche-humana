library(shiny)
library(shinyWidgets)
library(haven)
library(tidyverse)
library(broom)

# data frame : 
AA_Sex_1 <- read_sav("C:/Users/Personal/AA leche/datos/AMINOACIDOS POR SEXO_1.sav")

# my functions :
aaSelect <- function(df, AA){
  aaDf <- df %>% select(id = ID, group = GROUP, age = MATERNALAGE, sex = SEXO, starts_with(AA)) %>%
    mutate(id = factor(id), 
           sex =  factor(sex, labels = c('girl', 'boy')),
           group = factor(group, labels = c('teen', 'adult'))) 
  
  return(aaDf)
}
aaLong <-  function(df, AA){
  aaL <- df %>% gather(week, level, starts_with(AA))
  
  aaL[aaL == paste(AA, 'Calostrum', sep = '')] = 1
  aaL[aaL == paste(AA, 'Transition', sep = '')] = 2
  aaL[aaL == paste(AA, 'Mature2m', sep = '')] = 8
  aaL[aaL == paste(AA, 'Mature4m', sep = '')] = 16
  
  aaL <- aaL %>% mutate(week = as.numeric(week))
  
  return(aaL)
}
sexModels <- function(df){
  
  mS <- lm(level~week, data = df)
  
  return(mS)
}


# ui -----------------------------

ui <- fluidPage(
  
  # sidebarPanel -----------------------------
  sidebarPanel(
    pickerInput(
      inputId = "AA", 
      label = "Amino Acid", 
      choices = list(Essential = list("ARG", "HIS", "ILE", "LEU", "LYS", 
                                      "MET", "PHE", "THR", "TRP", "VAL"), 
                     Non_Essential = list("ASP", "ALA", "ASN", "CYS", 
                                          "GLY", "GLU", "GLN", "PRO",
                                          "SER", "TYR"))
    )
    
  ),
  
  mainPanel(
    #plotOutput("lines")
    #plotOutput("lines", hover = hoverOpts(id ="plot_hover"))
    #verbatimTextOutput("hover_info")
    plotOutput("linesSex")
  )
  
)

# server -----------------------------
server <- function(input, output){
  
  AAdf <- reactive({
    AAdf <- aaSelect(AA_Sex_1, input$AA)
    AAdf <- AAdf %>% filter(!is.na(AAdf$sex))
  })
  
  output$lines <- renderPlot({
    
    AAdf <- AAdf() %>% select(-contains('GLOB'))
    AAdfLong <- aaLong(AAdf, input$AA) 
    
    ggplot(AAdfLong, aes(x = week, y = level, group = id, color = group)) + 
      geom_line(size = 1.2, alpha = 0.5) + geom_point()
    
  })
  
  output$linesSex <- renderPlot({
    
    AAdf <- AAdf() %>% select(-contains('GLOB'))
    AAdfLong <- aaLong(AAdf, input$AA) 
    
    ggplot(AAdfLong, aes(x = week, y = level, group = id, color = sex)) + 
      geom_line(size = 1.2, alpha = 0.5) + geom_point()
    
  })
  
  # output$hover_info <- renderPrint({
  #   hover = input$plot_hover
  #   print(hover$x)
  # })
  
}

# App -----------------------------
shinyApp(ui = ui, server = server)
