library(shiny)

# Define the overall UI
# Define UI for application that plots random distributions 
shinyUI(pageWithSidebar(
  
  # Application title
  headerPanel("Coincidence Analysis (Unamuno Foto Archive)"),
  
  # Sidebar with a slider input for number of observations
  sidebarPanel(
    selectInput("pValue",
                "Coincidence type:",
                c("Mere" = 1,"Probable" = .5,"Significant (p<.05)" = 0.05,"Quite significant (p<.01)" =0.01,"Very significant (p<.001)" =0.001),
                selected= .5
    ),
    selectInput("layout",
                "Layout:",
                c("Fruchterman-Reingold" = "fruchterman.reingold",
                  "GEM force-directed" = "gem",
                  "Kamada-Kawai" = "kamada-kawai",
                  "Star" = "star",
                  "Circle" = "circle",
                  "Random" = "random"),           
                selected= "fruchterman.reingold"
    ),
    selectInput("color",
                "Color:",
                c("None" = "$",
                  "Gender" = "Sexo",
                  "Generation" = "Edad",
                  "Sphere" = "Ambito"),
                selected= "Sexo"
    ),
    selectInput("shape",
                "Shape:",
                c("None" = "$",
                  "Gender" = "sexo",                  
                  "Sphere" = "ambito"),
                selected= "ambito"
    ),
  
    sliderInput("Minimum",
                "Node minimum frequency:",
                min = 1,
                max = 20,
                value = 5
    ),
    sliderInput("NodeSize",
                "Node size:",
                min = 1,
                max = 100,
                value = 50
    ),
    sliderInput("EdgeSize",
                "Edge width:",
                min = 1,
                max = 20,
                value = 5)
  ),
  # Show a plot of the generated distribution
  mainPanel(
    plotOutput("distPlot", height=600)
  )
))
