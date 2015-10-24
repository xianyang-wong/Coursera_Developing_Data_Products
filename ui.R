library(ggvis)
require(markdown)

# For dropdown menu
actionLink <- function(inputId, ...) {
  tags$a(href='javascript:void',
         id=inputId,
         class='action-button',
         ...)
}

axis_vars <- c(
  'Calories',
  'Rating',
  'Protein',
  'Fat',
  'Sodium',
  'DietaryFiber',
  'Carbohydrates', 
  'Sugars',
  'Potassium',
  'Vitamins'
)

shinyUI(navbarPage("Cereal Explorer",
          tabPanel("Visualize the Data",
            fluidRow(
              column(3,
                     wellPanel(
                       selectInput("yvar", "Y-axis variable", axis_vars, selected = "Rating"),
                       selectInput("xvar", "X-axis variable", axis_vars, selected = "Calories")),
                     wellPanel(
                       h4("Filter"),
                       
                       selectInput("Manufacturer", "Name of Manufacturer",
                                   c("All", "Nabisco", "Quaker Oats", "Kellogs", "Ralston Purina",
                                     "General Mills", "Post", "American Home Food Products"),
                                   # multiple = TRUE,
                                   selected = "All"),
                       
                       sliderInput("calories", "Calories", 0, 200, value = c(50, 160)),
                       
                       sliderInput("protein", "Protein (g)", 0, 10, value = c(1, 6)),
                       
                       sliderInput("fat", "Fat (g)", 0, 5, value = c(0, 5)),
                       
                       sliderInput("sodium", "Sodium (mg)", 0, 350, value = c(0, 320)),
                       
                       sliderInput("fiber", "Dietary Fiber (g)", 0, 15, value = c(0, 14)),
                       
                       sliderInput("carbo", "Carbohydrates (g)", -1, 25, value = c(-1, 23)),
                       
                       sliderInput("sugars", "Sugars (g)", -1, 15, value = c(-1, 15)),
                       
                       sliderInput("potass", "Potassium (mg)", -1, 350, value = c(-1, 330)),
                       
                       sliderInput("vitamins", "Vitamins & Minerals", 0, 100, c(0, 100), step = 25)
                       
                     )
                   
            ),
            column(9,
                   ggvisOutput("plot1"),
                   wellPanel(
                     span("Number of Cereals selected:",
                          textOutput("n_cereals"))
                   )
            )
          )
          )
          ,
          tabPanel("Documentation",
                   mainPanel(
                     includeMarkdown("about.md")
                   ))
        )
                 
)


