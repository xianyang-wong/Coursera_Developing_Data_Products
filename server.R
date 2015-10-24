library(ggvis)
library(dplyr)
if (FALSE) library(RSQLite) 

# Set up handles to database tables on app start
all_cereal <- read.csv("cereal_full2.csv", header = TRUE, stringsAsFactors = FALSE)

mfr <- all_cereal$mfr
mfr[mfr=="N"] <- "Nabisco" 
mfr[mfr=="A"] <- "American Home Food Products" 
mfr[mfr=="G"] <- "General Mills" 
mfr[mfr=="K"] <- "Kellogs" 
mfr[mfr=="P"] <- "Post" 
mfr[mfr=="Q"] <- "Quaker Oats" 
mfr[mfr=="R"] <- "Ralston Purina" 

type <- all_cereal$type
type[type=="C"] <- "Cold"
type[type=="H"] <- "Hot"

all_cereal$mfr <- mfr
all_cereal$type <- type

names(all_cereal) <- c("Cereal", "Manufacturer", "Type", "Calories"
                       , "Protein", "Fat", "Sodium", "DietaryFiber"
                       , "Carbohydrates", "Sugars", "Potassium", "Vitamins"
                       , "Shelf", "ServingSize", "Cups", "Rating")



crl <- all_cereal
crl$id <- 1:nrow(crl)
all_cereal$id <- crl$id

# axis_vars <- c(
#   'Calories'='calories',
#   'Rating'='rating',
#   'Protein'='protein',
#   'Fat'='fat',
#   'Sodium'='sodium',
#   'DietaryFiber'='fiber',
#   'Carbohydrates'='carbo', 
#   'Sugars'='sugars',
#   'Potassium'='potass',
#   'Vitamins'='vitamins'
# )

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


shinyServer(function(input, output, session) {
  
  # Filter the Cereals, returning a data frame
  cereals <- reactive({

    mincalories <- input$calories[1]
    maxcalories <- input$calories[2]
    minprotein <- input$protein[1]
    maxprotein <- input$protein[2]
    minfat <- input$fat[1]
    maxfat <- input$fat[2]
    minsodium <- input$sodium[1]
    maxsodium <- input$sodium[2]
    minfiber <- input$fiber[1]
    maxfiber <- input$fiber[2]
    mincarbo <- input$carbo[1]
    maxcarbo <- input$carbo[2]
    minsugars <- input$sugars[1]
    maxsugars <- input$sugars[2]
    minpotass <- input$potass[1]
    maxpotass <- input$potass[2]
    minvitamins <- input$vitamins[1]
    maxvitamins <- input$vitamins[2]
    
    # Apply filters
    c <- all_cereal %>%
      filter(
              Calories >= mincalories,
              Calories <= maxcalories,
           Protein >= minprotein,
          Protein <= maxprotein,
             Fat >= minfat,
             Fat <= maxfat,
             Sodium >= minsodium,
             Sodium <= maxsodium,
             DietaryFiber >= minfiber,
             DietaryFiber <= maxfiber,
             Carbohydrates >= mincarbo,
             Carbohydrates <= maxcarbo,
             Sugars >= minsugars,
             Sugars <= maxsugars,
             Potassium >= minpotass,
             Potassium <= maxpotass,
             Vitamins >= minvitamins,
             Vitamins <= maxvitamins
      ) 
    
    # Optional: Filter by Manufacturer
    if (input$Manufacturer != "All") {
      c <- c %>% filter(Manufacturer == input$Manufacturer)
    }
    # Optional: Filter by Cereal
  c <- as.data.frame(c)
    
  c
    
  })
  
  
  
  # Function for generating tooltip text
  cereal_tooltip <- function(x) {
    if (is.null(x)) return(NULL)
    if (is.null(x$id)) return(NULL)
    
    # Pick out the cereal with this ID
    # row <- crl[crl$id == x$id, ]
    
    all_cereal <- isolate(cereals())
    cereal1 <- all_cereal[all_cereal$id == x$id, ]
    
    paste0("<b>", cereal1$Cereal, "</b><br>", cereal1$Type)
  }
  
  # A reactive expression with the ggvis plot
  vis <- reactive({
    # Lables for axes
    xvar_name <- names(axis_vars)[axis_vars == input$xvar]
    yvar_name <- names(axis_vars)[axis_vars == input$yvar]

    xvar <- prop("x", as.symbol(input$xvar))
    yvar <- prop("y", as.symbol(input$yvar))
    
    cereals %>%
      ggvis(x= xvar, y= yvar, fill=~Manufacturer) %>%
      layer_points(size := 50, size.hover := 200,
                   key := ~id) %>%
      add_tooltip(cereal_tooltip, "hover") 
  })
  
  vis %>% bind_shiny("plot1")
  output$n_cereals <- renderText({ nrow(cereals()) })
})