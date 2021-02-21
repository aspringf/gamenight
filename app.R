#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
# library( rsconnect )
# deployApp( account = "springford" )
#
# https://docs.google.com/spreadsheets/d/1lOfM7muOUG7XH5aBhCxtVEN4ZbuqZs0GmZ99S74dEqg/edit?usp=sharing
####

library(shiny)
# library(googlesheets)

# Define UI for application that draws a histogram
ui <- fluidPage(
    titlePanel( "What should we play today?" ),
    sidebarLayout(
        sidebarPanel(
            sliderInput( "nplay", "How many are playing?", 
                         min = 1, max = 8, value = 6, step = 1, 
                         round = TRUE, post = " players", ticks = FALSE ),
            actionButton( "go", "Tell me what to play!" ),
            checkboxInput( "random", "Randomize result" )
        ),
        mainPanel(
            htmlOutput( "one" )
        )
    )
    
    
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    # all_games <- read.csv( "all_games.csv" )
    all_games <- read.csv( "https://docs.google.com/spreadsheets/d/e/2PACX-1vTss-XmJLZ9wrgAC16B8e4U86f3SNmMyAEBxn7q_m0Nh7MKmUuJrZ8TNbI2qPgrrsrx--H2Ozs9hiTc/pub?gid=1763014796&single=true&output=csv" )
    # game, min_p, max_p, img_link, bgg_link
    
    gm_react <- reactiveValues()
    
    # limit to games with the appropriate number of players
    observe({ gm_react$all_games_pp <- subset( all_games, min_p <= input$nplay & max_p >= input$nplay )
        output$one <- renderUI( tags$img( src = "https://www.queensu.ca/sites/default/files/assets/fp-slideshow/2018-07/DroneKingston-201806-OntarioHall_800x457.jpg" ) )
    })
    
    observeEvent( input$go, {
        
        if( !input$random ){
            set.seed( as.integer( floor( 
                julian( Sys.time(), 
                        origin = as.POSIXct("1970-01-01", tz = "America/New_York") )
                ) ) )
        }else{
            set.seed( as.integer(Sys.time()) )
        }
        
        # Randomize!
        gm_react$ii <- sample( nrow(gm_react$all_games_pp) )
        
        output$one <- renderUI({
            tagList(
                h3("Here's one suggestion:"),
                tags$a( href=gm_react$all_games_pp[ gm_react$ii[1], "bgg_link" ],
                        target = "_blank",
                        tags$img( src = gm_react$all_games_pp[ gm_react$ii[1], "img_link" ],
                                  alt = gm_react$all_games_pp[ gm_react$ii[1], "game" ] ) ),
                br(),
                h3("If you don't like that, here's a second:"),
                tags$a( href=gm_react$all_games_pp[ gm_react$ii[2], "bgg_link" ],
                        target = "_blank",
                        tags$img( src = gm_react$all_games_pp[ gm_react$ii[2], "img_link" ],
                                  alt = gm_react$all_games_pp[ gm_react$ii[2], "game" ] ) ),
                br(),
                h3("Still not happy? Here's a third:"),
                tags$a( href=gm_react$all_games_pp[ gm_react$ii[3], "bgg_link" ],
                        target = "_blank",
                        tags$img( src = gm_react$all_games_pp[ gm_react$ii[3], "img_link" ],
                                  alt = gm_react$all_games_pp[ gm_react$ii[3], "game" ] ) ),
                br()
            )
        })
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)



