# mocktest of combining multiple separate shiny's into one app.
# each shiny runs wihtin it's own tab. The tab1.r and tab2.r files each
# contain an 'entire shiny', i.e. tab1 <- shinyApp() with the arguments 
# being the ui and server definitions. 
# (do not include calls to runApp() though) 
#

library(shiny)
library(shinyBS)
library(shinyjs)
library(shinyWidgets)
library(DT)


# run: theColor <- "#db0a76" to change contrast color to color of your choice

if(!exists(as.character(substitute(theColor)))) {theColor <- '#E69F00'}


source('app2trialtypes.R', local = TRUE)
source('app3trialtypes.R', local = TRUE)
source('app4trialtypes.R', local = TRUE)



# Define UI for application that draws a histogram
ui <- fluidPage(

  tags$body(
    tags$link(rel="stylesheet", type = "text/css", href = "https://fonts.googleapis.com/css?family=Roboto")
  ),
  
  tags$style(HTML("body {
                  font-family: 'roboto';
                  }")),
    
  tags$style(HTML(paste(".nav>li>a {
                    color: #999999; font-size: 16px; 
                    border-bottom: 1px solid", theColor, "; 
                   }" ))), # set colour and size of tab titles font and the shadow line underneath

  tags$style(HTML(paste(".nav-tabs > li > a:hover, .nav-tabs > li.active > a, .nav-tabs > li.active > a:focus,
                  .nav-tabs > li.active > a:hover, .nav-tabs > li.active > a:focus:hover {
                   border: none; background-color: transparent;
              #    -webkit-box-shadow: inset 0 0px 0", theColor, ";
              #    box-shadow: inset 0 -2px 0", theColor, ";
                  color:", theColor, ";
                   }"))), # set colour of tab titles active, hovering, focus 

  tags$style(HTML(paste(".nav-tabs {
    border-bottom: 1px solid", theColor, "; }"))),



  # Application title
  titlePanel("on pre-existing bias affecting training contingency in CBM"),
  
  #tab
    tabsetPanel(
              tabPanel("two trialtypes", app2trialtypes),
              tabPanel("three trialtypes", app3trialtypes),
              tabPanel("four trialtypes", app4trialtypes),
              tabPanel("information", 
                       h3("information"),
                       uiOutput("htmlText"),
                       br(), br(),
                      DT::dataTableOutput("infoTable"),
                      br()
  )))
 

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$htmlText <- renderUI(HTML(paste(
    "This shiny illustrates how pre-existing bias may affect the experienced training contingency in hidden-contingency CBM tasks, as described in:", "<br><br>", em("Mechanics of contingency-based Cognitive Bias Modification: pre-existing bias affects potency of active CBM but not placebo training."), "<br><br>", "Preprint available:", a("psyarxiv.com/scqf3", class = "web", href = "https://psyarxiv.com/scqf3"), "<br><br><br>", "The table below provides info on how to interpret the information in this app for different tasks:")))

  ### table with info on interpretations for different tasks:
  tabdat <- NULL
  
  tabdat <- data.frame(matrix(vector(), 0, 10, 
                              dimnames=list(c(), c("row", "task", "n_trialtypes", "stimclass", "trialtype_expl", "tt_C", "tt_I", "IRT_expl", "IRT_D", "IRT_U" ))),
                       stringsAsFactors=F)
  
  tabdat[1,] <- c("1", "dot-probe training with a single bias dimension", "2", "training", "response cue on location", "beneficial stim", "harmful stim", "gaze at", "beneficial stim", "harmful stim")
  tabdat[2,] <- c("2", "CBM-I comprehension question", "2", "positive training", "positive feedback will be given for:", "positive",	"negative",	"initial interpretation is",	"positive",	"negative")
  tabdat[3,] <- c("3", "dot-probe with single dimension + neutral trials", "3", "training", "response cue on location", "beneficial stim", "harmful stim", "gaze at", "beneficial stim", "harmful stim")
  tabdat[4,] <- c("3", "", "", "neutral", "", "-", "-", "", "-", "-")
  tabdat[5,] <- c("4", "dot-probe with two bias dimensions", "4", "HS", "response cue on location", "neutral stim", "harmful stim", "gaze at", "neutral stim", "harmful stim")
  tabdat[6,] <- c("4", "", "", "BS", "", "beneficial stim", "neutral stim", "", "beneficial stim", "neutral stim")
  tabdat[7,] <- c("5", "Posner/single cueing training", "4", "HS", "stim cue location is", "opposite location", "stimulus location", "gaze", "away from stim", "towards stim")
  tabdat[8,] <- c("5", "", "", "BS", "", "stimulus location", "opposite location", "", "towards stim", "away from stim")
  tabdat[9,] <- c("6", "Approach Avoidance Training", "4", "HS", "response cue indicates to", "push", "pull", "tendency to", "push", "pull")
  tabdat[10,] <- c("6", "", "", "BS", "", "pull", "push", "", "pull", "push")
  tabdat[11,] <- c("7", "word–sentence association training", "4", "HS (cue word)", "positive feedback will be given for response:", "unrelated",	"related",	"initial interpretation is",	"unrelated",	"related")
  tabdat[12,] <- c("7", "", "", "BS (cue word)", "", "related", "unrelated", "", "related", "unrelated")
  tabdat[13,] <- c("8", "go/no-go training", "4", "HS", "response cue indicates to", "nogo", "go", "tendency to", "nogo", "go")
  tabdat[14,] <- c("8", "", "", "BS", "", "go", "nogo", "", "go", "nogo")
  tabdat[15,] <- c("9", "stop-signal training", "4", "HS", "stop signal", "yes", "no", "initial tendency to", "stop", "go")
  tabdat[16,] <- c("9", "", "", "BS", "", "no", "yes", "", "go", "stop")

  
  sketch = htmltools::withTags(table(
    thead(style="background-color: darkgray; color: white",
      tr(
        th(class = 'dt-left', rowspan = 2, style="padding: 10px; border-bottom:1px solid #fff", ''),
        th(class = 'dt-left', rowspan = 2, style="padding: 10px; border-bottom:1px solid #fff", 'task'),
        th(class = 'dt-left', rowspan = 2, style="padding: 10px; border-bottom:1px solid #fff", 'n trial types'),
        th(class = 'dt-left', rowspan = 2, style="padding: 10px; border-bottom:1px solid #fff", 'trial class'),
        th(class = 'dt-left', rowspan = 1, colspan = 3, style="padding: 10px; border-bottom:0px solid #fff", 'interpretation of trialtype:'),
        th(class = 'dt-left', rowspan = 1, colspan = 3, style="padding: 10px; border-bottom:0px solid #fff", 'interpretation of initial response tendency:')),
      tr(
        th(class = 'dt-left', rowspan = 1, style="padding: 10px; border-bottom:1px solid #fff", ''),
        th(class = 'dt-left', colspan = 1, style="padding: 10px; border-bottom:1px solid #fff", 'congruent'),
        th(class = 'dt-left', colspan = 1, style="padding: 10px; border-bottom:1px solid #fff", 'incongruent'),
        th(class = 'dt-left', rowspan = 1, style="padding: 10px; border-bottom:1px solid #fff", ''),
        th(class = 'dt-left', colspan = 1, style="padding: 10px; border-bottom:1px solid #fff", 'desired'),
        th(class = 'dt-left', colspan = 1, style="padding: 10px; border-bottom:1px solid #fff", 'undesired'))
    )
  ))
  
  output$infoTable <- DT::renderDataTable({
    
    colorRows <- tabdat$row [as.numeric(paste(tabdat$row)) %% 2 == 0]
    
    datatable(tabdat, container = sketch, rownames = FALSE, 
              class = 'dt-left', 
              options = list(pageLength = nrow(tabdat), dom='t', ordering=F,  # hide all filtering searching etc user options
                             columnDefs = list(list(visible=FALSE, targets=c(0))))) %>%  # hide the first (0) column which has the info for the row coloring. Pipe into formatStyle
      formatStyle('row',target="row",backgroundColor = styleEqual(levels = colorRows, c(rep("#FAFAFA",length(colorRows))))) #upon initcomplete color the rows with odd values on the (hidden) colorRows variable
       
    })
  
}

# Run the application 
shinyApp(ui = ui, server = server)

