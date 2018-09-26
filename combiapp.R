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

theColor <- "#E69F00" 
#theColor <- "deepskyblue4" 

source('app2trialtypes.R', local = TRUE)
source('app3trialtypes.R', local = TRUE)
source('app4trialtypes.R', local = TRUE)



# Define UI for application that draws a histogram
ui <- fluidPage(
  tags$style(HTML(".nav>li>a {color: #999999; }")), # set colour of tab titles font
 # tags$style(HTML(".nav-tabs >li.active>a, .nav-tabs >li.active>a:focus, .nav-tabs >li.active>a:hover { font-weight: bold}")), # set bold on title active tab

  # Application title
  titlePanel("On the interaction between pre-existing bias and training contingency in CBM"),
  
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
    "This shiny illustrates how pre-existing bias affects the experienced training contingency in hidden-contingency CBM tasks, as described in:", "<br><br>", em("Mechanics of contingency-based Cognitive Bias Modification: pre-existing bias affects potency of active CBM but not placebo training."), "<br><br>", "Preprint available: [url here]", "<br><br><br>", "The table below provides info on how to interpret the information in this app for different tasks:")))

  ### table with info on interpretations for different tasks:
  tabdat <- NULL
  
  tabdat <- data.frame(matrix(vector(), 0, 10, 
                              dimnames=list(c(), c("row", "task", "type", "stimclass", "trialtype_expl", "tt_C", "tt_I", "IRT_expl", "IRT_D", "IRT_U" ))),
                       stringsAsFactors=F)
  
  tabdat[1,] <- c("1", "dot-probe training with a single bias dimension", "2-trialtypes", "training", "response cue on location", "beneficial stim", "harmful stim", "gaze at", "beneficial stim", "harmful stim")
  tabdat[2,] <- c("2", "dot-probe with single dimension + neutral trials", "3-trialtypes", "training", "response cue on location", "beneficial stim", "harmful stim", "gaze at", "beneficial stim", "harmful stim")
  tabdat[3,] <- c("2", "", "", "neutral", "", "-", "-", "", "-", "-")
  tabdat[4,] <- c("3", "dot-probe with two bias dimensions", "4-trialtypes", "HS", "response cue on location", "neutral stim", "harmful stim", "gaze at", "neutral stim", "harmful stim")
  tabdat[5,] <- c("3", "", "", "BS", "", "beneficial stim", "neutral stim", "", "beneficial stim", "neutral stim")
  tabdat[6,] <- c("4", "Posner/single cueing training", "4-trialtypes", "HS", "stim cue location is", "opposite location", "stimulus location", "gaze", "away from stim", "towards stim")
  tabdat[7,] <- c("4", "", "", "BS", "", "stimulus location", "opposite location", "", "towards stim", "away from stim")
  tabdat[8,] <- c("5", "Approach Avoidance Training", "4 trialtypes", "HS", "response cue indicates to", "push", "pull", "tendency to", "push", "pull")
  tabdat[9,] <- c("5", "", "", "BS", "", "pull", "push", "", "pull", "push")
  tabdat[10,] <- c("6", "word–sentence association training", "4-trialtypes", "HS (cue word)", "positive feedback will be given for response:", "unrelated",	"related",	"initial interpretation is",	"unrelated",	"related")
  tabdat[11,] <- c("6", "", "", "BS (cue word)", "", "related", "unrelated", "", "related", "unrelated")
  tabdat[12,] <- c("7", "go/no-go training", "4-trialtypes", "HS", "response cue indicates to", "nogo", "go", "tendency to", "nogo", "go")
  tabdat[13,] <- c("7", "", "", "BS", "", "go", "nogo", "", "go", "nogo")
  tabdat[14,] <- c("8", "stop-signal training", "4-trialtypes", "HS", "stop signal", "yes", "no", "initial tendency to", "stop", "go")
  tabdat[15,] <- c("8", "", "", "BS", "", "no", "yes", "", "go", "stop")

  
  sketch = htmltools::withTags(table(
    thead(style="background-color: darkgray; color: white",
      tr(
        th(class = 'dt-left', rowspan = 2, style="padding: 10px; border-bottom:1px solid #fff", ''),
        th(class = 'dt-left', rowspan = 2, style="padding: 10px; border-bottom:1px solid #fff", 'task'),
        th(class = 'dt-left', rowspan = 2, style="padding: 10px; border-bottom:1px solid #fff", 'type'),
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

