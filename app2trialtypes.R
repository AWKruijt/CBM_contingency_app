
# Define UI 
app2trialtypes <- shinyApp(options=list(height = "1500px"),
  

  ui <- fluidPage( 
    
    if(is.na(theColor)) {theColor <- '#E69F00'},

    # remove 'input bar' from slider inputs
    tags$style(HTML(".irs-bar, .irs-bar-edge{background: none; border: none}")),
    # style slider handle and label:
    tags$style(HTML(paste(".irs-single {background: ", theColor,"; ,size: 10} .irs-slider {top: 20px; width: 12px; height: 22px; border: 2px solid theColor;} .irs-slider:hover {background: #DDD;} "))),
  
    tags$style(HTML("hr {margin-top: 5px; margin-bottom: 5px }")), # reduce margins (top/bottom) for the horizontal rule ( hr() )
    tags$style(HTML("br {margin-top: 3px; margin-bottom: 3px }")), # reduce margins (top/bottom) for the break ( br() )
    tags$style(HTML("form.well {padding: 0px 15px}")), # reduce margins (top/bottom) for well panels (e.g. the sidebarPanel)
   
    tags$style(HTML(paste("#runAgain{background-color:", theColor,"; color: white; border-color: #DDD}"))), #style the 'runAgain' button (notice that this one is 'named' to match a specific button)
    tags$style(type="text/css", ".smallandgrayTxt {color: #999999; font-size: 12px; font-style: italic;}"), #create a custom text style to apply using "div(class=".smallandgrayTxt" "
    
  br(),
  
  sidebarLayout(
    sidebarPanel(style = "background-color: #ffffff; border-color: gray92 ; border-width: 2px", width = 3,
                 h5("I-TRIC:"),
                 sliderTextInput(inputId = "ITRIC", 
                                 label = h6("Intended Training Ratio Incongruent/Congruent:"),
                                 choices = c("100/0", "95/5", "90/10", "85/15", "80/20", "75/25", 
                                             "70/30", "65/35", "60/40", "55/45", "50/50", 
                                             "55/45", "40/60", "35/65", "30/70", "25/75", 
                                             "20/80", "15/85", "10/90", "5/95", "0/100"),
                                 selected = "50/50"),
                 br(),
                 h5("Pre-existing bias:"),
                 tags$div(title= "50/50 = no bias 
100/0 = absolute bias towards beneficial stimuli/desired response
0/100 = absolute bias towards harmful stimuli/undesired response",
                          sliderTextInput(inputId = "ppbias", 
                                          label = h6("ratio of desired/undesired initial response tendencies:"), 
                                          choices = c("100% desired", "95/5", "90/10", "85/15", "80/10", "75/25", 
                                                      "70/30", "65/35", "60/40", "55/45", "no bias", 
                                                      "45/55", "40/60", "35/65", "30/70", "25/75", 
                                                      "20/80", "15/85", "10/90", "5/95", "100% undesired"),
                                          selected = "no bias")),
              
              tags$div(title= "By default, the IRT sequence is created as a series of independent draws because it is a stochastic process not controlled by the researcher. Tick this box to make this a series of dependent trials, thereby removing variation in the totals of desired and undesired initial response tendencies",
              checkboxInput("ppbiasDependency",  div(class="smallandgrayTxt", "make consecutive draws of pp bias dependent"), value = F)),

              #div(class="smallandgrayTxt", "make consecutive draws of pp bias dependent"),
              #materialSwitch(inputId = "ppbiasDependency", label = "", status = "warning", right = TRUE, width = '2%', inline = T),
               
                
                 #actionButton("runAgain", "again!"), 
                 column(12, align="center",
                 actionButton(inputId = "runAgain", label = "again", icon = icon("repeat")) ),
                 
                 br(), br(), br(),
                 hr(),
                 sliderInput("ntrials",
                             h5("Number of trials:"),
                             min = 2,
                             max = 128,
                             step = 2,
                             value = 64)
    ),
    
    
    mainPanel(width = 9,
              #wellPanel(style = "background-color: #ffffff; border-width: 3px",
              fluidRow(
                column(4, h6("random sequence of trial types:")),
                column(4,h6("random sequence of initial response tendencies:")),
                column(4, h6("trial type & response tendency combined:"))),
              fluidRow(
                column(4,
                       # Show plots of the generated distributions
                       tags$div(title="this is a randomized sequence of the predefined number of trials for each trial type",
                                
                                plotOutput("trialtypeMatrix", height = 200),
                                htmlOutput("captionTrialtypeMatrix"))),
                column(4,
                       tags$div(title="because initial response tendency is not controlled by the researcher, it is determined for each trial independently and the resultant totals may vary",
                                plotOutput("RespTendencyMatrix", height = 200),
                                htmlOutput("captionRespTendencyMatrix"))),
                column(4,
                       tags$div(title="combining the two random matrices, we can see on which trials the participants' initial response aligns with the required response. Note that this 'mapping' is not controlled and that the resultant totals for each combination may vary",
                                
                                plotOutput("mergeplot", height = 200),
                                htmlOutput("captionMergeplot")))),
              hr(),
              
              fluidRow( column(2), column(8,
                tags$div(title="this plot summarizes the ME-TRIC: the Maximum Experienced Training Ratio Incongruent/Congruent, i.e the poportion of trials could consolidate or weaken the participant's initial response tendency",
                         h5("ME-TRIC:"),
                         h6("Maximum Experienced Training Ratio Incongruent/Congruent"),
                         br(),
                         plotOutput("METRICplot", height = 200))) )
    ))),
  
####

server <- 
  shinyServer(function(input, output) {
    
    require(ggplot2)
    require(reshape2)
    
    
    observe({
      p_congruent <- as.numeric(sub(".*\\/", "", input$ITRIC))
      p_congruent <<- p_congruent

      URtemp <- input$ppbias

      if (URtemp == "100% desired") {
        p_UR <- 0
      } else if (URtemp == "no bias") {
        p_UR <- .5
      } else if (URtemp == "100% undesired") {
        p_UR <- 1
      } else {p_UR <- as.numeric(sub(".*\\/", "", URtemp))/100 }
      

      ppbiasDependency <- input$ppbiasDependency
      
      nTrials <- input$ntrials
      nTrials <<- nTrials
      
      again <- input$runAgain
      
      dimX <- ceiling(sqrt(nTrials))
      dimY <- ceiling(nTrials / dimX)
      ###
      
      
      mHSBS <- structure(c(
        rep("HSBS", nTrials),
        rep(NA, (dimX*dimY) - nTrials)),
        .Dim = c(dimX, dimY))
      
      mmHSBS <- melt(mHSBS)
      
      mmHSBS <- mmHSBS[1:nTrials,] # cut the df back to the lenght of nTrials so that we don't need to deal with NA rows. 
      
      names(mmHSBS)[names(mmHSBS)=="value"] <- "stim_class"
      
      mmHSBS$trialtype [mmHSBS$stim_class == "HSBS"] <-  
        sample(c(rep("C", round(nTrials * p_congruent/100)), 
                 rep("I", round(nTrials * (100-p_congruent)/100))), replace = F)
      
      output$trialtypeMatrix <- renderPlot({  #plot
        
        ggplot(mmHSBS, aes(x = mmHSBS[,1], y = mmHSBS[,2])) +  #at some point for some reasons the name of these vars changed from Var1 and Var2 to X1 and X2 - sorting it by calling the var index instead)
          geom_tile(fill = "transparent") +
          # geom_text(aes(label=trialtype, fontface = fontface)) +
          geom_text(aes(label=trialtype)) +
          coord_fixed()  +
          scale_y_reverse( lim=c(dimY+.5,.5), breaks = c(1:dimY), labels = c(1:dimY)*dimX- (dimX-1)) + 
          theme_void() + 
          theme(axis.text.y = element_text(colour = "Gray", size = 10, angle =0, debug = F),
                legend.position = "none")
        
      })
      
      output$captionTrialtypeMatrix <- renderText({ 
        paste0("<sub> <font color='#999999'> total training objective incongruent (I): ", nrow(na.omit(mmHSBS[mmHSBS$trialtype == "I", ])),
               "</sub> <br> <sup>", "total training objective congruent (C): ", nrow(na.omit(mmHSBS[mmHSBS$trialtype == "C",])))
      })
      
      ###
      
      # generate sequence of participant initial response tendency:
      
      if (ppbiasDependency == F) {
        
        mmHSBS$IRT [mmHSBS$stim_class == "HSBS"] <-
          sample(c("U", "D"), nTrials, replace = T, 
                 prob =(c(p_UR, 1- p_UR)))
      } 
      
      
      if (ppbiasDependency == T)  {  
        mmHSBS$IRT [mmHSBS$stim_class == "HSBS"] <-
          sample(c(rep("U", round(nTrials *  p_UR)), 
                   rep("D", round(nTrials * (1- p_UR)))), replace = F)
      }
      
      
      output$RespTendencyMatrix <- renderPlot({  #plot
        
        ggplot(mmHSBS, aes(x = mmHSBS[,1], y = mmHSBS[,2])) + 
          geom_tile(fill = "transparent") +
          #  geom_text(aes(label=IRT, fontface = fontface)) +
          geom_text(aes(label=IRT)) +
          coord_fixed()  +
          scale_y_reverse( lim=c(dimY+.5,.5), breaks = c(1:dimY), labels = c(1:dimY)*dimX- (dimX-1)) + 
          theme_void() + 
          theme(axis.text.y = element_text(colour = "Gray", size = 10, angle =0, debug = F),
                legend.position = "none")
        
      })
      
      output$captionRespTendencyMatrix <- renderText({ 
        paste0("<sub> <font color='#999999'> total undesired response (U): ", nrow(na.omit(mmHSBS[mmHSBS$IRT == "U", ])),
               "</sub> <br> <sup>", "total desired response (D): ", nrow(na.omit(mmHSBS[mmHSBS$IRT == "D",])))
      })
      
      
      ###
      mmHSBS$combiCICBIAS [mmHSBS$trialtype== "C" & mmHSBS$IRT == "D"] <- "CD"
      mmHSBS$combiCICBIAS [mmHSBS$trialtype== "C" & mmHSBS$IRT == "U"] <- "CU"
      mmHSBS$combiCICBIAS [mmHSBS$trialtype== "I" & mmHSBS$IRT == "U"] <- "IU"
      mmHSBS$combiCICBIAS [mmHSBS$trialtype== "I" & mmHSBS$IRT == "D"] <- "ID"
      
      
      mmHSBS$combinedEffect [mmHSBS$trialtype== "C" & mmHSBS$IRT == "D"] <- "consolidate"
      mmHSBS$combinedEffect [mmHSBS$trialtype== "C" & mmHSBS$IRT == "U"] <- "weaken"
      mmHSBS$combinedEffect [mmHSBS$trialtype== "I" & mmHSBS$IRT == "U"] <- "consolidate"
      mmHSBS$combinedEffect [mmHSBS$trialtype== "I" & mmHSBS$IRT == "D"] <- "weaken"
      
      
      output$mergeplot <- renderPlot({
        
        ggplot(mmHSBS, aes(x = mmHSBS[,1], y = mmHSBS[,2], fill = combinedEffect)) + 
          geom_tile(colour = "white") +
          # geom_text(aes(label=combiCICBIAS, fontface = fontface), colour = "white") +
          geom_text(aes(label=combiCICBIAS), color = "white") +
          scale_fill_manual(values=setNames(c("#999999", theColor), c("consolidate","weaken"))) + 
          coord_fixed()  +
          scale_y_reverse( lim=c(dimY+.5,.5), breaks = c(1:dimY), labels = c(1:dimY)*dimX- (dimX-1)) +  
          theme_void() + 
          theme(axis.text.y = element_text(colour = "Gray", size = 10, angle =0, debug = F),
                legend.position = "none")
      })
      
      output$captionMergeplot <- renderText({ 
        paste0("<sub>	<font color='#999999'> trial may consolidate initial response",
               "</sub> <br> <sup> <font color=", theColor," > trial may weaken initial response") })
      
      mmHSBS <<- mmHSBS

    ###
    props <- NULL
    props$combinedEffect <- c("consolidate", "weaken")
    props <- as.data.frame(props)
    props$freq[props$combinedEffect== "consolidate"] <- nrow( mmHSBS[mmHSBS$combinedEffect == "consolidate",] )
    props$freq[props$combinedEffect== "weaken"] <- nrow( mmHSBS[mmHSBS$combinedEffect == "weaken",] )
    props$percent <- round((props$freq/sum(props$freq))*100)
    props$percent[1] <- -1 * props$percent[1] 
    names(props)[1] <- "combinedEffect"
    props$onex <- 0
    
    
    props <<- props
    
    output$METRICplot <- renderPlot({
      
      ggplot(props, aes( x=onex, y=percent, fill = combinedEffect)) +
        scale_fill_manual(values=setNames(c("#999999", theColor), c("consolidate", "weaken"))) + 
        geom_bar(stat="identity", colour = "white", size = 3) + 
        coord_flip() + 
        scale_x_discrete(expand = c(.1, 0) )+
        scale_y_continuous(limits = c(-100, 100) ) +
        theme_minimal()+
        theme( axis.text.x =  element_blank(),
               axis.title.y = element_text(colour = "#999999", size = 14, angle = 90, hjust=.5, debug = F),
               axis.title.x = element_blank(),
               panel.grid = element_blank(),
               legend.position="none") +
        labs(x = "all trials combined") +
        geom_hline(yintercept = 0,color = "gray92", size=.8) + 
        annotate("text", x = c(.55, .55), y = c(0,0), hjust= c(1.2,-0.2), label = c(
          paste0("← ", abs(props$percent[1]), "% "), 
          paste0(abs(props$percent[2]), "% →")), 
          size = (0.35 * 14), colour = "#999999")
    })
    
  })

})
)

