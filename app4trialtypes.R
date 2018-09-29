
# Define UI
app4trialtypes <- shinyApp(options=list(height = "1500px"),
  
  ui <- fluidPage(
    
    if(is.na(theColor)) {theColor <- '#E69F00'},
    
### everything from here to the next ### is all styling related and non essential code    
    tags$body(
      tags$link(rel="stylesheet", type = "text/css", href = "https://fonts.googleapis.com/css?family=Roboto")
    ), # get custom font roboto
    
    tags$style(HTML("body {
                    font-family: 'roboto';
                    }")), # set custom font roboto
        
    tags$style(HTML("input[type='checkbox']{
                    width: 9px !important;
                    height: 9px !important;
                    -webkit-appearance: none;
                    -moz-appearance: none;
                    -o-appearance: none;
                    appearance:none;
                    outline: 2px solid #999999;}")),  # Custom checkbox
    
    tags$style(HTML(paste("input[type='checkbox']:checked{
                          background:", theColor, ";}"))), # Custom checkbox check
    
    tags$style(HTML(".irs-bar, .irs-bar-edge{background: none; border: none}")), # remove 'input bar' from slider inputs
    
    tags$style(HTML(paste(".irs-single {background: ", theColor,"; ,size: 10} .irs-slider {top: 20px; width: 12px; height: 22px; border: 2px solid theColor;} .irs-slider:hover {background: #DDD;} "))),    # style slider handle and label:
    
    tags$style(HTML("hr {margin-top: 5px; margin-bottom: 5px }")), # reduce margins (top/bottom) for the horizontal rule ( hr() )
    tags$style(HTML("br {margin-top: 3px; margin-bottom: 3px }")), # reduce margins (top/bottom) for the break ( br() )
    
    tags$style(HTML("form.well {
                    padding: 0px 25px 0px 20px; 
                    background-color: transparent; 
                    border: none;  
                    box-shadow: none;}")), # redistribute margins and remove border and shadow for well panels (e.g. the sidebarPanel)
    
    tags$style(HTML(paste("#runAgain{background-color:", theColor,"; color: white; border-color: #DDD}"))), #style the 'runAgain' button (notice that this one is 'named' to match a specific button)
    tags$style(type="text/css", ".smallandgrayTxt {color: #999999; font-size: 12px; font-style: italic;}"), #create a custom text style to apply using "div(class=".smallandgrayTxt" "

###    

    br(),
    
    sidebarLayout(
      sidebarPanel(width = 3,
                   tags$div(title= "Congruent and incongruent refer to whether trials align with or oppose the intended direction of training",
                            sliderTextInput(inputId = "HS_ITRinco", 
                                            label = h6(HTML("Intended Training Ratio <br> (incongruent/congruent) for harmful class stimuli trials:")),
                                            choices = c("100/0", "95/5", "90/10", "85/15", "80/20", "75/25", 
                                                        "70/30", "65/35", "60/40", "55/45", "50/50", 
                                                        "55/45", "40/60", "35/65", "30/70", "25/75", 
                                                        "20/80", "15/85", "10/90", "5/95", "0/100"),
                                            selected = "100/0"),
                            sliderTextInput(inputId = "BS_ITRinco", 
                                            label = h6(HTML("Intended Training Ratio <br> (incongruent/congruent) for beneficial class stimuli trials:")),
                                            choices = c("100/0", "95/5", "90/10", "85/15", "80/20", "75/25", 
                                                        "70/30", "65/35", "60/40", "55/45", "50/50", 
                                                        "55/45", "40/60", "35/65", "30/70", "25/75", 
                                                        "20/80", "15/85", "10/90", "5/95", "0/100"),
                                            selected = "50/50")),
                   br(),
                   h5("Pre-existing bias:"),
                   tags$div(title= paste("50/50 = no bias", "100/0 = absolute bias away from harmful stimuli/undesired response", "0/100 = absolute bias towards harmful stimuli/desired response",sep="\n"),                            
                        sliderTextInput(inputId = "HS_ppbias", 
                                  label = h6("harmful stimuli:"), 
                                  choices = c("100% desired", "95/5", "90/10", "85/15", "80/10", "75/25", 
                                              "70/30", "65/35", "60/40", "55/45", "no bias", 
                                              "45/55", "40/60", "35/65", "30/70", "25/75", 
                                              "20/80", "15/85", "10/90", "5/95", "100% undesired"),
                                  selected = "100% undesired")),
                  tags$div(title= paste("50/50 = no bias", "100/0 = absolute bias away from beneficial stimuli/undesired response", "0/100 = absolute bias towards beneficial stimuli/desired response",sep="\n"),
                        sliderTextInput(inputId = "BS_ppbias", 
                                  label = h6("beneficial stimuli:"),  
                                  choices = c("100% undesired", "95/5", "90/10", "85/15", "80/10", "75/25", 
                                              "70/30", "65/35", "60/40", "55/45", "no bias", 
                                              "45/55", "40/60", "35/65", "30/70", "25/75", 
                                              "20/80", "15/85", "10/90", "5/95", "100% desired"),
                                  selected = "100% desired")),
        
                   tags$div(title= "By default, the IRT sequence is drawn stochastically and independent from the trial type sequence. Tick this box to remove random variation in the totals of desired and undesired initial response tendencies and to distribute the response tendencies equally over the trial types.",
                            checkboxInput("distributeBiasEqually",  div(class="smallandgrayTxt", "distribute response tendency equally over the trialtypes"), value = F)),
                   br(),
                   
                   column(12, align="center",
                          actionButton(inputId = "runAgain", label = "again", icon = icon("repeat")) ),
                   
                   br(), br(), br(),
                   hr(),
                   sliderInput("HS_nTrials",
                               h6("Number of harmful stimuli trials:"),
                               min = 2,
                               max = 128,
                               step = 2,
                               value = 40),
                   sliderInput("BS_nTrials",
                               h6("Number of beneficial stimuli trials:"),
                               min = 2,
                               max = 128,
                               step = 2,
                               value = 40)
      ),
      
      
      mainPanel(width = 9,
                fluidRow(h6("")),
                fluidRow(
                  column(4, h6("random sequence of trial types:")),
                  column(4, h6("random sequence of initial response tendencies:")),
                  column(4, h6("trial type & response tendency combined:"))),
                fluidRow(
                  column(4,
                         # Show plots of the generated distributions
                         tags$div(title="this is a randomized sequence of the predefined number of trials for each trial type",
                                  plotOutput("trialtypeMatrix", height = 200),
                                  htmlOutput("captionTrialtypeMatrix")),
                         htmlOutput("captionStimmclass")),
                  
                  column(4,
                         tags$div(title="because initial response tendency is not under experimental control, it is determined for each trial independently and the resultant totals may vary",
                                  plotOutput("RespTendencyMatrix", height = 200),
                                  htmlOutput("captionRespTendencyMatrix"))),
                  column(4,
                         tags$div(title="combining the two random matrices, we can see on which trials the participants' initial response aligns with the required response. Note that this 'mapping' is not controlled and that the resultant totals for each combination may vary",
                                  
                                  plotOutput("mergeplot", height = 200),
                                  htmlOutput("captionMergeplot")))),
                hr(),
                fluidRow(
                  tags$div(title="these plots summarize tthe MER(co/we): the Maximum Experienced Ratio (consolidating/weakening), i.e the poportion of trials that could consolidate or weaken the participant's initial response tendency",
                           h4("MER(co/we):"),
                           column(6,
                                  plotOutput("hiddenPropPlot", height = 200),
                                  htmlOutput("captionhiddenPropPlot")),
                           column(6,
                                  plotOutput("MERcoweplot", height = 200),
                                  htmlOutput("captionMERcoweplot"))))
      ))),

server <- 
  
  shinyServer(function(input, output) {
    
    require(ggplot2)
    require(reshape2)
    
    observe({
      HS_p_congruent <- as.numeric(sub(".*\\/", "", input$HS_ITRinco))
      BS_p_congruent <- as.numeric(sub(".*\\/", "", input$BS_ITRinco))
      
      HS_URtemp <- input$HS_ppbias
      
      if (HS_URtemp == "100% desired") {
        HS_p_U <- 0
      } else if (HS_URtemp == "no bias") {
        HS_p_U <- .5
      } else if (HS_URtemp == "100% undesired") {
        HS_p_U <- 1
      } else {HS_p_U <- as.numeric(sub("\\/.*", "", HS_URtemp))/100 }
      
      
      BS_p_U <- as.numeric(sub("\\%.*", "", input$BS_ppbias))/100
      
      BS_URtemp <- input$BS_ppbias
      
      if (BS_URtemp == "100% desired") {
        BS_p_U <- 0
      } else if (BS_URtemp == "no bias") {
        BS_p_U <- .5
      } else if (BS_URtemp == "100% undesired") {
        BS_p_U <- 1
      } else {BS_p_U <- as.numeric(sub(".*\\/", "", BS_URtemp))/100 }
      
      
      distributeBiasEqually <- input$distributeBiasEqually
      
      HS_nTrials <- input$HS_nTrials
      BS_nTrials <- input$BS_nTrials
      nTrials <- sum(HS_nTrials, BS_nTrials)
      
      again <- input$runAgain
      
      dimX <- ceiling(sqrt(nTrials))
      dimY <- ceiling(nTrials / dimX)
      
      
      ###
      
      
      mHSBS <- structure(c(
        sample(c(rep("HS", HS_nTrials), 
                 rep("BS", BS_nTrials))),
        rep(NA, (dimX*dimY) - nTrials)),
        .Dim = c(dimX, dimY))
      
      mmHSBS <- melt(mHSBS)
      
      mmHSBS <- mmHSBS[1:nTrials,] # cut the df back to the lenght of nTrials so that we don't need to deal with NA rows. 
      
      names(mmHSBS)[names(mmHSBS)=="value"] <- "stim_class"
      
      mmHSBS$trialtype [mmHSBS$stim_class == "HS"] <-  
        (sample(c(rep("C",  round(HS_nTrials *  HS_p_congruent/100)), 
                  rep("I", round(HS_nTrials * (100- HS_p_congruent)/100)) ), replace = F))
      
      mmHSBS$trialtype [mmHSBS$stim_class == "BS"] <-  
        sample(c(rep("C", round(BS_nTrials *  BS_p_congruent/100)), 
                 rep("I", round(BS_nTrials * (100- BS_p_congruent)/100))), replace = F)
      
      mmHSBS$fontface [mmHSBS$stim_class == "BS"] <- "italic"
      mmHSBS$fontface [mmHSBS$stim_class == "HS"] <- "plain"
      
      
      output$captionStimmclass <- renderText({ 
        paste0("<sub> <font color='#999999'> total HS: ", nrow(na.omit(mmHSBS[mmHSBS$stim_class == "HS", ])),
               "</sub> <br> <sup> <I>", "total BS: ", nrow(na.omit(mmHSBS[mmHSBS$stim_class == "BS",])) )
      })
      
      
      output$trialtypeMatrix <- renderPlot({  #plot
        
        ggplot(mmHSBS, aes(x = mmHSBS[,1], y = mmHSBS[,2])) + 
          geom_tile(fill = "transparent") +
          #geom_text(aes(label=trialtype, colour = stim_class), alpha = 1) +
          geom_text(aes(label=trialtype, fontface = fontface)) +
          scale_color_manual(values=setNames(c("#B81878","#1493C9"), c("HS","BS"))) +
          coord_fixed()  +
          scale_y_reverse( lim=c(dimY+.5,.5), breaks = c(1:dimY), labels = c(1:dimY)*dimX- (dimX-1)) + 
          theme_void() + 
          theme(axis.text.y = element_text(colour = "Gray", size = 10, angle =0, debug = F),
                legend.position = "none")
        
      })
      
      output$captionTrialtypeMatrix <- renderText({ 
        paste0("<sub> <font color='#999999'> total training objective incongruent (I): ", nrow(na.omit(mmHSBS[mmHSBS$trialtype == "I", ])),
               "</sub> <br> <sup>", "total training objective congruent (C): ", nrow(na.omit(mmHSBS[mmHSBS$trialtype == "C",])) )
      })
      
  
      ###
      
      # generate sequence of participant gaze:
      
      if (distributeBiasEqually == F) {
        
        mmHSBS$IRT [mmHSBS$stim_class == "HS"] <-
          sample(c("U", "D"), HS_nTrials, replace = T, 
                 prob =(c(HS_p_U, 1- HS_p_U)))
        
        mmHSBS$IRT [mmHSBS$stim_class == "BS"] <-
          sample(c("U", "D"), BS_nTrials, replace = T, 
                 prob =(c(BS_p_U, 1- BS_p_U)))
      } 
      

      if (distributeBiasEqually == T)  {  
        mmHSBS$IRT [mmHSBS$trialtype == "C" & mmHSBS$stim_class == "HS"] <-
          sample(c(rep("U", round(HS_nTrials * HS_p_congruent/100 *  HS_p_U)), 
                   rep("D", round(HS_nTrials * HS_p_congruent/100* (1- HS_p_U)))), replace = F)
        
        mmHSBS$IRT [mmHSBS$trialtype == "I" & mmHSBS$stim_class == "HS"] <-
          sample(c(rep("U", round(HS_nTrials * (100-HS_p_congruent)/100*  HS_p_U)), 
                   rep("D", round(HS_nTrials * (100-HS_p_congruent)/100 * (1- HS_p_U)))), replace = F)
        
        mmHSBS$IRT [mmHSBS$trialtype == "C" & mmHSBS$stim_class == "BS"] <-
          sample(c(rep("U", round(BS_nTrials * BS_p_congruent/100 *  BS_p_U)), 
                   rep("D", round(BS_nTrials * BS_p_congruent/100 * (1- BS_p_U)))), replace = F)
        
        mmHSBS$IRT [mmHSBS$trialtype == "I" & mmHSBS$stim_class == "BS"] <-
          sample(c(rep("U", round(BS_nTrials * (100-BS_p_congruent)/100*  BS_p_U)), 
                   rep("D", round(BS_nTrials * (100-BS_p_congruent)/100 * (1- BS_p_U)))), replace = F)
      }
      
      
      
      output$RespTendencyMatrix <- renderPlot({  #plot
        
        ggplot(mmHSBS, aes(x = mmHSBS[,1], y = mmHSBS[,2])) + 
          geom_tile(fill = "transparent") +
          #  geom_text(aes(label=IRT, colour = stim_class), alpha = 1) +
          geom_text(aes(label=IRT, fontface = fontface)) +
          scale_color_manual(values=setNames(c("#B81878","#1493C9"), c("HS","BS"))) +
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
        
        ggplot(na.omit(mmHSBS), aes(x = mmHSBS[,1], y = mmHSBS[,2], fill = combinedEffect)) + 
          geom_tile(colour = "white") +
          # geom_text(aes(label=combiCICBIAS, colour = stim_class), alpha = 1) +
          geom_text(aes(label=combiCICBIAS, fontface = fontface), colour = "white") +
          scale_color_manual(values=setNames(c("#B81878","#1493C9"), c("HS","BS"))) +
          scale_fill_manual(values=setNames(c("#999999", theColor), c("consolidate","weaken"))) + 
          coord_fixed()  +
          scale_y_reverse( lim=c(dimY+.5,.5), breaks = c(1:dimY), labels = c(1:dimY)*dimX- (dimX-1)) +  
          theme_void() + 
          theme(axis.text.y = element_text(colour = "Gray", size = 10, angle =0, debug = F),
                legend.position = "none")
      })
      
      output$captionMergeplot <- renderText({ 
        paste0("<sub>	<font color='#999999'> trial may consolidate initial response",
               "</sub> <br> <sup> <font color='", theColor, "'> trial may weaken initial response") })
      

      ###
      
      hiddenprops <- NULL
      hiddenprops$effect <- c(  "weaken bias towards HS", "weaken bias towards BS",  "consolidate bias towards HS", "consolidate bias towards BS")
      hiddenprops <- as.data.frame(hiddenprops)
      
      #levels(hiddenprops$effect) <- c("weaken gaze towards neutral", "weaken gaze towards emo", "consolidate gaze towards emo", "consolidate gaze towards neutral")
      
      hiddenprops$biasdir [grepl("HS", hiddenprops$effect)] <- "harmful"
      hiddenprops$biasdir [grepl("BS", hiddenprops$effect)] <- "beneficial"
      
      hiddenprops$freq <- 0
      hiddenprops$freq [hiddenprops$effect ==  "weaken bias towards BS"] <- 
        nrow(na.omit(mmHSBS [mmHSBS$stim_class == "BS" & mmHSBS$combinedEffect == "weaken" ,]))
      
      hiddenprops$freq [hiddenprops$effect ==  "weaken bias towards HS"] <- 
        nrow(na.omit(mmHSBS [mmHSBS$stim_class == "HS" & mmHSBS$combinedEffect == "weaken" ,]))
      
      hiddenprops$freq [hiddenprops$effect ==  "consolidate bias towards HS"] <- 
        nrow(na.omit(mmHSBS [mmHSBS$stim_class == "HS" & mmHSBS$combinedEffect == "consolidate" ,]))
      
      hiddenprops$freq [hiddenprops$effect ==  "consolidate bias towards BS"] <- 
        nrow(na.omit(mmHSBS [mmHSBS$stim_class == "BS" & mmHSBS$combinedEffect == "consolidate" ,]))
      
      hiddenprops$percent <- round((hiddenprops$freq/sum(hiddenprops$freq))*100,1)
      
      hiddenprops$percent [grepl("consolidate", hiddenprops$effect)] <- -1*hiddenprops$percent [grepl("consolidate", hiddenprops$effect)]
   
      output$hiddenPropPlot <- renderPlot({
      
        ggplot(hiddenprops, aes( x=biasdir, y=percent, fill = effect))+
          scale_fill_manual(values=c(  "#999999", "#999999", theColor, theColor))+ 
          geom_bar(stat="identity", colour = "white", size = 3) + 
          coord_flip() + 
          scale_x_discrete(expand = c(.1, 0) )+
          scale_y_continuous(limits = c(-100, 100) ) +
          theme_minimal()+
          theme( axis.text.y =  element_text(colour = "#999999", size = 13, angle = 90, hjust=.5, debug = F),
                 axis.text.x =  element_blank(),
                 axis.title.y = element_text(colour = "Gray"),
                 axis.title.x = element_blank(),
                 panel.grid = element_blank(),
                 legend.position="none") +
          labs(x= "stimulus class") +
          geom_hline(yintercept = 0,color = "gray92", size=.8) + 
          #  geom_vline(xintercept = 1.5,color = "gray92", size=.8) + 
          annotate("text", x = c(2.7, 2.7), y = c(0,0), hjust= c(1.1,-0.1), label = c("← consolidate","weaken →"), 
                   size = (0.35 * 14), colour = "#999999")
        
      })
      
      output$captionhiddenPropPlot <- renderText({ 
        paste0("<font color='#999999'> MER ( co/", "<font color=", theColor,">we", "<font color='#999999'> ) per stimulus class") })

      
      ###
      props <- NULL
      props$combinedEffect <- c("consolidate", "weaken")
      props <- as.data.frame(props)
      props$freq[props$combinedEffect== "consolidate"] <- nrow( mmHSBS[mmHSBS$combinedEffect == "consolidate",] )
      props$freq[props$combinedEffect== "weaken"] <- nrow( mmHSBS[mmHSBS$combinedEffect == "weaken",] )
      props$percent <- round((props$freq/sum(props$freq))*100, 1)
      props$percent[1] <- -1 * props$percent[1] 
      names(props)[1] <- "combinedEffect"
      props$onex <- 0

      
      output$MERcoweplot <- renderPlot({
        
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
        # annotate("text", x = 0, y = -100,  label = "all trials", 
        #         size = (0.35 * 14), colour = "#999999")
        
      })

    output$captionMERcoweplot <- renderText({ 
      paste0("<font color='#999999'> Maximum Experienced Ratio ( consolidating/", "<font color=", theColor,">weakening", "<font color='#999999'> )") }) 
    
  })
  })

)


