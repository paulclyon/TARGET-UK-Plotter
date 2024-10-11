## Source all the files in lib/ (These will be listed in an alphanumerical order - files starting with "_" will be read first)
file.sources <- list.files(
  paste(dirname(sys.frames()[[length(sys.frames()) - 3]]$ofile), 'lib/', sep = '/'),
  pattern = '*.R',
  full.names = T,
  ignore.case = T
)

logger(file.sources)
sapply(file.sources, source, .GlobalEnv) 

if (require(knitr)) {
  mtime <- function(files) {
    lapply(Sys.glob(files), function(x) file.info(x)$mtime)
  }
  
  # This allows knitr to know it needs to re-calculate data if these files change
  knitr::opts_chunk$set(cache.extra = mtime(file.sources))
}

makeRxPathwayPlots <- function()
{
  if (!is.null(nrow(rxDoneData)))
  {  
    rxdonePlotColors <<- c(
      "Ref to DTT"           = "yellow",
      "DTT to Rx"            = "orange",
      "Ref to Rx"            = "green",
      "Clock Stops Pre-DTT"  = "purple",
      "Clock Stops Post-DTT" = "cyan",
      "Operator1"            = "blue"
    )
    rxdonePlot <<- ggplot(rxDoneData, aes(x=RxDate, text=paste(ID, ' (',Organs,')\n', ClockStopWhy),sep='')) +
      geom_point( aes(y=Ref_DTT,              color="Ref to DTT")) + 
      geom_point( aes(y=DTT_Rx,               color="DTT to Rx")) + 
      geom_point( aes(y=Ref_RxDone,           color="Ref to Rx")) +
      geom_point( aes(y=ClockStopDaysPreDTT,  color="Clock Stops Pre-DTT")) +
      geom_point( aes(y=ClockStopDaysPostDTT, color="Clock Stops Post-DTT")) +
      theme(legend.position="bottom") +
      scale_color_manual(values = rxdonePlotColors) +
      guides(color=guide_legend("Treated Patients...")) +
      labs(y="Number of Days") +
      ggtitle("Time to Treatment")
    
    
    #operatorPlot <<- ggplot(rxDoneData, aes(x=RxDate, text=paste(ID, ' (',paste(Operator1,Operator2,Operator3),')\n'),sep='')) +
    #  geom_point( aes(y=Operator1,      color="Operator1")) +
    #  theme(legend.position="bottom") +
    #  scale_color_manual(values = rxdonePlotColors) +
    #  guides(color=guide_legend("Treated Patients...")) +
    #  labs(y="First Operator (bug in that it doesn't count multiple Rx same day!") +
    #  ggtitle("Treatment Operators")
    
    # Create a custom color scale/palette
    # There are 11 colours in the Spectral palette but if we have more than 11 operqtors, we need to interpolate new colours hence the colour ramp
    opColors <- colorRampPalette(brewer.pal(11,"Spectral"))(length(operator1Factors))
    names(opColors) <- operator1Factors
    opColScale <- scale_fill_manual(name = "Operators", values = opColors)
    
    # ... and for the organs
    orgColors <- brewer.pal(length(organFactors),"Pastel1")
    names(orgColors) <- organFactors
    orgColScale <- scale_fill_manual(name = "Organs", values = orgColors) 
    
    # The operatorPlot gets subsetted to a single operator in app.R
    #operatorPlot <<- ggplot(rxDoneData, aes(x=format(RxDate, "%Y-%m"), text=paste(ID, ' (',paste(Operator1,Operator2,Operator3),')\n'),sep='')) +

    operatorPlot <<- ggplot(rxDoneData, aes(x=lubridate::floor_date(RxDate, "month"), fill=Operator1, 
                                             text=paste(ID, '-', RxDate, '\n',
                                                        paste('Operators: ',Operator1,Operator2,Operator3),'\n',
                                                        paste('Anaesthetists: ',Anaesthetist1,Anaesthetist2,Anaesthetist3)))) +
      geom_bar() +
      theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1)) +
      scale_y_continuous(breaks = seq(0, 100, by = 1)) +  # Use this to get an integer y-axis, the 100 is the number of max ticks after which x>100 still plots tickless
      #guides(color=guide_legend("1o Operator Legend")) +
      labs(x="Date", y="Ablation Count") +
      ggtitle("Primary Operator Logbook") +
      theme(legend.position="bottom") +
      opColScale
    
    # This method works but does not give you Pt ID per case....
    #rxDateByMonth <- rxDoneData %>%  
    #  group_by(month = lubridate::floor_date(RxDate, "month"), operator=Operator1) %>% 
    #  reframe(casesPerMonth = str_count(operator))
    
    #operatorPlot <<- ggplot(rxDateByMonth, aes(x=month, y=operator)) +
    #  geom_bar() +
    #  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1)) +
    #  scale_y_continuous(breaks = seq(0, 100, by = 1)) +  # Use this to get an integer y-axis, the 100 is the number of max ticks after which x>100 still plots tickless
    #  guides(color=guide_legend("1o Operator Legend")) +
    #  labs(x="Date", y="Ablation Count") +
    #  ggtitle("Primary Operator Logbook") +
    #  theme(legend.position="bottom") +
    #  opColScale
    
    
    volumePlot <<- ggplot(rxDoneData, aes(x=lubridate::floor_date(RxDate, "month"), fill=Organs, 
                                            text=paste(ID, '-', RxDate, '\n',
                                                       paste('Operators: ',Operator1,Operator2,Operator3,'\n'),
                                                       paste('Rx Modality: ',Modality)
                                                       ))) +
      geom_bar() +
      theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1)) +
      scale_y_continuous(breaks = seq(0, 100, by = 1)) +  # Use this to get an integer y-axis, the 100 is the number of max ticks after which x>100 still plots tickless
      labs(x="Date", y="Ablation Count") +
      ggtitle("Treatment Volume Plot") +
      theme(legend.position="bottom") +
      orgColScale
    
  }
  else
  {
    rxdonePlot <<- NA
    operatorPlot <<- NA
    volumePlot <<- NA
  }
  
  if (!is.null(nrow(rxWaitData)))
  {
    rxwaitPlotColors <<- c(
      "Days to DTT"          = "blue",
      "Days Waiting"         = "red",
      "Clock Stops Pre-DTT"  = "purple",
      "Clock Stops Post-DTT" = "cyan"
    )
    rxwaitPlot <<- ggplot(rxWaitData, aes(x=RefDate, text=paste(ID, ' (',Organs,')\n', 'Provisional RxDate=', ProvisionalRxDate, ClockStopWhy, sep=""))) +
      geom_point( aes(y=Ref_DTT,             colour="Days to DTT")) + 
      geom_point( aes(y=DaysWaiting,         colour="Days Waiting")) +
      geom_point( aes(y=ClockStopDaysPreDTT, color="Clock Stops Pre-DTT")) +
      geom_point( aes(y=ClockStopDaysPostDTT,color="Clock Stops Post-DTT")) +
      theme(legend.position="bottom") +
      scale_color_manual(values = rxwaitPlotColors) +
      guides(color=guide_legend("Patients with DTT...")) +
      labs(y="Number of Days") +
      ggtitle("Time on Waiting List")  
  }
  else
  {
    rxwaitPlot <<- NA
  }
}
  
makeRxPathwayPies <- function(inputStartDate, inputEndDate, inputOrganList)
{
  # Initialise some stuff
  startDate=as.Date(inputStartDate, format = "%d/%m/%Y")
  endDate=as.Date(inputEndDate, format = "%d/%m/%Y")
  organCounts <<- c()
  organPercents <<- c()
  rxdonePie <<- NA
  
  if (is.data.frame(rxDoneData) && nrow(rxDoneData>0))
  {
    logger(paste("FIXME: Need to filter the audit report to get it from ",startDate,endDate))
    logger(paste("FIXME: Need to filter the audit report to get just organs ",inputOrganList))  
    
    # Filter just the dates we need from rxDoneData
    rxDoneData.filtered <- rxDoneData
    
    #rxDoneData.filtered <- rxDoneData %>% filter(between(RxDate, as.Date(inputStartDate, format = "%d/%m/%Y"), 
    #                                                             as.Date(inputEndDate, format = "%d/%m/%Y")))
    #rxDoneData.filtered <- rxDoneData.filtered %>% filter(Organs %in% inputOrganList)
    organFactors.filtered = levels(rxDoneData$Organs)
    
    # Get the data for the Organ Pie Chart

    for (organ in organFactors)
    {
      count = length(which(rxDoneData$Organs==organ))
      organCounts <<- c(organCounts,count)
      organPercents <<- c(organPercents,count)
    }
    
    treatedTotal=sum(organCounts)
    organPercents <<- organPercents/treatedTotal
    organPie.df <<- data.frame(
      Organs = organFactors,
      OrganRxCounts = organCounts,
      OrganRxPercents = organPercents
    )
    
    # Make and label the pie chart
    organPie.df2 <<- organPie.df %>% 
      mutate(csum = rev(cumsum(rev(OrganRxCounts))), 
             pos = OrganRxCounts/2 + lead(csum, 1),
             pos = if_else(is.na(pos), OrganRxCounts/2, pos))
             
    rxdonePie <<- ggplot(organPie.df2, aes(x = "", y = OrganRxCounts, fill = fct_inorder(Organs))) + 
      geom_col(width = 1, color = 1) +
      coord_polar(theta = "y") +
      scale_fill_brewer(palette = "Pastel1") +
      geom_label_repel(data = organPie.df2,
                       aes(y = pos,
                           label = glue::glue("{OrganRxCounts} ({scales::percent(organPercents)})"), 
                           fill = Organs),
                       size = 4, nudge_x = 3, show.legend = FALSE) +
      guides(fill = guide_legend(title = "Group")) +
      theme_void()
  }

}



