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
      "Ref to DTT"           = "red",
      "DTT to Rx"            = "yellow",
      "Ref to Rx"            = "green",
      "Clock Stops Pre-DTT"  = "purple",
      "Clock Stops Post-DTT" = "cyan",
      "Operator1"            = "blue"
    )
    rxdonePlot <<- ggplot(rxDoneData, aes(x=RxDate, text=paste(ID, ' (',Organs,')\n', ClockStopWhy, sep=''))) +
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
  }
  else
  {
    rxdonePlot <<- NA
    operatorPlot <<- NA
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
    rxwaitPlot <<- ggplot()
  }
}

makeRxDonePie <- function(inputStartDate, inputEndDate, inputOrganList)
{
  # Initialise some stuff
  startDate = as.Date(inputStartDate, format = "%d/%m/%Y")
  endDate   = as.Date(inputEndDate,   format = "%d/%m/%Y")
  rxDoneOrganCounts   <<- c()
  rxDoneOrganPercents <<- c()

  if (is.data.frame(rxDoneData) && nrow(rxDoneData>0))
  {
    # Filter just the dates we need from rxDoneData
    rxDoneData.filtered <- rxDoneData %>% filter(between(RxDate, as.Date(inputStartDate, format = "%d/%m/%Y"), 
                                                                 as.Date(inputEndDate,   format = "%d/%m/%Y")))
    rxDoneData.filtered <- rxDoneData.filtered %>% filter(rxDoneData.filtered$Organs %in% inputOrganList)
    rxDoneOrganFactors.filtered <<- levels(factor(rxDoneData.filtered$Organs))
    rxDoneOrganFactors.final <<- c()
    
    # Get the data for the Rx Done Organ Pie Chart
    for (organ in rxDoneOrganFactors.filtered)
    {
      count = length(which(rxDoneData.filtered$Organs==organ))
      if (count > 0)
      {
        rxDoneOrganFactors.final <<- c(rxDoneOrganFactors.final, organ)
        rxDoneOrganCounts        <<- c(rxDoneOrganCounts,count)
        rxDoneOrganPercents      <<- c(rxDoneOrganPercents,count)
      }
    }
    
    treatedTotal  <- sum(rxDoneOrganCounts)
    rxDoneOrganPercents <<- rxDoneOrganPercents/treatedTotal
    rxDoneOrganPie.df   <<- data.frame(
      OrganRxFactors = rxDoneOrganFactors.final,
      OrganRxCounts = rxDoneOrganCounts,
      OrganRxPercents = rxDoneOrganPercents
    )
    
    logger(paste("FIXME4",rxDoneOrganCounts))
    logger(paste("FIXME5",rxDoneOrganFactors.final))
    logger(paste("FIXME6",treatedTotal))
    logger(paste("FIXME7",rxDoneOrganPercents))
    logger(paste(glue::glue("{rxDoneOrganPie.df$OrganRxCounts} ({scales::percent(rxDoneOrganPie.df$OrganRxPercents)})")))
    logger(paste(glue::glue("{rxDoneOrganCounts} ({scales::percent(rxDoneOrganPercents)})")))
    
    if (treatedTotal > 0)
    {
      # Make and label the pie chart
      rxDoneOrganPie.df <<- rxDoneOrganPie.df %>% 
        mutate(csum = rev(cumsum(rev(OrganRxCounts))), 
               pos = OrganRxCounts/2 + lead(csum, 1),
               pos = if_else(is.na(pos), OrganRxCounts/2, pos))
               
      p <- ggplot(rxDoneOrganPie.df, aes(x = "", y = OrganRxCounts, fill = fct_inorder(OrganRxFactors))) + 
           geom_col(width = 1, color = "grey") +
           coord_polar(theta = "y") +
           scale_fill_brewer(palette = "Pastel1") +
           guides(fill = guide_legend(title = "Treated Organs Piechart")) +
           theme_void()
      
      logger(paste("FIXME8",nrow(rxDoneOrganPie.df)))
      
      # Weirdly the labelling is tied in with the dynamic refresh, without this line the pie doesn't refresh itself
      # The label glue attribute is interesting: if you replace with OrganRxCounts and OrganPercents automatic update is lost
      p <- p + geom_label_repel(data = rxDoneOrganPie.df,
                         aes(y = pos,
                             label = glue::glue("{rxDoneOrganCounts} ({scales::percent(rxDoneOrganPercents)})"), 
                             fill = OrganRxFactors),
                         size = 5, nudge_x = 3, show.legend = FALSE)
      rxdonePie <<- p
    }
    else
    {
      rxdonePie <<- ggplot()
    }
  }
}

makeRxWaitPie <- function(inputStartDate, inputEndDate, inputOrganList)
{
  # Initialise some stuff
  startDate = as.Date(inputStartDate, format = "%d/%m/%Y")
  endDate   = as.Date(inputEndDate,   format = "%d/%m/%Y")
  rxWaitOrganCounts   <<- c()
  rxWaitOrganPercents <<- c()
  
  # Now for the Rx waiting Pie...
  if (is.data.frame(rxWaitData) && nrow(rxWaitData>0))
  {
    # Filter just the organs we need from rxWaitData
    rxWaitData.filtered <<- rxWaitData %>% filter(rxWaitData$Organs %in% inputOrganList)
    
    # Now just because the user asked for an organ doesn't mean we have any of that organ on waiting list, so need to factor...
    rxWaitOrganFactors.filtered <<- levels(factor(rxWaitData.filtered$Organs))
    rxWaitOrganFactors.final <<- c()
    
    # Get the data for the Rx Wait Organ Pie Chart
    for (organ in rxWaitOrganFactors.filtered)
    {
      count = length(which(rxWaitData.filtered$Organs==organ))
      if (count > 0)
      {
        rxWaitOrganFactors.final <<- c(rxWaitOrganFactors.final, organ)
        rxWaitOrganCounts        <<- c(rxWaitOrganCounts,count)
        rxWaitOrganPercents      <<- c(rxWaitOrganPercents,count)
      }
    }
    
    waitingTotal  <- sum(rxWaitOrganCounts)
    rxWaitOrganPercents <<- rxWaitOrganPercents/waitingTotal
    rxWaitOrganPie.df   <<- data.frame(
      OrganRxFactors = rxWaitOrganFactors.final,
      OrganRxCounts = rxWaitOrganCounts,
      OrganRxPercents = rxWaitOrganPercents
    ) 
    
    if (waitingTotal > 0)
    {
      # Make and label the pie chart
      rxWaitOrganPie.df <<- rxWaitOrganPie.df %>% 
        mutate(csum = rev(cumsum(rev(OrganRxCounts))), 
               pos = OrganRxCounts/2 + lead(csum, 1),
               pos = if_else(is.na(pos), OrganRxCounts/2, pos))
      
      logger(paste("FIXME123",length(rxWaitOrganPie.df$OrganRxCounts),length(fct_inorder(rxWaitOrganPie.df$OrganRxFactors)),length(rxWaitOrganCounts),length(rxWaitOrganPercents)))
      logger(paste("Glue: ",glue::glue("{rxWaitOrganCounts} ({scales::percent(rxWaitOrganPercents)}")))
      
      p <- ggplot(rxWaitOrganPie.df, aes(x = "", y = OrganRxCounts, fill = fct_inorder(OrganRxFactors))) + 
        geom_col(width = 1, color = "grey") +
        coord_polar(theta = "y") +
        scale_fill_brewer(palette = "Pastel1") +
        guides(fill = guide_legend(title = "Waiting List Piechart")) +
        theme_void()
      
      # Weirdly the labelling is tied in with the dynamic refresh, without this line the pie doesn't refresh itself
      # The label glue attribute is interesting: if you replace with OrganRxCounts and OrganPercents automatic update is lost
      p <- p + geom_label_repel(data = rxWaitOrganPie.df,
                                aes(y = pos,
                                    label = glue::glue("{rxWaitOrganCounts} ({scales::percent(rxWaitOrganPercents)})"), 
                                    fill = OrganRxFactors),
                                size = 5, nudge_x = 3, show.legend = FALSE)
      rxwaitPie <<- p
    }
    else
    {
      rxwaitPie <<- ggplot()
    }
  }
}

makeRxVolumePlot <- function(filteredRxDoneData, volumePlotDuration)
{
  # Organ colour scale
  orgColors <- brewer.pal(length(organFactors),"Pastel1")
  names(orgColors) <- organFactors
  orgColScale <- scale_fill_manual(name = "Organs", values = orgColors) 
  
  if (!is.null(nrow(filteredRxDoneData)))
  {
    if(volumePlotDuration == "year")
    {
      yAxisFreq <- 5
    }
    else if(volumePlotDuration == "month")
    {
      yAxisFreq <- 2
    }
    else # Must be by week...
    {
      yAxisFreq <- 1
    }
      
    volumePlot <<- ggplot(filteredRxDoneData, aes(x=lubridate::floor_date(RxDate, volumePlotDuration), fill=Organs, 
                                               text=paste(ID, '-', RxDate, '\n',
                                                     paste('Operators: ',Operator1,Operator2,Operator3,'\n'),
                                                     paste('Rx Modality: ',Modality, ' (£', Tariff,')')
                                          ))) +
      geom_bar() +
      theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1)) +
      scale_y_continuous(breaks = seq(0, 100, by = yAxisFreq)) +  # Use this to get an integer y-axis, the 100 is the number of max ticks after which x>100 still plots tickless
      labs(x="Date", y="Ablation Count") +
      ggtitle("Treatment Volume Plot") +
      theme(legend.position="bottom") +
      orgColScale
  }
  else
  {
    volumePlot <<- NA
  }
  return(volumePlot)
}
