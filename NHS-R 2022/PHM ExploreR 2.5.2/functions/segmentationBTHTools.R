
segment_proportion_bars <- function(dat2,
                                    stacked,
                                    percent_or_freq,
                                    fill_colours = "default",
                                    number_of_columns=2,
                                    focusName){

  number_of_segments <- length(unique(dat2$util.segmentation.BtHSegment)) 

  segment_colours <- if(fill_colours=="colourblind"){
    colorblind_pal()(number_of_segments)
  } else if(fill_colours=="default"){
    hue_pal()(number_of_segments)
  } else {
    brewer_pal(type="qual")(number_of_segments)
  }
  
  plot_dat <- dat2 %>% 
    mutate(text = if(percent_or_freq=="percent"){
      paste0(round(pcn_percent,2),
             "% of ",
             focusName, 
             " ",
             focus,
             " patients",
             "\nAre in the ",
             util.segmentation.BtHSegment,
             " segment")
    } else {
      paste0(case_when(pcn_freq > 5 ~ format(pcn_freq,big.mark=","), T ~"*supressed"), # format(pcn_freq,big.mark=","),
             " ",
             focusName, 
             " ",
             focus,
             " patients",
             "\nAre in the ",
             util.segmentation.BtHSegment,
             " segment")
    })
  
  plot <- plot_dat %>% 
    ggplot(aes(x=factor(focus),
               y=!!rlang::sym(paste0("pcn_",percent_or_freq)),
               fill=util.segmentation.BtHSegment,
               text=text
    ),
    ) +
    geom_col() +
    scale_fill_manual(values=segment_colours) +
    theme_minimal() +
    labs(x="",
         y="",
         fill = "Segments") +
    coord_flip() +
    scale_y_continuous(labels=ifelse(percent_or_freq=="percent",
                                     function(x) paste0(x,"%"),
                                     scales::comma))+
    scale_fill_viridis_d()
  
  if(stacked == "separate"){
    plot <- plot +
      facet_wrap(~util.segmentation.BtHSegment,
                 ncol = number_of_columns
      ) +
      theme(legend.position = "none",
            panel.spacing.y=unit(-0.5,"lines")
      )
  }
  return(ggplotly(plot,tooltip="text"))
}

gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}

bridges_to_health <- function(dat, field = NA) {
  dataOutput <- if(is.na(field)) {
    dat$attributes %>% 
      select(util.segmentation.BtHSegment) %>% mutate(focus = "")
  } else {
    dat$attributes %>% 
      select(!!sym(field),
             util.segmentation.BtHSegment
      ) %>% rename_at(field,~"focus")
  } 
  dataOutput %>%
    group_by(focus,
             util.segmentation.BtHSegment) %>%
    summarise(pcn_freq = n()) %>% 
    ungroup() %>% 
    group_by(focus) %>% 
    mutate(pcn_percent = 100*pcn_freq/sum(pcn_freq)) %>% 
    ungroup() %>% 
    group_by(util.segmentation.BtHSegment) %>% 
    mutate(bnssg_freq = sum(pcn_freq)) %>% 
    ungroup() %>% 
    group_by(focus) %>% 
    mutate(bnssg_percent = 100*bnssg_freq/sum(bnssg_freq)) %>% 
    ungroup() %>% 
    return()
}


getBtHGraph <- function(dat, dat2, col) {
  if("util.segmentation.BtHSegment"%in%colnames(dat$attributes)) {
    if(col == "") {
      bridges_to_health(dat, NA)
    } else {
      bridges_to_health(dat, dat2$groupByList[which(dat2$groupByListText == input$twoBTHFocus)][[1]])
    }
  }
}

