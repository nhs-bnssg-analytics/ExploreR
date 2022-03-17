convertMenuItem <- function(mi,tabName) {
  mi$children[[1]]$attribs['data-toggle']="tab"
  mi$children[[1]]$attribs['data-value'] = tabName
  if(length(mi$attribs$class)>0 && mi$attribs$class=="treeview"){
    mi$attribs$class=NULL
  }
  mi
}
sidebar <- sidebarMenu(id="tabsAll",
                       
                       menuItem("Landing Page",
                                tabName="landingPage"),
                       menuItem("Navigation",
                                tabName="navTab"
                       ),
                       menuItem("Cohort Identification",
                                tabName="threeID"
                       ),
                       convertMenuItem(menuItem("Descriptive Summaries", tabName = "oneTab",selected=T, startExpanded = TRUE,
                                                menuSubItem("Demographic Overview",
                                                            tabName="oneDemog"),
                                                menuSubItem("Clinical Characteristics",
                                                            tabName="oneClinic"),
                                                menuSubItem("Activity Overview",
                                                            tabName="oneActivity"),
                                                menuSubItem("Deprivation",
                                                            tabName="oneDeprivation"),
                                                menuSubItem("Geography",
                                                            tabName="oneGeo"),
                                                menuSubItem("Wider Determinants",
                                                            tabName="oneWidDet"),
                                                menuSubItem("Generalised Barcharts",
                                                            tabName="oneGBars")
                                                ),"oneTab"
                       ),
                       menuItemOutput("twoSegOutput"),
                       menuItem("Theoplots",
                                tabName="fourTab"
                       ),
                       menuItem("Risk Stratification",
                                tabName="fiveTab"
                                
                       )
)

  
