convertMenuItem <- function(mi,tabName) {
  mi$children[[1]]$attribs['data-toggle']="tab"
  mi$children[[1]]$attribs['data-value'] = tabName
  if(length(mi$attribs$class)>0 && mi$attribs$class=="treeview"){
    mi$attribs$class=NULL
  }
  mi
}
sidebar <- sidebarMenu(id="tabsAll",
                       
                       # menuItem("Data And Landing",
                       #          tabName="landing",
                       #          menuSubItem("0.1 Landing",
                       #                      tabName="zeroLanding"),
                       #          menuSubItem("0.2 Data Loading",
                       #                      tabName="zeroData"),
                       #          menuSubItem("0.3 Field Assignment",
                       #                      tabName="zeroField")
                       #          ),
                       menuItem("Landing Page",
                                tabName="landingPage"),
                       menuItem("Navigation",
                                tabName="navTab"
                       ),
                       menuItem("Cohort Identification",
                                tabName="threeID"#,#threeTab
                                # menuSubItem("Cohort ID",
                                #             tabName="threeID")
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
                                                            tabName="oneWidDet")
                                                ),"oneTab"
                       ),
                       menuItemOutput("twoSegOutput"),
                       # menuItem("Segmentation",
                       #          tabName="twoTab",
                       #          menuSubItem("LTC by Age",
                       #                      tabName="twoLTCAge"),
                       #          menuSubItem("Bridges to Health",
                       #                      tabName="twoBTH"),
                       #          menuSubItem("Decision Trees",
                       #                      tabName="twoCART"),
                       #          menuSubItem("K-Means Clustering",
                       #                      tabName="twoClusters")
                       #          
                       # ),
                       menuItem("Theoplots",
                                tabName="fourTab"
                       ),
                       menuItem("Risk Stratification",
                                tabName="fiveTab"
                                
                       )
)

  
