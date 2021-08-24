#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd






# This UI will be split into three main components, header, sidebar, and dashboard. All three objects are called by the app_ui function.
page <- function(user_id, run_id) {

  # attaches color schema 
  mytheme <- my_theme()

  tagList(
    bs4Dash::bs4DashPage(
      fresh::use_theme(
        mytheme
      ),
        title = "Safer Streets Priority Finder",
        sidebar_collapsed = FALSE,
        sidebar_mini = TRUE,
        navbar = bs4Dash::bs4DashNavbar(
        skin = "dark",
        status = "primary",
        leftUi = h5("Safer Streets Priority Finder, Beta V 1.0", style="color:#f8f9fa; align-self: flex-end;"),
        rightUi =
            uiOutput('logout_header_area')

      ),

    footer = bs4DashFooter(
      tags$div(id = 'logout',  style = "padding-right:10px", 
               tags$div(id = 'top_right',
                        fluidRow(
                          
                          shiny::a(shiny::actionButton(inputId="contact_devs_footer", icon = icon("envelope"), label="", class="share_communication"),  
                                   href = "mailto:saferstreetspriorityfinder@tooledesign.com?subject=SSPF Developer Message", target="_blank", rel="noopener noreferrer"),
                          shiny::a(shiny::actionButton(inputId="facebook_share_footer", icon = icon("facebook-square"), label="", class="share_communication"), 
                                   href = "https://www.facebook.com/sharer/sharer.php?u=https%3A//www.saferstreetspriorityfinder.com/", target="_blank"),
                          shiny::a(shiny::actionButton(inputId="twitter_share_footer", icon = icon("twitter"), label="", class="share_communication"),
                                   href = "https://twitter.com/intent/tweet?text=Check%20out%20this%20great%20web%20application!%20The%20Safer%20Streets%20Priority%20Finder%20enables%20you%20to%20analyze%20the%20risk%20to%20vulnerable%20road%20users%20(bicyclists%20and%20pedestrians)%20on%20your%20community%E2%80%99s%20roads.%20https%3A//www.saferstreetspriorityfinder.com/", target="_blank"),
                          shiny::a(shiny::actionButton(inputId="linkedin_share_footer", icon = icon("linkedin"), label="", class="share_communication"),
                                   href = "https://www.linkedin.com/shareArticle?mini=true&url=https%3A//www.saferstreetspriorityfinder.com/&title=Safer%20Streets%20Priority%20Finder&summary=The%20Safer%20Streets%20Priority%20Finder%20enables%20you%20to%20analyze%20the%20risk%20to%20vulnerable%20road%20users%20(bicyclists%20and%20pedestrians)%20on%20your%20community%E2%80%99s%20roads.%20&source=https%3A//www.saferstreetspriorityfinder.com/", target="_blank")
                        )
               ))
      ),
    
    
    sidebar = bs4DashSidebar(
      fresh::use_theme(
        mytheme
      ),
      id = "sidebarID",
      skin = "dark",
      status = "primary",
      title = "Safer Streets Priority Finder",
      brandColor = "primary",
      elevation = 3,
      opacity = 0.8,
      # left sidebar menu
      bs4Dash::bs4SidebarMenu(
        #bs4SidebarHeader("Header"),
        bs4Dash::bs4SidebarMenuItem(
          "Welcome",
          tabName = "welcome",
          icon = 'handshake'
        ),
        bs4Dash::bs4SidebarMenuItem(
          "Overview/Methodology",
          tabName = "overview",
          icon = 'book-open'
        ),
        bs4Dash::bs4SidebarMenuItem(
          "Instructions",
          tabName = "instructions",
          icon = 'info-circle'
        ),
        bs4Dash::bs4SidebarMenuItem(
          "Load Data",
          tabName = "load_data",
          icon = 'database'
        ),
        bs4Dash::bs4SidebarMenuItem(
          "Analysis",
          tabName = "analysis_production",
          icon = 'cogs'
        ),
        bs4Dash::bs4SidebarMenuItem(
          "Dashboard",
          tabName = "manage_data",
          icon = 'sliders-h'
        ),
        bs4Dash::bs4SidebarMenuItem(
          "Map Visualization",
          tabName = "map_results",
          icon = 'globe'
        ),
        bs4Dash::bs4SidebarMenuItem(
          "Use Cases",
          tabName = "use_cases",
          icon = 'briefcase'
        ),
        bs4Dash::bs4SidebarMenuItem(
          "FAQs",
          tabName = "faqs",
          icon = 'info'
        )
      )
    ),

body = bs4Dash::bs4DashBody(
  shinyjs::useShinyjs(),
  waiter::use_waiter(),
  shinyalert::useShinyalert(),
  fresh::use_theme(
    mytheme
  ),
  #actionButton('the_start', 'Start!'),
  bs4Dash::bs4TabItems(
    
    bs4Dash::bs4TabItem(
    tabName = "welcome",
    bs4Dash::bs4Jumbotron(
      title = tags$html("Welcome!"),
      lead = "The Safer Streets Priority Finder enables you to analyze the risk to vulnerable road users (bicyclists and pedestrians) on your community’s roads. You can use your local road, crash, and study area data or select from nationally available datasets to:",
      status = "primary",
      fluidRow(
        col_12(
          tags$ul(
            tags$li(
              "Explore descriptive statistics related to your crash data"
              ),
            tags$li(
              "Develop a Sliding Windows Analysis using historical crash data to inform a High Injury Network"
                   ),
            tags$li(
              "Develop a Safer Streets Model to estimate risk along your road network, even in areas that haven't had any reported crashes recently"
              )
            )
          )
        ),
      tags$hr(),
      fluidRow(
        col_6(
          shinycssloaders::withSpinner(proxy.height='30px',
                                       type = 8,
                                       size = .65,
                                       color = "#277da1",
                                       ui_element = uiOutput("login_status")
          )),
        col_6(
          shinycssloaders::withSpinner(proxy.height='30px',
                                       type = 8,
                                       size = .65,
                                       color = "#277da1",
                                       ui_element = uiOutput("model_prod_status")
          )
        )
      )
    ),
    fluidRow(
      col_6(
        bs4Dash::bs4UserCard(
          title = "SSPF Contributors",
          status = "info",
          width = 12,
          list_to_li(
            list(
              tags$html("City of New Orleans: Daniel Jatres and Jennifer Ruley, PE"),
              tags$html("Toole Design: Jessica Schoner PhD, Daniel Patterson, Rachel Finfer, Jacob Nigro, Theja Putta PhD, and Brendan Murphy"),
              tags$html("New Orleans Regional Transit Authority: Robert Stickney"),
              tags$html("University of New Orleans Transportation Institute: Tara Tolford AICP and Maryam Izadi"),
              tags$html("Funded through USDOT's Safety Data Initiative Grant"),
              tags$html("Additional acknowledgements to Frank Proulx and Ted Mansfield  for their contributions.")
            )
          )
        )
      ),
      col_6(
        bs4Dash::bs4UserCard(
          title = "Tool Purpose",
          status = "info",
          width = 12,
          tags$html('Safer Streets Priority Finder is a free and open source resource that allows practitioners and advocates to analyze and understand the risk to vulnerable road users (bicyclists and pedestrians) on their local roadways. With just some minimal data prep required on your end, this tool uses a Bayesian statistical framework to make a robust estimation of crash risk along the road network.')
          )
        )
    ),
    fluidRow(
      col_12( #I'm hoping this makes a div with class col-sm-12...hopefully that exists?
        bs4Dash::bs4UserCard(
          title = "Disclaimers",
          status = "info",
          width= 24,
          p('This document and the information contained herein is prepared solely for the purpose of identifying, evaluating and planning safety improvements on public roads which may be implemented utilizing federal aid highway funds; and is therefore exempt from discovery or admission into evidence pursuant to 23 U.S.C. 409.  Contact the Louisiana Department of Transportation and Development\'s Traffic Safety Office at (225) 379-1871 before releasing any information.'),
          p('This tool and associated data outputs are provided for informational purposes only, and all results, recommendations, geographic and mapping information, and commentary contained herein are based on limited data available at the national scale or user uploaded data—user uploaded data may be from unverified sources. The project creators make no warranties, expressed or implied, concerning the accuracy, completeness, or suitability of the underlying source data used in this analysis or the recommendations and conclusions derived therefrom.'),
          p('The project creators make no representation as to the accuracy, adequacy, reliability, availability or completeness of the crash records or is not responsible for any errors or omissions in such records or data. Motor vehicle crashes are complex occurrences that often result from multiple contributing factors. The data posted available within this tool, including crash data, are collected for the purpose of identifying or planning safety enhancements for potential crash sites, improving roadway safety, or improving conditions or railway-highway crossings. Under federal law, this information is not subject to discovery and cannot be admitted into evidence in any federal or state court proceeding or considered for other purposes in any action for damages that involves the sites mentioned in these records (see 23 USC, Section 409).'),
          p('Accessibility: If you need assistance using this tool, please call the City of New Orleans ADA Administrator at 504-658-4021 for support.')
          )
       )

    )),
  bs4Dash::bs4TabItem(
    tabName = "overview",
    mod_overview_ui("overview_ui_1")
  ),
  bs4Dash::bs4TabItem(
    tabName = "instructions",
    mod_instructions_ui("instructions_ui_1")
  ),
  bs4Dash::bs4TabItem(
  tabName= "test",
  ),
  bs4Dash::bs4TabItem(
    tabName = "load_data",
    mod_load_data_ui("load_data_ui_1")
  ),
  bs4Dash::bs4TabItem(
    tabName = "manage_data",
    mod_reporter_ui("reporter_ui_1")
  ),
  bs4Dash::bs4TabItem(
    tabName = "analysis_production",
   bs4TabSetPanel(
       id = "data_explorer_tab_select",
       side='left',

       bs4Dash::bs4TabPanel(tabName = HTML("Sliding Windows"),
                             hr(),
                            mod_build_sliding_windows_ui("build_sliding_windows_ui_1"),

      ),
      bs4Dash::bs4TabPanel(tabName = HTML("Safer Streets Model"), #may need to adjust sizing/include <br> depending on how these two buttons look with the new longer names
                           hr(),
                           mod_build_model_ui("build_model_ui_1"),

      )
      ),
    tags$div(br())
  ),
  bs4Dash::bs4TabItem(
    tabName = 'map_results',
    mod_visualize_data_reporter_ui("visualize_data_reporter_ui_1")
  ),

  bs4Dash::bs4TabItem(
    tabName = "use_cases",
    mod_use_case_ui("use_case_ui_1")
  ),
  bs4Dash::bs4TabItem(
    tabName = "faqs",
    mod_faq_ui("faq_ui_1")
    )
))
))
}




app_ui <- function(request) {
  tagList(
    #Add modules
    shinybusy::use_busy_spinner(spin = "fading-circle",
                                position="full-page"),
    shinydisconnect::disconnectMessage(
      text = "Disconnected from the server.",
      refresh = "Reload",
      background = "rgb(64, 64, 64);",
      colour = "white",
      overlayColour = "grey",
      overlayOpacity = 0.4,
      refreshColour = "rgb(153, 153, 255)",
      css = "border-radius: 7px;"
    ),
    # List the first level UI elements here
    ui <- uiOutput("app"),

    # Leave this function for adding external resources
    #tags$head(tags$script(src = "www/Mybs4Dash.js")),
    shinyjs::useShinyjs(),
    waiter::use_waiter(),
    golem_add_external_resources()

  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
#' @import shinyWidgets
#' @import leaflet
#' @import sf
#' @import shinydashboard
#' @import rgdal
golem_add_external_resources <- function(){
  add_resource_path(
    'www', app_sys('app/www')
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys('app/www'),
      app_title = 'Safer Streets Priority Finder'
    ),
    tags$link(rel="stylesheet", type="text/css", href="www/overall_css.css")
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
