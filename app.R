#Required libraries
library(polynom)
library(shiny)

#SERVER FUNCTION SECTION BEGINS HERE########################################################################################
############################################################################################################################
############################################################################################################################
############################################################################################################################

#SERVER FUNCTION GENERAL SECTION############################################################################################
############################################################################################################################

#Define function to solve system with simplified equilibrium model
Free_Solver <- function(pH, C_TOTCl2_mg_L, C_TOTCy_mg_L, temp) {

  #Calculate molar hydrogen ion concentration from pH
  C_H <- 10^-pH

  #Calculate temperature in Kelvin
  T_K <- temp + 273.15

  #Convert total free chlorine and total cyanurate concentrations to molar concentrations
  C_TOTCl2  <- C_TOTCl2_mg_L / 71 / 1000
  C_TOTCy   <- C_TOTCy_mg_L / 129.07 / 1000

  #Calculate equilibrium constants based on temperature in Kelvin
  K6       <- 10^-(1743/T_K + 1.12)
  K7a      <- 10^-(2028/T_K - 2.15)
  K9a      <- 10^-(2229/T_K - 1.65)
  K        <- 10^-(3000/T_K - 10.0686 + 0.0253 * T_K)

  #Required calculations
  alpha0  <- 1 / (1 + K / C_H)
  A       <- K6 * alpha0^2 / (K7a * K9a * C_H)
  B       <- K6 * alpha0 / (K9a * C_H)
  C       <- 1 + K6 / C_H
  D       <- C_TOTCl2
  E       <- C_TOTCy
  a       <- A
  b       <- 2 * E * A + B - D * A
  c       <- E * B + C - D * B
  d       <- -D * C

  #Define polynomial equation to solve
  equation <- polynomial(coef = c(d, c, b, a))

  #Solve polynomial equation
  solution <- solve(equation)

  #Denote real solution
  real_solution <- lapply(solution, function (x) if (Im(z <- zapsmall(x))==0) as.numeric(z) else 0)

  #Select only positive solutions
  C_TOTFree <- as.numeric(real_solution[real_solution > 0])

  #Calcualate free chlorine concentration in mg chlorine/L from molar concentration
  C_TOTFree_mg_L <- C_TOTFree * 71 * 1000

  #Create output line
  C_TOTFree_output <- paste0("Estimated free chlorine concentration = ", format(round(C_TOTFree_mg_L, 2), nsmall = 2), " mg/L as chlorine")

  #Return solution
  return(C_TOTFree_output)
}

#SERVER FUNCTION DEFINITION SECTION#########################################################################################
############################################################################################################################

#Define server logic required to run simulations and produce output
server <- function(input, output, session) {

  # Calculate estimated free chlorine concentration based on provided sample conditions
  output$sim <- reactive({do.call(Free_Solver, list(input$pH, input$C_TOTCl2_mg_L, input$C_TOTCy_mg_L, input$temp))})
}

#UI OBJECT SECTION BEGINS HERE##############################################################################################
############################################################################################################################
############################################################################################################################
############################################################################################################################

#Define UI layout
ui <- shinyUI(fluidPage(

####Added from EPA template####################################################################################################################################
HTML("<html lang = 'en'>"),

tags$body(class = "html wide-template"),
tags$head(tags$link(rel = "stylesheet",
                    type = "text/css",
                    href = "style.css")),

  # Header
  HTML("<header class='masthead clearfix' role='banner'>
       <img alt='' class='site-logo' src='https://www.epa.gov/sites/all/themes/epa/logo.png'>
       <div class='site-name-and-slogan'>
       <h1 class='site-name'><a href='https://www.epa.gov' rel='home' title='Go to the home page'><span>US EPA</span></a></h1>
       <div class='site-slogan'>
       United States Environmental Protection Agency
       </div>
       </div>
       <div class='region-header'>
       <div class='block-epa-core-gsa-epa-search' id='block-epa-core-gsa-epa-search'>"),

  HTML("</div>
       </div>
       </header>
       <nav class='nav main-nav clearfix' role='navigation'>
       <div class='nav__inner'>
       <h2 class='element-invisible'>Main menu</h2>
       <ul class='menu' role='menu'>
       <li class='expanded active-trail menu-item' role='presentation'>
       <a class='active-trail menu-link' href='https://www.epa.gov/environmental-topics' role='menuitem' title='View links to the most popular pages for each of EPA&#8217s top environmental topics.'>Environmental Topics</a></li>
       <li class='menu-item' role='presentation'>
       <a class='menu-link' href='https://www.epa.gov/laws-regulations' role='menuitem' title='View links to regulatory information by topic and sector, and to top pages about environmental laws, regulations, policies, compliance, and enforcement.'>Laws &amp; Regulations</a></li>
       <li class='expanded menu-item' role='presentation'>
       <a class='menu-link' href='https://www.epa.gov/aboutepa' role='menuitem' title='Learn more about our mission, organization, and locations.'>About EPA</a></li>
       </ul>
       </div>
       </nav>
       <div class='mobile-nav' id='mobile-nav'>
       <div class='mobile-bar clearfix'>
       <label class='menu-button' for='mobile-nav-toggle'>Menu</label>
       </div><input checked id='mobile-nav-toggle' type='checkbox'>
       <div class='mobile-links element-hidden' id='mobile-links' style='height:2404px;'>
       <ul class='mobile-menu'>
       <li class='expanded menu-item'><a class='menu-link' href='https://www.epa.gov/environmental-topics' tabindex='-1' title='View links to the most popular pages for each of EPA&#8217s top environmental topics.'>Environmental Topics</a></li>
       <li class='menu-item'><a class='menu-link' href='https://www.epa.gov/laws-regulations' tabindex='-1' title='View links to regulatory information by topic and sector, and to top pages about environmental laws, regulations, policies, compliance, and enforcement.'>Laws &amp; Regulations</a></li>
       <li class='expanded menu-item'><a class='menu-link' href='https://www.epa.gov/aboutepa' tabindex='-1' title='Learn more about our mission, organization, and locations.'>About EPA</a></li>
       </ul>
       </div>
       </div>
       <section class='main-content clearfix' id='main-content' lang='en' role='main' tabindex='-1'>
       <div class='region-preface clearfix'>
       <div class='block-views-revision-hublinks-block' id='block-views-revision-hublinks-block'>
       <div class='view view-revision-hublinks view-id-revision_hublinks'>
       <span class='related-info'><strong>Related Topics:</strong></span>
       <ul class='menu pipeline'>
       <li class='menu-item'><a href='https://www.epa.gov/environmental-topics'>Environmental Topics</a></li>
       </ul>
       </div>
       </div>
       <div class='block block-pane block-pane-epa-web-area-connect' id='block-pane-epa-web-area-connect'>
       <ul class='menu utility-menu'>
       <li class='menu-item'><a class='menu-link' href='https://www.epa.gov/water-research/forms/contact-us-about-water-research'>Contact Us</a></li>
       </ul>
       </div>
       </div>
       <div class='main-column clearfix'><!--googleon:all-->
       <h1  class='page-title'>Free Chlorine Estimator for Dichlor and Trichlor Systems</h1>
       <div class='panel-pane pane-node-content'>
       <div class='pane-content'>
       <div class='node node-page clearfix view-mode-full'>"),
####Added from EPA template####################################################################################################################################

#Application title block
h4("Version 1.0, Last Updated March 18, 2019"),

h4("Created by David G. Wahman (wahman.david@epa.gov), United States Environmental Protection Agency"),

p("The provided application allows the user to estimate the free chlorine concentration when cyanuric acid is present as is the case when adding chlorine-containing chemicals
    commonly referred to as Dichlor (anhydrous sodium dichloroisocyanurate or sodium dichloroisocyanurate dihydrate) or Trichlor (trichloroisocyanuric acid) to water.
    The equilibrium equations and associated temperature dependent constants used for the free chlorine estimation are for the Simple Model developed by ",

  a(target = "_blank", href="https://www.liebertpub.com/doi/10.1089/ees.2018.0387", "Wahman and Alexander (2019)"),

  " and references therein. To open a document describing the application in a new window, click on the following link: ",

  a(target = "_blank", href = "manual.pdf", "Application Documentation.")
  ),

p("The application was developed by the United States Environmental Protection Agency (EPA).
    No warranty expressed or implied is made regarding the accuracy
    or utility of the system, nor shall the act of distribution constitute any such warranty.  Any reference to specific commercial products, processes, or services by service mark,
    trademark, manufacturer, or otherwise, does not constitute or imply their endorsement, recommendation, or favoring by
    EPA. The EPA seal and logo shall not be used in any manner to imply endorsement of any commercial product or activity
    by EPA or the United States Government.  The views expressed in this application do not necessarily represent the views
    or policies of the EPA. Although a reasonable effort has been made to assure that the results obtained are correct,
    this application is experimental.  Therefore, the author and the EPA are not responsible and assume no liability whatsoever
    for any results or any use made of the results obtained from this application, nor for any damages or litigation that result
    from the use of the application for any purpose."),

sidebarLayout(
  sidebarPanel(width = 8,

    h3("Inputs for Free Chlorine Estimate"),

    # Create input and tooltip for measured sample temperature in Celsius
    sliderInput("temp",
                label = p("Temperature (Celsius) - Measured",
                          style = "font-size: 14px"),
                min = 5.0,
                max = 35.0,
                value = 25.0,
                step = 0.5),

    # Create input and tooltip for measured sample pH
    sliderInput("pH",
                label = p("pH - Measured",
                          style = "font-size: 14px"),
                min = 6.50,
                max = 9.50,
                value = 8.00,
                step = 0.05),

    # Create input and tooltip for measured total chlorine concentration
    sliderInput("C_TOTCl2_mg_L",
                label = p(HTML("Total Chlorine (mg Cl<sub>2</sub>/L) - Measured with a free chlorine method (see Application Documentation)"),
                          style = "font-size: 14px"),
                min = 0.05,
                max = 10.00,
                value = 3.00,
                step = 0.05),

    # Create input and tooltip for low range known total cyanuric acid concentration
    sliderInput("C_TOTCy_mg_L",
                label = p("Total Cyanurate (mg/L as cyanuric acid) - Estimated from chemical dosing (see Application Documentation)",
                          style = "font-size: 14px"),
                min = 0.05,
                max = 10.00,
                value = 3.00,
                step = 0.05)
  ),

  # Output calculated free chlorine concentration in mg chlorine/L
  mainPanel(
    h3(textOutput("sim"))
  )
),

####Additional required contact section########################################################################################################################
hr(),
p( a(href="https://www.epa.gov/water-research/forms/contact-us-about-water-research", "Contact Us"),
  " to ask a question, provide feedback, or report a problem."),

####Added from EPA template####################################################################################################################################
# Footer
HTML("</div>
     </div>
     </div>
     </div>
     </section>
     <footer class='main-footer clearfix' role='contentinfo'>
     <div class='main-footer__inner'>
     <div class='region-footer'>
     <div class='block-pane-epa-global-footer' id='block-pane-epa-global-footer'>
     <div class='row cols-3'>
     <div class='col size-1of3'>
     <div class='col__title'>
     Discover.
     </div>
     <ul class='menu'>
     <li><a href='https://www.epa.gov/accessibility'>Accessibility</a></li>
     <li><a href='https://www.epa.gov/aboutepa/administrator-gina-mccarthy'>EPA Administrator</a></li>
     <li><a href='https://www.epa.gov/planandbudget'>Budget &amp; Performance</a></li>
     <li><a href='https://www.epa.gov/contracts'>Contracting</a></li>
     <li><a href='https://www.epa.gov/home/grants-and-other-funding-opportunities'>Grants</a></li>
     <li><a href='https://19january2017snapshot.epa.gov'>January 19, 2017 Web Snapshot</a></li>
     <li><a href='https://www.epa.gov/ocr/whistleblower-protections-epa-and-how-they-relate-non-disclosure-agreements-signed-epa-employees'>No FEAR Act Data</a></li>
     <li><a href='https://www.epa.gov/privacy'>Privacy</a></li>
     </ul>
     </div>
     <div class='col size-1of3'>
     <div class='col__title'>
     Connect.
     </div>
     <ul class='menu'>
     <li><a href='https://www.data.gov/'>Data.gov</a></li>
     <li><a href='https://www.epa.gov/office-inspector-general/about-epas-office-inspector-general'>Inspector General</a></li>
     <li><a href='https://www.epa.gov/careers'>Jobs</a></li>
     <li><a href='https://www.epa.gov/newsroom'>Newsroom</a></li>
     <li><a href='https://www.epa.gov/open'>Open Government</a></li>
     <li><a href='https://www.regulations.gov/'>Regulations.gov</a></li>
     <li><a href='https://www.epa.gov/newsroom/email-subscriptions'>Subscribe</a></li>
     <li><a href='https://www.usa.gov/'>USA.gov</a></li>
     <li><a href='https://www.whitehouse.gov/'>White House</a></li>
     </ul>
     </div>
     <div class='col size-1of3'>
     <div class='col__title'>
     Ask.
     </div>
     <ul class='menu'>
     <li><a href='https://www.epa.gov/home/forms/contact-epa'>Contact Us</a></li>
     <li><a href='https://www.epa.gov/home/epa-hotlines'>Hotlines</a></li>
     <li><a href='https://www.epa.gov/foia'>FOIA Requests</a></li>
     <li><a href='https://www.epa.gov/home/frequent-questions-specific-epa-programstopics'>Frequent Questions</a></li>
     </ul>
     <div class='col__title'>
     Follow.
     </div>
     <ul class='social-menu'>
     <li><a class='menu-link social-facebook' href='https://www.facebook.com/EPA'>Facebook</a></li>
     <li><a class='menu-link social-twitter' href='https://twitter.com/epa'>Twitter</a></li>
     <li><a class='menu-link social-youtube' href='https://www.youtube.com/user/USEPAgov'>YouTube</a></li>
     <li><a class='menu-link social-flickr' href='https://www.flickr.com/photos/usepagov'>Flickr</a></li>
     <li><a class='menu-link social-instagram' href='https://www.instagram.com/epagov'>Instagram</a></li>
     </ul>
     <p class='last-updated'>Last updated on March 18, 2019</p>
     </div>
     </div>
     </div>
     </div>
     </div>
     </footer>")
####Added from EPA template####################################################################################################################################
)
)

#APPLICATION FUNCTION CALL DEFINITION#######################################################################################
############################################################################################################################
shinyApp(ui = ui, server = server)