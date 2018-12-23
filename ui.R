## Vivek Suhag(11810007) and Ratna Prashanth Kumar(11810113)

shinyUI(
  dashboardPage( skin="blue",
    dashboardHeader(title = "Text Analytics using UDPipe", titleWidth = 400),

    dashboardSidebar(
      width = 400,
      sidebarMenu(
        menuItem("Overview", tabName = "overview", icon = icon("home")),
        menuItem("Data Input", tabName = "data_input", icon= icon("cloud-upload-alt")),
        menuItem("Annotated Data", tabName = "data_annotate", icon = icon("th")),
        menuItem("Co-Occurrence", tabName = "cooccur", icon = icon("retweet")),
        menuItem("Word Cloud", tabName = "cloud", icon = icon("object-group")),
        menuItem("Acknowledgement", tabName = "ack", icon = icon("info-circle"))
      )
    ),
    
    dashboardBody(
      tags$head(tags$style(HTML('
        .main-header .logo {
          font-family: "Georgia", Times, "Times New Roman", serif;
          font-weight: bold;
          font-size: 24px;
        }
        .sidebar-menu {font-size: 18px;}
        .skin-blue .main-header .logo {
          background-color: #3c8dbc;}
        .skin-blue .main-header .logo:hover {
          background-color: #3c8dbc;}
        .nav-tabs-custom {font-size: 18px;
          font-weight: bold;
          font-family: "Georgia", Times, "Times New Roman", serif;
          background-color: #3c8dbc;}
        .tab-content {font-weight: normal;}
      '))),
      
      tabItems(
        tabItem("overview",
                h1("An App for Text Analytics using UDPipe Package"),
                h4("The purpose of this App is to do Text analytics using UDPipe R package. UDPipe provides the standard NLP functionalities of tagging, parsing and dependency evaluations.
                   You can do data annotations and plot Co-occurrence graphs and build Wordclouds"),
                
                h4("The 'udpipe' package is based on ", a(href="http://universaldependencies.org/", "Stanford's Universal Dependency (ud) Linguistic typology")),
                h4("It comes pre-packaged with trained models in as many as ", span(strong("52 languages")), "."),
                h3('How to use this App:'),
                h4("The App has a navigation panel on left side for intuitive user interactions. The key functionality that can be accomplished by each item is as below:"),
                tags$div(
                  tags$ul(
                    tags$li(h4(span(strong("Data Input:")))),
                    h4("Use this tab to upload the text Data and the udpipe Model Files. Wait for the annotation status button to turn green."),
                    tags$li(h4(span(strong("Annotated Data:")))),
                    h4("View quick stats about uploaded data. Analyze and download the annotated data."),
                    tags$li(h4(span(strong("Co-Occurrence:")))),
                    h4("Analyze Co-occurrence plots of 3 different types: Sentence based, General(non-sentence) based and skipgram based"),
                    h4("Interact and play around using the Universal Part-of-speech Tag filters."),
                    tags$li(h4(span(strong("Word Cloud:")))),
                    h4("Analyze term frequencies and weightages of each type of word in the form of data-table and wordcloud plot.")

                  )
                ),
                br(),
                h3("Solution By:"),
                tags$div(
                  tags$ul(
                    tags$li(h4(span(strong("Vivek Suhag")), "(PGID: 11810007)")),
                    tags$li(h4(span(strong("Ratna Prashanth Kumar")), "(PGID: 11810113)"))
                  )
                )
        ),
        
        tabItem("data_input",
                h2("Input File Uploads:"),
                h4("This section is used to upload the data and model files."),
                h4("The App currently supports File upload of up-to 50MB size only."),
                h3("Data File:"),
                h4("This app supports data files in .txt format only.",align="justify"),
                h4("Please refer to the link below for sample txt file."),
                h4(a(href="https://github.com/viveksuhag/text-udpipe/blob/master/reviews.txt"
                  ,"Sample data input file")),   
                br(),
                h4(" "),
                fileInput("data_file", "Upload the data (txt file)"),
                h3("Model File:"),
                fileInput("ud_model", "Upload model File (*.udpipe file)"),
                h3("Data Annotation:"),
                #infoBox("Status","Incomplete", icon = icon("info"),color = "blue"),
                br(),
                h4(span(strong("Wait unless the status box turns Green!!")), "(it may take couple of minutes depending on file size)"),
                br(),
                infoBoxOutput("ann_status"),
                br(),
                br(),
                br(),
                br(),
                br(),
                h4("You may now visit the 'Annotated Data' section using Left panel to explore the uploaded dataset.")
        ),
        
        tabItem("data_annotate",
                h2("Quick Stats about Data:"),
                fluidRow(
                  valueBoxOutput("total_docs"),
                  valueBoxOutput("total_sent"),
                  valueBoxOutput("total_lines")
                  #valueBox(120, "Number of Documents", icon = icon("tasks"),color = "blue"),
                  #valueBox(930, "Number of Sentences", icon = icon("list"),color = "blue"),
                  #valueBox(17650, "# of Records in Annotated matrix", icon = icon("th"),color = "blue")
                ),
                
                h3("Sample Annotated Data:"),
                p("(First 50 records are shown here. Use the Download option at the bottom of the page to view the entire dataset.)"),
                dataTableOutput('data_annotate'),

                h3("Download the table data using the below button:"),
                downloadButton("data_ann_download", "Download as CSV")

        ),
        
        tabItem("cooccur",
                h2("Co-occurrences"),
                fluidRow(
                  tabBox(
                    width = 10,
                    tabPanel("Sentence based", 
                             plotOutput('cooc_sn_plot', height = 700), p("*UPOS column is used for the plots")),
                    tabPanel("Non-sentence based", 
                             plotOutput('cooc_nsn_plot', height = 700)),                    
                    tabPanel("Skipgram based", 
                             sliderInput("skipgram", "# of Words(n) distance to use for finding Co-occurremce:", min = 2,  max = 8, value = 4),
                             plotOutput('cooc_skp_plot', height = 700))
                    ),

                  box(
                    title = "Part-of-Speech Tags", width = 2, solidHeader = TRUE,background = "light-blue",
                    checkboxGroupInput("pos_tags", "Select part-of-speech Tags",
                                       c(" Adjective" = "ADJ",
                                         " Noun" = "NOUN",
                                         " Proper Noun" = "PROPN",
                                         " Adverb" = "ADV",
                                         " Verb" = "VERB"),
                                       selected = c("ADJ","NOUN", "PROPN"))
                  )
                )
        ),
        
        tabItem("cloud",
                h2("Most Common occurring Words:"),
                br(),
                radioButtons("term_choice","Select Term Type:",
                               c(" Adjective" = "ADJ",
                                 " Noun" = "NOUN",
                                 " Proper Noun" = "PROPN",
                                 " Adverb" = "ADV",
                                 " Verb" = "VERB"),
                               selected = "ADV",inline = TRUE
                ),
                br(),
                fluidRow(
                  box(width = 6,
                    sliderInput("wc_freq", "Minimum Frequency in Word Cloud Graphs:", min = 1,  max = 20, value = 5)
                  ),
                  box( width = 6,
                    sliderInput("wc_max", "Maximum Words to show in Word Cloud Graphs:", min = 20,  max = 500, value = 100)
                  )
                ),
                
                fluidRow(
                  box(
                    width = 8, status = "info", solidHeader = TRUE,
                    title = "Word Cloud",
                    plotOutput("wc_plot", height = 700)
                  ),
                  box(
                    width = 4, status = "info",
                    title = "Weights Distribution of Terms (Top 15)",
                    dataTableOutput('term_freq_data')
                  )
                )
        ),
        tabItem("ack",
                h4("We are ", span(strong("Thankful")), "to following for the help, guidance, support and reference material provided in building this app:"),
                br(),
                tags$div(
                  tags$ul(
                    tags$li(h4(span(strong("Prof. Sudhir Voleti:")))),
                    h4("For introducing us to the wonderful world of Shiny and more importantly 'Text Analytics'."),
                    tags$li(h4(a(href="https://shiny.rstudio.com/" ,"Shiny RStudio"))),
                    h4("building blocks and examples of responsive Shiny apps."),
                    tags$li(h4(a(href="https://rstudio.github.io/shinydashboard/index.html" ,"Shiny Dashboard"))),
                    h4("making it fun to create layouts and panels with so much ease."),
                    tags$li(h4("Icons:", a(href="https://fontawesome.com/icons?d=gallery&m=free" ,"Font Awesome"), "and", 
                               a(href="https://fontawesome.com/icons?d=gallery&m=free" ,"Glyphicons"))),
                    h4("Awesome Free icons."),
                    tags$li(h4(span(strong("Anisha Mula & Yogesh Khandelwal:")))),
                    h4("For resolving queries through Tutorials and emails.")

                  )
                )
                
        )
      ) ## tabItems
    ) ## dashboardBody
  ) ## dashboardPage
) ## shinyUI
