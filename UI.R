#UI Info

#Req Packages
require(XLConnect)
require(RSQLite)
require(ggplot2)
require(dplyr)
require(shiny)
require(DT)
require(dplyr)

#generateConnection
con = dbConnect(RSQLite::SQLite(), dbname = "X:\\Jwisotsky\\2018BloodIso\\JNW.E05.01.01_24MAy18\\DBwork\\Testerdb2.sqlite")
#GetList of Tables-----
ListofTables <- dbListTables(con)
#Table Stripperfunction----------------------
tablestripper<-function(Donor,Spec){
  #strip vector of lot Numbers
  Donorsamples<-Donor$Lot.Number
  Specsamples<-Spec$Lot.Number
  #Lets check if our new table has any unorginal lotnumbers
  for ( i in Specsamples){
    if(i %in% Donor$samples){
      print (i)
    }
    else{
      #First I want to strip the actual  row that lot number resides in
      Tempholder<-rbind(Donor,Spec)
    }
  }
  return(Tempholder)
}

#Code From Data camp to strip

ui <- navbarPage(
  #Title of the database  
  title = "Sample Databse",
  #tabpanel one reflects the Manifest--------------------------------------
  tabPanel('Manifest', 
           mainPanel (DT::dataTableOutput('ex1'))
  ),
  #tab pannel two refelects the Spec table---------------------
  tabPanel('Specs', 
           sidebarLayout(
             sidebarPanel(
               sliderInput("nanograms","Nanograms:",
                           min=0, max=100,
                           value=c(0,100)),
               sliderInput("ratio2623","260/230 ratio:",
                           min=.01, max=2.0,
                           value=c(.01,2.0)),
               sliderInput("ratio2628","260/280 ratio:",
                           min=.05, max=3.0,
                           value=c(.05,3.0))
             ),
             
             #main pannel
             mainPanel(DT::dataTableOutput('ex2'))
           )),
  #TabPannel for Isolation info-----
  tabPanel('Isolation', 
           mainPanel (DT::dataTableOutput('dt3'))
  ),
  #TabPannel for Sample info----
  tabPanel('Sample', 
           mainPanel (DT::dataTableOutput('dt4'))
  ),
  #TabPannel for Storage-----
  tabPanel('Storage', 
           mainPanel (DT::dataTableOutput('dt5'))
  ),
  #Tab 3.1Genearting data------
  tabPanel('Generate New Data',
           sidebarLayout(
             sidebarPanel(
               #Have the user select the table the want to add too
               radioButtons(
                 'typetable',
                 "Type of File",
                 choices = c(Manifest_File = 'Donor',
                             Sample_File='Sample',
                             Isolation_File = 'Iso',
                             Spec_File='Spec',
                             Sotrage_file='Storage'
                 ),
                 selected = 'Iso'
               ),
               numericInput('Newaddedvalues',label = 'InputNumbers',value=1),
               downloadButton("downloadNewData","Download"),
               actionButton("NewTableUpdate","Update the Database with my Choice")
             ),
             mainPanel (
               tableOutput('Newtableholder')
             )
           )),
  # Tab3.2uploadData --------------------------------------------------------
  tabPanel("Upload Data",
           sidebarLayout(
             sidebarPanel(
               fileInput(
                 "file1",
                 "Choose CSV File",
                 multiple = FALSE,
                 accept = c("text/csv",
                            "text/comma-separated-values,text/plain",
                            ".csv")
               ),
               # Horizontal line
               tags$hr(),
               
               # Input: Checkbox if file has header
               checkboxInput("header", "Header", TRUE),
               
               # Input: Select separator 
               radioButtons(
                 "sep",
                 "Separator",
                 choices = c(
                   Comma = ",",
                   Semicolon = ";",
                   Tab = "\t"
                 ),
                 selected = ","
               ),
               
               # Input: Select quotes
               radioButtons(
                 "quote",
                 "Quote",
                 choices = c(
                   None = "",
                   "Double Quote" = '"',
                   "Single Quote" = "'"
                 ),
                 selected = '"'
               ),
               
               # Horizontal line 
               tags$hr(),
               # Input: Select number of rows to display 
               radioButtons(
                 "disp",
                 "Display",
                 choices = c(Head = "head",
                             All = "all"),
                 selected = "head"
               ),
               #Horizontal line
               tags$hr(),
               #Have user specify file type
               radioButtons(
                 'typefile',
                 "Type of File",
                 choices = c(Manifest_File = 'Donor',
                             Sample_File='Sample',
                             Isolation_File = 'Isolation',
                             Spec_File='Spec',
                             Sotrage_file='Storage'
                 ),
                 selected = 'Iso'
               ),
               
               #horziontal line
               tags$hr(),
               #action button right now this does nothing becasuse I dont want to touch the server data
               actionButton("Updatebutton", "Update Database"),
               p("Only Click the above button if the table to the left looks right")
             ),
             #table Display----           
             mainPanel (tableOutput('textholder'))
           )
  ),
  
  
  #Tab4Down-------------------------------
  #currently this is broken due to some issues with old code Will enable this at a later point
  tabPanel("Download Data",
           sidebarLayout(
             sidebarPanel(
               textInput(inputId = "words",
                         label = "Enter search terms, separated by commas", 
                         value = ""),
               uiOutput("choose_dataset"),
               
               uiOutput("choose_columns"),
               
               uiOutput("subsetinfo"),
               downloadButton("downloadData","Download")
             ),
             mainPanel(
               DT::dataTableOutput("downloadTable")
             )
           ))
  #Closed Bracket for UI----
)