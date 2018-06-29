#
#Server -----------------------------------------------------
#Server function
server <- function(input, output) {
  # shows data table for the first table--------
  output$ex1 <- DT::renderDataTable(DT::datatable({
    data<- dbGetQuery(con, 'select * from Donor')
    data
  }))
  # shows the second table-------
  #Handle Slider Input and returnt the table for rendering
  specfiltertable<-reactive({
    minng<-input$nanograms[1]
    maxng<-input$nanograms[2]
    min2623<-input$ratio2623[1]
    max2623<-input$ratio2623[2]
    min2628<-input$ratio2628[1]
    max2628<-input$ratio2628[2]
    SpecTab <- dbGetQuery(con, 'select * from Spec')
    Specfilter<-filter(SpecTab,
                       SpecTab$Conc..>=minng, 
                       SpecTab$Conc..<=maxng,
                       SpecTab$X260.230.>=min2623, 
                       SpecTab$X260.230.<=max2623,
                       SpecTab$X260.280.>=min2628, 
                       SpecTab$X260.280.<=max2628
    )
    return(Specfilter)
  })
  output$ex2 <- DT::renderDataTable(DT::datatable({
    data <- specfiltertable()
    data
  }))
  #Isolation tab-----
  output$dt3 <- DT::renderDataTable(DT::datatable({
    data<- dbGetQuery(con, 'select * from Isolation')
    data
  }))  
  #Sample tab------
  output$dt4 <- DT::renderDataTable(DT::datatable({
    data<- dbGetQuery(con, 'select * from Sample')
    data
  })) 
  #Storage Tab---
  output$dt5 <- DT::renderDataTable(DT::datatable({
    data<- dbGetQuery(con, 'select * from Storage')
    data
  }))  
  #Tab3.1 values-------
  #generate the new table using some reactive values
  newtable<-reactive({
    #establish our first important value the table we are looking at
    tableinfo<-input$typetable
    #Lets find out waht that value is
    #IsoCase-----
    if(tableinfo=='Iso'){
      Iso<-dbGetQuery(con,'Select * from Isolation')
      Placeholder<-length(Iso$Isolate_UNID)
      LastplaceID<-(Iso$Isolate_UNID)[Placeholder]
      valuewanttoadd<-input$Newaddedvalues
      Newvaluemax<-valuewanttoadd+Placeholder
      Listofnewvals<-seq(Placeholder+1,Newvaluemax)
      NewIsoIDs<-paste0('ISID',Listofnewvals,sep="")
      Newdf1<-add_row(Iso,Isolate_UNID=NewIsoIDs)
      return(Filterframe<-filter(Newdf1,Newdf1$Isolate_UNID%in%NewIsoIDs))
    }
    #DonortableCase----
    if(tableinfo=='Donor'){
      Donor <- dbGetQuery(con, 'select * from Donor')
      Placeholder<-length(Donor$Donor_ID)
      LastplaceID<-(Donor$Donor_ID)[Placeholder]
      valuewanttoadd<-input$Newaddedvalues
      Newvaluemax<-valuewanttoadd+Placeholder
      Listofnewvals<-seq(Placeholder+1,Newvaluemax)
      NewDonorIDs<-paste0('RSNA',Listofnewvals,sep="")
      Newdf1<-add_row(Donor,Donor_ID=NewDonorIDs)
      return(filter(Newdf1,Newdf1$Donor_ID%in%NewDonorIDs))
    }
    #SampleCase----
    if(tableinfo=='Sample'){
      Sample<-dbGetQuery(con,'Select * from Sample')
      Placeholder<-length(Sample$SampleID)
      LastplaceID<-(Sample$SampleID)[Placeholder]
      valuewanttoadd<-input$Newaddedvalues
      Newvaluemax<-valuewanttoadd+Placeholder
      Listofnewvals<-seq(Placeholder+1,Newvaluemax)
      NewIsoIDs<-paste0('SID',Listofnewvals,sep="")
      Newdf1<-add_row(Sample,SampleID=NewIsoIDs)
      return(Filterframe<-filter(Newdf1,Newdf1$SampleID%in%NewIsoIDs))
    }
    #SpecCase----
    if(tableinfo=='Spec'){
      Spec <- dbGetQuery(con, 'select * from Spec')
      Placeholder<-length(Spec$Spec_UNID)
      LastplaceID<-(Spec$Spec_UNID)[Placeholder]
      valuewanttoadd<-input$Newaddedvalues
      Newvaluemax<-valuewanttoadd+Placeholder
      Listofnewvals<-seq(Placeholder+1,Newvaluemax)
      NewIsoIDs<-paste0('Spec',Listofnewvals,sep="")
      Newdf1<-add_row(Spec,Spec_UNID=NewIsoIDs)
      return(Filterframe<-filter(Newdf1,Newdf1$Spec_UNID%in%NewIsoIDs))
    }
    #storageCase----
    if(tableinfo=='Storage'){
      Storage<-dbGetQuery(con,'Select * from Storage')
      Placeholder<-length(Storage$StorageID)
      LastplaceID<-(Storage$StorageID)[Placeholder]
      valuewanttoadd<-input$Newaddedvalues
      Newvaluemax<-valuewanttoadd+Placeholder
      Listofnewvals<-seq(Placeholder+1,Newvaluemax)
      NewIsoIDs<-paste0('Storage',Listofnewvals,sep="")
      Newdf1<-add_row(Storage,StorageID=NewIsoIDs)
      return(Filterframe<-filter(Newdf1,Newdf1$StorageID%in%NewIsoIDs))
    }
  })
  #Renders the upload file after mergning
  output$Newtableholder<-renderTable({
    return(newtable())
  })
  #Download the NewDataTable
  output$downloadNewData <- downloadHandler(
    filename = function() {
      paste(input$typetable, ".csv", sep = "")
    },
    content = function(file) {
      write.csv(newtable(), file, row.names = FALSE)
    }
  )
  #action Button that Updates the Database
  observeEvent(input$NewTableUpdate,{
    tableinfo<-input$typetable
    if(tableinfo=='Iso'){
      Iso<-dbGetQuery(con,'Select * from Isolation')
      Unionframe<-union_all(Iso,newtable())
      Booltest<-dim(Unionframe)>dim(Iso)
      Booltest<-Booltest[1]
      if(Booltest==T){
        dbWriteTable(con,'Isolation',Unionframe,overwrite=T)
      }
    }
    if(tableinfo=='Donor'){
      Donor <- dbGetQuery(con, 'select * from Donor')
      Unionframe<-union_all(Donor,newtable())
      Booltest<-dim(Unionframe)>dim(Donor)
      Booltest<-Booltest[1]
      if(Booltest==T){
        dbWriteTable(con,'Donor',Unionframe,overwrite=T)
      }
    }
    if(tableinfo=='Sample'){
      Sample<-dbGetQuery(con,'Select * from Sample')
      Unionframe<-union_all(Sample,newtable())
      Booltest<-dim(Unionframe)>dim(Sample)
      Booltest<-Booltest[1]
      if(Booltest==T){
        dbWriteTable(con,'Sample',Unionframe,overwrite=T)
      }
    }
    if(tableinfo=='Spec'){
      Spec <- dbGetQuery(con, 'select * from Spec')
      Unionframe<-union_all(Spec,newtable())
      Booltest<-dim(Unionframe)>dim(Spec)
      Booltest<-Booltest[1]
      if(Booltest==T){
        dbWriteTable(con,'Spec',Unionframe,overwrite=T)
      }
    }
    if(tableinfo=='Storage'){
      Storage<-dbGetQuery(con,'Select * from Storage')
      Unionframe<-union_all(Storage,newtable())
      Booltest<-dim(Unionframe)>dim(Storage)
      Booltest<-Booltest[1]
      if(Booltest==T){
        dbWriteTable(con,'Storage',Unionframe,overwrite=T)
      }
    }
  })
  
  #Tab3.2 Server values----
  #This takes in the data frame from the upload section and merges them together
  #Current issue Race has . after it for manifest files
  #not sanatizing input
  a <- reactive({
    (newdf <- read.csv(
      input$file1$datapath,
      header = input$header,
      sep = input$sep,
      quote = input$quote,
      stringsAsFactors = FALSE)
    )
    if(input$typefile=='Isolation'){
      Isolation <- dbGetQuery(con, 'select * from Isolation')
      if(colnames(Isolation)==colnames(newdf)){
        ind <- match(newdf$Isolate_UNID, Isolation$Isolate_UNID)
        Isolation[ind,]<-newdf[]
        return(Isolation)}
      else{
        return(message('Wrong Table'))
      }
    }
    if(input$typefile=='Donor'){
      Donor <- dbGetQuery(con, 'select * from Donor')
      if(colnames(Donor)==colnames(newdf)){
        ind <- match(newdf$Donor_ID, Donor$Donor_ID)
        Donor[ind,]<-newdf[]
        return(Donor)}
      else{
        return(message('Wrong Table'))
      }
    }
    if(input$typefile=='Spec'){
      Spec <- dbGetQuery(con, 'select * from Spec')
      if(colnames(Spec)==colnames(newdf)){
        ind <- match(newdf$Spec_UNID, Spec$Spec_UNID)
        Spec[ind,]<-newdf[]
        return(Spec)}
      else{
        return(message('Wrong Table'))
      }
    }
    if(input$typefile=='Sample'){
      Sample <- dbGetQuery(con, 'select * from Sample')
      if(colnames(Sample)==colnames(newdf)){
        ind <- match(newdf$SampleID, Sample$SampleID)
        Sample[ind,]<-newdf[]
        return(Sample)}
      else{
        return(message('Wrong Table'))
      }
    }
    if(input$typefile=='Storage'){
      Storage <- dbGetQuery(con, 'select * from Storage')
      if(colnames(Storage)==colnames(newdf)){
        ind <- match(newdf$StorageID, Storage$StorageID)
        Storage[ind,]<-newdf[]            
        return(Storage)}
      else{
        return(message('Wrong Table'))
      }
    }
    else{
      return(dfholder)
    }
  })
  #New Reactive for rendering the table
  tab32table<-reactive({
    view32df<-a()
    newdf <- read.csv(
      input$file1$datapath,
      header = input$header,
      sep = input$sep,
      quote = input$quote,
      stringsAsFactors = FALSE)
    if(input$typefile=='Isolation'){
      holder<-newdf[1,1]
      dimlast<-dim(view32df)[1]
      ind<-match(holder,view32df$Isolate_UNID)
      ind<-ind-2
      view32df<-view32df[ind:dimlast,]
      return(view32df)
    }
    if(input$typefile=='Donor'){
      holder<-newdf[1,2]
      dimlast<-dim(view32df)[1]
      ind<-match(holder,view32df$Donor_ID)
      ind<-ind-2
      view32df<-view32df[ind:dimlast,]
      return(view32df)
    }
    if(input$typefile=='Spec'){
      Spec <- dbGetQuery(con, 'select * from Spec')
      holder<-newdf[1,1]
      dimlast<-dim(view32df)[1]
      ind<-match(holder,view32df$Spec_UNID)
      ind<-ind-2
      view32df<-view32df[ind:dimlast,]
      return(view32df)
    }
    if(input$typefile=='Sample'){
      holder<-newdf[1,2]
      dimlast<-dim(view32df)[1]
      ind<-match(holder,view32df$SampleID)
      ind<-ind-2
      view32df<-view32df[ind:dimlast,]
      return(view32df)
    }
    if(input$typefile=='Storage'){
      holder<-newdf[1,1]
      dimlast<-dim(view32df)[1]
      ind<-match(holder,view32df$StorageID)
      ind<-ind-2
      view32df<-view32df[ind:dimlast,]
      return(view32df)
    }
  })
  #Renders the upload file after mergning
  output$textholder<-renderTable({
    return(tab32table())
  })
  #ObserveEvent that updates db with new info
  observeEvent(input$Updatebutton,{
    tableupdate<-a()
    if(input$typefile=='Isolation'){
      dbWriteTable(con,'Isolation',tableupdate,overwrite=T)
    }
    if(input$typefile=='Donor'){
      dbWriteTable(con,'Donor',tableupdate,overwrite=T)
    }
    if(input$typefile=='Sample'){
      dbWriteTable(con,'Sample',tableupdate,overwrite=T)
    }
    if(input$typefile=='Spec'){
      dbWriteTable(con,'Spec',tableupdate,overwrite=T)
    }
    if(input$typefile=='Storage'){
      dbWriteTable(con,'Storage',tableupdate,overwrite=T)
    }
  })
  #Following code creates the reactive download information for Tab4 used source code from   https://gist.github.com/4211337---------
  # Drop-down selection box for which data set
  output$choose_dataset <- renderUI({
    selectInput("dataset", "Data set", as.list(Listofdt))
  })
  # Render Search dropdown
  output$choose_columns <- renderUI({
    # If missing input, return to avoid error later in function
    if(is.null(input$dataset))
      return()
    
    # Get the data set with the appropriate name
    dat <- get(input$dataset)
    colnames <- names(dat)
    
    # Create the checkboxes and select them all by default
    selectInput("columns", "Choose column to search", 
                choices  = colnames,
    )
    
  })
  output$subsetinfo<-renderUI({
    dat <- get(input$dataset)
    colnames <- names(dat)
    checkboxGroupInput("subcol", "Choose columns", 
                       choices  = colnames,
                       selected = colnames)
  })
  # #This chunck of code handles the user inputing information about the tables for Tab4-----------------
  uinp <- reactive({
    uinpstr<-input$words
    uinpstr<-strsplit(uinpstr,",")
    uinpstr<-unlist(uinpstr)
  })
  downloadtablefiltered<-reactive({
    req(uinp())
    dat <- get(input$dataset)
    #column<-input$columns
    dat <-filter(dat,get(input$columns)%in%uinp())
    dat <- dat[, input$subcol, drop = FALSE]
    dat}
  )
  # Output the data tabel 
  output$downloadTable <- renderDataTable({ downloadtablefiltered()})
  # now lets make the download button actually do something
  output$downloadData <- downloadHandler(
    filename = function() {
      paste(input$dataset, ".csv", sep = "")
    },
    content = function(file) {
      write.csv(downloadtablefiltered(), file, row.names = FALSE)
    }
  )
  
}
shinyApp(ui = ui, server = server)


