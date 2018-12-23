## Vivek Suhag(11810007) and Ratna Prashanth Kumar(11810113)

shinyServer( function(input, output) {
  
  ## Dataset input File
  Dataset <- reactive({
    if (is.null(input$data_file)) {   # locate 'data_file' from ui.R
      return(NULL) } else{
        
        inp_data = readLines(input$data_file$datapath)
        clean_data = str_replace_all(inp_data, "<.*?>", "") # get rid of html junk
        return(clean_data)
      }
  })
  
  ## Annotate Data using Uploaded model file
  Dataset_an <- reactive({
    if (is.null(input$ud_model)) {  
      return(NULL) } else{
        load_model = udpipe_load_model(input$ud_model$datapath)  # ud_model uploaded file
        
        # now annotate text dataset using ud_model above
        x <- udpipe_annotate(load_model, x = Dataset())
        x <- as.data.frame(x)
        return(x[,-4]) ## remove "sentence" column as it is repetitive and increases the size.
      }
  })
  
  ## Annotation Status
  output$ann_status <- renderInfoBox({
    status <- if (is.null(Dataset_an())) {
      "Incomplete"} else {"Done"}
    infoBox(
      "Annotation Status", status, icon = if (status == "Incomplete") icon("exclamation-triangle") else icon("thumbs-up"),
      color = if (status == "Incomplete") "yellow" else "green"
    )
  })
  
  ## Total Documents    
  output$total_docs <- renderValueBox({
    docs <- length(Dataset())
    valueBox(
      value = formatC(docs, digits = 1, format = "d"),
      subtitle = "Number of Documents",
      icon = icon("tasks"),
      color = "blue"
    )
  })
  
  ## Total Sentences    
  output$total_sent <- renderValueBox({
    sent <- Dataset_an() %>% group_by(doc_id) %>% filter(sentence_id == max(sentence_id)) %>% select(doc_id,sentence_id) %>% unique()
    sum_sent <- sum(sent$sentence_id)
    
    valueBox(
      value = formatC(sum_sent, digits = 1, format = "d"),
      subtitle = "Number of Sentences",
      icon = icon("list"),
      color = "blue"
    )
  })

  ## Total Records    
  output$total_lines <- renderValueBox({
    lines <- nrow(Dataset_an())
    valueBox(
      value = formatC(lines, digits = 1, format = "d"),
      subtitle = "# of Records in Annotated matrix",
      icon = icon("th"),
      color = "blue"
    )
  })

  ## Data table; 50 Records
  output$data_annotate = renderDataTable({
    out = head(Dataset_an(), 50)
    out
  })
  
  ## Download Data
  output$data_ann_download <- downloadHandler(
    filename = function(){paste('annotated_data_', Sys.Date(), '.csv', sep='')},
    content = function(file){write.csv(Dataset_an(),file,row.names = FALSE)},
    contentType = "text/csv"
  )  
  
  ## Co-occurrence Plot: Sentence    
  output$cooc_sn_plot = renderPlot({ 
    cooc_sn <- udpipe::cooccurrence( 
      x = subset(Dataset_an(), upos %in% input$pos_tags), 
      term = "lemma", 
      group = c("doc_id", "paragraph_id", "sentence_id"))  # group by document, paragraph and sentence
    
    wordnetwork <- head(cooc_sn, 50)
    wordnetwork <- igraph::graph_from_data_frame(wordnetwork) 
    
    ggraph(wordnetwork, layout = "fr") +  
      geom_edge_link(aes(width = cooc, edge_alpha = cooc), edge_colour = "orange") +  
      geom_node_text(aes(label = name), col = "darkgreen", size = 4) +
      theme_graph(base_family = "Georgia") +  
      theme(legend.position = "none") +
      labs(title = "Sentence based Co-occurrences", subtitle = "Select the Part-of-Speech Tags (checkboxes) from Right panel")
  })

  ## Co-occurrence Plot: non-Sentence    
  output$cooc_nsn_plot = renderPlot({ 
    cooc_nsn <- udpipe::cooccurrence( 
      x = Dataset_an()$lemma, relevant = Dataset_an()$upos %in% input$pos_tags)
    
    nsn_wordnetwork <- head(cooc_nsn, 50)
    nsn_wordnetwork <- igraph::graph_from_data_frame(nsn_wordnetwork) 
    
    ggraph(nsn_wordnetwork, layout = "fr") +  
      geom_edge_link(aes(width = cooc, edge_alpha = cooc), edge_colour = "orange") +  
      geom_node_text(aes(label = name), col = "darkgreen", size = 4) +
      theme_graph(base_family = "Georgia") +  
      theme(legend.position = "none") +
      labs(title = "General (non-sentence based) Co-occurrences", subtitle = "Select the Part-of-Speech Tags (checkboxes) from Right panel")
  })
  
  ## Co-occurrence Plot: skip    
  output$cooc_skp_plot = renderPlot({ 
    cooc_skp <- udpipe::cooccurrence( 
      x = Dataset_an()$lemma, relevant = Dataset_an()$upos %in% input$pos_tags,
      skipgram = input$skipgram)
    
    skp_wordnetwork <- head(cooc_skp, 50)
    skp_wordnetwork <- igraph::graph_from_data_frame(skp_wordnetwork) 
    
    ggraph(skp_wordnetwork, layout = "fr") +  
      geom_edge_link(aes(width = cooc, edge_alpha = cooc), edge_colour = "orange") +  
      geom_node_text(aes(label = name), col = "darkgreen", size = 4) +
      theme_graph(base_family = "Georgia") +  
      theme(legend.position = "none") +
      labs(title = "Cooccurrences within (n-1) words distance", subtitle = "Select the Part-of-Speech Tags (checkboxes) from Right panel")
  })
  
  ## Cloud: Top terms of the selected choice type
  top_terms <- reactive({ 
    all_words = Dataset_an() %>% subset(., upos %in% input$term_choice) 
    top_words = txt_freq(all_words$lemma)  # txt_freq() provides word freqs in desc order
    return(top_words)	
  })
  
  ## Cloud: Term table
  output$term_freq_data = renderDataTable({
    out = head(top_terms(),15)
    out
  })
  
  ## word cloud
  output$wc_plot = renderPlot({
    wordcloud(top_terms()$key, scale = c(8,1), top_terms()$freq, min.freq = input$wc_freq, max.words = input$wc_max, colors = 1:10, 
              random.order = FALSE)
  })
  
  
})