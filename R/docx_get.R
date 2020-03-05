body_get_chart_rid_title <- function(doc) {
  chart_nodes = xml_find_all(doc$doc_obj$get(), ".//c:chart")
  
  lengthChart_nodes = length(xml_length(chart_nodes))
  for(j in 1:lengthChart_nodes) {
    cell = xml_parents(chart_nodes[j])
    lengthCell = length(xml_length(cell))
    for(i in lengthCell:1) {
      if(xml_name(cell[i])%in%c("tc")||xml_name(cell[i])%in%c("p")) {
        break
      }
    }
    
    # PREVIOUS PARAGRAPH BEFORE THE CHART INSIDE THE TABLE
    if(xml_name(cell[i])%in%c("tc")) {
      for(p in 1:lengthCell) {
        if(xml_name(cell[p])%in%c("p")) {
          break
        }
      }
      
      paragraphs = xml_children(cell[i])
      lengthParagraphs = length(xml_length(paragraphs))
      for(c in 1:lengthParagraphs) {
        if(xml_path(paragraphs[c])==xml_path(cell[p])) {
          break
        }
      }
      
      a = list()
      if(c-1>0) {
        previousParagraph = xml_children(paragraphs[c-1])
        lengthPreviousParagraph = length(xml_length(previousParagraph))
        for(d in 1:lengthPreviousParagraph) {
          if(xml_name(previousParagraph[d])%in%c("r")) {
            AA = xml_children(previousParagraph[d])
            lengthAA = length(xml_length(AA))
            for(e in 1:lengthAA) {
              if(xml_name(AA[e])%in%c("t")) {
                a[[paste0(d,"-",e)]] = xml_text(AA[e])
              }
            }
          }
        }
      }
      
      # PARAGRAPH BEFORE THE TABLE
      for(t in 1:lengthCell) {
        if(xml_name(cell[t])%in%c("tbl")) {
          break
        }
      }
      paragraphs = xml_children(cell[t+1])
      lengthParagraphs = length(xml_length(paragraphs))
      for(c in 1:lengthParagraphs) {
        if(xml_path(paragraphs[c])==xml_path(cell[t])) {
          break
        }
      }
      
      b = list()
      if(c-1>0) {
        previousParagraph = xml_children(paragraphs[c-1])
        lengthPreviousParagraph = length(xml_length(previousParagraph))
        for(d in 1:lengthPreviousParagraph) {
          if(xml_name(previousParagraph[d])%in%c("r")) {
            AA = xml_children(previousParagraph[d])
            lengthAA = length(xml_length(AA))
            for(e in 1:lengthAA) {
              if(xml_name(AA[e])%in%c("t")) {
                b[[paste0(d,"-",e)]] = xml_text(AA[e])
              }
            }
          }
        }
      }
      
      bookmark = paste0(unlist(a)," ||| ",paste0(unlist(b),collapse = ""),collapse = "")
    } else if(xml_name(cell[i])%in%c("p")) {
      paragraphs = xml_children(xml_parent(cell[i]))
      lengthParagraphs = length(xml_length(paragraphs))
      for(c in 1:lengthParagraphs) {
        if(xml_path(paragraphs[c])==xml_path(cell[i])) {
          break
        }
      }
      
      a = list()
      if(c-1>0) {
        previousParagraph = xml_children(paragraphs[c-1])
        lengthPreviousParagraph = length(xml_length(previousParagraph))
        for(d in 1:lengthPreviousParagraph) {
          if(xml_name(previousParagraph[d])%in%c("r")) {
            AA = xml_children(previousParagraph[d])
            lengthAA = length(xml_length(AA))
            for(e in 1:lengthAA) {
              if(xml_name(AA[e])%in%c("t")) {
                a[[paste0(d,"-",e)]] = xml_text(AA[e])
              }
            }
          }
        }
      }
      
      bookmark = paste0(unlist(a),collapse = "")
    }
    
    if(j==1) {
      result = data.table(
        rId = xml_attrs(chart_nodes[j])[[1]]["id"],
        chartPreviousParagraph = bookmark
      )
    } else {
      result = rbind(result,
                     data.table(
                       rId = xml_attrs(chart_nodes[j])[[1]]["id"],
                       chartPreviousParagraph = bookmark
                     )
      )
    }
  }
  result
}