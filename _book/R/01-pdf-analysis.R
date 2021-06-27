########################################
# PDF analysis and methods extraction
#######################################

# trying to follow the guidelines from https://medium.com/@CharlesBordet/how-to-extract-and-clean-data-from-pdf-files-in-r-da11964e252e

#path.fulltext.raw <- './fulltext/raw/': path to the downloaded papers
#path.fulltext<- './fulltext/': path to the directory where we'll save the methods and fulltext files

pdf_analysis <- function(path.fulltext.raw, path.fulltext){
  files <- dir(path=path.fulltext.raw,pattern='.pdf')
# If we have already extracted some methods sections, let's do this
# only for the ones that need the extraction
files_extracted <- dir(path = path.fulltext, pattern = "_methods.RData")
names_extracted <- unlist(lapply(strsplit(files_extracted, "_methods"), 
                                 `[[`, 1))
names_to_extract <- unlist(lapply(strsplit(files, ".pdf"), `[[`, 1))
files <- files[which(!names_to_extract %in% names_extracted)]  # new list of files to extract methods from

library(tm)
library(stringr)
library(beepr) # I only use beepr because I think it's cool

### we are doing it specifically for each journal format
journals_group_1 <- c("ajp","aqc","ece","ecm","ecs","ecy","jwmg","rra","wsb",
                      "zoo","s","alr","others-group1","hukin")
nick_group_1 <- c("10_1071","10_1080","10_1086","10_1089","10_1111","10_1121","10_1134","10_1177","10_1260",
                  "10_1515","10_15376","10_1590","10_1655","10_1899","10_2108","10_2478","10_3354","10_3758",
                  "10_3996","10_4098")
journals_group_2 <- c("eap")

journals_group_3 <- c("nature","ncomms","srep","others-group3")
nick_group_3 <- c("10_1038","10_1126_sciadv")

journals_group_4 <- c("pnas","bmc")
nick_group_4 <- c("10_1186")

journals_group_5 <- c("LSP")

journals_group_6 <- c("MEE")
nick_group_6 <- c("10_1111_2041")

journals_group_7 <- c("science") # no methods section
nick_group_7 <- c("10_1126_science")

sink(file=paste0(path.fulltext,'pdfs_no_extracted_method.txt'))
cat("")
sink()


for (i in 1:length(files)){ 
  # my computer crashed when I did very long lists, so I had to change
  # this manually (i)
  print(i)
  read<-readPDF(engine="xpdf",control=list(text="-layout"))
  # suppressWarnings(Corpus)
  document <- Corpus(URISource(paste0(path.fulltext.raw,files[i])), readerControl = list(reader = read))
  doc <- content(document[[1]])
  set_name <- substr(files[i],9,nchar(files[i])-5) # taking out a possible last letter 
  set_name <- str_replace_all(gsub('[0-9]+', '', set_name),"[[:punct:]]", '')
  nat_name <- substr(files[i],1,7)
  
  # more 'accommodating group names based on file names'
  if (nat_name %in% nick_group_3){
    set_name <- "ncomms"
  }else if(nat_name %in% nick_group_1){
    set_name <- "others-group1"
  }else if(nat_name %in% nick_group_4){
    set_name <- "bmc"
  }
  nat_name_2 <- substr(files[i],1,12)
  if(nat_name_2 %in% nick_group_6){
    set_name <- "MEE"
  }
  nat_name_3 <- substr(files[i],1,14)
  if(nat_name_3 %in% nick_group_3){
    set_name <- "others-group3"
  }
  nat_name_4 <- substr(files[i],1,8)
  if(nat_name_4 %in% nick_group_1){
    set_name <- "others-group1"
  }
  if (set_name %in% journals_group_1 || set_name %in% journals_group_2 || 
      set_name %in% journals_group_3 || set_name %in% journals_group_4 ||
      set_name %in% journals_group_5 || set_name %in% journals_group_6){
    # basically, everything but science-like format
    head(doc)
    page_breaks <- grep("\\f", doc)
    doc[page_breaks[1]]
    # 1. Clean the headers and footers on all pages. # It seems that this is something I can't do automatically
    # Remove footer on first page
    footer_row_2 <- grep("\\f", doc)[1] - 1
    doc <- doc[- footer_row_2]
    # Remove headers on other pages
    header_rows <- grep("^\\f", doc) # Remember: \f are for page breaks
    doc[header_rows] <- "page" # I put a marker here that will be useful later
    doc <- doc[- (header_rows - 1)]
    # 2. Get the two columns together.
    doc_split <- strsplit(doc, "   ") # Split each row whene there are 2 spaces
    doc_split <- lapply(doc_split, function(x) {
      # For each element, extract:
      #    - doc1 that is the first column. 
      #    - doc2 that is the second column.
      line_length <- length(x)
      half <- round(line_length/2)
      doc1 <- x[1:half][x[1:half] != ""][1] # The first piece of text that's not empty
      if (is.na(doc1)) doc1 <- ""
      # doc2 takes the next non-empty piece of text
      doc2 <- x[x != ""] 
      if (doc1 != "") doc2 <- doc2[-1]
      if (length(doc2) == 0) doc2 <- ""
      # Clean it before returning it
      doc2 <- paste(doc2, collapse = " ")
      doc1 <- str_trim(doc1) # stringr::str_trim trim the spaces before/after
      doc2 <- str_trim(doc2)
      list(doc1 = doc1, doc2 = doc2)
    })
    doc1 <- sapply(doc_split, `[[`, 1) # First column
    doc2 <- sapply(doc_split, `[[`, 2) # Second column
    # write.table(doc1,"test1.txt")
    # write.table(doc2,"test2.txt") # I was saving these things to check visually if everything was OK
   
    # Vector of the page breaks coordinates:
    # finding where to make the first split (in the first page)
    pages_spacebreaks <- which(doc1 == "")
    list_breaks <- split(pages_spacebreaks, cumsum(c(1, diff(pages_spacebreaks) != 1)))
    if (set_name %in% journals_group_1 || set_name %in% journals_group_6){
      break_1 <- which(lengths(list_breaks) > 4)[1]
    }else if(set_name %in% journals_group_2){
      break_1 <- which(lengths(list_breaks) > 4)[2]
    }else if(set_name %in% journals_group_4 || set_name %in% journals_group_5){
      # I can't remember why, but this group has a weird format
      # First, find Discussion section
      dis <- grep("Discussion",doc1,ignore.case=FALSE)[1]
      if (is.na(dis)){
        dis <- grep("Discussion",doc2,ignore.case=FALSE)[1]
        if (is.na(dis)){
          dis <- grep("Conclusion",doc2,ignore.case=FALSE)[1]
        }
      }
      if (!is.na(dis)){ # I'll be very conservative here
        ref.1 <- grep("1\\.",doc1[dis:length(doc1)])
        ref.2 <- grep("1\\.",doc2[dis:length(doc2)])
        refs <- c(ref.1,ref.2) + dis - 1
        ok <- 0
        counter <- 1
        while(ok == 0 &&  counter <= length(refs)){
          if (all(c(refs[counter]-1,refs[counter]-2) %in% pages_spacebreaks)){
            ok <- 1
          }  
          counter <- counter + 1
        }
        if (ok == 1){
          break_end <- refs[counter-1]-1
        }
      }
    }
    if (exists("break_1") == TRUE){
    doc1[list_breaks[break_1][[1]][length(list_breaks[break_1][[1]])-1]] <- "page" # check if this rule is always kept (seems so)
    }
    if (exists("break_end") == TRUE){
      doc1[break_end] <- "page"
    }
    
    pages_rows <- c(0, which(doc1 == "page"), length(doc1))
    doc <- c()
    # Page by page, we fill a new vector:
    for (j in 1:(length(pages_rows) - 1)) {
      doc <- c(doc, c(doc1[(pages_rows[j] + 1):pages_rows[j + 1]],
                      doc2[(pages_rows[j] + 1):pages_rows[j + 1]]))
    }
    full_text_vector <- doc[doc != "page"]
    # write.table(full_text_vector,"test3.txt")
    save(full_text_vector,file=paste0(path.fulltext,unlist(strsplit(files[i],'[.]'))[1],'_fulltext.RData'))
    # finally the body text is ready!!!
    
    
    full_text_collapse <- sapply(1:length(full_text_vector),function(x){
      line_text <- gsub("[[:space:]]", "", full_text_vector[x])
    })
    
    # trying to find the first line of the methods section, and that depends on the group
    if (set_name %in% journals_group_6){
      Methods <- grep("Materialsandmethods",full_text_collapse,ignore.case = TRUE)[1]
      if (is.na(Methods)){
        Methods <- grep("andmethods",full_text_collapse,ignore.case = FALSE)[1]
        if (is.na(Methods)){
          Methods <- grep("Methodology",full_text_collapse,ignore.case = FALSE)[1]
          if (is.na(Methods)){
            Methods <- grep("StatisticalAnalys",full_text_collapse,ignore.case = FALSE)[1]
          }
        }
      }
    }else if(set_name %in% journals_group_3 || set_name %in% journals_group_4){
      Methods <- grep("Materialsandmethods",full_text_collapse,ignore.case = TRUE)[1]
      if (is.na(Methods)){
        Methods <- grep("METHODS",full_text_collapse,ignore.case = FALSE)[1]
        if (is.na(Methods)){
          Methods <- grep("METHOD",full_text_collapse,ignore.case = FALSE)[1]
          if (is.na(Methods)){
            Methods <- grep("Methods",full_text_collapse,ignore.case = FALSE)[1]
            if (is.na(Methods)){
              Methods <- grep("StatisticalAnalys",full_text_collapse,ignore.case = FALSE)[1]
              if (is.na(Methods)){
                Methods <- grep("andmethods",full_text_collapse,ignore.case = FALSE)[1]
                if (is.na(Methods)){
                  Methods <- grep("andmethod",full_text_collapse,ignore.case = FALSE)[1]
                  if (is.na(Methods)){
                    Methods <- grep("Methodology",full_text_collapse,ignore.case = FALSE)[1]
                  }
                }
              }
            }
          }
        }
      }
    }else{
      Methods <- grep("Materialsandmethods",full_text_collapse,ignore.case = TRUE)[1]
      if (is.na(Methods)){
        Methods <- grep("METHODS",full_text_collapse,ignore.case = FALSE)[1]
        if (is.na(Methods)){
          Methods <- grep("METHOD",full_text_collapse,ignore.case = FALSE)[1]
          if (is.na(Methods)){
            Methods <- grep("Methods",full_text_collapse,ignore.case = FALSE)[1]
            if (is.na(Methods)){
              Methods <- grep("StatisticalAnalys",full_text_collapse,ignore.case = FALSE)[1]
              if (is.na(Methods)){
                Methods <- grep("andmethods",full_text_collapse,ignore.case = FALSE)[1]
                if (is.na(Methods)){
                  Methods <- grep("methods",full_text_collapse,ignore.case = FALSE)[1]
                  if (is.na(Methods)){
                    Methods <- grep("Method",full_text_collapse,ignore.case = FALSE)[1]
                    if (is.na(Methods)){
                      Methods <- grep("andmethod",full_text_collapse,ignore.case = FALSE)[1]
                      if (is.na(Methods)){
                        Methods <- grep("Methodology",full_text_collapse,ignore.case = FALSE)[1]
                        if (is.na(Methods)){
                          Methods <- grep("Model",full_text_collapse,ignore.case = FALSE)[1]
                        }
                      }
                    }
                  }
                }
              }
            }
          }
        }
      }
    }
    if (is.na(Methods)){
      print(paste0(i," : non Methods section"))
      sink(paste0(path.fulltext,'pdfs_no_extracted_method.txt'),append=TRUE)
      print(paste0(files[i],"; ","No methods"))
      sink()
    }
    # now trying to find the last line
    Results <- grep("RESULTS",full_text_collapse,ignore.case = FALSE)[1]
      if (is.na(Results)){
        Results <- grep("Results",full_text_collapse,ignore.case = FALSE)[1]
        if (is.na(Results)){
          Results <- grep("APPLICATION",full_text_collapse,ignore.case = FALSE)[1]
          if (is.na(Results)){
            Results <- grep("Application",full_text_collapse,ignore.case = FALSE)[1]
            if (is.na(Results)){
              Results <- grep("Experimentation",full_text_collapse,ignore.case = FALSE)[1]
              if (is.na(Results)){
                Results <- grep("andresults",full_text_collapse,ignore.case = FALSE)[1]
                if (is.na(Results)){
                  Results <- grep("Discussion",full_text_collapse,ignore.case = FALSE)[1]
                  if (is.na(Results)){
                    Results <- grep("Acknowledgments",full_text_collapse,ignore.case = TRUE)[1]
                    if (is.na(Results)){
                      Results <- grep("References",full_text_collapse,ignore.case = FALSE)[1]
                      if (is.na(Results)){
                        Results <- grep("REFERENCES",full_text_collapse,ignore.case = FALSE)[1]
                        if (is.na(Results)){
                          Results <- grep("Literaturecited",full_text_collapse,ignore.case = TRUE)[1]
                        }
                      }
                    }
                  }
                }
              }
            }
          }
        }
      }
    if (is.na(Results) || Results < max(0,Methods,na.rm=TRUE)){
      if(set_name %in% journals_group_3){
        Results <- grep("References",full_text_collapse,ignore.case = TRUE)[1]
        if (is.na(Results) || Results < max(0,Methods,na.rm=TRUE)){
          Results <- grep("Received",full_text_collapse,ignore.case = FALSE)[1]
          if (is.na(Results) || Results < max(0,Methods,na.rm=TRUE)){ # to avoid errors when Methods is NA
            Results <- grep("RESULTS",full_text_collapse,ignore.case = FALSE)[1]
            if (is.na(Results) || Results < max(0,Methods,na.rm=TRUE)){
              Results <- grep("Results",full_text_collapse,ignore.case = FALSE)[1]
            }
          }
        }
      }else if(set_name %in% journals_group_4){
        Results <- grep("ACKNOWLEDGMENTS",full_text_collapse,ignore.case = TRUE)[1]
        if (is.na(Results)){
          Results <- grep("ACKNOWLEDGEMENTS",full_text_collapse,ignore.case = TRUE)[1]
        }
      }
    }

    if (is.na(Results)){
      print(paste0(i," : non Results section"))
      sink(paste0(path.fulltext,'pdfs_no_extracted_method.txt'),append=TRUE)
      print(paste0(files[i],"; ","No results"))
      sink()
    }
    if (!is.na(Methods) && !is.na(Results) && Methods < Results){ # if there is a first and last line of this section
      methods_vector <- full_text_vector[Methods:(Results-1)]
      save(methods_vector,file=paste0(path.fulltext,unlist(strsplit(files[i],'[.]'))[1],'_methods.RData'))
    }
    
  }else if(set_name %in% journals_group_7){
    cat('Not dealing with Science papers now since \n',
          '1) it comes with bits of other papers, \n', 
          '2) there is no method section and \n',
          '3) it has 3 columns \n')
  }else if(nat_name == "10_7755"){
    cat('Not dealing with NOAAs Fishery Bulletin because
         it has 3 columns and it is a very small journal \n')
  }
    
}
rm(doc_split)
rm(break_1,break_end)
beep(6)
}
