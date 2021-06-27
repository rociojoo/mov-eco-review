########################################
# XML analysis and methods extraction
#######################################
# Using xml2
#path.fulltext.raw <- './fulltext/raw/': path to the downloaded papers
#path.fulltext<- './fulltext/': path to the directory where we'll save the methods and fulltext files
xml_analysis <- function(path.fulltext.raw, path.fulltext){
  library(xml2)
files <- dir(path=path.fulltext.raw,pattern='.xml')

# If we have already extracted some methods sections, let's do this
# only for the ones that need the extraction
files_extracted <- dir(path = path.fulltext, pattern = "_methods.RData")
names_extracted <- unlist(lapply(strsplit(files_extracted, "_methods"), 
                                 `[[`, 1))
names_to_extract <- unlist(lapply(strsplit(files, ".xml"), `[[`, 1))
files <- files[which(!names_to_extract %in% names_extracted)]  # new list of files to extract methods from

# journals have different structures in their .xml files, so I divided
# them in two groups
journals_group_1 <- c("j_amc", "j_anbehav", "j_biocon", "j_cub", "j_evolhumbehav", 
                      "j_jembe", "j_jenvman", "j_jtbi", "j_physa", "j_prevetmed", "j_rama",'anbe')
journals_group_2 <- c("zeb", "cjfas", "j_cjz", "journal_pbio", "F", "journal_pcbi", 
                      "journal_pone", "AUK", "wlb", "JRR", "bg", "peerj",'cjz')
# journals_group_2b <- c('F')
journals_group_1_edit <- str_replace_all(gsub("[0-9]+", "", journals_group_1), 
                                         "[[:punct:]]", "")
journals_group_2_edit <- str_replace_all(gsub("[0-9]+", "", journals_group_2), 
                                         "[[:punct:]]", "")
for (i in seq_len(length(files))) {
  # my computer crashed when I did very long lists, so I had to change
  # this manually (i)
  print(i)
  mot.cle <- "method"
  x <- read_xml(paste0(path.fulltext.raw, files[i]))
  
  # first, to extract the body text
  
  if (substr(files[i], 1, 3) == "10_") {
    set_name <- substr(files[i], 9, nchar(files[i]) - 4)
    set_name <- str_replace_all(gsub("[0-9]+", "", set_name), "[[:punct:]]", 
                                "")
    if (set_name %in% journals_group_1_edit) {
      full_data <- xml_children(x)[length(xml_children(x))]
      full_text <- xml_children(xml_children(xml_children(xml_children(full_data)[1])[2]))[4]
      full_text_sections <- xml_children(xml_children(full_text)[1])
    } else if (set_name %in% journals_group_2_edit || nchar(set_name) == 
               0) {
      full_data <- xml_children(x)[xml_name(xml_children(x)) == "body"]
      full_text_sections <- xml_children(full_data)
      
    }
  } else {
    full_data <- xml_children(x)[length(xml_children(x))]
    full_text_sections <- xml_children(xml_children(full_data)[2])
  }
  if(length(full_text_sections)==0||is.na(full_text_sections)  ) { next}
  full_text_vector <- xml_text(full_text_sections)
  save(full_text_vector, file = paste0(path.fulltext, unlist(strsplit(files[i], 
                                                                      "[.]"))[1], "_fulltext.RData"))
  
  # then, to extract from the body text, the methods section
  
  ind.methods <- 0
  met <- 1
  while (ind.methods == 0 & met <= length(full_text_sections)) {
    methods_xml <- xml_children(full_text_sections[met])  # ok, file 10_1016_j_amc_2016_02_032.xml does not use the classic structure, so I'm taking part 3
    methods_vector <- xml_text(methods_xml)
    title_sec <- methods_vector[1]
    if (length(grep(mot.cle, title_sec, ignore.case = TRUE)) > 0 && 
        nchar(title_sec) < 50) {
      ind.methods <- 1
      print(title_sec)
      save(methods_vector, file = paste0(path.fulltext, unlist(strsplit(files[i], 
                                                                        "[.]"))[1], "_methods.RData"))
    } else {
      title_sec <- methods_vector[2]
      if (length(grep(mot.cle, title_sec, ignore.case = TRUE)) > 
          0 && nchar(title_sec) < 50) {
        ind.methods <- 1
        print(title_sec)
        save(methods_vector, file = paste0(path.fulltext, unlist(strsplit(files[i], 
                                                                          "[.]"))[1], "_methods.RData"))
      } else {
        met <- met + 1
        if (met > length(full_text_sections) && mot.cle == "method") {
          met <- 2
          mot.cle <- "model"
        } else if (met > length(full_text_sections) && mot.cle == 
                   "model") {
          met <- 2
          mot.cle <- "case study"
        } else if (met > length(full_text_sections) && mot.cle == 
                   "case study") {
          met <- 2
          mot.cle <- "data"
        }
      }
      
    }
  }
  
  suppressWarnings(rm('full_text_sections','full_data','full_text'))
}
}
