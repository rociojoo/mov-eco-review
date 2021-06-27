# function to clean words from abstract. Got to have column called abstract with the text.
# depends on stringr, textstem, tidytext and tidyverse libraries 
# and of source function in ./R/ called Americanizing

cleaning_words_abstract <- function(papers_topic, path_dict_tools){ 
  
  # papers_topic : df with abstract column
  # path_dict_tools : path where dictionary auxiliary data are
  
  # Separating the abstracts by word
  df_words <- papers_topic %>% 
    unnest_tokens(output = word, # name of the new column
                  input = abstract, # column that is going to be splitted
                  token = "words", # unit of splitting
                  format = "text") 
  
  # Remove English stopwords 
  df_words <- df_words %>%
    anti_join(stop_words)
  
  
  # Remove other unuseful words
  bad.words <- c("use","using","used","however","based","high",
                 "show","shown","higher","term","can","non","first","two","three",
                 "one","also","may","well","among","elsevier")
  bad.words <- data.frame(word = bad.words, stringsAsFactors = FALSE)
  
  df_words <- df_words %>% 
    anti_join(bad.words)
  
  # Removing Numbers
  # http://stla.github.io/stlapblog/posts/Numextract.html
  indexRemoveNumbers <- which(str_detect(df_words$word, "\\-*\\d+\\.*\\d*") == TRUE)
  if (length(indexRemoveNumbers) > 0){
    df_words <- df_words[-indexRemoveNumbers,]
  }
  # Remove the abbreviations 'eg' and 'ie'
  abbr <- data.frame(word = c("eg","ie","i.e.","e.g.","e.g","i.e"), stringsAsFactors = FALSE)
  df_words <- df_words %>% 
    anti_join(abbr)
  
  # Find the abbreviation 'km' and replace it with 'kilometer'
  indexkm <- which(df_words$word == 'km')
  df_words$word[indexkm] <- 'kilometer'
  
  # Find the abbreviation 'tdrs' and replace it with 'tdr'
  indextdrs <- which(df_words$word == 'tdrs')
  df_words$word[indextdrs] <- 'tdr'
  
  
  # Homogenizing the American and British words
  Am2Br <- read.csv(paste0(path_dict_tools,"Am2Br.csv"), stringsAsFactors = FALSE)
  names(Am2Br) <- c("Am","Br")
  
  df_words_am <- Americanizing(df_words$word, Am2Br)
  # Adding the new words
  df_words$word_am <- df_words_am
  
  
  # LEMMATIZING
  lem <- lemmatize_words(df_words$word_am)
  df_words$word_am_lem <- lem
  
  # A few more corrections by hand
  
  indexbeh <- which(df_words$word_am_lem == 'behavioral' | df_words$word_am_lem == 'behaviorally' | df_words$word_am_lem == 'behave')
  df_words$word_am_lem[indexbeh] <- 'behavior'
  
  indexmov <- which(df_words$word_am_lem == 'move')
  df_words$word_am_lem[indexmov] <- 'movement'
  
  indexdatum <- which(df_words$word_am_lem == 'datum')
  df_words$word_am_lem[indexdatum] <- 'data'
  
  indexdatum <- which(df_words$word_am_lem == 'gp' & df_words$word == "gps")
  df_words$word_am_lem[indexdatum] <- 'gps'
  
  # Removing Numbers again because could have split from words after lemmatizing
  indexRemoveNumbers <- which(str_detect(df_words$word_am_lem, "\\-*\\d+\\.*\\d*") == TRUE)
  if (length(indexRemoveNumbers) > 0){
    df_words <- df_words[-indexRemoveNumbers,]
  }
  
  # Removing words in Spanish (probably from places with Spanish names)
  spanishSW <- data.frame(word_am_lem = stopwords(kind = "sp"), stringsAsFactors = FALSE)
  df_words <- df_words %>% 
    anti_join(spanishSW)
  
  # now take the output
  return(df_words)
}
