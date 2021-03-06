require(quanteda)

# https://github.com/quanteda/quanteda/issues/48
# https://github.com/quanteda/quanteda/issues/1321

trivial_3_texts <- c("a aa aaa", "b bb bbb", "d dd ddd dddd")

extremely_minimal_overview <- function() {

  require(quanteda)
  # corpus with 3 texts
  qc <- corpus(trivial_3_texts)
  
  #dfm
  dfm_ret <- dfm(qc
                 ,ngrams = 3
                 ,remove_numbers = T
                 ,remove_punct = T
                 ,remove_symbols = T
                 ,remove_separators = T
                 ,remove_twitter = T
                 ,remove_hyphens = FALSE
                 ,remove_url = FALSE,
  )

  # get 3rd feature NON sicuro è modo giusto
  featnames(dfm_ret)[3]
  
  freq <- textstat_frequency(dfm_ret)
}

extremely_minimal_overview()

experiment_01 <- function() {

  for (n in 1:2) {

    tk <- tokens("a b 1 2 3 a b 2 3 4 a b 3 4 5", ngrams = n)
    print(paste(rep("-",50), collapse = ""))

    for ( i in 1:5) {
      t <- tokens_select(tk, c("a"), selection = "keep",
        valuetype = "regex" , window = c(0,i))
      print(paste(paste0("ngr=",n," w=",i),paste(t, collapse = " ")))
    }
  }
}






sample_code <- function() {

  require(quanteda)

  print(paste("based on","https://github.com/quanteda/quanteda/issues/1413#issuecomment-414795832"))
  print("great package great support, thanks")

  ngms <- tokens("a b 1 2 3 a b 2 3 4 a b 3 4 5", n = 2:5)
  
  # get rid of metadata tokens not necessary for specific tasks
  ngms_lst <-  as.list(ngms)
  ngms_unlst  <- unlist(ngms_lst) # (named) character with _ sep. ngrams
  
  # split in " " separatedpair;  "n-1 tokens", nth token
  ngms_blank_sep <- stringi::stri_replace_last_fixed(ngms_unlst,"_", " ")

  # list of character(2)  ( (n-1)gram ,nth token )
  tk2_lst <- tokens(ngms_blank_sep)
  
  # --- end of tokens/ngrams pre-processing
  
  # ordinary fcm
  fcm_ord <- fcm(tk2_lst , ordered = TRUE)
  
  fcm_ord[33:39, 1:6]
}


# sample_code()


