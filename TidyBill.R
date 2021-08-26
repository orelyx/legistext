################################################################################
# TidyBill.R:  Translate a bill introduced in the US Senate or House, and avail-
#     able in pdf format at the US government web site 
#     https://www.govinfo.gov/app/details/, into a 'tidy' format with each para-
#     graph of the bill in its own row, accompanied by its full outline tag. (As
#     would be used to reference the paragraph). 
#     Write the 'tidy' bill to a .csv file (tab-separated) from which it can be 
#     imported into a spreadsheet program or other analysis tools. 
#
#     Copyright (c) 2021 Orebed Analytics LLC under MIT License; see LICENSE.txt. 
# 
#     Data files produced by this software are released under Creative Commons 
#     license (see https://creativecommons.org/licenses/by/4.0/legalcode) to the 
#     extent that they are not already in the public domain. 

library(tidyverse)
library(stringi)
library(pdftools)
library(tidytext)
library(tokenizers)
library(cleanNLP)
library(quanteda)
library(glue)

source("./digestBill.R", local = TRUE)

source("./billOutline.R", local = TRUE)

cat("TidyBill.R -- tidy a single bill of US federal legislation\n")
cat("              for later analysis.")
cat("Input: name and filename of bill\n")
cat("Output: .csv (tab-separated) text file showing all paragraphs of the \n")
cat("        bill together with the full paragraph number, token list, \n")
cat("        and list of keywords.\n")
cat("        The output file lends itself to being read into a spreadsheet\n")
cat("        program (MS Excel or equivalent) for formatting and further\n")
cat("        analysis.\n")
cat("Input is read from file tidybill.txt (if it exists) or console.\n\n")

bill1 <- list(name = "", filename = "")

if (file.exists("tidybill.txt")) {
  fromFile <- TRUE
  connex <- file("tidybill.txt", "rt")
} else {
  fromFile <- FALSE
  connex <- stdin()
}

if (!fromFile) {
  cat("Bill name (typically Axxxx or Sxxxx etc.): ")
}
bill1$name <- readLines(connex, 1)
if (!fromFile) {
  cat("Bill filename (a pdf file, typically Axxxx.pdf etc.): ")
}
bill1$filename <- readLines(connex, 1)
stopifnot(file.exists(bill1$filename))

if (fromFile) {
  close(connex)
}

if (!file.exists("tidybill.txt")) {
  connex <- file("tidybill.txt", "wt")
  writeLines(c(bill1$name, bill1$filename),
             connex)
  close(connex)
}

bill1.d <- digestBill(".", bill1$filename, bill1$name) %>%
  group_by(Item) %>%
  summarize(Outline = first(Outline_Tag), 
            Page_Lines = str_c(first(Page_Line), " - ", last(Page_Line)),
            Text = trimws(glue_collapse(Text)),
            Quoted = first(Quoted)) %>%
  mutate(across(Text, ~str_replace_all(., "[[:space:]]+", " "))) %>%
  filter(!is.na(Outline))

bill1.df <- processOutline(bill1.d)

bill.df <- bill1.df %>%
  filter(!Omit) %>%
  mutate(Para = Tag) %>%
  mutate(across(Keywords, ~sapply(., function(x){ 
    y = unlist(x)
    if (length(y) == 0) { "" } else { glue_collapse(y, sep = ", ") } 
  } ))) %>%
  mutate(across(Tokens, ~sapply(., function(x){ 
    y = unlist(x)
    if (length(y) == 0) { "" } else { glue_collapse(y, sep = ", ") } 
  } ))) %>%
  select(Item, Page_Lines, Para, Text, Keywords)
  

write_delim(bill.df, str_c(bill1$name, "-keywords", ".csv"), delim = "\t")

bill.slim.df <- bill.df %>% 
  select(Item, Page_Lines, Para, Text)

write_delim(bill.slim.df, str_c(bill1$name, ".csv"), delim = "\t")


