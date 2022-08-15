################################################################################
# digestBill.R:  From a bill introduced in the US Senate or House, in pdf format
#                as posted at https://www.govinfo.gov/app/details/, extract the 
#                section numbering and text of the bill, processing only the 
#                text with line numbers. 
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

# "(^[ \t]*(Section|SECTION|SEC.)[ ]+[0-9]{1,9}[A-Z]{0,1}[-–]{0,1}[0-9]{0,3}[A-Z]{0,3}[.]{0,1}[ ]{0,1})"

outlineTagRegex <- 
  str_c("(?:^[ ]*[:punct:]{0,2}(?:Section|SECTION|SEC.)[ ]+[0-9]{1,9}[A-Za-z]{0,3}[-–]{0,1}[0-9]{0,3}[A-Z]{0,3}[.]{0,1}[ ]{0,1})|",
        "(?:^[ ]*[:punct:]{0,2}(?:TITLE|Title)[ ]+(?:[0-9]{1,9}|[IXV]{1,12})(?:-[A-Za-z]{1}){0,1}[.]{0,1}[ ]*)|",
        "(?:^[ ]*[:punct:]{0,2}(?:ARTICLE|Article)[ ]+(?:[0-9]{1,9}|[IXV]{1,12})(?:-[A-Za-z]{1}){0,1}[.]{0,1}[ ]*)|",
        "(?:^[ ]*[:punct:]{0,2}(?:SUBTITLE|Subtitle)[ ]+(?:[0-9]{1,9}|[IXV]{1,12}|[A-Z]{1,3})(?:-[A-Za-z]{1}){0,1}[.]{0,1}[ ]*)|",
        "(?:^[ ]*[:punct:]{0,2}(?:CHAPTER|Chapter)[ ]+(?:[0-9]{1,9}|[IXV]{1,12})(?:-[A-Za-z]{1}){0,1}[.]{0,1}[ ]*)|",
        "(?:^[ ]*[:punct:]{0,2}(?:SUBCHAPTER|Subchapter)[ ]+(?:[0-9]{1,9}|[IXV]{1,12}[A-Z])(?:-[A-Za-z]{1}){0,1}[.]{0,1}[ ]*)|",
        "(?:^[ ]*[:punct:]{0,2}(?:PART|Part)[ ]+(?:[0-9]{1,9}|[IXV]{1,12}[A-Z]{0,1}|[A-Z]{1,2})(?:-[A-Za-z]{1}){0,1}[.]{0,1}[ ]*)|",
        "(?:^[ ]*[:punct:]{0,2}§[ ]{0,1}[0-9-]{1,9}([a-z]{0,1})\\2{0,4}[\\.]{0,1}[ ]*)|",
        "(?:^[ ]*[:punct:]{0,2}[a-z]{1}[\\.]{1}[ ]*)|",                         # 2020-04-20 only lower-case
        "(?:^[ ]*[:punct:]{0,2}[(]{1}[a-zA-Z0-9]{1,2}[)]{1}[ ]*)|",
        "(?:^[ ]*[:punct:]{0,2}([ivxIVX]+[\\.]|[(][ivxIVX]+[)])[ ]*)")

digestBill <- function(path, filename, name) {
  bill <- file.path(path, filename)
  pages <- pdf_text(bill)
  lines <- unlist(sapply(1:length(pages), 
                         function(x) str_extract_all(pages[x], "[^\n]+")))
 
  # Guessing that placement of these items will be consistent from one bill to another
  titleInfo <- tibble(Senate_Bill = str_extract(lines[4], "S. [0-9]{1,4}"),
                      House_Bill = str_extract(lines[4], "H. RES. [0-9]{1,4}"),
                      Congress = str_extract(lines[2], "[0-9]{3}TH CONGRESS"),
                      Session = str_extract(lines[3], "[A-Za-z0-9]+[ ]+SESSION"),
                      Date = str_extract(lines[11], "[A-Z]+ [0-9]+, [0-9]{4}"))
  
  text <- sapply(lines, function(x) str_extract(x, "^[ ]{0,80}([0-9]+)[ ]+(.*)$"))
  names(text) <- NULL
  textMatrix <- str_match(text[which(!is.na(text))], "^[ ]{0,80}([0-9]+)[ ]+(.*)$")
  textTibble <- tibble(Line_Numbers = textMatrix[,2], Text = textMatrix[,3]) %>%
    mutate(Page = cumsum(Line_Numbers == "1")) %>%
    select(Page, everything()) %>%
    mutate(Outline_Tag = str_extract(Text, outlineTagRegex)) %>%
    
    mutate(across(Text, ~str_remove(., outlineTagRegex))) %>%
    mutate(across(Text, 
                  ~str_replace_all(., c("$" = " ", "- $" = "", "^[ ]*" = "",
                                        "[\t]+" = " ", "[ ]{2,}" = " ",
                                        "—" = " ")))) %>%

    # mutate_at("Text", function(x) str_remove(x, outlineTagRegex)) %>%
    # mutate_at("Text", function(x) str_replace(x, "$", " ")) %>%
    # mutate_at("Text", function(x) str_replace(x, "- $", "")) %>%
    # mutate_at("Text", function(x) str_replace(x, "^[ ]*", "")) %>%
    # mutate_at("Text", function(x) str_replace(x, "[\t]+", " ")) %>%
    # mutate_at("Text", function(x) str_replace(x, "[ ]{2,}", " ")) %>%
    mutate(Quoted = sapply(Outline_Tag, function(x) { 
      quoted <- str_detect(x, "^[^\\(A-Za-z0-9]+")
      if (is.na(quoted)) { FALSE } else { quoted }
    })) %>%
    mutate_at("Outline_Tag", function(x) (str_replace(x, "[ ]+$", " "))) %>%
    mutate_at("Outline_Tag", function(x) (str_replace(x, "^[^\\(A-Za-z0-9§]*", ""))) %>%
    mutate(Item = cumsum(!is.na(Outline_Tag))) %>%
    mutate(Page_Line = str_c(as.character(Page), ".", as.character(Line_Numbers))) %>%
    select(Item, Page_Line, Outline_Tag, everything())
  textTibble
}
# thisBill.df <- digestBill(".", "a9856.pdf", "A9856")
