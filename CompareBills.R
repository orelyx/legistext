################################################################################
# CompareBills.R:  Translate two bills introduced in the US Senate or House, and 
#     available in pdf format at the US government web site 
#     https://www.govinfo.gov/app/details/, into a 'tidy' format with each para-
#     graph of each bill in its own row, accompanied by its full outline tag. 
#     Extract keywords from each paragraph of each bill by listing all words and 
#     deleting the words found in a list of common words. Use the keywords to 
#     match each paragraph in the first bill with paragraphs of the second bill 
#     sharing a 'significant' number of keywords. Prepare a table showing the 
#     matched paragraphs and the common keywords between them; output this table 
#     to a .csv file which can be input to a spreadsheet program or other 
#     analysis tool. The constant values in the function areRelated() can be 
#     tweaked to tune the identification of related paragraphs, or this algorithm
#     can be replaced with a better one. 
#
#     The design of this script makes no assumption that the two bills have any 
#     common origin, although it would be quite successful in identifying the 
#     text in common between two such bills (while also spewing a lot of false 
#     positives). The intended function is very different from the text comparison 
#     capabilities of word processors or source code 'diff' utilities, as well as 
#     from software for detecting plagiarism (turnitin). 
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

cat("CompareBills.R -- compare two bills of US federal legislation\n")
cat("Input: name and filename of bills to be compared\n")
cat("Output: .csv (tab-separated) text file showing all paragraphs of the \n")
cat("        first bill together with similar paragraphs of the second.\n")
cat("        The output file lends itself to being read into a spreadsheet\n")
cat("        program (MS Excel or equivalent) for formatting and further\n")
cat("        analysis.\n")
cat("Input is read from file comparebills.txt (if it exists) or console.\n\n")

bill2 <-list(name = "", filename = "")
bill1 <- list(name = "", filename = "")

if (file.exists("comparebills.txt")) {
  connex <- file("comparebills.txt", "rt")
} else {
  connex <- stdin()
}

cat("First bill name (typically Axxx or Sxxx): ")
bill1$name <- readLines(connex, 1)
cat("First bill filename (a pdf file, typically Axxxx.pdf etc.): ")
bill1$filename <- readLines(connex, 1)
stopifnot(file.exists(bill1$filename))

cat("Second bill name (typically Axxx or Sxxx): ")
bill2$name <- readLines(connex, 1)
cat("Second bill filename (a pdf file, typically Axxxx.pdf etc.): ")
bill2$filename <- readLines(connex, 1)
stopifnot(file.exists(bill2$filename))


if (!file.exists("comparebills.txt")) {
  connex <- file("comparebills.txt", "wt")
  writeLines(c(bill1$name, bill1$filename,
               bill2$name, bill2$filename),
             connex)
  close(connex)
} else {
  close(connex)
}


bill1.df <- digestBill(".", bill1$filename, bill1$name) %>%
  group_by(Item) %>%
  summarize(Outline = first(Outline_Tag), 
            Page_Lines = str_c(first(Page_Line), " - ", last(Page_Line)),
            Text = trimws(glue_collapse(Text)),
            Quoted = first(Quoted)) %>%
  mutate(across(Text, ~str_replace_all(., "[[:space:]]+", " "))) %>%
  filter(!is.na(Outline))

bill2.df <- digestBill(".", bill2$filename, bill2$name) %>%
  group_by(Item) %>%
  summarize(Outline = first(Outline_Tag), 
            Page_Lines = str_c(first(Page_Line), " - ", last(Page_Line)),
            Text = trimws(glue_collapse(Text)),
            Quoted = first(Quoted)) %>%
  mutate(across(Text, ~str_replace_all(., "[[:space:]]+", " "))) %>%
  filter(!is.na(Outline))

bill1.df <- processOutline(bill1.df)
bill2.df <- processOutline(bill2.df)

areRelated <- function(keys1, keys2) {
#  rexp <- 1.4
  rexp <- 2.2
#  rexp <- 2.0
  threshold <- min(floor(log2(length(keys1)^(rexp) + length(keys2)^(rexp))), 
                   length(keys1), length(keys2), 4)
  (sum(keys1 %in% keys2) > threshold) &
    (length(keys1) > 0) &
    (length(keys2) > 0)
}

relatedMatrix <- sapply(bill1.df$Keywords, function(x) { sapply(bill2.df$Keywords, function(y) { areRelated(x, y) })})

relatedList <- mapply(function(x) { which(relatedMatrix[,x]) }, seq(1, length(bill1.df$Keywords)))

relatedPairs <- tibble(bill1Item = 0, bill2Item = 0) %>%
  filter(FALSE)

for (i in bill1.df$Item) {
  elt <- relatedList[[i]]
  if (!is.na(relatedList[[i]][1])) {
    for (j in seq(1, length(relatedList[[i]]))) {
      relatedPairs <- relatedPairs %>%
        bind_rows(tibble(bill1Item = i, bill2Item = relatedList[[i]][j]))
    }
  }
}

bill1_bill2 <- bill1.df %>%
  left_join(relatedPairs, by = c("Item" = "bill1Item")) %>%
  left_join(bill2.df, by = c("bill2Item" = "Item")) %>%
  mutate(Common = mapply(function(x, y) 
    { sapply(x, function(z) 
      { if(z %in% y) { z } else { "" } }) }, Keywords.x, Keywords.y)) %>%
  mutate(Common_Keywords = sapply(Common, function(x) 
    { glue_collapse(names(x[which(x != "")]), sep = ", ") } )) %>%
  select(-Common) %>%
  mutate(Common = NA)

for (i in seq(1, length(bill1_bill2$Common))) {
  if (length(bill1_bill2$Common_Keywords[[i]]) > 0) {
    bill1_bill2$Common[i] <- bill1_bill2$Common_Keywords[[i]]
  } else {
    bill1_bill2$Common[i] <- NA
  }
}

bill1_bill2_trimmed <- bill1_bill2 %>%
  select(Tag.x, Text.x, Common, Tag.y, Text.y) %>%
  mutate(Text.prev = lag(Text.x)) %>%
  mutate(Text.x = mapply(function(x, y) { if (is.na(y)) { x } else if (x == y) { "" } else { x }},
                         Text.x, Text.prev)) %>%
  select(-Text.prev) %>%
  mutate_all(~sapply(., function(y) { if (is.na(y)) { "" } else { y }}))
  

colnames(bill1_bill2_trimmed)[which(str_ends(colnames(bill1_bill2_trimmed), fixed(".x")))] <- 
  str_c(bill1$name, " ", str_remove(
    colnames(bill1_bill2_trimmed)[which(str_ends(colnames(bill1_bill2_trimmed), 
                                                 fixed(".x")))], fixed(".x")))

colnames(bill1_bill2_trimmed)[which(str_ends(colnames(bill1_bill2_trimmed), 
                                             fixed(".y")))] <- 
  str_c(bill2$name, " ", str_remove(
    colnames(bill1_bill2_trimmed)[which(str_ends(colnames(bill1_bill2_trimmed), 
                                                 fixed(".y")))], fixed(".y")))

colnames(bill1_bill2_trimmed)[which(str_ends(colnames(bill1_bill2_trimmed), fixed(" Tag")))] <-
  str_replace(colnames(bill1_bill2_trimmed)[which(str_ends(colnames(bill1_bill2_trimmed), 
                                                           fixed(" Tag")))],
              fixed(" Tag"),
              " Outline")

write_delim(bill1_bill2_trimmed, str_c(bill1$name, "_", bill2$name, ".csv"), delim = "\t")

