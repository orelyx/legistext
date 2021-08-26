################################################################################
# billOutline.R: Parse the data frame representation of a bill prepared by 
#                digestBill.R to extract a full outline tag for each paragraph 
#                by which the paragraph can be referenced. The algorithm used 
#                attempts through cleverness to get around the lack of a formal 
#                specification of bill formats; it succeeds on S2085 (2021) but 
#                gets fooled in a couple of places in S685 (2021) so there's 
#                room for improvement as well as testing over a wider variety 
#                of legislation. 
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


outlineTags <- 
      c("(^[ \t]*TITLE[ ]+([0-9]{1,9}|[IXV]{1,12})(-[A-Za-z]{1}){0,1}[.]{0,1}[ ]*)",      # 1
        "(^[ \t]*ARTICLE[ ]+([0-9]{1,9}|[IXV]{1,12})(-[A-Za-z]{1}){0,1}[.]{0,1}[ ]*)",    # 1
        "(^[ \t]*SUBTITLE[ ]+([0-9]{1,9}|[IXV]{1,12})(-[A-Za-z]{1}){0,1}[.]{0,1}[ ]*)",   # 1
        "(^[ \t]*CHAPTER[ ]+([0-9]{1,9}|[IXV]{1,12})(-[A-Za-z]{1}){0,1}[.]{0,1}[ ]*)",    # 1
        "(^[ \t]*(SUBCHAPTER|Subchapter)[ ]+([0-9]{1,9}|[IXV]{1,12})(-[A-Za-z]{1}){0,1}[.]{0,1}[ ]*)", # 1
        "(^[ \t]*PART[ ]+([0-9]{1,9}|[IXV]{1,12})(-[A-Za-z]{1}){0,1}[.]{0,1}[ ]*)",       # 1
        "(^[ \t]*(Section|SECTION|SEC.)[ ]+[0-9-]{1,9}[.]{0,1}[ ]{0,1})",                 # 2
        "(^[ \t]*§[ ]{0,1}[0-9-]{1,9}[a-z]{0,1}[.]{0,1}[ ]*)",                            # 2
        "(^[ \t]*[(]{1}[a-z]{1}[)]{1}[ ]*)",                                              # 3
        "(^[ \t]*[(]{1}[0-9]{1,3}[)]{1}[ ]*)",                                            # 4
        "(^[ \t]*[(]{1}[A-Z]{1}[)]{1}[ ]*)",                                              # 5
        "(^[ \t]*[(][ivx]+[)][ ]*)",                                                      # 6
        "(^[ \t]*[(][IVX]+[)][ ]*)",                                                      # 7
        "(^[ \t]*[(]{1}[a-z]{2}[)]{1}[ ]*)",                                              # 8
        "(^[ \t]*[(]{1}[A-Z]{2}[)]{1}[ ]*)"                                               # 9
      )

matchIndices <- c("TITLE", "ARTICLE", "SUBTITLE", "CHAPTER", "SUBCHAPTER", "PART", 
                "Section09", "§09", "(az)", "(09)", "(AZ)", "(ivx)", "(IVX)",
                "(az2)", "(AZ2)")
# Will have to refer to these using backquotes ``
for (m in matchIndices) {
  assign(m, first(which(matchIndices == m)))
}

tagIndices <- c("BIG_PART", "SECTION", "p_az", "p_09", "p_AZ", "p_ivx", "p_IVX", "p_az2", "p_AZ2")
for (n in tagIndices) {
  assign(n, first(which(tagIndices == n)))
}
tagLevels <- c(BIG_PART, BIG_PART, BIG_PART, BIG_PART, BIG_PART, BIG_PART, 
               SECTION, SECTION, p_az, p_09, p_AZ, p_ivx, p_IVX, p_az2, p_AZ2)
firstTag <- function(d) {   # look up the first tag value at a given depth
  c("", "", "(a)", "(1)", "(A)", "(i)", "(I)", "(aa)", "(AA)")[d]
}

dropWords <- read_delim("first1000.csv", delim = ",", escape_double = FALSE, trim_ws = TRUE) %>%
  filter(is.na(Keep)) %>%
  select(Word) %>%
  unlist()

last_char <- function(s) {
  if (length(s) != 1) {
    warning("last_char() called with non-string argument.")
  } else if (is.na(str_length(s))) {
    warning("last_char() called with non-string argument.")
  } else if (str_length(s) == 0) {
    ""
  } else {
    substr(s, str_length(s), str_length(s))
  }
}

is.upper <- function(s) {
  if (!is.na(to_upper(s))) {
    to_upper(s) == s
  } else {
    FALSE
  }
}

processOutline <- function(bill) {
  processedBill <- bill %>%
    mutate(Tokens = tokenize_words(Text)) %>%
    mutate(across(Tokens, function(x) { 
      sapply(x, function(y) { 
        if(length(y) == 0) { 
          "" 
        } else { 
          y 
        }
      }) 
    })) %>%
    mutate(Keywords = mapply(function(x, y) {
      x[which(sapply(y, function(z) {
        !((z %in% dropWords) | (str_remove(z, "[0-9.]+") == ""))}))]},
      Tokens, Tokens)) %>%
    mutate_at("Keywords", function(x) sapply(x, unique)) %>%
    mutate(Tag = "") %>%
    mutate(Number_of_Keywords = sapply(Keywords, length)) 
  
  processedBill <- processedBill %>%
    mutate(Matches = mapply(
      function (x, y) { 
        list(stri_extract_first_regex(x, y)[TITLE:`(AZ2)`]) },
      Outline, 
      MoreArgs = list(outlineTags))) %>%
    mutate(TextMatch = mapply(
      function (x, y) { 
        list(stri_extract_first_regex(x, y)[TITLE:`(AZ2)`]) },
      Text, 
      MoreArgs = list(outlineTags)))

  processedBill <- processedBill %>%
    mutate(Match1 = sapply(Matches, function(x) first(which(!is.na(x))))) %>%
    mutate(Level1 = sapply(Matches, function(x) tagLevels[first(which(!is.na(x)))])) %>%
    mutate(Match2 = sapply(TextMatch, function(x) first(which(!is.na(x))))) %>%
    mutate(Level2 = sapply(TextMatch, function(x) tagLevels[first(which(!is.na(x)))])) %>%
    mutate(Omit = FALSE)
  
  letter <- lastLetter <- ""
  
  carriedTags <- c(rep("", p_AZ2))
  interpolationTags <- c(rep("", p_AZ2))
  
  carriedDepth <- interpolationDepth <- 0
  
  inInterpolation <- function(i) {
    processedBill$Quoted[i]
  }
  
  currentSection <- ""
  
  outlineTag <- function(i) { 
    if (inInterpolation(i)) {
      trimws(str_c("[", 
                   str_c(carriedTags, collapse = ""), 
                   "] ", 
                   str_c(interpolationTags, collapse = "")))
    } else {
      trimws(str_c(carriedTags, collapse = ""))
    }
  }
  
  is_upper <- function(s) {
    str_to_upper(s) == s
  }
  
  safe_extract <- function(s, p) {
    if (is.na(str_extract(s, p))) {
      ""
    } else {
      str_extract(s, p)
    }
  }
  
  # Determine whether tag1 and tag2 are consecutive tags. 
  # Limitations: assumes tags being compared are at the same level and of the 
  # format "SEC. 4591" or "(a)" or "(3)" or "(D)" or "(gg)" or "(BB)". And we 
  # even deal with Roman numerals!
  tagsAreConsecutive <- function(tag1, tag2) {
    # browser()
    t1 <- str_remove_all(tag1, "[()]")
    t2 <- str_remove_all(tag2, "[()]")
    t1n <- suppressWarnings(as.integer(str_extract(t1, "[0-9]+")))
    t2n <- suppressWarnings(as.integer(str_extract(t2, "[0-9]+")))
    if (!is.na(t1n) & !is.na(t2n)) {
      t2n == t1n + 1
    } else {
      t1az <- str_extract(t1, "[a-zA-Z]+")
      t2az <- str_extract(t2, "[a-zA-Z]+")
      if (is.na(t1az) | is.na(t2az)) {
        FALSE
      } else if (((str_to_lower(t1az) != t1az) & (str_to_upper(t1az) != t1az)) |
                 ((str_to_lower(t2az) != t2az) & (str_to_upper(t2az) != t2az))) {
        FALSE
      } else if ((is_upper(t1az) != is_upper(t2az))) {
        FALSE
      } else if ((str_length(t1az) == str_length(safe_extract(str_to_lower(t1az), "[xvi]+"))) &
                 ((str_length(t2az) == str_length(safe_extract(str_to_lower(t2az), "[xvi]+"))))) {
        # We've got roman numerals, folks!
        as.integer(as.roman(t2az)) == as.integer(as.roman(t1az)) + 1
      } else if (str_length(t1az) != str_length(t2az)) {
        FALSE
      } else {
        last1 <- substr(str_to_lower(t1az), str_length(t1az), str_length(t1az))
        last2 <- substr(str_to_lower(t2az), str_length(t2az), str_length(t2az))
        which(letters == last2) == which(letters == last1) + 1
      }
    }
  }
  
  massageFormat <- function(depth, tag) {
    if (depth > SECTION) {
      trimws(tag)
    } else if (depth == SECTION) {
      myTag <- str_replace(tag, "Section ", "SEC. ")
      if (!str_detect(myTag, "SEC.")) {
        myTag <- str_c("SEC. ", trimws(myTag))
      }
      # Following lines are needed only if we can have section numbers like "13.1":
      # pos <- stri_locate(myTag, regex = "[.]", mode = "last")
      # if(is.na(pos[1])) {
      #   myTag <- str_c(myTag, ".")
      # } else if (pos[2] != str_length(myTag)) {
      #   myTag <- str_c(myTag, ".")
      # }
      trimws(myTag)
    } else {
      str_c(tag, " ")
    }
  }
  
  carriedDepth <- depth <- processedBill$Level1[1]
  if (depth == SECTION) { # Section number
    currentSection <- str_extract(processedBill$Outline[1], "[0-9-.]+")
    carriedTags[SECTION] <- str_c("SEC. ", currentSection)
  } else {
    message("Expected first section number not found.")
    currentSection <- NA
    carriedTags[depth] <- processedBill$Outline[1]
  }
  processedBill$Tag[1] <- outlineTag(1)
  
  for (i in 2:length(processedBill$Outline)) {
    # if (i == 182) {
    #   browser()
    # }
    newDepth <- processedBill$Level1[i]
    # print(as.character(i))
    if (is.na(depth) | is.na(newDepth)) {
      errorCondition(str_c("Invalid depth or newDepth at i = ", as.character(i)))
      if (!is.na(depth)) {    # band-aid for "1900." at the start of a line
        newDepth <- depth
      } else {
        depth <- newDepth
      }
    }
    newTag <- massageFormat(newDepth, processedBill$Outline[i])
    # Sometimes we need to look ahead in order to disambiguate a tag
    nextTag <- trimws(processedBill$Outline[i + 1])
    nextDepth <- processedBill$Level1[i + 1]
    
    # Here we may need to fix-up newDepth due to a roman numeral being misinterpreted 
    # as a p_az or p_AZ. 
    priorTag <- ifelse(inInterpolation(i), 
                       interpolationTags[newDepth], 
                       carriedTags[newDepth])
    if ((newDepth == p_az) & 
        !is.na(str_extract(newTag, "[()ivx]+")) &
        (newTag == str_extract(newTag, "[()ivx]+"))) {
      if ((depth >= p_AZ) & (!(priorTag %in% c("(h)", "(u)", "(w)")) |
                             ((nextDepth == p_ivx) && tagsAreConsecutive(newTag, nextTag)))) {
        newDepth <- p_ivx
      }
    } else if ((newDepth == p_AZ) & 
               !is.na(str_extract(newTag, "[()IVX]+")) &
               (newTag == str_extract(newTag, "[()IVX]+"))) {
      if ((depth >= p_ivx) & (!(priorTag %in% c("(H)", "(U)", "(W)")) |
                              ((nextDepth == p_IVX) & tagsAreConsecutive(newTag, nextTag)))) {
        newDepth <- p_IVX
      }
    }
    
    # We may need to use context to detect and remove what looks like a tag at the 
    # beginning of a line, but isn't. 
    spuriousTag <- FALSE
    if (inInterpolation(i)) {
      if (((newDepth > (interpolationDepth + 1)) &
           (interpolationDepth > 0)) |
          ((newDepth == (interpolationDepth + 1)) &
           (newDepth >= p_az) &       # checks are only valid for p_az and below
           (newTag != firstTag(newDepth))) |
          ((newDepth <= interpolationDepth) &
           (newDepth >= p_az) &       # checks are only valid for p_az and below
           (!tagsAreConsecutive(interpolationTags[newDepth], newTag) |
            str_detect(str_to_lower(processedBill$Text[i - 1]), "paragraph$") |
            str_detect(str_to_lower(processedBill$Text[i - 1]), "subsection$") |
            (!inInterpolation(i - 1) & (substr(processedBill$Text[i - 1], 
                                               str_length(processedBill$Text[i - 1]),
                                               str_length(processedBill$Text[i - 1])) != ":"))))) {
        spuriousTag <- TRUE
      }
    } else { # not in an interpolation; do analogous checks
      if ((newDepth > (carriedDepth + 1)) |
          ((newDepth == (carriedDepth + 1)) &
           (newDepth >= p_az) &       # checks are only valid for p_az and below
           (newTag != firstTag(newDepth))) |
          ((newDepth <= carriedDepth) &
           (newDepth >= p_az) &       # checks are only valid for p_az and below
           (!tagsAreConsecutive(carriedTags[newDepth], newTag) | 
            str_detect(str_to_lower(processedBill$Text[i - 1]), "paragraph$") |
            str_detect(str_to_lower(processedBill$Text[i - 1]), "subsection$") |
            (inInterpolation(i - 1) & 
             !str_detect(processedBill$Text[i - 1], ".$") &
             !str_detect(processedBill$Text[i - 1], "and$") &
             !str_detect(processedBill$Text[i - 1], "or$"))))) {
        spuriousTag <- TRUE
      }
    }
    if (spuriousTag) {
      # browser()
      processedBill$Text[i - 1] <- 
        str_c(processedBill$Text[i - 1],
              ifelse(last_char(processedBill$Text[i - 1]) != " ", " ", ""),
              newTag,
              ifelse(str_detect(processedBill$Text[i], "^[[:punct:]]"), 
                     "",
                     " "),
              processedBill$Text[i])
      processedBill$Quoted[i] <- processedBill$Quoted[i - 1]
      processedBill$Omit[i] <- TRUE
    } else {
      if (inInterpolation(i)) {
        priorTag <- interpolationTags[newDepth]
        interpolationTags[newDepth] <- massageFormat(newDepth, processedBill$Outline[i])
        if(newDepth < p_AZ2) {
          interpolationTags[(newDepth + 1):p_AZ2] <- ""
        }
      } else {
        priorTag <- carriedTags[newDepth]
        interpolationTags[1:p_AZ2] <- ""
        interpolationDepth <- 0
        carriedTags[newDepth] <- massageFormat(newDepth, processedBill$Outline[i])
        if(newDepth < p_AZ2) {
          carriedTags[(newDepth + 1):p_AZ2] <- ""
        }
      }
      if (newDepth <= depth) {
        if (priorTag != "") {
          unexpectedBreak <- (inInterpolation(i) & !tagsAreConsecutive(priorTag, interpolationTags[newDepth])) |
            (!inInterpolation(i) & !tagsAreConsecutive(priorTag, carriedTags[newDepth]))
          if (unexpectedBreak) {
            message(str_c("Unexpected break in outline tag succession at item ", 
                          as.character(processedBill$Item[i])))
          }
        }
      } else { # newDepth > depth 
        
      }
      # We may have an additional tag at the beginning of Text. We know it should be 
      # at depth newDepth + 1, and should be a firstTag for that depth. 
      if (!is.na(processedBill$Level2[i])) {
        extraTagCandidate <- trimws(processedBill$TextMatch[i][[1]][processedBill$Match2[i]])
        if (extraTagCandidate == firstTag(newDepth + 1)) {
          newDepth <- newDepth + 1
          if (inInterpolation(i)) {
            interpolationTags[newDepth] <- extraTagCandidate
          } else {
            carriedTags[newDepth] <- extraTagCandidate
          }
          processedBill$Text[i] <- 
            trimws(substr(processedBill$Text[i], 
                          str_length(extraTagCandidate) + 1, 
                          str_length(processedBill$Text[i])))
        }
      }
      if (inInterpolation(i)) {
        interpolationDepth <- newDepth
      } else {
        carriedDepth <- newDepth
      }
      processedBill$Tag[i] <- outlineTag(i)
      depth <- newDepth
    }
  }
  select(processedBill, -Matches, -Match1, -Level1)
}

