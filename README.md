# legistext

R scripts for basic text analysis of US federal legislation: bills introduced in the US Senate or House of Representatives. 
TidyBill.R reads a bill in .pdf format and converts it to a 'tidy' .csv format in which each text paragraph is accompanied 
by an outline tag usable to reference the paragraph and an optional list of keywords. CompareBills.R uses these keywords to 
identify paragraphs in a second bill that may be related to each paragraph in a first bill. Lightly tested by processing two 
bills from the current session (2021-22).
