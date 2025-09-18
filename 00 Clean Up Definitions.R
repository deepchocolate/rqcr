#### READ IN ALL DEFINITIONS FROM EXCEL 
# "https://folkehelse.sharepoint.com/:x:/s/1648/EQzUF_isVlVAnzbzllKXLSUB-d3032b62n59LZanWuZqOA?e=bF9ecu"

library(readxl)
library(data.table)

# Specify the file path

codebook_path <- "OZANIMOD_CodeBook_V0.3_20250228.xlsx"
#codebook_path <- "C:/Users/inba/Ozanimod PASS_CodeBook_v2.0_20250620.xlsx"

norway        <- "Norwegian Registries, (Norway)"
  
# Retrieve sheet names
sheet_names <- excel_sheets(codebook_path)

# Check sheet names
print(sheet_names)

# FUNCTION for reading sheets 
FUN_read_sheets <- function(sheetname="UC") {
  
      sheet_read <- read_excel(codebook_path, sheet = sheetname, skip=2) |> setDT()
      
      # Check if variable "Database" is in there:

      if ("Database" %in% names(sheet_read)) {
        message(paste("The variable name Database", "exists in the sheet ",sheetname,"."))
        sheet_new <- sheet_read[ Database == norway,.(Vocabulary,Code,Inpatient,Outpatient,Emergency,comments)][,sheetname:=sheetname]
        return(sheet_new)
      } else {
        message(paste("The variable name Database", "does NOT exist in the sheet ",sheetname,"."))
        return(NULL)
      }
}

###

# EMPTY Data Table

code_book <- data.table()

# Run and append to list

for (i in 1:length(sheet_names)) {
  t <- FUN_read_sheets(sheet_names[i])
  
  if (!is.null(t)) {
  code_book <- rbind(code_book,t)  
  }
}
 
# Move the sheetname first

setcolorder(code_book,"sheetname")

#Check, looks OK:

code_book[,.SD,.SDcols = !"comments"]

# Print and inspect xlsx file

openxlsx::write.xlsx(code_book, file = "Norway_codebook.xlsx")
