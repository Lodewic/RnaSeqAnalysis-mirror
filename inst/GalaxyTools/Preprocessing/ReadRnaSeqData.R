
##============ Sink warnings and errors to a file ==============

## use the sink() function to wrap all code within it.

##==============================================================

zz = file('warnings_and_errors.txt')
sink(zz)
sink(zz, type = 'message')


#------------import libraries--------------------
options(stringsAsFactors = FALSE)

library(getopt)
library(rmarkdown)

spec_matrix = as.matrix(
  
  data.frame(stringsAsFactors=FALSE,
             
             long_flags = c("countdata", "coldata", "species", "keytype",
                            "echo", "tooldir", "messages", "X_o", "X_d"),
             short_flags = c("C", "S", "s", "k",
                             "e", "t", "m", "o", "d"),
             argument_mask_flags = c(1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L),
             data_type_flags = c("character", "character", "character", "character",
                                 "character", "character", "character", "character", "character")
  )
)


opt = getopt(spec_matrix)
# print(opt)

#---------- often used variables ----------------
# OUTPUT_DIR: path to the output associated directory, which stores all outputs
# TOOL_DIR: path to the tool installation directory
# RMD_NAME: name of Rmd file to be rendered
# OUTPUT_REPORT: path to galaxy output report
OUTPUT_DIR = opt$X_d
TOOL_DIR =   opt$tooldir
RMD_NAME = 'TestParameterReport.Rmd'
OUTPUT_REPORT = opt$X_o


if (!rmarkdown::pandoc_available()) Sys.setenv(RSTUDIO_PANDOC = "C:/Program Files/RStudio/bin/pandoc")


# create the output associated directory to store all outputs
if (!dir.exists(OUTPUT_DIR)) dir.create(OUTPUT_DIR, recursive = TRUE)



#-----------------render Rmd--------------
# Render with parameters
rmarkdown::render(RMD_NAME,
                  params = opt[c("echo", "countdata", "coldata", "species", "keytype")],
                  output_dir = OUTPUT_DIR)


#------------------------------------------

#==============the end==============





##--------end of code rendering .Rmd templates----------------

sink()

##=========== End of sinking output=============================
