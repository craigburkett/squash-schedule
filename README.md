# squash-schedule
Automatic scheduler that pulls from Rankenstein

# Instructions to get up and running

    Download and install R 
    Download and install RStudio 
    
### Windows
    
    https://cran.r-project.org/bin/windows/base/
    https://download1.rstudio.org/electron/windows/RStudio-2022.12.0-353.exe
    
### Mac

    Mac users are so hip they can find equivalent links with no trouble
    
    Either: 
      Pull the repo, or
      Go to https://github.com/craigburkett/squash-schedule and click on the green "Code" button, then "Download zip"
    Extract it somewhere, note the <dir>
    Open RStudio
    From the command line, run the following (you can set custom install location or environments if you want, but this is not needed):
    install.packages("tidyverse")
    install.packages("magrittr")
    install.packages("jsonlite")
    In the top right, click the (No project) down arrow and select "New Project", "Existing directory", and choose the one where you extracted the zip file
    Copy the current input file Steve uses into <dir>
    Run the Generate-Schedule.R script (Ctrl-A, Ctrl-Enter) and choose the input file when prompted
    ...
    Profit!