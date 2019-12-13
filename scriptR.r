# scriptR for Eyetrack experiments by Michael Wilson####
# Libraries ####
# Checks if the required packages are installed, and installs them if not
# Code from https://stackoverflow.com/questions/9341635/check-for-installed-packages-before-running-install-packages, user Sean Murphy
packages <- c("dplyr", "data.table", "gplots", "stringr", "tidyr", "plotrix")
if (length(setdiff(packages, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(packages, rownames(installed.packages())))
}

suppressMessages(library(dplyr)) # Several functions
suppressMessages(library(data.table)) # For %like% operator
suppressMessages(library(gplots)) # For col2hex
suppressMessages(library(stringr)) # For str_locate
suppressMessages(library(tidyr)) # For spread
suppressMessages(library(plotrix)) # For color.id()

# Get directory function ####
# From https://stackoverflow.com/questions/49196697/how-to-get-the-directory-of-the-executing-script-in-r, user "tinker"
get_directory <- function() {
  args <- commandArgs(trailingOnly = FALSE)
  file <- "--file="

  match <- grep(file, args)
  if (length(match) > 0) {
    return(dirname(normalizePath(sub(file, "", args[match]))))
  } else {
    return(dirname(normalizePath(sys.frames()[[1]]$ofile)))
  }
}

# Prompt for user parameters ####
{
  # Read in the stimuli file ####
  cat("Enter stimuli CSV location: ")
  filepath <- readLines("stdin", n = 1)
  if(!(filepath %like% "\\.csv$")){
    filepath <- paste(filepath, ".csv", sep = "")
  }
  while(!file.exists(filepath)){
    cat("No CSV file found at that location. Please enter a valid location: ")
    filepath <- readLines("stdin", n = 1)
    if(!(filepath %like% "\\.csv$")){
      filepath <- paste(filepath, ".csv", sep = "")
    }
  }

  filename <- sub(".*(\\/|\\\\)", "", filepath) %>% sub("\\.csv", "", .)
  filepath <- sub(paste(sep = "", filename, ".csv"), "", filepath)

  stimuli <- read.csv(paste(filepath, filename, ".csv", sep = ""))

  # Clean the stimuli file ####
  {
    # Warn if item_condition is already specified ####
    if(!((is.null(stimuli$item_condition)) | all(is.na(stimuli$item_condition)) | all(stimuli$item_condition == ""))){
      cat("Warning: item_condition already specified in stimuli CSV, and will be overwritten.\n")
      stimuli <- stimuli %>% dplyr::select(-item_condition) %>% droplevels()
    }

    # If some item number is not specified, exit ####
    if(is.null(stimuli$item_id) | any(is.na(stimuli$item_id)) | any(stimuli$item_id == "") | !all(stimuli$item_id %like% "^Message$|^[1-9][0-9]*$")){
      cat("Error: item numbers not properly specified. ")
      .Internal(.invokeRestart(list(NULL, NULL), NULL))
    }

    # If some experiment is not specified correctly, attempt to get practice items automatically ####
    if(is.null(stimuli$Experiment) | any(is.na(stimuli$Experiment)) | any(stimuli$Experiment == "") | !all(stimuli$Experiment %like% "^P$|^[1-9][0-9]*$")){

      stimuli$Experiment <- ""

      # Find the first and last Message items, if they exist, and assume they delimit the practice items
      for(i in 1:nrow(stimuli)){
        if(stimuli[i,]$item_id == "Message"){
          if(!exists("first.practice.row")){
            first.practice.row <- i
          } else {
            last.practice.row <- i
          }
        }
      }

      # If there's only one practice row, set the value of last practice row equal to the value of first practice row
      if(!exists("last.practice.row")){
        if(exists("first.practice.row")){
          last.practice.row <- first.practice.row
        }
      }

      # If there are practice items, set the experiment value for those to P
      if(exists("first.practice.row")) {
        stimuli[c(first.practice.row:last.practice.row),]$Experiment <- "P"
      }
    }

    # Temporarily discard any practice items ####
    practice <- stimuli %>% filter(Experiment == "P") %>% droplevels()
    stimuli <- stimuli %>% filter(Experiment != "P") %>% transform(Experiment = as.factor(Experiment)) %>% droplevels()
    rownames(stimuli) <- c(1:nrow(stimuli))

    # Practice defaults ####
    if(nrow(practice) > 0){
      # Specify a dummy item_condition for practice items
      practice$item_condition <- "1"

      # Set any non Message practice items to item 1
      practice$item_id <- ifelse(practice$item_id == "Message", "Message", "1")

      if(is.null(practice$Timeout)){
        practice$Timeout <- "30000"
      } else if(any(is.na(practice$Timeout)) | any(practice$Timeout == "") | !all(practice$Timeout %like% "^[1-9][0-9]*$")) {
        practice$Timeout <- as.character(practice$Timeout)
        practice[is.na(practice$Timeout) | practice$Timeout == "" | !(practice$Timeout %like% "^[1-9][0-9]*$"), ]$Timeout <- "30000"
      }

      if(is.null(practice$QTimeout)){
        practice$QTimeout <- "30000"
      } else if(any(is.na(practice$QTimeout)) | any(practice$QTimeout == "") | !all(practice$QTimeout %like% "^[1-9][0-9]*$")) {
        practice$QTimeout <- as.character(practice$QTimeout)
        practice[is.na(practice$QTimeout) | practice$QTimeout == "" | !(practice$QTimeout %like% "^[1-9][0-9]*$"),]$QTimeout <- "30000"
      }

      if(is.null(practice$Sentence2)){
        practice$Sentence2 <- ""
      } else if(any(is.na(practice$Sentence2)) | any(practice$Sentence2 == "")) {
        practice[is.na(practice$Sentence2) | practice$Sentence2 == "",]$Sentence2 <- ""
      }

      if(is.null(practice$Question)){
        practice$Question <- ""
      } else if(any(is.na(practice$Question)) | any(practice$Question == "")) {
        practice[is.na(practice$Question) | practice$Question == "",]$Question <- ""
      }

      if(is.null(practice$LeftAnswer)){
        practice$LeftAnswer <- ""
      } else if(any(is.na(practice$LeftAnswer)) | any(practice$LeftAnswer == "")) {
        practice[is.na(practice$LeftAnswer) | practice$LeftAnswer == "",]$LeftAnswer <- ""
      }

      if(is.null(practice$RightAnswer)){
        practice$RightAnswer <- ""
      } else if(any(is.na(practice$RightAnswer)) | any(practice$RightAnswer == "")) {
        practice[is.na(practice$RightAnswer) | practice$RightAnswer == "",]$RightAnswer <- ""
      }

      if(is.null(practice$AnswerButton)){
        practice$AnswerButton <- "leftTrigger"
      } else if(any(is.na(practice$AnswerButton)) | any(practice$AnswerButton == "") | !all(practice$AnswerButton %in% c("A", "B", "X", "Y", "leftTrigger", "rightTrigger", "toggle", "mouse"))) {
        practice$AnswerButton <- as.character(practice$AnswerButton)
        practice[is.na(practice$AnswerButton) | practice$AnswerButton == "" | !(practice$AnswerButton %in% c("A", "B", "X", "Y", "leftTrigger", "rightTrigger", "toggle", "mouse")),]$AnswerButton <- "leftTrigger"
        practice$AnswerButton <- as.factor(practice$AnswerButton)
      }

    }

    # Stimuli defaults ####
    {
      # If any item numbers are not specified, exit ####
      if(is.null(stimuli$item_id) | any(is.na(stimuli$item_id)) | any(stimuli$item_id == "") | !all(stimuli$item_id %like% "^[1-9][0-9]*$")){
        cat("Error: item numbers not properly specified in experimental stimuli. ")
        .Internal(.invokeRestart(list(NULL, NULL), NULL))
      }

      # If an experiment is not specified, attempt to calculate experiment numbers automatically ####
      if((is.null(stimuli$Experiment) | any(is.na(stimuli$Experiment)) | any(stimuli$Experiment == "")) | !all(stimuli$Experiment %like% "^[1-9][0-9]*$")){

        stimuli$Experiment <- ""

        cat(sep = "", "Warning: Experiment numbers improperly set. Attempting to calculate automatically using item_id. Double-check ", filename, "-formatted.csv to ensure correct values.\n")

        # Calculate item_condition numbers ####
        stimuli$item_condition <- 0

        # Set item_condition numbers for each item
        current.item_condition <- 1

        # For each row in the stimuli
        for(i in 1:nrow(stimuli)){

          # Set the item_condition of the row to the current item_condition, and increase that by one
          stimuli[i,]$item_condition <- current.item_condition
          current.item_condition <- current.item_condition + 1

          # If there is a next row
          if(!is.na(stimuli[i+1,]$item_condition)){

            # If the item in the next row is different from the item in the current row
            if(stimuli[i+1,]$item_id != stimuli[i,]$item_id){

              # Reset current item_condition
              current.item_condition <- 1
            }
          }
        }

        # Get a list of item_condition values for each item for the experiment ####
        item_condition.per.item <- {}

        # Set the initial maximum item_condition to 1
        current.max.item_condition <- 1

        # For each row in the stimuli
        for(i in 1:nrow(stimuli)){
          # If we're not on the last row
          if(i < nrow(stimuli)){

            # If the next row has a higher item_condition number
            if(stimuli[i+1,]$item_condition > stimuli[i,]$item_condition){

              # If that item_condition number is higher than the current max.item_condition, set the current max item_condition to that value
              current.max.item_condition <- stimuli[i+1,]$item_condition

              # Otherwise, if the next row has a lower or equal item_condition number
            } else {
              item_condition.per.item <- append(item_condition.per.item, current.max.item_condition)
              current.max.item_condition <- 1
            }

            # If we're on the last row, add its item_condition as the maximum item_condition
          } else {
            item_condition.per.item <- append(item_condition.per.item, stimuli[i,]$item_condition)
          }
        }

        # Set up a variable for the current experiment number ####
        current.exp <- 1

        # Set up a data frame to hold the stimuli with experiment numbers added
        stimuli.with.exp <- data.frame()

        # For each item, as designated by item_condition.per.item ####
        for(i in 1:length(item_condition.per.item)){

          # Get the current item and set its experiment to the current experiment
          current.item <- stimuli %>%
            slice(1:item_condition.per.item[i]) %>%
            transform(Experiment = current.exp)

          # Remove that item from the stimuli
          stimuli <- stimuli %>% slice(-1:-item_condition.per.item[i])

          # If we're not on the last item
          if(i != length(item_condition.per.item)){

            # If the next item has the same number of item_conditions as the current item
            if(item_condition.per.item[i] == item_condition.per.item[i+1]) {

              # Get the next item
              next.item <- stimuli %>% slice(1:item_condition.per.item[i+1])

              # If the next item number is less than or equal to the current item number
              if(max(next.item$item_id) <= max(current.item$item_id)){
                # Increase the experiment counter by one
                current.exp <- current.exp + 1
              }
            } else {
              # If the next item has a different number of item_conditions from the previous item, increase the experiment counter by one
              current.exp <- current.exp + 1
            }
          }

          stimuli.with.exp <- rbind(stimuli.with.exp, current.item)

        }


        # Bind the stimuli back together with practice items ####
        stimuli.with.exp$item_condition <- as.character(stimuli.with.exp$item_condition)

        stimuli <- stimuli.with.exp
      }

      stimuli <- stimuli %>% transform(Experiment = as.factor(Experiment))

      # Check experiment numbers to make sure that they make sense ####
      stimuli <- stimuli %>% arrange(Experiment)

      # Get a list of the experiment numbers
      exp.list <- levels((stimuli %>% droplevels())$Experiment)

      # Temporarily transform experiment to a non-factor so we can add values to it if needed
      stimuli$Experiment <- as.numeric(as.character(stimuli$Experiment))

      # For each experiment
      for(i in 1:length(exp.list)){

        # If the current experiment's number doesn't match its position in the list, fix that
        if(!(all(stimuli[stimuli$Experiment == exp.list[i],]$Experiment == i))){
          cat(sep = "", "Warning: Renumbering experiment ", exp.list[i], " to experiment ", i, ". Experiment numbers should be consecutive integers starting at 1.\n")
          stimuli[stimuli$Experiment == exp.list[i],]$Experiment <- i
        }
      }

      stimuli$Experiment <- as.factor(stimuli$Experiment)

      # Check item numbers to ensure they make sense ####
      # Reorder just in case things are out of order
      stimuli <- stimuli %>% arrange(Experiment, item_id)

      # Set up a thing to hold the values
      stimuli.fixed.items <- data.frame()

      # For each experiment
      for(i in 1:nlevels(stimuli$Experiment)){

        # Get the current experiment
        current.exp <- stimuli %>% filter(Experiment == i) %>% droplevels()

        # Get a list of the levels of the item in the current experiment
        item.list <- levels(current.exp$item_id %>% droplevels() %>%  as.factor())

        # For each item in that experiment
        for(j in 1:length(item.list)){

          current.item <- current.exp %>% filter(item_id == item.list[j]) %>% droplevels()

          #If that item's number isn't equal to its position in the list
          if(!(all(current.item$item_id == j))){

            cat(sep = "", "Warning: Renumbering item ", item.list[j], " in experiment ", current.exp[1,]$Experiment, " to item ", j, ". For each experiment, items should be consecutive integers starting at 1.\n")

            # Fix that
            current.item$item_id <- j
          }

          # Add the current item to the fixed stimuli
          stimuli.fixed.items <- rbind(stimuli.fixed.items, current.item)
        }
      }

      stimuli <- stimuli.fixed.items

      # Check to make sure all items in each experiment have the same number of rows/item_conditions, throw an error and exit if not ####

      # For each experiment
      for(i in 1:nlevels(stimuli$Experiment)){

        # Get the current experiment
        current.exp <- stimuli %>% filter(Experiment == i) %>% droplevels()

        rows.per.item <- {}

        # For each item
        for(j in 1:nlevels(current.exp$item_id %>% droplevels() %>% as.factor())){
          rows.per.item <- append(rows.per.item, nrow(current.exp %>% filter(item_id == j)))
        }

        # There's more than one value in rows.per.item, then not all items have equal numbers of item_conditions, and we exit
        if(length(unique(rows.per.item)) != 1){

          err.message <- ""

          for(k in 1:length(unique(rows.per.item))){
            err.message <- err.message %>% append(" ") %>% append(unique(rows.per.item)[k])
          }

          cat(sep = "", "Error: items in experiment ", as.character(current.exp[1,]$Experiment), " have differing numbers of item_conditions:", err.message, ". Each experiment must have the same number of item_conditions per item. ")
          .Internal(.invokeRestart(list(NULL, NULL), NULL))
        }
      }

      # Generate item_condition numbers per item per experiment ####
      stimuli$item_condition <- 0

      # For each experiment
      for(i in 1:nlevels(stimuli$Experiment)){
        # Get the number of rows per item (assume all items in one experiment have the same number of item_conditions)
        item_condition.num <- nrow(stimuli %>% filter(Experiment == i, item_id == "1"))

        # Set the item_condition number to repeat from 1 to the number of items for each item in the experiment
        stimuli[stimuli$Experiment == i,]$item_condition <- rep(c(1:item_condition.num), nlevels((stimuli %>% filter(Experiment == i) %>% droplevels)$item_id %>% as.factor()))
      }

      stimuli$item_id <- stimuli$item_id %>% as.numeric()
      stimuli$item_condition <- stimuli$item_condition %>% as.numeric()

      # Remaining defaults ####

      # If no sentences are specified, warn
      if(is.null(stimuli$Sentence) | all(is.na(stimuli$Sentence)) | all(stimuli$Sentence == "")){
        cat("Warning: No sentences specified.\n")
        stimuli$Sentence <- ""
      } else if(any(is.na(stimuli$Sentence)) | any(stimuli$Sentence == "")){
        cat("Warning: Some sentences not specified.\n")
        stimuli[is.na(stimuli$Sentence) | stimuli$Sentence == "", ]$Sentence <- ""
      }

      # Remove initial and final region specifiers, since we'll generate those automatically
      stimuli$Sentence <- gsub("^/|/$", "", stimuli$Sentence)

      if(is.null(stimuli$Timeout)){
        stimuli$Timeout <- "30000"
      } else if(any(is.na(stimuli$Timeout)) | any(stimuli$Timeout == "") | !all(stimuli$Timeout %like% "^[1-9][0-9]*$")) {
        stimuli$Timeout <- as.character(stimuli$Timeout)
        stimuli[is.na(stimuli$Timeout) | stimuli$Timeout == "" | !(stimuli$Timeout %like% "^[1-9][0-9]*$"),]$Timeout <- "30000"
      }

      if(is.null(stimuli$QTimeout)){
        stimuli$QTimeout <- "30000"
      } else if(any(is.na(stimuli$QTimeout)) | any(stimuli$QTimeout == "") | !all(stimuli$QTimeout %like% "^[1-9][0-9]*$")) {
        stimuli$QTimeout <- as.character(stimuli$QTimeout)
        stimuli[is.na(stimuli$QTimeout) | stimuli$QTimeout == "" | !(stimuli$QTimeout %like% "^[1-9][0-9]*$"),]$QTimeout <- "30000"
      }

      if(is.null(stimuli$Sentence2)){
        stimuli$Sentence2 <- ""
      } else if(any(is.na(stimuli$Sentence2)) | any(stimuli$Sentence2 == "")) {
        stimuli[is.na(stimuli$Sentence2) | stimuli$Sentence2 == "",]$Sentence2 <- ""
      }

      if(is.null(stimuli$Question)){
        stimuli$Question <- ""
      } else if(any(is.na(stimuli$Question)) | any(stimuli$Question == "")) {
        stimuli[is.na(stimuli$Question) | stimuli$Question == "",]$Question <- ""
      }

      if(is.null(stimuli$LeftAnswer)){
        stimuli$LeftAnswer <- ""
      } else if(any(is.na(stimuli$LeftAnswer)) | any(stimuli$LeftAnswer == "")) {
        stimuli[is.na(stimuli$LeftAnswer) | stimuli$LeftAnswer == "",]$LeftAnswer <- ""
      }

      if(is.null(stimuli$RightAnswer)){
        stimuli$RightAnswer <- ""
      } else if(any(is.na(stimuli$RightAnswer)) | any(stimuli$RightAnswer == "")) {
        stimuli[is.na(stimuli$RightAnswer) | stimuli$RightAnswer == "",]$RightAnswer <- ""
      }

      if(is.null(stimuli$AnswerButton)){
        stimuli$AnswerButton <- "leftTrigger"
      } else if(any(is.na(stimuli$AnswerButton)) | any(stimuli$AnswerButton == "") | !all(stimuli$AnswerButton %in% c("A", "B", "X", "Y", "leftTrigger", "rightTrigger", "toggle", "mouse"))) {
        stimuli$AnswerButton <- as.character(stimuli$AnswerButton)
        stimuli[is.na(stimuli$AnswerButton) | stimuli$AnswerButton == "" | !(stimuli$AnswerButton %in% c("A", "B", "X", "Y", "leftTrigger", "rightTrigger", "toggle", "mouse")),]$AnswerButton <- "leftTrigger"
        stimuli$AnswerButton <- as.factor(stimuli$AnswerButton)
      }

    }

    display.changes <- F
    for(i in 1:nrow(stimuli)){
      if(stimuli[i,]$Sentence2 != ""){
        display.changes <- T
      }
    }
  }
  # Read in config file ####
  cat("If you are using a config CSV file, enter its location (otherwise, press enter): ")
  config.location <- readLines("stdin", n = 1)
  if(config.location != "") { use.config <- T } else { use.config <- F }
  if(use.config){
    if(!(config.location %like% "\\.csv$")){
      config.location <- paste(config.location, ".csv", sep = "")
    }
    while((!file.exists(config.location) & use.config) | config.location == paste(filepath, filename, ".csv", sep = "")){
      if(config.location == paste(filepath, filename, ".csv", sep = "")){
        cat("Config file cannot be the same as the stimuli file. Please enter a valid location: ")
        config.location <- readLines("stdin", n = 1)
        if(config.location != "") { use.config <- T } else { use.config <- F }
        if(!(config.location %like% "\\.csv$")){
          config.location <- paste(config.location, ".csv", sep = "")
        }
      } else {
        cat("No config CSV file found at that location. Please enter a valid location: ")
        config.location <- readLines("stdin", n = 1)
        if(config.location != "") { use.config <- T } else { use.config <- F }
        if(!(config.location %like% "\\.csv$")){
          config.location <- paste(config.location, ".csv", sep = "")
        }
        if(!(config.location %like% "\\/|\\\\")){
          config.location <- paste(sep = "", get_directory(), "/", config.location)
        }
      }
    }
  }

  # If no config file is used, prompt for parameters ####
  if(!use.config){
    cat("Background color (name or hex)? ")
    background.color <- readLines("stdin", n = 1)
    if(background.color == ""){ background.color <- "white" }
    while(!(background.color %in% colors()) &
          !grepl("^((0x)|#|)[0-9A-Fa-f]{6}$", background.color)){
      cat("Invalid background color. Please enter a valid background color: ")
      background.color <- readLines("stdin", n = 1)
      if(background.color == ""){ background.color <- "white" }
    }
    if(background.color %in% colors()){
      background.color <- background.color %>% col2hex
    }
    background.color <- background.color %>% gsub("#|(0x)", "", .) %>% strtoi(base = 16)

    cat("Text color (name or hex)? ")
    foreground.color <- readLines("stdin", n = 1)
    if(foreground.color == ""){ foreground.color <- "black" }
    while(!(foreground.color %in% colors()) &
          !grepl("^((0x)|#|)[0-9A-Fa-f]{6}$", foreground.color)){
      cat("Invalid text color. Please enter a valid text color: ")
      foreground.color <- readLines("stdin", n = 1)
      if(foreground.color == ""){ foreground.color <- "black" }
    }
    if(foreground.color %in% colors()){
      foreground.color <- foreground.color %>% col2hex
    }
    foreground.color <- foreground.color %>% gsub("#|(0x)", "", .) %>% strtoi(base = 16)

    cat("Number of calibration points? ")
    calibration.type <- readLines("stdin", n = 1)
    if(calibration.type == ""){ calibration.type <- "9" }
    while(!(calibration.type %in% c("3", "9", "13"))){
      cat("Invalid response. Please enter one of: 3, 9, 13. ")
      calibration.type <- readLines("stdin", n = 1)
      if(calibration.type == ""){ calibration.type <- "9" }
    }
    calibration.type <- ifelse(calibration.type == "3", "0",
                               ifelse(calibration.type == "9", "1", "2"))

    cat("Font name? ")
    font <- readLines("stdin", n = 1)
    if(font == ""){ font <- "Monaco" }

    cat("Font size (pt)? ")
    font.size <- readLines("stdin", n = 1)
    if(font.size == ""){ font.size <- 12 }
    while(!(font.size %like% "^[1-9][0-9]*$")){
      cat("Invalid response. Please enter a number not starting with 0: ")
      font.size <- readLines("stdin", n = 1)
      if(font.size == ""){ font.size <- 12 }
    }

    cat("Font style? ")
    font.weight <- readLines("stdin", n = 1)
    if(font.weight == ""){ font.weight <- "normal non-italic" }
    while(!(font.weight %in% c("normal non-italic", "normal italic", "bold non-italic", "bold italic"))){
      cat("Invalid response. Please choose one of: normal non-italic, normal italic, bold non-italic, bold italic. ")
      font.weight <- readLines("stdin", n = 1)
      if(font.weight == ""){ font.weight <- "normal non-italic" }
    }

    cat("Font smoothing? ")
    font.smoothing <- readLines("stdin", n = 1)
    if(font.smoothing == ""){ font.smoothing <- "nonantialiased" }
    while(!(font.smoothing %in% c("nonantialiased", "antialiased", "cleartype"))){
      cat("Invalid response. Please enter one of: nonantialiased, antialiased, cleartype. ")
      font.smoothing <- readLines("stdin", n = 1)
      if(font.smoothing == ""){ font.smoothing <- "nonantialiased" }
    }

    cat("Which unit to use for line spacing, x offset, and y offset? ")
    unit <- readLines("stdin", n = 1)
    if(unit == ""){ unit <- "px" }
    if(unit %like% "px|pix|pixel|pixels"){
      unit <- "px"
      use.pix <- T
    } else if (unit %like% "pt|pts|point|points"){
      unit <- "pt"
      use.pix <- F
    }
    while(!(unit %like% "px|pt")){
      cat("Invalid response. Please enter one of: px, pt. ")
      unit <- readLines("stdin", n = 1)
      if(unit == ""){ unit <- "px" }
      if(unit %like% "px|pix|pixel|pixels"){
        unit <- "px"
        use.pix <- T
      } else if (unit %like% "pt|pts|point|points"){
        unit <- "pt"
        use.pix <- F
      }
    }

    cat("Line spacing (", unit, ")? ", sep = "")
    line.spacing <- readLines("stdin", n = 1)
    if(line.spacing == ""){ line.spacing <- "0" }
    while(!(line.spacing %like% "^[0-9]+$")){
      cat("Invalid response. Please enter a number: ")
      line.spacing <- readLines("stdin", n = 1)
      if(line.spacing == ""){ line.spacing <- "0" }
    }
    if(use.pix){
      line.spacing.pix <- as.numeric(as.character(line.spacing))
      line.spacing <- round(as.numeric(as.character(line.spacing))/1.32)
    } else {
      line.spacing.pix <- as.numeric(as.character(line.spacing)) * 1.32
    }

    cat("X offset (", unit, ")? ", sep = "")
    x.offset <- readLines("stdin", n = 1)
    if(x.offset == ""){ if(use.pix) { x.offset <- "20" } else { x.offset <- "15" }}
    while(!(x.offset %like% "^[0-9]+$")){
      cat("Invalid response. Please enter a number: ")
      x.offset <- readLines("stdin", n = 1)
      if(x.offset == ""){ if(use.pix) { x.offset <- "20" } else { x.offset <- "15" } }
    }
    if(use.pix){
      x.offset.pix <- as.numeric(as.character(x.offset))
      x.offset <- round(as.numeric(as.character(x.offset))/(1 + (1/3)))
    } else {
      x.offset.pix <- as.numeric(as.character(x.offset)) * (1 + (1/3))
    }

    cat("Y offset (", unit, ")? ", sep = "")
    y.offset <- readLines("stdin", n = 1)
    if(y.offset == ""){ if(use.pix) { y.offset <- "357" } else { y.offset <- "268" } }
    while(!(y.offset %like% "^[0-9]+$")){
      cat("Invalid response. Please enter a number: ")
      y.offset <- readLines("stdin", n = 1)
      if(y.offset == ""){ if(use.pix) { y.offset <- "357" } else { y.offset <- "268" } }
    }
    if(use.pix){
        y.offset.pix <- as.numeric(as.character(y.offset))
        y.offset <- round(as.numeric(as.character(y.offset))/(1 + (1/3)))
    } else {
      y.offset.pix <- as.numeric(as.character(y.offset)) * (1 + (1/3))
    }

    cat("Sentence button? ")
    sentence.button <- readLines("stdin", n = 1)
    if(sentence.button == ""){ sentence.button <- "rightTrigger" }
    while(!(sentence.button %in% c("A", "B", "X", "Y", "leftTrigger", "rightTrigger", "toggle", "mouse"))){
      cat("Invalid sentence button. Please enter a valid option (A, B, X, Y, leftTrigger, rightTrigger, toggle, mouse): ")
      sentence.button <- readLines("stdin", n = 1)
      if(sentence.button == ""){ sentence.button <- "rightTrigger" }
    }

    cat("Sentence trigger? ")
    sentence.trigger <- readLines("stdin", n = 1)
    if(sentence.trigger == ""){ sentence.trigger <- "driftandgaze" }
    while(!(sentence.trigger %in% c("gaze", "nogaze", "driftcorrect", "driftandgaze"))) {
      cat("Invalid response. Please choose one of: gaze, nogaze, driftcorrect, driftandgaze. ")
      sentence.trigger <- readLines("stdin", n = 1)
      if(sentence.trigger == ""){ sentence.trigger <- "driftandgaze" }
    }

    sentence.delay <- "0"

    if(sentence.trigger %like% "nogaze"){
      cat("Sentence delay (ms)? ")
      sentence.delay <- readLines("stdin", n = 1)
      if(sentence.delay == ""){ sentence.delay <- "0" }
      while(!(sentence.delay %like% "^[0-9]*$")){
        cat("Invalid response. Please enter a number: ")
        sentence.delay <- readLines("stdin", n = 1)
        if(sentence.delay == ""){ sentence.delay <- "0" }
      }
    }

    cat("Sentence output? ")
    sentence.output <- readLines("stdin", n = 1)
    if(sentence.output == ""){ sentence.output <- "stream" }
    while(!(sentence.output %in% c("stream", "nostream"))){
      cat("Invalid response. Please choose one of: stream, nostream. ")
      sentence.output <- readLines("stdin", n = 1)
      if(sentence.output == ""){ sentence.output <- "stream" }
    }

    cat("Question trigger? ")
    question.trigger <- readLines("stdin", n = 1)
    if(question.trigger == ""){ question.trigger <- "nogaze" }
    while(!(question.trigger %in% c("gaze", "nogaze", "driftcorrect", "driftandgaze"))) {
      cat("Invalid response. Please choose one of: gaze, nogaze, driftcorrect, driftandgaze. ")
      question.trigger <- readLines("stdin", n = 1)
      if(question.trigger == ""){ question.trigger <- "nogaze" }
    }

    question.delay <- "0"

    if(question.trigger %like% "nogaze"){
      cat("Question delay (ms)? ")
      question.delay <- readLines("stdin", n = 1)
      if(question.delay == ""){ question.delay <- "0" }
      while(!(question.delay %like% "^[0-9]*$")){
        cat("Invalid response. Please enter a number: ")
        question.delay <- readLines("stdin", n = 1)
        if(question.delay == ""){ question.delay <- "0" }
      }
    }

    cat("Question output? ")
    question.output <- readLines("stdin", n = 1)
    if(question.output == ""){ question.output <- "nostream" }
    while(!(question.output %in% c("stream", "nostream"))){
      cat("Invalid response. Please choose one of: stream, nostream. ")
      question.output <- readLines("stdin", n = 1)
      if(question.output == ""){ question.output <- "nostream" }
    }

    display.update.threshold <- "0"
    display.revert <- "0"
    dc.delay <- "0"

    if(display.changes){
      cat("How many horizontal pixels per character (make sure to check with your font and your monitor)? ")
      h.pixels.per.char <- readLines("stdin", n = 1)
      while(!(h.pixels.per.char %like% "^[1-9][0-9]*$")) {
        cat("Invalid response. Please enter a number not starting with 0: ")
        h.pixels.per.char <- readLines("stdin", n = 1)
      }
      h.pixels.per.char <- as.numeric(as.character(h.pixels.per.char))

      cat("How many vertical pixels per character (make sure to check with your font and your monitor)? ")
      v.pixels.per.char <- readLines("stdin", n = 1)
      while(!(v.pixels.per.char %like% "^[1-9][0-9]*$")) {
        cat("Invalid response. Please enter a number not starting with 0: ")
        v.pixels.per.char <- readLines("stdin", n = 1)
      }
      v.pixels.per.char <- as.numeric(as.character(v.pixels.per.char))

      cat("How much vertical padding around each character (", unit, ")? ", sep = "")
      dcr.padding <- readLines("stdin", n = 1)
      if(dcr.padding == "") { if(use.pix){ dcr.padding <- line.spacing.pix } else { dcr.padding <- line.spacing } } else if (dcr.padding == "line.spacing" | dcr.padding == "line spacing"){ if(use.pix){ dcr.padding <- line.spacing.pix } else {dcr.padding <- line.spacing } }
      while(!(dcr.padding %like% "^[0-9]+$")){
        cat("Invalid response. Please enter a number: ")
        dcr.padding <- readLines("stdin", n = 1)
        if(dcr.padding == "") { if(use.pix){ dcr.padding <- line.spacing.pix } else { dcr.padding <- line.spacing } } else if (dcr.padding == "line.spacing" | dcr.padding == "line spacing"){ if(use.pix){ dcr.padding <- line.spacing.pix } else {dcr.padding <- line.spacing } }
      }
      if(use.pix){
        dcr.padding.pix <- as.numeric(as.character(dcr.padding))
        dcr.padding <- round(as.numeric(as.character(dcr.padding))/1.32)
      } else {
        dcr.padding.pix <- as.numeric(as.character(dcr.padding)) * 1.32
      }

      cat("Display update threshold? ")
      display.update.threshold <- readLines("stdin", n = 1)
      if(display.update.threshold == "") { display.update.threshold <- "0" }
      while(!(display.update.threshold %like% "^[0-9]*$")){
        cat("Invalid response. Please enter a number: ")
        display.update.threshold <- readLines("stdin", n = 1)
        if(display.update.threshold == ""){ display.update.threshold <- "0" }
      }

      cat("Revert display changes? (T/F): ")
      display.revert <- readLines("stdin", n = 1)
      if(display.revert == ""){ display.revert <- F } else { display.revert <- display.revert %>% toupper() %>% as.logical() }
      while(is.na(display.revert)){
        cat("Invalid response. Please enter T or F: ")
        display.revert <- readLines("stdin", n = 1)
        if(display.revert == ""){ display.revert <- F } else { display.revert <- display.revert %>% toupper() %>% as.logical() }
      }

      if(display.revert){
        cat("Display change reversion delay (ms)? ")
        dc.delay <- readLines("stdin", n = 1)
        if(dc.delay == ""){ dc.delay <- "0" }
        while(!(dc.delay %like% "^[0-9]*$")){
          cat("Invalid response. Please enter a number: ")
          dc.delay <- readLines("stdin", n = 1)
          if(dc.delay == ""){ dc.delay <- "0" }
        }
      }

      if(display.revert){ display.revert <- "1" } else { display.revert <- "0" }
    }

    if((sentence.trigger %like% "gaze" & !(sentence.trigger %like% "nogaze")) |
       (question.trigger %like% "gaze" & !(question.trigger %like% "nogaze"))){
      cat("Automatically calculate gaze control rect params? (T/F): ")
      calculate.gc.params <- readLines("stdin", n = 1)
      if(calculate.gc.params == "") { calculate.gc.params <- F } else { calculate.gc.params <- calculate.gc.params %>% toupper() %>% as.logical() }
      while(is.na(calculate.gc.params)){
        cat("Invalid response. Please enter T or F: ")
        calculate.gc.params <- readLines("stdin", n = 1)
        if(calculate.gc.params == "") { calculate.gc.params <- F } else { calculate.gc.params <- calculate.gc.params %>% toupper() %>% as.logical() }
      }
      if (calculate.gc.params){
        if(!exists("h.pixels.per.char")){
          cat("How many horizontal pixels per character (make sure to check with your font and your monitor)? ")
          h.pixels.per.char <- readLines("stdin", n = 1)
            while(!(h.pixels.per.char %like% "^[1-9][0-9]*$")) {
              cat("Invalid response. Please enter a number not starting with 0: ")
              h.pixels.per.char <- readLines("stdin", n = 1)
            }
        }
        h.pixels.per.char <- as.numeric(as.character(h.pixels.per.char))

        if(!exists("v.pixels.per.char")){
          cat("How many vertical pixels per character (make sure to check with your font and your monitor)? ")
          v.pixels.per.char <- readLines("stdin", n = 1)
          while(!(v.pixels.per.char %like% "^[1-9][0-9]*$")) {
            cat("Invalid response. Please enter a number not starting with 0: ")
            v.pixels.per.char <- readLines("stdin", n = 1)
          }
        }
        v.pixels.per.char <- as.numeric(as.character(v.pixels.per.char))

        gc.params <- paste(sep = " ",
                           round(y.offset.pix + ((line.spacing.pix+v.pixels.per.char)/2) - (v.pixels.per.char * 2)),
                           round(x.offset.pix),
                           round(y.offset.pix + ((line.spacing.pix+v.pixels.per.char)/2) + (v.pixels.per.char * 2)),
                           round(x.offset.pix + (h.pixels.per.char*4)))

      } else {
        cat("Gaze control rect params? ")
        gc.params <- readLines("stdin", n = 1)
        if(gc.params == ""){ gc.params <- "0 0 0 0" }
        while(!(gc.params %like% "^([0-9]+ ){3}[0-9]+$")){
          cat("Invalid response. Please enter four numbers separated by spaces to define the size of the gaze control box in the format Y1 X1 Y2 X2: ")
          gc.params <- readLines("stdin", n = 1)
          if(gc.params == ""){ gc.params <- "0 0 0 0" }
        }
      }
    } else {
      gc.params <- "0 0 0 0"
    }

    some.exp.uses.only.left.and.right <- F
    left.or.right.list <- {}
    for(i in 1:nlevels(stimuli$Experiment)){
      if(all(stimuli[stimuli$Experiment == levels(stimuli$Experiment)[i],]$AnswerButton %in% c("leftTrigger", "rightTrigger"))){
        some.exp.uses.only.left.and.right <- T
        left.or.right.list <- append(left.or.right.list, i)
      }
    }

    if(some.exp.uses.only.left.and.right){
      cat("Balance order of correct answers within items for which experiments? Valid experiments are the following: ", paste(left.or.right.list, collapse = " "), ". ", sep = "")
      balance.answer.order.exps <- readLines("stdin", n = 1)
      if(paste(balance.answer.order.exps, collapse = "") != "") {
        if(tolower(balance.answer.order.exps) == "all"){
          balance.answer.order.exps <- left.or.right.list %>% unlist
        } else if (balance.answer.order.exps == "none") {
          balance.answer.order.exps <- ""
        } else {
          balance.answer.order.exps <- str_split(balance.answer.order.exps, " ") %>% unlist
        }
      }
      while(paste(balance.answer.order.exps, collapse = "") != "" & (!all(grepl("^[1-9]*$", balance.answer.order.exps)) | !all(balance.answer.order.exps %in% left.or.right.list))){
        cat("Invalid response. Please enter a list of experiment numbers separated by spaces, 'all', or 'none'. Valid experiments are the following: ", paste(left.or.right.list, collapse = " "), ". ", sep= "")
        balance.answer.order.exps <- readLines("stdin", n = 1)
        if(paste(balance.answer.order.exps, collapse = "") != "") { if(tolower(balance.answer.order.exps) == "all"){ balance.answer.order.exps <- left.or.right.list %>% unlist } else if (balance.answer.order.exps == "none") { balance.answer.order.exps <- "" } else { balance.answer.order.exps <- str_split(balance.answer.order.exps, " ") %>% unlist } }
      }
    } else {
      balance.answer.order.exps <- ""
    }

#    for(i in 1:nlevels(stimuli$Experiment)){
#      if(all(stimuli[stimuli$Experiment == i,]$Experiment %in% balance.answer.order.exps) &
#         !all(stimuli[stimuli$Experiment == i,]$AnswerButton %in% c("leftTrigger", "rightTrigger"))){
#        cat("Error: cannot balance answer order for answer buttons other than leftTrigger or rightTrigger. Not balancing answer order for experiment ", i, ".\n", sep = "")
#        balance.answer.order.exps <- balance.answer.order.exps[balance.answer.order.exps != i]
#      }
#    }

    some.exp.has.even.number <- F
    even.number.list <- {}
    for(i in 1:nlevels(stimuli$Experiment)){
      if(max(stimuli[stimuli$Experiment == levels(stimuli$Experiment)[i],]$item_id) %% 2 == 0 &
         max(stimuli[stimuli$Experiment == levels(stimuli$Experiment)[i],]$item_condition) %% 2 == 0){
          some.exp.has.even.number <- T
          even.number.list <- append(even.number.list, i)
      }
    }

    if(some.exp.has.even.number){
      cat("Pair items for which experiments?\nWarning: to work correctly, all item-related factors must have an even number of conditions. See scriptR manual for details.\nPossibly valid experiments are the following: ", paste(even.number.list, collapse = " "), ". ", sep = "")
      paired.items.exps <- readLines("stdin", n = 1)
      if(paste(paired.items.exps, collapse = "") != ""){ if (tolower(paired.items.exps) == "all"){ paired.items.exps <- even.number.list %>% unlist } else if (paired.items.exps == "none") { paired.items.exps = "" } else { paired.items.exps <- str_split(paired.items.exps, " ") %>% unlist } }
      while(paste(paired.items.exps, collapse = "") != "" & (!all(grepl("^[1-9]*$", paired.items.exps)) | !all(paired.items.exps %in% even.number.list))) {
        cat("Invalid response. Please enter a list of experiment numbers separated by spaces, 'all', or 'none'. Possibly valid experiments are the following: ", paste(even.number.list, collapse = " "), ". ", sep = "")
        paired.items.exps <- readLines("stdin", n = 1)
        if(paste(paired.items.exps, collapse = "") != ""){ if (tolower(paired.items.exps) == "all"){ paired.items.exps <- even.number.list %>% unlist } else if (paired.items.exps == "none") { paired.items.exps = "" } else { paired.items.exps <- str_split(paired.items.exps, " ") %>% unlist } }
      }
    } else {
      paired.items.exps <- ""
    }

#    for(i in 1:nlevels(stimuli$Experiment)){
#      # If we don't have an even number of items, remove that experiment from the list
#      if(all(stimuli[stimuli$Experiment == levels(stimuli$Experiment)[i],]$Experiment %in% paired.items.exps) &
#         (max(stimuli[stimuli$Experiment == levels(stimuli$Experiment)[i],]$item_id) %% 2 != 0) &
#         (max(stimuli[stimuli$Experiment == levels(stimuli$Experiment)[i],]$item_condition) %% 2 != 0)) {
#        cat("Warning: odd number of stimuli or conditions in experiment ", i, ". Unable to pair items for this experiment.\n", sep = "")
#        # Remove that experiment from the paired items list
#        paired.items.exps <- paired.items.exps[paired.items.exps != i]
#      }
#    }

    cat("Generate sentences.txt base for sideeye? (T/F): ")
    create.sentences.txt <- readLines("stdin", n = 1)
    if(create.sentences.txt == "") { create.sentences.txt <- F } else { create.sentences.txt <- create.sentences.txt %>% toupper() %>% as.logical() }
    while(is.na(create.sentences.txt)){
      cat("Invalid response. Please enter T or F: ")
      create.sentences.txt <- readLines("stdin", n = 1)
      if(create.sentences.txt == "") { create.sentences.txt <- F } else { create.sentences.txt <- create.sentences.txt %>% toupper() %>% as.logical() }
    }

    if(create.sentences.txt){
       # Check to see if analysis regions are the same for each item in an experiment, warn if not
      for(i in 1:nlevels(stimuli$Experiment)){
        current.sentences <- stimuli[stimuli$Experiment == i,]$Sentence
        if(var(str_count(current.sentences, "/")) != 0){
          cat("Warning: Unequal numbers of analysis regions specified in experiment ", i, ".\n", sep = "")
        }
      }
   }

    create.nogaze <- F
    if (sentence.trigger != "nogaze" | question.trigger != "nogaze"){
      cat("Create nogaze version? (T/F): ")
      create.nogaze <- readLines("stdin", n = 1)
      if(create.nogaze == ""){ create.nogaze <- F } else { create.nogaze <- create.nogaze %>% toupper() %>% as.logical() }
      while(is.na(create.nogaze)){
        cat("Invalid response. Please enter T or F: ")
        create.nogaze <- readLines("stdin", n = 1)
        if(create.nogaze == ""){ create.nogaze <- F } else { create.nogaze <- create.nogaze %>% toupper() %>% as.logical() }
      }
    }

    create.test <- F
    if(sentence.button != "mouse" | !all(stimuli$AnswerButton == "mouse")){
      cat("Create testing version that uses only mouse clicks? (T/F): ")
      create.test <- readLines("stdin", n = 1)
      if(create.test == ""){ create.test <- F } else { create.test <- create.test %>% toupper() %>% as.logical() }
      while(is.na(create.test)){
        cat("Invalid response. Please enter T or F: ")
        create.test <- readLines("stdin", n = 1)
        if(create.test == ""){ create.test <- F } else { create.test <- create.test %>% toupper() %>% as.logical() }
      }
    }

    cat("Create fix_align start pts matrix? (T/F): ")
    create.fix.align.start.pts <- readLines("stdin", n = 1)
    if(create.fix.align.start.pts == ""){ create.fix.align.start.pts <- T }
    create.fix.align.start.pts <- create.fix.align.start.pts %>% toupper() %>% as.logical()
    while(is.na(create.fix.align.start.pts)){
      cat("Invalid response. Please enter T or F: ")
      create.fix.align.start.pts <- readLines("stdin", n = 1)
      if(create.fix.align.start.pts == ""){ create.fix.align.start.pts <- T } else { create.fix.align.start.pts <- create.fix.align.start.pts %>% toupper() %>% as.logical() }
    }

    if(create.fix.align.start.pts){
      if(!exists("h.pixels.per.char")){
        cat("How many horizontal pixels per character (make sure to check with your font and your monitor)? ")
        h.pixels.per.char <- readLines("stdin", n = 1)
        while(!(h.pixels.per.char) %like% "^[1-9][0-9]*$") {
          cat("Invalid response. Please enter a number not starting with 0: ")
          h.pixels.per.char <- readLines("stdin", n = 1)
        }
      }
      h.pixels.per.char <- as.numeric(as.character(h.pixels.per.char))

      if(!exists("v.pixels.per.char")){
        cat("How many vertical pixels per character (make sure to check with your font and your monitor)? ")
        v.pixels.per.char <- readLines("stdin", n = 1)
        while(!(v.pixels.per.char %like% "^[1-9][0-9]*$")) {
          cat("Invalid response. Please enter a number not starting with 0: ")
          v.pixels.per.char <- readLines("stdin", n = 1)
        }
      }
      v.pixels.per.char <- as.numeric(as.character(v.pixels.per.char))
    }

    cat("Save current settings as ", filename, "-config.csv? (T/F): ", sep = "")
    save.settings <- readLines("stdin", n = 1)
    if(save.settings == ""){ save.settings <- F } else { save.settings <- save.settings %>% toupper() %>% as.logical() }
    while(is.na(save.settings)){
      cat("Invalid response. Please enter T or F: ")
      save.settings <- readLines("stdin", n = 1)
      if(save.settings == ""){ save.settings <- F } else { save.settings <- save.settings %>% toupper() %>% as.logical() }
    }
  }
}

# Settings when using config file ####

# If using a config file, read it in and transfrom the necessary parameters
if(use.config){
  config <- read.csv(config.location)

  if(is.null(config$option) | all(config$option == "") | all(is.na(config$option))){
    cat("Error: options column missing or unspecified. ")
    .Internal(.invokeRestart(list(NULL, NULL), NULL))
  }

  config <- config %>% filter(option %in% c("background.color", "foreground.color", "calibration.type",
                                            "font", "font.size", "font.weight", "font.smoothing", "unit",
                                            "line.spacing", "x.offset", "y.offset", "sentence.button",
                                            "sentence.trigger", "sentence.delay", "sentence.output",
                                            "question.trigger", "question.delay", "question.output",
                                            "calculate.gc.params", "gc.params", "h.pixels.per.char",
                                            "v.pixels.per.char", "dcr.padding", "display.update.threshold",
                                            "display.revert", "dc.delay", "create.sentences.txt",
                                            "create.nogaze", "create.test", "create.fix.align.start.pts",
                                            "paired.items.exps", "balance.answer.order.exps"))

  if(is.null(config$setting) | all(config$setting == "") | all(is.na(config$setting))){
    cat("Warning: settings column missing or unspecified. Using all defaults.\n")
    config$setting <- ""
  }

  # If an option is specified twice
  if(length(config$option) != length(unique(config$option))){
    # Find the duplicated value
    n.occur <- data.frame(table(config$option))

    duplicates <- n.occur[n.occur$Freq > 1,]$Var1 %>% paste(collapse = ", ")

    cat("Error: option(s) specified multiple times: ", duplicates, ". ", sep = "")
    .Internal(.invokeRestart(list(NULL, NULL), NULL))
  } else {
    config <- config %>% spread(option, setting) %>% droplevels()
  }

  if(is.null(config$background.color)){ background.color <- "" } else { background.color <- config$background.color }
  if(background.color == ""){ background.color <- "white" }
  while(!(background.color %in% colors()) &
        !grepl("^((0x)|#|)[0-9A-Fa-f]{6}$", background.color)){
    cat("Invalid background color set. Please enter a valid background color: ")
    background.color <- readLines("stdin", n = 1)
    if(background.color == ""){ background.color <- "white" }
  }
  if(background.color %in% colors()){
    background.color <- background.color %>% col2hex
  }
  background.color <- background.color %>% gsub("#|(0x)", "", .) %>% strtoi(base = 16)

  if(is.null(config$foreground.color)){ foreground.color <- "" } else { foreground.color <- config$foreground.color }
  if(foreground.color == ""){ foreground.color <- "black" }
  while(!(foreground.color %in% colors()) &
        !grepl("^((0x)|#|)[0-9A-Fa-f]{6}$", foreground.color)){
    cat("Invalid text color set. Please enter a valid text color: ")
    foreground.color <- readLines("stdin", n = 1)
    if(foreground.color == ""){ foreground.color <- "black" }
  }
  if(foreground.color %in% colors()){
    foreground.color <- foreground.color %>% col2hex
  }
  foreground.color <- foreground.color %>% gsub("#|(0x)", "", .) %>% strtoi(base = 16)

  if(is.null(config$calibration.type)){ calibration.type <- "" } else { calibration.type <- config$calibration.type }
  if(calibration.type == ""){ calibration.type <- "9" }
  while(!(calibration.type %in% c("3", "9", "13"))){
    cat("Invalid number of calibration points set. Please enter one of: 3, 9, 13. ")
    calibration.type <- readLines("stdin", n = 1)
    if(calibration.type == ""){ calibration.type <- "9" }
  }
  calibration.type <- ifelse(calibration.type == "3", "0",
                             ifelse(calibration.type == "9", "1", "2"))

  if(is.null(config$font)){ font <- "" } else { font <- config$font }
  if(font == ""){ font <- "Monaco" }

  if(is.null(config$font.size)){ font.size <- "" } else { font.size <- config$font.size }
  if(font.size == ""){ font.size <- 12 }
  while(!(font.size %like% "^[1-9][0-9]*$")){
    cat("Invalid font size set. Please enter a number not starting with 0: ")
    font.size <- readLines("stdin", n = 1)
    if(font.size == ""){ font.size <- 12 }
  }

  if(is.null(config$font.weight)){ font.weight <- "" } else { font.weight <- config$font.weight }
  if(font.weight == ""){ font.weight <- "normal non-italic" }
  while(!(font.weight %in% c("normal non-italic", "normal italic", "bold non-italic", "bold italic"))){
    cat("Invalid font style set. Please choose one of: normal non-italic, normal italic, bold non-italic, bold italic. ")
    font.weight <- readLines("stdin", n = 1)
    if(font.weight == ""){ font.weight <- "normal non-italic" }
  }

  if(is.null(config$font.smoothing)){ font.smoothing <- "" } else { font.smoothing <- config$font.smoothing }
  if(font.smoothing == ""){ font.smoothing <- "nonantialiased" }
  while(!(font.smoothing %in% c("nonantialiased", "antialiased", "cleartype"))){
    cat("Invalid font smoothing option set. Please enter one of: nonantialiased, antialiased, cleartype. ")
    font.smoothing <- readLines("stdin", n = 1)
    if(font.smoothing == ""){ font.smoothing <- "nonantialiased" }
  }

  if(is.null(config$unit)){ unit <- "" } else { unit <- config$unit }
  if(unit == ""){ unit <- "px" }
  if(unit %like% "px|pix|pixel|pixels"){
    unit <- "px"
    use.pix <- T
  } else if (unit %like% "pt|pts|point|points"){
    unit <- "pt"
    use.pix <- F
  }
  while(!(unit %like% "px|pt")){
    cat("Invalid unit setting. Please enter one of: px, pt. ")
    unit <- readLines("stdin", n = 1)
    if(unit == ""){ unit <- "px" }
    if(unit %like% "px|pix|pixel|pixels"){
      unit <- "px"
      use.pix <- T
    } else if (unit %like% "pt|pts|point|points"){
      unit <- "pt"
      use.pix <- F
    }
  }

  if(is.null(config$line.spacing)){ line.spacing <- "" } else { line.spacing <- config$line.spacing }
  if(line.spacing == ""){ line.spacing <- "0" }
  while(!(line.spacing %like% "^[0-9]+$")){
    cat("Invalid line spacing (", unit, ") set. Please enter a number: ", sep = "")
    line.spacing <- readLines("stdin", n = 1)
    if(line.spacing == ""){ line.spacing <- "0" }
  }
  if(use.pix){
    line.spacing.pix <- as.numeric(as.character(line.spacing))
    line.spacing <- round(as.numeric(as.character(line.spacing))/1.32)
  } else {
    line.spacing.pix <- as.numeric(as.character(line.spacing)) * 1.32
  }

  if(is.null(config$x.offset)){ x.offset <- "" } else { x.offset <- config$x.offset }
  if(x.offset == ""){ if(use.pix) { x.offset <- "20" } else { x.offset <- "15" } }
  while(!(x.offset %like% "^[0-9]+$")){
    cat("Invalid X offset (", unit, ") set. Please enter a number: ", sep = "")
    x.offset <- readLines("stdin", n = 1)
    if(x.offset == ""){ if(use.pix){ x.offset <- "20" } else { x.offset <- "15" } }
  }
  if(use.pix){
    x.offset.pix <- as.numeric(as.character(x.offset))
    x.offset <- round(as.numeric(as.character(x.offset))/(1 + (1/3)))
  } else {
    x.offset.pix <- as.numeric(as.character(x.offset)) * (1 + (1/3))
  }

  if(is.null(config$y.offset)){ y.offset <- "" } else { y.offset <- config$y.offset }
  if(y.offset == ""){ if(use.pix) { y.offset <- "357" } else { y.offset <- "268" } }
  while(!(y.offset %like% "^[0-9]+$")){
    cat("Invalid Y offset (", unit, ") set. Please enter a number: ", sep = "")
    y.offset <- readLines("stdin", n = 1)
    if(y.offset == ""){ if(use.pix) { y.offset <- "357" } else { y.offset <- "268" } }
  }
  if (use.pix){
    y.offset.pix <- as.numeric(as.character(y.offset))
    y.offset <- round(as.numeric(as.character(y.offset))/(1 + (1/3)))
  } else {
    y.offset.pix <- as.numeric(as.character(y.offset)) * (1 + (1/3))
  }

  if(is.null(config$sentence.button)){ sentence.button <- "" } else { sentence.button <- config$sentence.button }
  if(sentence.button == ""){ sentence.button <- "rightTrigger" }
  while(!(sentence.button %in% c("A", "B", "X", "Y", "leftTrigger", "rightTrigger", "toggle", "mouse"))){
    cat("Invalid sentence button set. Please enter a valid option (A, B, X, Y, leftTrigger, rightTrigger, toggle, mouse): ")
    sentence.button <- readLines("stdin", n = 1)
    if(sentence.button == ""){ sentence.button <- "rightTrigger" }
  }

  if(is.null(config$sentence.trigger)){ sentence.trigger <- "" } else { sentence.trigger <- config$sentence.trigger }
  if(sentence.trigger == ""){ sentence.trigger <- "driftandgaze" }
  while(!(sentence.trigger %in% c("gaze", "nogaze", "driftcorrect", "driftandgaze"))) {
    cat("Invalid sentence trigger set. Please choose one of: gaze, nogaze, driftcorrect, driftandgaze. ")
    sentence.trigger <- readLines("stdin", n = 1)
    if(sentence.trigger == ""){ sentence.trigger <- "driftandgaze" }
  }

  sentence.delay <- "0"

  if(sentence.trigger %like% "nogaze"){
    if(is.null(config$sentence.delay)) { sentence.delay <- "" } else { sentence.delay <- config$sentence.delay }
    if(sentence.delay == ""){ sentence.delay <- "0" }
    while(!(sentence.delay %like% "^[0-9]*$")){
      cat("Invalid setting for sentence delay (ms). Please enter a number: ")
      sentence.delay <- readLines("stdin", n = 1)
      if(sentence.delay == ""){ sentence.delay <- "0" }
    }
  }

  if(is.null(config$sentence.output)){ sentence.output <- "" } else { sentence.output <- config$sentence.output }
  if(sentence.output == ""){ sentence.output <- "stream" }
  while(!(sentence.output %in% c("stream", "nostream"))){
    cat("Invalid setting for sentence output. Please choose one of: stream, nostream. ")
    sentence.output <- readLines("stdin", n = 1)
    if(sentence.output == ""){ sentence.output <- "stream" }
  }

  if(is.null(config$question.trigger)){ question.trigger <- "" } else { question.trigger <- config$question.trigger }
  if(question.trigger == ""){ question.trigger <- "nogaze" }
  while(!(question.trigger %in% c("gaze", "nogaze", "driftcorrect", "driftandgaze"))) {
    cat("Invalid question trigger set. Please choose one of: gaze, nogaze, driftcorrect, driftandgaze. ")
    question.trigger <- readLines("stdin", n = 1)
    if(question.trigger == ""){ question.trigger <- "nogaze" }
  }

  question.delay <- "0"

  if(question.trigger %like% "nogaze"){
    if(is.null(config$question.delay)){ question.delay <- "" } else { question.delay <- config$question.delay }
    if(question.delay == ""){ question.delay <- "0" }
    while(!(question.delay %like% "^[0-9]*$")){
      cat("Invalid setting for question delay. Please enter a number: ")
      question.delay <- readLines("stdin", n = 1)
      if(question.delay == ""){ question.delay <- "0" }
    }
  }

  if(is.null(config$question.output)) { question.output <- "" } else { question.output <- config$question.output }
  if(question.output == ""){ question.output <- "nostream" }
  while(!(question.output %in% c("stream", "nostream"))){
    cat("Invalid setting for question output. Please choose one of: stream, nostream. ")
    question.output <- readLines("stdin", n = 1)
    if(question.output == ""){ question.output <- "nostream" }
  }

  calculate.gc.params <- F

  if((sentence.trigger %like% "gaze" & !(sentence.trigger %like% "nogaze")) |
     (question.trigger %like% "gaze" & !(question.trigger %like% "nogaze"))){
    if(is.null(config$calculate.gc.params)){ calculate.gc.params <- "" } else { calculate.gc.params <- config$calculate.gc.params }
    if(calculate.gc.params == "") { calculate.gc.params <- F } else { calculate.gc.params <- calculate.gc.params %>% toupper() %>% as.logical() }
    while(is.na(calculate.gc.params)){
      cat("Invalid setting for calculating gaze control rect parameters automatically. Please enter T or F: ")
      calculate.gc.params <- readLines("stdin", n = 1)
      if(calculate.gc.params == "") { calculate.gc.params <- F } else { calculate.gc.params <- calculate.gc.params %>% toupper() %>% as.logical() }
    }
  }

  display.update.threshold <- "0"
  display.revert <- "0"
  dc.delay <- "0"

  if(display.changes){
    if(is.null(config$h.pixels.per.char)){ h.pixels.per.char <- "" } else { h.pixels.per.char <- config$h.pixels.per.char }
    while(!(h.pixels.per.char %like% "^[1-9][0-9]*$")) {
      cat("Invalid setting for horizontal pixels per character. Please enter a number not starting with 0: ")
      h.pixels.per.char <- readLines("stdin", n = 1)
    }
    h.pixels.per.char <- as.numeric(as.character(h.pixels.per.char))

    if(is.null(config$v.pixels.per.char)){ v.pixels.per.char <- "" } else { v.pixels.per.char <- config$v.pixels.per.char }
    while(!(v.pixels.per.char %like% "^[1-9][0-9]+$")) {
      cat("Invalid setting for vertical pixels per character. Please enter a number not starting with 0: ")
      v.pixels.per.char <- readLines("stdin", n = 1)
    }
    v.pixels.per.char <- as.numeric(as.character(v.pixels.per.char))

    if(is.null(config$dcr.padding)){ dcr.padding <- "" } else { dcr.padding <- config$dcr.padding }
    if(dcr.padding == "") { if(use.pix){ dcr.padding <- line.spacing.pix } else { dcr.padding <- line.spacing } } else if (dcr.padding == "line.spacing" | dcr.padding == "line spacing"){ if(use.pix){ dcr.padding <- line.spacing.pix } else {dcr.padding <- line.spacing } }
    while(!(dcr.padding %like% "^[0-9]+$")){
      cat("Invalid setting for display change region vertical padding (", unit, "). Please enter a number: ", sep = "")
      dcr.padding <- readLines("stdin", n = 1)
      if(dcr.padding == "") { if(use.pix){ dcr.padding <- line.spacing.pix } else { dcr.padding <- line.spacing } } else if (dcr.padding == "line.spacing" | dcr.padding == "line spacing"){ if(use.pix){ dcr.padding <- line.spacing.pix } else {dcr.padding <- line.spacing } }
    }
    if(use.pix){
      dcr.padding.pix <- as.numeric(as.character(dcr.padding))
      dcr.padding <- round(as.numeric(as.character(dcr.padding))/1.32)
    }
    else {
      dcr.padding.pix <- as.numeric(as.character(dcr.padding)) * 1.32
    }

    if(is.null(config$display.update.threshold)) { display.update.threshold <- "" } else { display.update.threshold <- config$display.update.threshold }
    if(display.update.threshold == "") { display.update.threshold <- "0" }
    while(!(display.update.threshold %like% "^[0-9]*$")){
      cat("Invalid setting for display update threshold. Please enter a number: ")
      display.update.threshold <- readLines("stdin", n = 1)
      if(display.update.threshold == ""){ display.update.threshold <- "0" }
    }

    if(is.null(config$display.revert)) { display.revert <- "" } else { display.revert <- config$display.revert }
    if(display.revert == ""){ display.revert <- F }
    display.revert <- display.revert %>% toupper() %>% as.logical()
    while(is.na(display.revert)){
      cat("Invalid setting for whether to revert display changes. Please enter T or F: ")
      display.revert <- readLines("stdin", n = 1)
      if(display.revert == ""){ display.revert <- F } else { display.revert <- display.revert %>% toupper() %>% as.logical() }
    }

    if(display.revert){
      if(is.null(config$dc.delay)) { dc.delay <- "" } else { dc.delay <- config$dc.delay }
      if(dc.delay == ""){ dc.delay <- "0" }
      while(!(dc.delay %like% "^[0-9]*$")){
        cat("Invalid setting for display change reversion delay. Please enter a number: ")
        dc.delay <- readLines("stdin", n = 1)
        if(dc.delay == ""){ dc.delay <- "0" }
      }
    }

    if(display.revert){ display.revert <- "1" } else { display.revert <- "0" }

  }

  if (calculate.gc.params){
    if(!exists("h.pixels.per.char")){ if(is.null(config$h.pixels.per.char)){ h.pixels.per.char <- "" } else { h.pixels.per.char <- config$h.pixels.per.char } }
    while(!(h.pixels.per.char %like% "^[0-9]+$")) {
      cat("Horizontal pixels per character must be set to automatically calculate gc rect params. Please enter a number: ")
      h.pixels.per.char <- readLines("stdin", n = 1)
      while(!(h.pixels.per.char %like% "^[1-9][0-9]*$")) {
        cat("Invalid response. Please enter a number not starting with 0: ")
        h.pixels.per.char <- readLines("stdin", n = 1)
      }
    }
    h.pixels.per.char <- as.numeric(as.character(h.pixels.per.char))

    if(!exists("v.pixels.per.char")){ if(is.null(config$v.pixels.per.char)){ v.pixels.per.char <- "" } else { v.pixels.per.char <- config$v.pixels.per.char } }
    while(!(v.pixels.per.char %like% "^[0-9]+$")) {
      cat("Vertical pixels per character must be set to automatically calculate gc rect params. Please enter a number: ")
      v.pixels.per.char <- readLines("stdin", n = 1)
      while(!(v.pixels.per.char %like% "^[1-9][0-9]*$")) {
        cat("Invalid response. Please enter a number not starting with 0: ")
        v.pixels.per.char <- readLines("stdin", n = 1)
      }
    }
    v.pixels.per.char <- as.numeric(as.character(v.pixels.per.char))

    gc.params <- paste(sep = " ",
                       round(y.offset.pix + ((line.spacing.pix+v.pixels.per.char)/2) - (v.pixels.per.char * 2)),
                       round(x.offset.pix),
                       round(y.offset.pix + ((line.spacing.pix+v.pixels.per.char)/2) + (v.pixels.per.char * 2)),
                       round(x.offset.pix + (h.pixels.per.char*4)))

  } else {
    if(is.null(config$gc.params)){ gc.params <- "" } else { gc.params <- config$gc.params }
    if(gc.params == ""){ gc.params <- "0 0 0 0" }
    while(!(gc.params %like% "^([0-9]+ ){3}[0-9]+$")){
      cat("Invalid gaze control rect parameters set. Please enter four numbers separated by spaces to define the size of the gaze control box in the format Y1 X1 Y2 X2: ")
      gc.params <- readLines("stdin", n = 1)
      if(gc.params == ""){ gc.params <- "0 0 0 0" }
    }
  }

  some.exp.uses.only.left.and.right <- F
  left.or.right.list <- {}
  for(i in 1:nlevels(stimuli$Experiment)){
    if(all(stimuli[stimuli$Experiment == levels(stimuli$Experiment)[i],]$AnswerButton %in% c("leftTrigger", "rightTrigger"))){
      some.exp.uses.only.left.and.right <- T
      left.or.right.list <- append(left.or.right.list, i)
    }
  }

  if(some.exp.uses.only.left.and.right){
    if(is.null(config$balance.answer.order.exps)){ balance.answer.order.exps <- "" } else { balance.answer.order.exps <- config$balance.answer.order.exps }
    if(paste(balance.answer.order.exps, collapse = "") != "") { if(tolower(balance.answer.order.exps) == "all"){ balance.answer.order.exps <- left.or.right.list %>% unlist } else if (balance.answer.order.exps == "none"){ balance.answer.order.exps <- "" } else { balance.answer.order.exps <- str_split(balance.answer.order.exps, " ") %>% unlist } }
    while(paste(balance.answer.order.exps, collapse = "") != "" & (!all(grepl("^[1-9]*$", balance.answer.order.exps)) | !all(balance.answer.order.exps %in% left.or.right.list))){
      cat("Invalid setting for which experiments to balance answer order for. Please enter a list of experiment numbers separated by spaces, 'all', or 'none'. Valid experiments are the following: ", paste(left.or.right.list, collapse = " "), ". ", sep = "")
      balance.answer.order.exps <- readLines("stdin", n = 1)
      if(paste(balance.answer.order.exps, collapse = "") != "") { if(tolower(balance.answer.order.exps) == "all"){ balance.answer.order.exps <- left.or.right.list %>% unlist } else if (balance.answer.order.exps == "none"){ balance.answer.order.exps <- "" } else { balance.answer.order.exps <- str_split(balance.answer.order.exps, " ") %>% unlist } }
    }
  } else {
    if(!is.null(config$balance.answer.order)){
      cat("Invalid setting for which experiments to balance answer orders for; no experiment uses only leftTrigger or rightTrigger. Cannot balance answer orders.\n")
    }
    balance.answer.order.exps <- ""
  }

#  for(i in 1:nlevels(stimuli$Experiment)){
#    if(all(stimuli[stimuli$Experiment == i,]$Experiment %in% balance.answer.order.exps) &
#       !all(stimuli[stimuli$Experiment == i,]$AnswerButton %in% c("leftTrigger", "rightTrigger"))){
#      cat("Error: cannot balance answer order for answer buttons other than leftTrigger or rightTrigger. Not balancing answer order for experiment ", i, ".\n", sep = ".")
#      balance.answer.order.exps <- balance.answer.order.exps[balance.answer.order.exps != i]
#    }
#  }

  some.exp.has.even.number <- F
  even.number.list <- {}
  for(i in 1:nlevels(stimuli$Experiment)){
    if(max(stimuli[stimuli$Experiment == levels(stimuli$Experiment)[i],]$item_id) %% 2 == 0 &
       max(stimuli[stimuli$Experiment == levels(stimuli$Experiment)[i],]$item_condition) %% 2 == 0){
      some.exp.has.even.number <- T
      even.number.list <- append(even.number.list, i)
    }
  }

  if(some.exp.has.even.number){
    if(is.null(config$paired.items.exps)){ paired.items.exps <- "" } else { paired.items.exps <- config$paired.items.exps }
    if(paste(paired.items.exps, collapse = "") != ""){ if (tolower(paired.items.exps) == "all"){ paired.items.exps <- even.number.list %>% unlist } else if (paired.items.exps == "none") { paired.items.exps = "" } else { paired.items.exps <- str_split(paired.items.exps, " ") %>% unlist } }
    while(paste(paired.items.exps, collapse = "") != "" & (!all(grepl("^[1-9]*$", paired.items.exps)) | !all(paired.items.exps %in% even.number.list))) {
      cat("Invalid setting for which experiments to pair items for.\nWarning: to work correctly, all item-related factors must have an even number of conditions. See scriptR manual for details.\nPlease enter a list of experiment numbers separated by spaces, 'all', or 'none'. Possibly valid experiments are the following: ", paste(even.number.list, collapse = " "), ". ", sep = "")
      paired.items.exps <- readLines("stdin", n = 1)
      if(paste(paired.items.exps, collapse = "") != ""){ if (tolower(paired.items.exps) == "all"){ paired.items.exps <- even.number.list %>% unlist } else if (paired.items.exps == "none") { paired.items.exps = "" } else { paired.items.exps <- str_split(paired.items.exps, " ") %>% unlist } }
    }
  } else {
    if(!is.null(config$paired.items.exps)){
      cat("Invalid setting for which experiments have paired items; no experiment has an even number of items with an even number of conditions. Cannot pair any items.\n")
    }
    paired.items.exps <- ""
  }

#  for(i in 1:nlevels(stimuli$Experiment)){
#    # If we don't have an even number of items, remove that experiment from the list
#    if(all(stimuli[stimuli$Experiment == levels(stimuli$Experiment)[i],]$Experiment %in% paired.items.exps) &
#       (max(stimuli[stimuli$Experiment == levels(stimuli$Experiment)[i],]$item_id) %% 2 != 0) &
#       (max(stimuli[stimuli$Experiment == levels(stimuli$Experiment)[i],]$item_condition) %% 2 != 0)) {
#      cat("Warning: odd number of stimuli or conditions in experiment ", i, ". Unable to pair items for this experiment.\n", sep = "")
#      # Remove that experiment from the paired items list
#      paired.items.exps <- paired.items.exps[paired.items.exps != i]
#    }
#  }

  if(is.null(config$create.sentences.txt)){ create.sentences.txt <- "" } else { create.sentences.txt <- config$create.sentences.txt }
  if(create.sentences.txt == ""){ create.sentences.txt <- F }
  create.sentences.txt <- create.sentences.txt %>% toupper() %>% as.logical()
  while(is.na(create.sentences.txt)){
    cat("Invalid setting for whether to create a sentences.txt base for sideeye. Please enter T or F: ")
    create.sentences.txt <- readLines("stdin", n = 1)
    if(create.sentences.txt == ""){ create.sentences.txt <- F } else { create.sentences.txt <- create.sentences.txt %>% toupper() %>% as.logical() }
  }

  # Check to see if analysis regions are the same for each item in an experiment, warn if not
  for(i in 1:nlevels(stimuli$Experiment)){
    current.sentences <- stimuli[stimuli$Experiment == i,]$Sentence
    if(var(str_count(current.sentences, "/")) != 0){
      cat("Warning: Unequal numbers of analysis regions specified in experiment ", i, ".\n", sep = "")
    }
  }

  if(is.null(config$create.nogaze)){ create.nogaze <- "" } else { create.nogaze <- config$create.nogaze }
  if(create.nogaze == ""){ create.nogaze <- F }
  create.nogaze <- create.nogaze %>% toupper() %>% as.logical()
  while(is.na(create.nogaze)){
    cat("Invalid setting for whether to create a nogaze version of the script. Please enter T or F: ")
    create.nogaze <- readLines("stdin", n = 1)
    if(create.nogaze == ""){ create.nogaze <- F } else { create.nogaze <- create.nogaze %>% toupper() %>% as.logical() }
  }

  if(is.null(config$create.test)){ create.test <- "" } else { create.test <- config$create.test }
  if(create.test == ""){ create.test <- F }
  create.test <- create.test %>% toupper() %>% as.logical()
  while(is.na(create.test)){
    cat("Invalid setting for whether to create a script for testing that only uses mouse clicks. Please enter T or F: ")
    create.test <- readLines("stdin", n = 1)
    if(create.test == ""){ create.test <- F } else { create.test <- create.test %>% toupper() %>% as.logical() }
  }

  if(is.null(config$create.fix.align.start.pts)) { create.fix.align.start.pts <- "" } else { create.fix.align.start.pts <- config$create.fix.align.start.pts }
  if(create.fix.align.start.pts == ""){ create.fix.align.start.pts <- T }
  create.fix.align.start.pts <- create.fix.align.start.pts %>% toupper() %>% as.logical()
  while(is.na(create.fix.align.start.pts)){
    cat("Invalid setting for whether to create a fix_align start pts matrix. Please enter T or F: ")
    create.fix.align.start.pts <- readLines("stdin", n = 1)
    if(create.fix.align.start.pts == ""){ create.fix.align.start.pts <- T } else { create.fix.align.start.pts <- create.fix.align.start.pts %>% toupper() %>% as.logical() }
  }

  if(create.fix.align.start.pts){
    if(!exists("h.pixels.per.char")){
      cat("Horizontal pixels per character must be set to create a fix_align start pts matrix. Please enter a number: ")
      h.pixels.per.char <- readLines("stdin", n = 1)
      while(!(h.pixels.per.char) %like% "^[1-9][0-9]*$") {
        cat("Invalid response. Please enter a number not starting with 0: ")
        h.pixels.per.char <- readLines("stdin", n = 1)
      }
    }
    h.pixels.per.char <- as.numeric(as.character(h.pixels.per.char))

    if(!exists("v.pixels.per.char")){
      cat("Vertical pixels per character must be set to create a fix_align start pts matrix. Please enter a number: ")
      v.pixels.per.char <- readLines("stdin", n = 1)
      while(!(v.pixels.per.char %like% "^[1-9][0-9]*$")) {
        cat("Invalid response. Please enter a number not starting with 0: ")
        v.pixels.per.char <- readLines("stdin", n = 1)
      }
    }
    v.pixels.per.char <- as.numeric(as.character(v.pixels.per.char))
  }

  save.settings <- F
}

# Balance the position of the answers if desired ####
if(paste(balance.answer.order.exps, collapse = "") != ""){

    balanced.qs.stimuli <- data.frame()

    # Square the question answer order ####
    for(i in 1:nlevels(stimuli$Experiment)){
        current.exp <- stimuli %>% filter(Experiment == i)

        if(all(current.exp$Experiment %in% balance.answer.order.exps)){
          extra.qs <- current.exp

          extra.qs$AnswerButton <- ifelse(current.exp$AnswerButton == "leftTrigger", "rightTrigger", "leftTrigger")

          extra.qs$item_condition <- extra.qs$item_condition + max(current.exp$item_condition)
          extra.qs <- extra.qs %>%
            transform(LeftAnswer = RightAnswer,
                      RightAnswer = LeftAnswer)
          balanced.qs.stimuli <- rbind(balanced.qs.stimuli, current.exp, extra.qs)
        } else {
          balanced.qs.stimuli <- rbind(balanced.qs.stimuli, current.exp)
        }
    }
    stimuli <- balanced.qs.stimuli %>% arrange(Experiment, item_id, item_condition)
}

# Square items correctly if they are paired ####
paired.squared <- data.frame()

# Check the data
if(paste(paired.items.exps, collapse = "") != ""){

  paired.stimuli <- data.frame()

  for(i in 1:nlevels(stimuli$Experiment)) {
    current.exp <- stimuli %>% filter(Experiment == i) %>% droplevels()

    if(all(current.exp$Experiment %in% paired.items.exps)){
      # Loop through each item, and reverse the order of the even numbered ones, excepting the first item_condition
      for(j in 1:(max(current.exp$item_id)/2)){
        current.item <- current.exp %>%
          filter(item_id == j * 2)

        resorted <- current.item[-1,] %>%
          arrange(desc(item_condition))

        resorted$item_condition <- c(2:(nrow(resorted)+1))

        current.item <- rbind(current.item[1,], resorted)

        paired.stimuli <- rbind(current.exp %>% filter(item_id == j * 2 - 1), current.item)
        paired.squared <- rbind(paired.squared, paired.stimuli)
      }
    } else {
        paired.squared <- rbind(paired.squared, current.exp)
    }
  }
  stimuli <- paired.squared
}

# Renumber item_conditions and items to fit into the EyeTrack format ####
# If there's more than one experiment, renumber item_conditions and items
if(nlevels(stimuli$Experiment) > 1){
  for(i in 1:(nlevels(stimuli$Experiment)-1)){
    stimuli[stimuli$Experiment == i + 1,]$item_condition <- (stimuli %>% filter(Experiment == i + 1))$item_condition + max((stimuli %>% filter(Experiment == i))$item_condition)
    stimuli[stimuli$Experiment == i + 1,]$item_id <- (stimuli %>% filter(Experiment == i + 1))$item_id + max((stimuli %>% filter(Experiment == i))$item_id)
  }
}

# Format the items for the EyeTrack script
formatted.stimuli <- {}

for(i in 1:nrow(stimuli)) {
  if(!(stimuli[i,]$Sentence == "")) {
    sentence.trial <- paste(sep = "", "E", stimuli[i,]$item_condition, "I", stimuli[i,]$item_id, "D0")

    # If there's no display change
    if(stimuli[i,]$Sentence2 == "" | !display.changes) {
    current.sentence <- paste(sep = "",
                              "trial ", sentence.trial, "\n",
                                "\tgc_rect =\t(", gc.params, ")\n",
                                "\tinline =\t|", gsub("/", "", stimuli[i,]$Sentence), "\\n\n",
                                "\tmax_display_time =\t", stimuli[i,]$Timeout, "\n",
                                "\ttrial_type =\tsentence\n",
                              "end ", sentence.trial)
    } else if(display.changes) { # If there is a display change (thanks to Adrian Staub's scripter2 for how to approach this part)
      # Find the display change region
      # Get the area of interest by splitting on %s
      display.change.region <- regmatches(stimuli[i,]$Sentence2, gregexpr("%.*?%", stimuli[i,]$Sentence2)) %>% unlist()
      region.params <- ""

      current.sentence <- stimuli[i,]$Sentence2

      # If there actually is something that changes
      if(!(length(display.change.region) == 0)){
        for(j in 1:length(display.change.region)){
          # Split the sentence into lines and find the line that the area of interest appears on
          target.lines <- str_split(current.sentence, "\\\\n") %>% unlist()
          display.change.line <- 1
          while(!(grepl(display.change.region[j], target.lines[display.change.line]))){
            display.change.line <- display.change.line + 1
          }

          # Get the starting and ending position of the area of interest in chars in the target line
          display.change.chars <- str_locate(target.lines[display.change.line],
                                             display.change.region[j]) %>%
            as.data.frame

          # Subtract from both to correct for the % characters
          display.change.chars$start <- display.change.chars$start - 1
          display.change.chars$end <- display.change.chars$end - 2

          # Get the region numbers: start at y/x offsets, and add number of lines/chars times the number of pixels per each
          region.y1 <- round(y.offset.pix + ((line.spacing.pix+v.pixels.per.char)/2) - # Start at the midline of the text of the first line
                               ((dcr.padding.pix/2)+(v.pixels.per.char/2)) + # Subtract half the pixel height of a character to get to the top of the box
                               ((display.change.line-1)*10) + # If our line is after the first line, add the actual space between lines in pixels when line spacing is set to 0 for each line
                               ((display.change.line-1) * v.pixels.per.char) + # If our line is after the first line, add character height for each additional line
                               ((display.change.line-1) * line.spacing.pix)) # If our line is after the first line, add the amount of line spacing in pixels per additional line

          region.y2 <- round(y.offset.pix + ((line.spacing.pix+v.pixels.per.char)/2) + # Start at the midline of the text of the first line
                               ((dcr.padding.pix/2)+(v.pixels.per.char/2)) + # Add half the pixel height of a character to get to the bottom of the box
                               ((display.change.line-1)*10) + # If our line is after the first line, add the actual space between lines in pixels when line spacing is set to 0 for each line
                               ((display.change.line-1) * v.pixels.per.char) + # If our line is after the first line, add character height for each additional line
                               ((display.change.line-1) * line.spacing.pix)) # If our line is after the first line, add the amount of line spacing in pixels per additional line

          region.x1 <- round(x.offset.pix + (display.change.chars$start*h.pixels.per.char)) # Start with the x offset in pixels, and add the number of pixels for each character before the display change region
          region.x2 <- round(x.offset.pix + (display.change.chars$end*h.pixels.per.char)) # Start with the x offset in pixels, and add the number of pixels for each character to the end of the display change region

          # Paste them together to add to the sentence
          region.params <- paste(sep = "", region.params, "(", region.y1, " ", region.x1, " ", region.y2, " ", region.x2, ")")

          # Remove percentage markers from the sentence to line up correctly if there are multiple display change regions on the same line
          current.sentence <- sub(display.change.region[j], gsub("%", "", display.change.region[j]), current.sentence)
        }
      } else {
        cat("Error: trial ", i, " doesn't specify display change region in Sentence2. ", sep = "")
        .Internal(.invokeRestart(list(NULL, NULL), NULL))
      }

      current.sentence <- paste(sep = "",
                                "trial ", sentence.trial, "\n",
                                  "\tgc_rect =\t(", gc.params, ")\n",
                                  "\tinline =\t|", gsub("/", "", stimuli[i,]$Sentence), "\\n\n",
                                  "\tinline =\t|", gsub("%", "", stimuli[i,]$Sentence2), "\\n\n",
                                  "\tmax_display_time =\t", stimuli[i,]$Timeout, "\n",
                                  "\tregion =\t", region.params, "\n",
                                  "\ttrial_type = \tsentence\n",
                                "end ", sentence.trial)
    }

    question.trial <- ""
    current.question <- ""

    if(!(stimuli[i,]$Question == "")){
      question.trial <- paste(sep = "", "E", stimuli[i,]$item_condition + 100, "I", stimuli[i,]$item_id,"D", stimuli[i,]$item_condition)
      current.question <- paste(sep = "",
                                "trial ", question.trial, "\n",
                                "\tbutton =\t", stimuli[i,]$AnswerButton, "\n",
                                "\tinline =\t|", stimuli[i,]$Question, "\\n\\n", tolower(stimuli[i,]$LeftAnswer), "     ", tolower(stimuli[i,]$RightAnswer), "\\n\n",
                                "\tmax_display_time =\t", stimuli[i,]$QTimeout, "\n",
                                "\ttrial_type =\tquestion\n",
                                "end ", question.trial)

      sequence <- paste(sep = "",
                        "sequence SE", stimuli[i,]$item_condition, "I", stimuli[i,]$item_id, "\n",
                        "\t", sentence.trial, "\n",
                        "\t", question.trial, "\n",
                        "end SE", stimuli[i,]$item_condition, "I", stimuli[i,]$item_id)

    }

    if(!(current.question == "")) {
      formatted.item <- paste(sep = "", current.sentence, "\n\n", current.question, "\n\n", sequence, "\n")
    } else {
      formatted.item <- paste(sep = "", current.sentence, "\n")
    }

    formatted.stimuli <- append(formatted.stimuli, formatted.item)
  }
}

# Generate the header ####

# Get number of item_conditions per experiment
exp.item_condition <- ""

for(i in 1:nlevels(stimuli$Experiment)){
  current.exp <- stimuli %>% filter(Experiment == i) %>% droplevels()
  current.exp$item_condition <- as.factor(current.exp$item_condition) %>% droplevels()
  exp.item_condition <- paste(exp.item_condition, nlevels(current.exp$item_condition))
}

# Get a list of all answerbuttons for questions
if(exists("practice")){
  answer.buttons <- unique(c(levels(stimuli$AnswerButton), levels(practice$AnswerButton)))
} else {
  answer.buttons <- levels(stimuli$AnswerButton)
}

answer.buttons.msg <- ""

for(i in 1:length(answer.buttons)){
  current.msg <- paste(sep = "", "\tbutton =\t", answer.buttons[i], "\n")
  answer.buttons.msg <- append(answer.buttons.msg, current.msg)
}

answer.buttons.msg <- answer.buttons.msg[-1] %>% paste(collapse = "")

fa.start.pts <- ""

if(create.fix.align.start.pts){
  # Get the number of lines
  num.lines <- max(str_count(stimuli$Sentence, "\\\\n"), str_count(stimuli$Sentence2, "\\\\n")) + 1

  fa.start.pts <- {}

  for(i in 1:num.lines){
    fa.x <- round(x.offset.pix) # Get the starting x position of a line

    fa.y <- round(y.offset.pix +
                     ((line.spacing.pix + v.pixels.per.char)/2) + # Start at the midline of the text of the first line
                     ((i-1) * 10) + # If our line is after the first line, add the actual space between lines in pixels when line spacing is set to 0 for each line
                     ((i-1) * v.pixels.per.char) + # If our line is after the first line, add character height for each additional line
                     ((i-1) * line.spacing.pix)) # If our line is after the first line, add the amount of line spacing in pixels per additional line

    fa.start.pts[[i]] <- paste(sep = "", "c(", fa.x, ",", fa.y, ")")
  }

  fa.start.pts <- paste(fa.start.pts, collapse = ", ") %>% paste("\tfix_align start pts matrix:\tstart_pts = rbind(", ., ")", sep = "")
}

header <- paste(sep = "",
                "%BeginHeader\n", fa.start.pts, "\n%EndHeader\n\n",

                "set conditions = ", max(stimuli$item_condition), "\n",
                "set experiments = ", max(as.numeric(stimuli$Experiment)), "\n",
                "set expConditions = ", exp.item_condition, "\n",
                "set background = ", background.color, "\n",
                "set foreground = ", foreground.color, "\n",
                "set filterMode = 2\n",
                "set windowThreshold = ", display.update.threshold, "\n",
                "set calibration_type = ", calibration.type, "\n",
                "set calibration_height = 775368240\n",
                "set display_type = LCD\n\n",

                "trial_type question\n",
                  "\ttext_format =\t'", font, "' ", font.size, " ", line.spacing, " ", x.offset, " ", y.offset, " ", font.smoothing, "\n",
                  "\ttext_weight =\t", font.weight, "\n",
                  answer.buttons.msg,
                  "\toutput =\t", question.output, "\n",
                  "\ttrigger =\t", question.trigger, "\n",
                  "\tcursor_size =\t0\n",
                  "\tdc_delay =\t", dc.delay, "\n",
                  "\tstimulus_delay =\t", question.delay, "\n",
                  "\trevert =\t", display.revert, "\n",
                  "\thighlight_color =\t591636\n",
                "end question\n\n",

                "trial_type sentence\n",
                  "\ttext_format =\t'", font, "' ", font.size, " ", line.spacing, " ", x.offset, " ", y.offset, " ", font.smoothing, "\n",
                  "\ttext_weight =\t", font.weight, "\n",
                  "\tbutton =\t", sentence.button, "\n",
                  "\toutput =\t", sentence.output, "\n",
                  "\ttrigger =\t", sentence.trigger, "\n",
                  "\tcursor_size =\t0\n",
                  "\tdc_delay =\t", dc.delay, "\n",
                  "\tstimulus_delay =\t", sentence.delay, "\n",
                  "\trevert =\t", display.revert, "\n",
                  "\thighlight_color =\t591636\n",
                "end sentence\n\n",

                "trial_type Message\n",
                  "\ttext_format =\t'", font, "' ", font.size, " ", line.spacing, " ", x.offset, " ", y.offset, " ", font.smoothing, "\n",
                  "\ttext_weight =\t", font.weight, "\n",
                  "\tbutton =\tY\n",
                  "\tbutton =\tX\n",
                  "\tbutton =\tB\n",
                  "\tbutton =\tA\n",
                  "\tbutton =\ttoggle\n",
                  "\tbutton =\tleftTrigger\n",
                  "\tbutton =\trightTrigger\n",
                  "\toutput =\tnostream\n",
                  "\ttrigger =\tnogaze\n",
                  "\tcursor_size =\t0\n",
                  "\tdc_delay =\t0\n",
                  "\tstimulus_delay =\t0\n",
                  "\trevert = \t0\n",
                  "\thighlight_color =\t197148\n",
                "end Message\n")


# Practice items ####
practice.trials <- ""
practice.sequence <- ""

if(nrow(practice) >= 1){
  for(i in 1:nrow(practice)){
    current.trial <- paste(sep = "", "P1I1D", i-1)
    if(practice[i,]$item_id == "Message"){

      practice.lines <- str_split(practice[i,]$Sentence, "\\\\n") %>% unlist()

      for(j in 1:length(practice.lines)){
        practice.lines[j] <- paste(sep = "", gsub("(.{67}?)\\s", "\\1\\\\n", practice.lines[j]), "\\n")
      }

      practice.message <- gsub("\\\\n$", "", paste(practice.lines, collapse = ""))

      practice.trials <- practice.trials %>%
        append(., paste(sep = "",
                        "trial ", current.trial, "\n",
                          "\tgc_rect =\t(", gc.params, ")\n",
                          "\tinline =\t|", practice.message, "\\n\n",
                          "\tmax_display_time = \t", practice[i,]$Timeout, "\n",
                          "\ttrial_type =\t", practice[i,]$item_id, "\n",
                        "end ", current.trial, "\n"))
      practice.sequence <- practice.sequence %>%
        paste(sep = "", ., "\t", current.trial, "\n")
    } else {
      if(practice[i,]$Sentence2 == "" | !display.changes){
        practice.trials <- practice.trials %>%
          append(., paste(sep = "",
                          "trial ", current.trial, "\n",
                            "\tgc_rect =\t(", gc.params, ")\n",
                            "\tinline =\t|", practice[i,]$Sentence, "\\n\n",
                            "\tmax_display_time =\t", practice[i,]$Timeout, "\n",
                            "\ttrial_type = sentence\n",
                          "end ", current.trial, "\n"))
        practice.sequence <- practice.sequence %>%
          paste(sep = "", ., "\t", current.trial, "\n")
      } else if(display.changes) { # If there is a display change (thanks to Adrian Staub's scripter2 for how to approach this part)
        # Find the display change region
        # Get the area of interest by splitting on %s
        display.change.region <- regmatches(stimuli[i,]$Sentence2, gregexpr("%.*?%", stimuli[i,]$Sentence2)) %>% unlist()
        region.params <- ""

        current.sentence <- stimuli[i,]$Sentence2

        if(!(length(display.change.region) == 0)){
          for(j in 1:length(display.change.region)){
            # Split the sentence into lines and find the line that the area of interest appears on
            target.lines <- str_split(current.sentence, "\\\\n") %>% unlist()
            display.change.line <- 1
            while(!(grepl(display.change.region[j], target.lines[display.change.line]))){
              display.change.line <- display.change.line + 1
            }

            # Get the starting and ending position of the area of interest in chars in the target line
            display.change.chars <- str_locate(target.lines[display.change.line],
                                               display.change.region[j]) %>%
              as.data.frame

            # Subtract from both to correct for the % characters
            display.change.chars$start <- display.change.chars$start - 1
            display.change.chars$end <- display.change.chars$end - 2

            # Get the region numbers: start at y/x offsets, and add number of lines/chars times the number of pixels per each
            region.y1 <- round(y.offset.pix + ((line.spacing.pix+v.pixels.per.char)/2) - # Start at the midline of the text of the first line
                                 ((dcr.padding.pix/2)+(v.pixels.per.char/2)) + # Subtract half the pixel height of a character to get to the top of the box
                                 ((display.change.line-1)*10) + # If our line is after the first line, add the actual space between lines in pixels when line spacing is set to 0 for each line
                                 ((display.change.line-1) * v.pixels.per.char) + # If our line is after the first line, add character height for each additional line
                                 ((display.change.line-1) * line.spacing.pix)) # If our line is after the first line, add the amount of line spacing in pixels per additional line

            region.y2 <- round(y.offset.pix + ((line.spacing.pix+v.pixels.per.char)/2) + # Start at the midline of the text of the first line
                                 ((dcr.padding.pix/2)+(v.pixels.per.char/2)) + # Add half the pixel height of a character to get to the bottom of the box
                                 ((display.change.line-1)*10) + # If our line is after the first line, add the actual space between lines in pixels when line spacing is set to 0 for each line
                                 ((display.change.line-1) * v.pixels.per.char) + # If our line is after the first line, add character height for each additional line
                                 ((display.change.line-1) * line.spacing.pix)) # If our line is after the first line, add the amount of line spacing in pixels per additional line

            region.x1 <- round(x.offset.pix + (display.change.chars$start*h.pixels.per.char)) # Start with the x offset in pixels, and add the number of pixels for each character before the display change region
            region.x2 <- round(x.offset.pix + (display.change.chars$end*h.pixels.per.char)) # Start with the x offset in pixels, and add the number of pixels for each character to the end of the display change region

            # Paste them together to add to the sentence
            region.params <- paste(sep = "", region.params, "(", region.y1, " ", region.x1, " ", region.y2, " ", region.x2, ")")

            # Remove percentage markers from the sentence to line up correctly if there are multiple display change regions on the same line
            current.sentence <- sub(display.change.region[j], gsub("%", "", display.change.region[j]), current.sentence)
          }
        } else {
          cat("Error: trial ", i, " doesn't specify display change region in Sentence2. ", sep = "")
          .Internal(.invokeRestart(list(NULL, NULL), NULL))
        }

        practice.trials <- practice.trials %>%
          append(., paste(sep = "",
                          "trial ", current.trial, "\n",
                            "\tgc_rect =\t(", gc.params, ")\n",
                            "\tinline =\t|", practice[i,]$Sentence, "\\n\n",
                            "\tinline =\t|", gsub("%", "", practice[i,]$Sentence2), "\\n\n",
                            "\tmax_display_time =\t", practice[i,]$Timeout, "\n",
                            "\tregion =\t(", region.params, ")\n",
                            "\ttrial_type = \tsentence\n",
                          "end ", current.trial))
      }

      question.trial <- ""
      if(practice[i,]$Question != ""){
        question.trial <- paste(sep = "", "P1I1D", i+100)
        practice.trials <- practice.trials %>%
          append(., paste(sep = "",
                                  "trial ", question.trial, "\n",
                                  "\tbutton =\t", practice[i,]$AnswerButton, "\n",
                                  "\tinline =\t|", practice[i,]$Question, "\\n\\n", practice[i,]$LeftAnswer, "     ", practice[i,]$RightAnswer, "\\n\n",
                                  "\tmax_display_time =\t", practice[i,]$QTimeout, "\n",
                                  "\ttrial_type =\tquestion\n",
                                  "end ", question.trial, "\n"))
        practice.sequence <- practice.sequence %>%
          paste(sep = "", ., "\t", question.trial, "\n")
      }
    }
  }

  practice.trials <- practice.trials[-1]

  practice.sequence <- practice.sequence %>%
    paste(sep = "",
          "sequence SP1I1\n",
          .,
          "end SP1I1\n")

  practice.trials <- practice.trials %>%
    append(practice.sequence)
} else if (nrow(practice) == 1){
  current.trial <- paste(sep = "", "P1I1D0")

  if(practice[1,]$item_id == "Message"){

    practice.lines <- str_split(practice[1,]$Sentence, "\\\\n") %>% unlist()

    for(j in 1:length(practice.lines)){
      practice.lines[j] <- paste(sep = "", gsub("(.{67}?)\\s", "\\1\\\\n", practice.lines[j]), "\\n")
    }

    practice.message <- gsub("\\\\n$", "", paste(practice.lines, collapse = ""))

    practice.trials <- practice.trials %>%
      append(., paste(sep = "",
                      "trial ", current.trial, "\n",
                      "\tgc_rect =\t(", gc.params, ")\n",
                      "\tinline =\t|", practice.message, "\\n\n",
                      "\tmax_display_time = \t", practice[1,]$Timeout, "\n",
                      "\ttrial_type =\t", practice[1,]$item_id, "\n",
                      "end ", current.trial, "\n"))
    practice.sequence <- practice.sequence %>%
      paste(sep = "", ., "\t", current.trial, "\n")
  } else {
    if(practice[1,]$Sentence2 == "" | !display.changes){
      practice.trials <- practice.trials %>%
        append(., paste(sep = "",
                        "trial ", current.trial, "\n",
                          "\tgc_rect =\t(", gc.params, ")\n",
                          "\tinline =\t|", practice[1,]$Sentence, "\\n\n",
                          "\tmax_display_time =\t", practice[1,]$Timeout, "\n",
                          "\ttrial_type = sentence\n",
                        "end ", current.trial, "\n"))
      practice.sequence <- practice.sequence %>%
        paste(sep = "", ., "\t", current.trial, "\n")
    } else if(display.changes) { # If there is a display change (thanks to Adrian Staub's scripter2 for how to approach this part)
      # Find the display change region
      # Get the area of interest by splitting on %s
      display.change.region <- regmatches(stimuli[i,]$Sentence2, gregexpr("%.*?%", stimuli[i,]$Sentence2)) %>% unlist()
      region.params <- ""

      current.sentence <- stimuli[i,]$Sentence2

      if(!(length(display.change.region) == 0)){
        for(j in 1:length(display.change.region)){
          # Split the sentence into lines and find the line that the area of interest appears on
          target.lines <- str_split(current.sentence, "\\\\n") %>% unlist()
          display.change.line <- 1
          while(!(grepl(display.change.region[j], target.lines[display.change.line]))){
            display.change.line <- display.change.line + 1
          }

          # Get the starting and ending position of the area of interest in chars in the target line
          display.change.chars <- str_locate(target.lines[display.change.line],
                                             display.change.region[j]) %>%
            as.data.frame

          # Subtract from both to correct for the % characters
          display.change.chars$start <- display.change.chars$start - 1
          display.change.chars$end <- display.change.chars$end - 2

          # Get the region numbers: start at y/x offsets, and add number of lines/chars times the number of pixels per each
          region.y1 <- round(y.offset.pix + ((line.spacing.pix+v.pixels.per.char)/2) - # Start at the midline of the text of the first line
                               ((dcr.padding.pix/2)+(v.pixels.per.char/2)) + # Subtract half the pixel height of a character to get to the top of the box
                               ((display.change.line-1)*10) + # If our line is after the first line, add the actual space between lines in pixels when line spacing is set to 0 for each line
                               ((display.change.line-1) * v.pixels.per.char) + # If our line is after the first line, add character height for each additional line
                               ((display.change.line-1) * line.spacing.pix)) # If our line is after the first line, add the amount of line spacing in pixels per additional line

          region.y2 <- round(y.offset.pix + ((line.spacing.pix+v.pixels.per.char)/2) + # Start at the midline of the text of the first line
                               ((dcr.padding.pix/2)+(v.pixels.per.char/2)) + # Add half the pixel height of a character to get to the bottom of the box
                               ((display.change.line-1)*10) + # If our line is after the first line, add the actual space between lines in pixels when line spacing is set to 0 for each line
                               ((display.change.line-1) * v.pixels.per.char) + # If our line is after the first line, add character height for each additional line
                               ((display.change.line-1) * line.spacing.pix)) # If our line is after the first line, add the amount of line spacing in pixels per additional line

          region.x1 <- round(x.offset.pix + (display.change.chars$start*h.pixels.per.char)) # Start with the x offset in pixels, and add the number of pixels for each character before the display change region
          region.x2 <- round(x.offset.pix + (display.change.chars$end*h.pixels.per.char)) # Start with the x offset in pixels, and add the number of pixels for each character to the end of the display change region

          # Paste them together to add to the sentence
          region.params <- paste(sep = "", region.params, "(", region.y1, " ", region.x1, " ", region.y2, " ", region.x2, ")")

          # Remove percentage markers from the sentence to line up correctly if there are multiple display change regions on the same line
          current.sentence <- sub(display.change.region[j], gsub("%", "", display.change.region[j]), current.sentence)
        }
      } else {
        cat("Error: trial ", i, " doesn't specify display change region in Sentence2. ", sep = "")
        .Internal(.invokeRestart(list(NULL, NULL), NULL))
      }

      practice.trials <- practice.trials %>%
        append(., paste(sep = "",
                        "trial ", current.trial, "\n",
                        "\tgc_rect =\t(", gc.params, ")\n",
                        "\tinline =\t|", practice[1,]$Sentence, "\\n\n",
                        "\tinline =\t|", gsub("%", "", practice[1,]$Sentence2), "\\n\n",
                        "\tmax_display_time =\t", practice[1,]$Timeout, "\n",
                        "\tregion =\t(", region.params, ")\n",
                        "\ttrial_type = \tsentence\n",
                        "end ", current.trial))
    }

    question.trial <- ""
    if(practice[1,]$Question != ""){
      question.trial <- paste(sep = "", "P1I1D101")
      practice.trials <- practice.trials %>%
        append(., paste(sep = "",
                        "trial ", question.trial, "\n",
                        "\tbutton =\t", practice[1,]$AnswerButton, "\n",
                        "\tinline =\t|", practice[1,]$Question, "\\n\\n", practice[1,]$LeftAnswer, "     ", practice[1,]$RightAnswer, "\\n\n",
                        "\tmax_display_time =\t", practice[1,]$QTimeout, "\n",
                        "\ttrial_type =\tquestion\n",
                        "end ", question.trial, "\n"))
      practice.sequence <- practice.sequence %>%
        paste(sep = "", ., "\t", question.trial, "\n")
    }
  }
}

# Combine everything ####
finished.stimuli <- append(header, practice.trials) %>% append(formatted.stimuli)

# Write out the formatted stimuli in the formats requested ####
dir.create(file.path(paste(sep = "", filepath, "ScriptR ", filename, " output/")),
           showWarnings = FALSE)

# Reorder into the preferred format
stimuli <- stimuli %>% dplyr::select(Experiment, item_condition, item_id, Timeout, QTimeout, Sentence, Sentence2,
                                     Question, LeftAnswer, RightAnswer, AnswerButton, everything())

write.table(finished.stimuli,
            paste(filepath, "ScriptR ", filename, " output/", filename, "-formatted.script", sep = ""),
            row.names = F, col.names = F, quote = F)

write.csv(stimuli,
          paste(filepath, "ScriptR ", filename, " output/", filename, "-formatted.csv", sep = ""),
          row.names = F, quote = F)

if(create.sentences.txt){
  write.table(stimuli %>% dplyr::select(item_id, item_condition, Sentence) %>% transform(Sentence = paste(sep = "", "/", Sentence, "/")),
              paste(filepath, "ScriptR ", filename, " output/", filename, "-sentences.txt", sep = ""),
              row.names = F, col.names = F, quote = F)

}

if(create.nogaze & sentence.trigger == "nogaze" & question.trigger == "nogaze"){
  cat("Error: create.nogaze is set, but all triggers are already nogaze. Not outputting separate nogaze version.\n")
} else if (create.nogaze) {
  write.table(gsub("\tgaze|\tdriftandgaze|\tdriftcorrect", "\tnogaze", finished.stimuli),
              paste(filepath, "ScriptR ", filename, " output/", filename, "_NG.script", sep = ""),
              row.names = F, col.names = F, quote = F)
}

if(create.test & sentence.button == "mouse" & answer.buttons.msg == "\tbutton =\tmouse\n"){
  cat("Error: create.test is set, but all buttons are already mouse. Not outputting separate test version.\n")
} else if (create.test) {
  test.header <- gsub("\tbutton =\t(.*?)\n", "\tbutton =\tmouse\n", header)
  while(grepl("\tbutton =\tmouse\n\tbutton =\tmouse", test.header)){
    test.header <- gsub("\tbutton =\tmouse\n\tbutton =\tmouse", "\tbutton =\tmouse", test.header)
  }
  write.table(append(test.header, practice.trials) %>% append(formatted.stimuli),
              paste(filepath, "ScriptR ", filename, " output/", filename, "-testing.script", sep = ""),
              row.names = F, col.names = F, quote = F)
}

if(save.settings){
  write.csv(data.frame(option = c('background.color', 'foreground.color', 'calibration.type',
                                  'font', 'font.size', 'font.weight', 'font.smoothing', 'unit',
                                  'line.spacing', 'x.offset', 'y.offset', 'sentence.button',
                                  'sentence.trigger', 'sentence.delay', 'sentence.output', 'question.trigger',
                                  'question.delay', 'question.output', 'calculate.gc.params',
                                  'gc.params', 'h.pixels.per.char', 'v.pixels.per.char', 'dcr.padding',
                                  'display.update.threshold', 'display.revert', 'dc.delay', 'balance.answer.order.exps',
                                  'paired.items.exps', 'create.sentences.txt', 'create.nogaze', 'create.test', 'create.fix.align.start.pts'),
                       setting = c(ifelse(!all(is.na(color.id(paste('#', sprintf("%06X", background.color), sep = "")))) &
                                            any(color.id(paste('#', sprintf("%06X", background.color), sep = "")) %in% colors()),
                                          color.id(paste('#', sprintf("%06X", background.color), sep = ""))[1],
                                          sprintf("%06X", background.color)),
                                   ifelse(!all(is.na(color.id(paste('#', sprintf("%06X", foreground.color), sep = "")))) &
                                            any(color.id(paste('#', sprintf("%06X", foreground.color), sep = "")) %in% colors()),
                                          color.id(paste('#', sprintf("%06X", foreground.color), sep = ""))[1],
                                          sprintf("%06X", foreground.color)),
                                   ifelse(calibration.type == "0", "3",
                                          ifelse(calibration.type == "1", "9", "13")),
                                   font,
                                   font.size,
                                   font.weight,
                                   font.smoothing,
                                   unit,
                                   ifelse(use.pix, line.spacing.pix, line.spacing),
                                   ifelse(use.pix, x.offset.pix, x.offset),
                                   ifelse(use.pix, y.offset.pix, y.offset),
                                   sentence.button,
                                   sentence.trigger,
                                   ifelse(exists('sentence.delay'), sentence.delay, ""),
                                   sentence.output,
                                   question.trigger,
                                   ifelse(exists('question.delay'), question.delay, ""),
                                   question.output,
                                   ifelse(exists('calculate.gc.params'), calculate.gc.params, FALSE),
                                   ifelse(exists('calculate.gc.params'), ifelse(calculate.gc.params, "", gc.params), gc.params),
                                   ifelse(exists('h.pixels.per.char'), h.pixels.per.char, ""),
                                   ifelse(exists('v.pixels.per.char'), v.pixels.per.char, ""),
                                   ifelse(exists('dcr.padding'), ifelse(dcr.padding == line.spacing, "line.spacing", ifelse(use.pix, dcr.padding.pix, dcr.padding)), ""),
                                   ifelse(exists('display.update.threshold'), display.update.threshold, ""),
                                   ifelse(exists('display.revert'), display.revert, ""),
                                   ifelse(exists('dc.delay'), dc.delay, ""),
                                   ifelse(!is.null(left.or.right.list) & all(left.or.right.list %in% balance.answer.order.exps), "all",
                                          ifelse(any(left.or.right.list %in% balance.answer.order.exps), balance.answer.order.exps, "none")),
                                   ifelse(!is.null(even.number.list) & all(even.number.list %in% paired.items.exps), "all",
                                          ifelse(any(even.number.list %in% paired.items.exps), paired.items.exps, "none")),
                                   create.sentences.txt,
                                   create.nogaze,
                                   create.test,
                                   create.fix.align.start.pts)),
            paste(filepath, "ScriptR ", filename, " output/", filename, "-config.csv", sep = ""),
            row.names = F, quote = F)
}

cat("Completed successfully!\n")