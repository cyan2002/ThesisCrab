
# Script made by Christopher Dwane, 11/15/24 with inspiration from a previous shiny package developed by Rui Seabra at ElectricBlue
#this is the correct script so that time is adding onto the file.

# Load the packages
library(shiny)
library(ggplot2)
library(ampd)
library(tidyverse)
library(pracma)
library(signal)

# Change path below to the folder containing your converted raw data for a particular trial

path <- "/Users/chanceyan/Documents/R/ThesisCrab/CrabHRData/Crab_Trial2_17up_Ci_converted/"

files <- list.files(path)
dfs <- lapply(paste(path, files, sep="/"), read.table)

restart <- TRUE
hr_data <- data.frame(file = character(), heart_rate = numeric(),sd=numeric(), std_err=numeric(), confidence=character(), start=as.numeric(),end=as.numeric(),stringsAsFactors = FALSE)

# it may be necessary to adjust the height of the 2 plots for them to fit in your screen
height <- 300

h1= str_c(100, "px")
h2= str_c(275, "px")
h3= str_c(150, "px")


ui <- fluidPage(
  fluidRow(
    column(12,
           fluidRow(
             column(4,
                    verbatimTextOutput("file_name_display")),
           column(8,
                  plotOutput('heartbeat_plot', height = h1, brush = brushOpts(id = "plot1_brush", direction = "x", resetOnNew = FALSE))
           )),
           plotOutput('ampd_plot', height = h2, brush = brushOpts(id = "plot2_brush", direction = "y", resetOnNew = FALSE)),
           plotOutput('heart_rate_plot', height = h3),
           fluidRow(
             column(3,
                    sliderInput("bandwidth", label = "bandwidth", animate=TRUE, animationOptions(interval=5), min = 0.1, max = 5, value = 1, step = 0.05)),
             column(3,
                    numericInput("manual_hr", label = "Manual Heart Rate", value = 1)),
             column(3,
                    radioButtons("apply_manual", "Manual override?", choices = list("Yes" = "Yes", "No" = "No"), selected = "No")),
             column(2,
                    numericInput("file", "Select a file:", value=1,min = 1, max = length(dfs)),
                    radioButtons("conf", label = "confidence", choices = list("Good" = "Good","Unsure" = "Unsure", "Discard" = "Discard", "Zero" = "Zero"), selected = "Good")),
             column(2,
                    helpText("Heart Rate (BPM)"),
                    verbatimTextOutput("info1"),
                    helpText("Standard Error"),
                    verbatimTextOutput("info2")),
             column(2,
                    downloadButton("download_hr", "Download Heart Rate Data"),
                    helpText("Length"),
                    verbatimTextOutput("info3")),  
             column(2,
                    radioButtons("apply_bartlett", "Apply Bartlett?", choices = list("Yes" = "Yes", "No" = "No"), selected = "No")),
             column(3,
                    sliderInput("bartlett_windowsize", label = "Bartlett window size", min = 10, max = 100, value = 20, step = 10))
                    
           )
    )
  )
)

# Define server logic
server <- function(input, output) {
  ranges <- reactiveValues(x = NULL)
  observe({
    brush <- input$plot1_brush
    if (!is.null(brush)) {
      # Round the brush x limits to the nearest 2-second increments
      ranges$x <- round(c(brush$xmin, brush$xmax) / 2) * 2
    } else {
      ranges$x <- NULL
    }
  })
  
  selected_data <- reactive({
    hb <- (dfs[[as.numeric(input$file)]])
    colnames(hb) <- c("hb")
    time <- seq(0, (nrow(hb) - 1) * (1/40), by = 1/40)  # Time sequence with interval of 1/40 seconds - change if different sample rate was used
    data <- cbind(hb, time)  # Add time column to data frame
  })
  
  subset_data <- reactive({
    data <- selected_data()
    if (input$apply_bartlett == "Yes") {
      windowsize <- input$bartlett_windowsize  # specify window size for the Bartlett window
      window <- bartlett(windowsize)
      convolved_data <- convolve(data$hb, window, type = "open")
      data$hb <- convolved_data[((windowsize/2)):(length(convolved_data) - ((windowsize/2)))]
    }
    if (!is.null(input$plot1_brush)) {
      subset_data <- data[data$time >= ranges$x[1] & data$time <= ranges$x[2], ]

    } else {
      subset_data <- data
    }
  })
  
  output_data <- reactive({
    data <- subset_data()
  if (!is.null(input$plot2_brush)) {
    threshold <- input$plot2_brush$ymax
  } else {
    threshold <- 1.0
  }
    smooth <- ksmooth(data$time, data$hb, "normal", input$bandwidth)
    peaks <- findpeaks(smooth$y, nups = 3, minpeakheight = threshold, minpeakdistance = 5) #changed from 10 to 5
    peak_times <- data$time[peaks[,2]]
    peak_times=as.data.frame(peak_times)
    peak_times=arrange(peak_times,desc(peak_times))
    peak_times$rr_intervals <- (peak_times$peak_times-lag(peak_times$peak_times))
    
    av_interval=mean(peak_times$rr_intervals, na.rm = TRUE)
    av_hr=-(60/av_interval)
    av_hr <- ifelse(is.na(av_hr), -1, av_hr)
    sd <- sd(peak_times$rr_intervals,na.rm = TRUE)
    sd <- ifelse(is.na(sd), -1, sd)
    std_err <- sd(peak_times$rr_intervals,na.rm = TRUE) / sqrt(length(peak_times$rr_intervals))
    std_err <- ifelse(is.na(std_err), -1, std_err)
    
    if(input$apply_manual=="Yes") {
      av_hr <- input$manual_hr / ((data$time[length(data$time)]) - data$time[1]) * 60
      std_err<- 0
    }
    
    if (input$conf == "Zero") {
      av_hr <- 0
      sd <- 0
      std_err <- 0
    }

    
    
    
    currentdata <- data.frame(file = files[as.numeric(input$file)], heart_rate = (av_hr), sd = (sd), std_err = (std_err), confidence = (input$conf), start=data$time[1], end=data$time[length(data$time)])
    #print(currentdata)
    write_data(currentdata)
    currentdata[currentdata < 0] <- NA
    return(currentdata)

    })
  

  
  write_data <- function(currentdata) {
    common_rows <- intersect(hr_data$file, currentdata$file)
      if (length(common_rows) > 0) {
        hr_data <<- hr_data %>%
        left_join(currentdata, by = "file", suffix = c(".hr_data", ".currentdata")) %>%
        mutate(
          heart_rate = coalesce(heart_rate.currentdata, heart_rate.hr_data),
          sd = coalesce(sd.currentdata, sd.hr_data),
          std_err = coalesce(std_err.currentdata, std_err.hr_data),
          confidence = coalesce(confidence.currentdata, confidence.hr_data),    
          start = coalesce(start.currentdata, start.hr_data),
          end = coalesce(end.currentdata, end.hr_data)
        ) %>%
        select(file, heart_rate, sd, std_err, confidence, start, end)
    } else {
      hr_data <<- rbind(hr_data, currentdata)
    }

      }
  
  # Download the heart rate data as a CSV
  output$download_hr <- downloadHandler(
    filename = function() {
      paste( sub(".*/", "", path),"_output",".csv", sep = "")
    },
    content = function(file) {
      hr_data=hr_data %>% separate(file, c("blank","Input","blank2","Trial","Treatment", "Species","date","time"), "_")# will need changing depending on file format
      hr_data=hr_data %>% separate(time, c("hms","empty"), ".txt")
      hr_data$time <- paste(hr_data$date, hr_data$hms, sep="_")
      hr_data$time=as.POSIXlt(hr_data$time,    format = "%Y%m%d_%H%M%S")
      hr_data=select(hr_data,Input,Trial,Treatment,time,heart_rate,sd,std_err,confidence,start,end)
      write.csv(hr_data, file, row.names = FALSE)
    }
  )
  
  
  output$info1 <- renderText({
    currentdata <-  output_data()
    round(currentdata[[2]], 2)
  })
  
  output$info2 <- renderText({
    currentdata <-  output_data()
    round(currentdata[[4]], 4)
  })
  
  output$info3 <- renderText({
    currentdata <-  output_data()
    result=currentdata[[7]]-currentdata[[6]]
    round(result, 2)
  })  
  
  # Display the selected file name
  output$file_name_display <- renderText({
    paste("File:", files[as.numeric(input$file)])
  })
  
  output$heartbeat_plot <- renderPlot({
    par(mar = c(1, 1, 1, 1))
    plot(selected_data()$time, selected_data()$hb, type = "l", 
         xlab = "Time (seconds)", ylab = "Amplitude")
  })
  
  output$ampd_plot <- renderPlot({
    par(mar = c(2, 2, 1, 1))
    data <- subset_data()
    
    if (!is.null(input$plot2_brush)) {
      threshold <- input$plot2_brush$ymax
    } else {
      threshold <- 1.4#CHANGED FROM 1.4 TO 1
    }
    smooth <- ksmooth(data$time, data$hb, "normal", input$bandwidth)
    peaks <- findpeaks(smooth$y, nups = 3, minpeakheight = threshold, minpeakdistance = 5) #changed from 10 to 5
    peak_times <- data$time[peaks[, 2]]
    
    plot(data$time, data$hb, type = "l", xlab = "Time (seconds)", ylab = "Amplitude")
    lines(smooth$x, smooth$y, col = "blue", type = "l")   
    points(peak_times, peaks[, 1], col = "red", pch = 19)
    abline(v = c(peak_times))
  })
  
  output$heart_rate_plot <- renderPlot({
    par(mar = c(2, 2, 1, 1))
    data <- subset_data()
    
    if (!is.null(input$plot2_brush)) {
      threshold <- input$plot2_brush$ymax
    } else {
      threshold <- 0.5
    }
    smooth <- ksmooth(data$time, data$hb, "normal", input$bandwidth)
    peaks <- findpeaks(smooth$y, nups = 3, minpeakheight = threshold, minpeakdistance = 5) #changed from 10 to 5
    peak_times <- data$time[peaks[,2]]
    peak_times=as.data.frame(peak_times)
    peak_times=arrange(peak_times,desc(peak_times))
    peak_times$rr_intervals <- (peak_times$peak_times-lag(peak_times$peak_times))
    
    av_interval=mean(peak_times$rr_intervals, na.rm = TRUE)
    av_hr=-(60/av_interval)
    plot(peak_times$peak_times,60/(-peak_times$rr_intervals), ylim=c(0,2*av_hr))
    abline(h = c(av_hr))
 
  })


  

}

  
  
# Run the application
shinyApp(ui = ui, server = server)

