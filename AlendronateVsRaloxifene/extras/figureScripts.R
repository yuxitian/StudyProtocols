library(xtable)
library(grid)
library(gridExtra)
library(ggplot2)

folders <- c("PPlus",
             "Optum_JRD", "CCAE_JRD", "CCAE_UNM",
             "MDCR_JRD", "MDCR_UNM", "MDCD_JRD", "Cerner",
             "Columbia", "Stride", "NHIS")

label1 <- c("P-Plus",
            "Optum", "Truven", "Truven",
            "Truven", "Truven", "Truven", "Cerner",
            "Columbia", "Stanford", "NHIS")

label2 <-  c("",
             "CEDM", expression(CCAE^1), expression(CCAE^2),
             expression(MDCR^1), expression(MDCR^2), "MDCD" ,"UT" ,
             "" , "", "NSC")

label3 <- c("P-Plus",
            "Optum CEDM",
            "Truven CCAE (1)", "Truven CCAE (2)",
            "Truven MDCR (1)", "Truven MDCR (2)",
            "Truven MDCD",
            "Cerner UT", "Columbia", "Stanford", "NHIS NSC")

folders = folders[c(1,2,3,5,11,7,8,9,10)]
label1 = label1[c(1,2,3,5,11,7,8,9,10)]
label2 = label2[c(1,2,3,5,11,7,8,9,10)]
label3 = label3[c(1,2,3,5,11,7,8,9,10)]
label2[3:4] = c("CCAE", "MDCR")
label3[3:4] = c("Truven CCAE", "Truven MDCR")

getData <- function(folders) {
  do.call(rbind,
          lapply(folders, FUN = function(file) {
            table <- read.csv(file.path(studyFolder,
                                        file, "tablesAndFigures",
                                        "EmpiricalCalibration.csv"))
            table$db <- file
            table
          }))
}

getAttrition <- function(folders) {
  do.call(rbind,
          lapply(folders, FUN = function(file) {
            table <- read.csv(file.path(studyFolder,
                                        file, "Attrition.csv"))
            table$db <- file
            table
          }))
}

getStratifiedCounts <- function(strata, folders, isNumeric = TRUE) {
  zero <- data.frame(group = strata, countTreated = 0, countComparator = 0,
                     fractionTreated = 0, fractionComparator = 0)

  do.call(rbind,
          lapply(folders, FUN = function(file) {
            table <- read.csv(file.path(studyFolder,
                                        file, "tablesAndFigures",
                                        "PopChar.csv"), as.is = TRUE)
            table <- table[table$group %in% strata,]
            start <- ifelse(isNumeric, 1, 2)
            table[,start:ncol(table)] <- sapply(table[,start:ncol(table)],
                                                as.numeric)
            table <- rbind(table, zero[!(zero$group %in% table$group), ])
            table <- table[order(table$group),]
            table$database <- file
            table
          }))
}

makeStratifiedPlot <- function(data, strata, databases,
                               label1, label2,
                               strataLabel = strata,
                               ymax = 10000) {

  nstrata <- length(strata)

  # Make plot
  layout(matrix(1:(length(databases) + 2), ncol = 1))
  par(mar = c(1,4,1,6) + 0.1, xpd = NA)

  # Skip header plot
  plot(0,0, type = "n", axes = FALSE, ylab = "", xlab = "")

  # Body
  for (i in 1:length(databases)) {
    file <- databases[i]
    table <- data[data$database == file, ]

    barplot(as.matrix(data.frame(log(table$countTreated, 10),
                                 log(table$countComparator, 10))),
            axes = FALSE,
            axisnames = FALSE,
            ylim = c(0,log(ymax,10)), beside = TRUE)
    axis(2, at = c(0,1,2,3,4),
         labels = c("0","10","100","1,000","10,000"),
         las = 1)

    for (y in 0:4) {
      lines(c(0,3 + 2 * nstrata), rep(y, 2), lty = 2, lwd = 0.25)
    }

    lines(c(1,nstrata + 1), c(0,0))
    lines(c(nstrata + 2,2 * nstrata + 2), c(0,0))

    text(2 * nstrata + 5.5, 2, label1[i], srt = -90, cex = 1.5)
    text(2 * nstrata + 4,   2, label2[i], srt = -90, cex = 1.5)

    if (i == 1) {
      text(mean(1:nstrata)  + 1, 5.5, "Alendronate", cex = 1.5)
      text(mean((nstrata + 2):(2 * nstrata + 1)) + 1, 5.5,
           "Raloxifene",  cex = 1.5)
    }
  }

  text(1:nstrata + 0.5, -0.5, strataLabel, srt = -90, adj = c(0,0.5))
  text((nstrata + 2):(2 * nstrata + 1) + 0.5, -0.5,
       strataLabel, srt = -90, adj = c(0,0.5))
}

getOrder <- function(dbs, folders) {
  unlist(sapply(dbs,
                function(x) {
                  which(folders == x)
                }))
}

saveTable <- function(obj, number = NULL, filename, size = "normalsize", Rsize = "1.2cm") {
  writeLines(getwd())
  first <- filename
  name <- paste(filename,".tex",sep="")
  sink(file=name)
  cat('
      \\documentclass[preview, 11pt]{standalone}
      \\usepackage{graphicx}
      \\usepackage[font = small]{caption}
      \\usepackage[textwidth=6.2in]{geometry}
      \\usepackage{array}
      \\usepackage{ragged2e}
      \\begin{document}
      ')
  if (!is.null(number)) cat(paste0("\\setcounter{table}{",number-1,"}"))
  if (is.null(number)) cat("\\captionsetup{labelformat=empty}")

  cat("\\captionsetup{width=\\textwidth}")
  cat(paste0("\\newcolumntype{R}{>{\\raggedleft\\arraybackslash}p{",Rsize,"}}"))
  cat(paste0("\\",size))
  cat(obj)
  cat('
      \\end{document}
      ')
  sink()
  tools::texi2dvi(file=name)
  cmd <- paste("dvipng -T tight -D 600", shQuote(paste(first,".dvi",sep="")))
  invisible(system(cmd))
  cleaner <- c(".tex",".aux",".log",".dvi")
  invisible(file.remove(paste(first,cleaner,sep="")))
}

saveFigure <- function(filename, newFile, caption = NULL, number = NULL, width, height, newWidth = NULL) {
  if (is.null(newWidth)) newWidth = width
  first <- newFile
  name <- paste(newFile,".tex",sep="")
  sink(file=name)
  cat(paste0('
      \\documentclass[preview, 11pt]{standalone}
      \\usepackage{graphicx}
      \\usepackage[font = small]{caption}
      \\usepackage[textwidth=',newWidth,']{geometry}
      \\begin{document}
      '))
  if (!is.null(number)) cat(paste0("\\setcounter{figure}{",number-1,"}"))
  cat("\\begin{figure}[hbt]\\centering")
  cat("\\captionsetup{width=\\textwidth}")
  cat(paste0("\\includegraphics[width=\\textwidth,natwidth=",width,",natheight=",height,"]{",filename,"}"))
  if (!is.null(caption)) cat(paste0("\\caption{",caption,"}"))
  cat("\\end{figure}")
  cat('
      \\end{document}
      ')
  sink()
  tools::texi2dvi(file=name)
  cmd <- paste("dvipng -T tight -D 600", shQuote(paste(first,".dvi",sep="")))
  invisible(system(cmd))
  cleaner <- c(".tex",".aux",".log",".dvi")
  invisible(file.remove(paste(first,cleaner,sep="")))
}

printOutcomeTable <- function(analysisId,
                              folders,
                              labels,
                              cohortOrder,
                              prettyCohortNames,
                              singleTable = FALSE,
                              caption = NULL,
                              floating = FALSE) {

  allCohorts <- read.csv(system.file("settings",
                                     "CohortsToCreate.csv",
                                     package = "AlendronateVsRaloxifene"))
  allCohorts <- allCohorts[3:nrow(allCohorts),]

  allCohorts <- allCohorts[allCohorts$name %in% cohortOrder, ]

  allResults <- getData(folders)

  results <- allResults
  results <- results[results$analysisId == analysisId, ]
  results <- results[results$outcomeName %in% cohortOrder, ]
  results <- results[results$db %in% folders, ]

  results$space1 <- ""
  results$space2 <- ""

  results$treatedYears = round(results$treatedDays/365.24)
  results$comparatorYears = round(results$comparatorDays/365.24)

  results$order1 <- getOrder(results$db, folders)
  results$order2 <- getOrder(as.character(results$outcomeName), cohortOrder)
  results <- results[order(results$order2,
                           results$order1,
                           decreasing = FALSE),]

  crude <- results[,c(# "space1",
    "outcomeName",
    "db","treated","treatedYears","eventsTreated",
    "space2",
    "comparator", "comparatorYears","eventsComparator",
    "order1","order2")]
  temp = aggregate(crude[,c("treated","treatedYears","eventsTreated",
                            "comparator","comparatorYears","eventsComparator")],
                   by = list(outcomeName = crude$outcomeName),
                   sum)
  temp$db = "Total"
  temp$space2 = ""
  temp$order1 = rep(max(getOrder(results$db, folders))+1, nrow(temp))
  temp$order2 = getOrder(as.character(temp$outcomeName), cohortOrder)
  crude <- rbind(crude, temp)
  crude$outcomeName <- NULL
  crude <- crude[order(crude$order2,
                       crude$order1,
                       decreasing = FALSE),]
  crude$order1 <- NULL
  crude$order2 <- NULL

  crude$db <- c(labels, "Total") # paste0(label1, " ", label2)
  # crude$db <- sub("\\^(\\d)", "$^\\1$", crude$db)

  for (column in c("treated","comparator")) {
    crude[,column] <- formatC(crude[,column],
                              format = "d", big.mark = ",")
  }

  for (column in c("treatedYears", "comparatorYears")) {
    crude[,column] <- formatC(round(crude[,column] / 1), # / 1000?
                              format = "d", big.mark = ",")
  }

  for (column in c("eventsTreated", "eventsComparator")) {
    crude[,column] <- ifelse(crude[,column] < 6,
                             ifelse(crude[,column] == 0, 0, "\\textless6"),
                             formatC(crude[,column],
                                     format = "d", big.mark = ","))
  }

  # colnames(crude) <- c("Data source",
  #                      "Patients", "Exposed days", "Events",
  #                      "Patients", "Exposed days", "Events" )
  addtorow = list()
  addtorow$pos <- c(list(0, 0, 0),
                    # as.list((length(folders)) * (0:(length(cohortOrder) - 1)) + 1),
                    nrow(crude)
                    # as.list(length(folders) * (0:(length(cohortOrder) - 1)))
  )
  if (singleTable) {
    addtorow$pos = c(addtorow$pos, nrow(crude) - 1)
  } else {
    addtorow$pos = c(addtorow$pos, as.list((length(folders)+1) * (0:(length(cohortOrder) - 1))))
  }

  getNameInBox <- function(name) {
    paste0("\\parbox[t]{2mm}{\\multirow{",
           round(length(folders) / 2),
           "}{*}{\\rotatebox[origin=c]{90}{",
           name,
           "}}}")
  }

  getNameAcross <- function(name) {
    paste0("\\\\[-0.5em]\n \\multicolumn{8}{c}{",
           name,
           "} \\\\\n")
  }

  topStrut = "\\rule{0pt}{2.6ex}"
  botStrut = "\\rule[-1.2ex]{0pt}{0pt}"
  crude[(length(folders)+1)*(1:length(cohortOrder)),"db"] = paste0("\\multicolumn{1}{c}{",topStrut,botStrut,"Total}")

  #"\\multicolumn{1}{c}{\\rule{0pt}{2.6ex}\\rule[-1.2ex]{0pt}{0pt}Total}"

  addtorow$command <- c("& \\multicolumn{3}{c}{Alendronate} & & \\multicolumn{3}{c}{Raloxifene} \\\\\n",
                        "\\cline{2-4} \\cline{6-8} \\\\[-1em]\n",
                        "Data source & Patients & Years & Events & & Patients & Years & Events \\\\\n \\hline \n",
                        # getNameInBox(prettyCohortNames),
                        # rep("\\\\\n\\\\\n", length(cohortOrder))
                        "[0.4em]\\hline"
  )
  if (singleTable) {
    addtorow$command = c(addtorow$command, "[0.5em]\\hline")
  } else {
    addtorow$command = c(addtorow$command, getNameAcross(prettyCohortNames))
  }

  tab <- xtable(crude, caption = caption)

  align(tab) <- "clrrrcrrr"
  print.xtable(tab, include.rownames = FALSE, include.colnames = FALSE,
               add.to.row = addtorow,
               hline.after = c(-1),
               sanitize.text.function=function(x) { x },
               floating = floating)
}

printOverallIncidence <- function(analysisId,
                                  folders,
                                  labels,
                                  cohortOrder,
                                  prettyCohortNames,
                                  caption = NULL,
                                  floating = FALSE) {

  allCohorts <- read.csv(system.file("settings",
                                     "CohortsToCreate.csv",
                                     package = "AlendronateVsRaloxifene"))
  allCohorts <- allCohorts[3:nrow(allCohorts),]

  allCohorts <- allCohorts[allCohorts$name %in% cohortOrder, ]

  allResults <- getData(folders)

  results <- allResults
  results <- results[results$analysisId == analysisId, ]
  results <- results[results$outcomeName %in% cohortOrder, ]
  results <- results[results$db %in% folders, ]

  results$space1 <- ""
  results$space2 <- ""

  results$order1 <- getOrder(results$db, folders)
  results$order2 <- getOrder(as.character(results$outcomeName), cohortOrder)
  results <- results[order(results$order2,
                           results$order1,
                           decreasing = FALSE),]

  results$treatedYears = round(results$treatedDays/365.24)
  results$comparatorYears = round(results$comparatorDays/365.24)

  crude <- results[,c(# "space1",
    "outcomeName",
    "treated","treatedYears","eventsTreated",
    "comparator","comparatorYears","eventsComparator")]

  crude <- aggregate(crude[,-c(1)], by = list(outcomeName = crude$outcomeName), sum)
  crude <- crude[match(cohortOrder,crude$outcomeName),]
  crude$outcomeName <- prettyCohortNames
  #crude$treatedIncidence = crude$eventsTreated/crude$treatedYears
  #crude$comparatorIncidence = crude$eventsComparator/crude$comparatorYears

  crude$treatedRate = crude$eventsTreated/crude$treatedYears*1000
  crude$comparatorRate = crude$eventsComparator/crude$comparatorYears*1000


  crude$space = ""

  crude = crude[,c("outcomeName","treated","treatedYears","eventsTreated","treatedRate","space",
                   "comparator","comparatorYears","eventsComparator","comparatorRate")]

  # for (column in c("treatedIncidence", "comparatorIncidence")) {
  #   crude[,column] <- paste0(formatC(100 * crude[,column], digits = 3), "\\%")
  # }

  for (column in c("eventsTreated", "eventsComparator", "treatedYears", "comparatorYears")) {
    crude[,column] <- ifelse(crude[,column] < 6,
                             ifelse(crude[,column] == 0, 0, "\\textless6"),
                             formatC(crude[,column],
                                     format = "d", big.mark = ","))
  }

  # for (row in 1:nrow(crude)) {
  #   if (crude$eventsTreated[row] == "<6") {
  #     crude$treatedIncidence[row] = paste0("<",formatC(100*6/crude$treated[row], digits = 3), "\\%")
  #   }
  #   if (crude$eventsComparator[row] == "<6") {
  #     crude$comparatorIncidence[row] = paste0("<",formatC(100*6/crude$comparator[row], digits = 3), "\\%")
  #   }
  # }

  for (column in c("treated","comparator")) {
    crude[,column] <- formatC(crude[,column],
                              format = "d", big.mark = ",")
  }


  addtorow <- list()
  addtorow$pos <- c(list(0,0,0),
                    nrow(crude))


  getNameInBox <- function(name) {
    paste0("\\parbox[t]{2mm}{\\multirow{",
           round(length(folders) / 2),
           "}{*}{\\rotatebox[origin=c]{90}{",
           name,
           "}}}")
  }

  getNameAcross <- function(name) {
    paste0("\\\\[-0.5em]\n \\multicolumn{8}{c}{",
           name,
           "} \\\\\n")
  }

  addtorow$command <- c("& \\multicolumn{4}{c}{Alendronate} & & \\multicolumn{4}{c}{Raloxifene} \\\\\n",
                        "\\cline{2-5} \\cline{7-10} \\\\[-1em]\n",
                        "\\multicolumn{1}{c}{Outcome} & Patients & Years & Events & Rate & & Patients & Years & Events & Rate \\\\\n  \\hline \\\\ [-0.5em] \n",
                        "[0.4em]\\hline")
                        #"[0.5em] \\hline \\\\ [-0.5em] \\multicolumn{10}{l}{Rate: incidence per 1,000 person-years}")

  tab <- xtable(crude, caption = caption)

  align(tab) <- "clrrrrcrrrr"
  print.xtable(tab, include.rownames = FALSE, include.colnames = FALSE,
               add.to.row = addtorow,
               hline.after = c(-1),
               sanitize.text.function=function(x) { x },
               floating = floating)
}
printAttrition <- function(folders,
                           labels,
                           caption = NULL,
                           floating = FALSE) {
  results <- getAttrition(folders)
  #results$space1 <- ""

  results$order1 <- getOrder(results$db, folders)
  results <- results[order(results$order1,
                           decreasing = FALSE),]
  results <- results[results$description%in%c("No prior outcome", "Trimmed to equipoise"),]
  results <- results[,c("description", "treatedPersons", "comparatorPersons")]
  results$totalPersons = results$treatedPersons + results$comparatorPersons

  results1 <- results[results$description == "No prior outcome", -c(1)] -
    results[results$description == "Trimmed to equipoise", -c(1)]

  crude <- results1 / results[results$description == "No prior outcome", -c(1)]

  for (column in c("treatedPersons", "comparatorPersons", "totalPersons")) {
    crude[,column] <- paste0(formatC(100 * crude[,column], digits = 2), "\\%")
  }

  crude$db <- c(labels) # paste0(label1, " ", label2)
  crude$db <- sub("\\^(\\d)", "$^\\1$", crude$db)
  crude$space1 <- ""
  crude$space2 <- ""

  crude = crude[,c("db","treatedPersons","comparatorPersons", "totalPersons")]

  addtorow <- list()
  addtorow$pos <- c(list(0), nrow(crude))

  addtorow$command <- c("\\multicolumn{1}{c}{Data source}& Alendronate & Raloxifene & Total \\\\ \\hline \\\\[-0.5em]\n",
                        "[0.4em]\\hline")

  tab <- xtable(crude, caption = caption)

  # align(tab) <- "clrrr"
  align(tab) <- "clRRR"
  print.xtable(tab, include.rownames = FALSE, include.colnames = FALSE,
               add.to.row = addtorow,
               hline.after = c(-1),
               sanitize.text.function=function(x) { x },
               floating = floating
  )
}

printBalance <- function(folders,
                         caption = NULL,
                         floating = FALSE) {
  table <-
    do.call(rbind,
            lapply(folders, FUN = function(file) {
              table <- read.csv(file.path(studyFolder,
                                          file,
                                          "Balance.csv"), as.is = TRUE)
              idx <- which(folders == file)
              name <- paste0(label1[idx], " ", label2[idx])
              name <-  sub("\\^(\\d)", "$^\\1$", name)
              table <- data.frame(db = name,
                                  before = max(abs(table$beforeMatchingStdDiff),
                                               na.rm = TRUE),
                                  after = max(abs(table$afterMatchingStdDiff),
                                              na.rm = TRUE))
              table
            }))

  addtorow <- list()
  addtorow$pos <- list()
  addtorow$pos[[1]] <- 0
  addtorow$pos[[2]] <- nrow(table)
  addtorow$command <- c("\\multicolumn{1}{c}{Data source} & \\multicolumn{1}{c}{Before PS} & \\multicolumn{1}{c}{After PS}  \\\\\n \\hline \\\\[-0.5em]",
                        "[0.4em]\\hline")

  output <- xtable(table, caption = caption)

  align(output) <- "clrr"
  print.xtable(output, include.rownames = FALSE, include.colnames = FALSE,
               add.to.row = addtorow,
               hline.after = c(-1),
               sanitize.text.function=function(x) { x },
               floating = floating
  )
}

plotForest <- function(logRr, logLb95Ci, logUb95Ci, names, labels,
                       xLabel = "Relative risk",
                       breaks = c(0.25, 0.5, 1, 2, 4, 6, 8, 10),
                       summaryExclude = c(),
                       plotExclude = c(),
                       fileName = NULL) {

  # logRr = results$logRr
  # logLb95Ci = log(results$ci95lb)
  # logUb95Ci = log(results$ci95ub)
  # names = results$db
  # labels = label3
  # xLabel = "Hazard Ratio"
  # breaks = c(0.25, 0.5, 1, 2, 4)
  # fileName = NULL
  # summaryExclude = c("CCAE_UNM", "MDCR_UNM")


  seLogRr <- (logUb95Ci-logLb95Ci) / (2 * qnorm(0.975))

  include <- !(names %in% summaryExclude)

  meta <- meta::metagen(logRr[include], seLogRr[include], studlab = names[include], sm = "RR")
  s <- summary(meta)$random

  d1 <- data.frame(logRr = logRr,
                   logLb95Ci = logLb95Ci,
                   logUb95Ci = logUb95Ci,
                   name = labels,
                   type = "db")
  d2 <- data.frame(logRr = s$TE,
                   logLb95Ci = s$lower,
                   logUb95Ci = s$upper,
                   name = "Summary",
                   type = "ma")
  d3 <- data.frame(logRr = NA,
                   logLb95Ci = NA,
                   logUb95Ci = NA,
                   name = "Data source",
                   type = "header")
  d <- rbind(d1, d2, d3)

  idx <- which(is.na(d$logRr))
  d$logLb95Ci[idx] <- NA
  d$logUb95Ci[idx] <- NA

  d$name <- factor(d$name,
                   levels = c("Summary", rev(as.character(labels)), "Source")
  )

  p <- ggplot2::ggplot(d,ggplot2::aes(x = exp(logRr),
                                      y = name, xmin = exp(logLb95Ci), xmax = exp(logUb95Ci))) +
    ggplot2::geom_vline(xintercept = breaks, colour = "#AAAAAA", lty = 1) +
    ggplot2::geom_vline(xintercept = 1, size = 0.5) +
    ggplot2::geom_errorbarh(height = 0.15, alpha = exp(0.5*(d$logLb95Ci-d$logUb95Ci))) +
    ggplot2::geom_point(size=3, shape = 23, ggplot2::aes(fill=type), alpha = exp(0.5*(d$logLb95Ci-d$logUb95Ci))) +
    ggplot2::scale_fill_manual(values = c("#000000", "#FFFFFF", "#FFFFFF")) +
    ggplot2::scale_x_continuous(xLabel, trans = "log10", breaks = breaks, labels = breaks) +
    ggplot2::coord_cartesian(xlim = c(min(breaks), max(breaks))) +
    ggplot2::theme(panel.grid.major = ggplot2::element_blank(),
                   panel.grid.minor = ggplot2::element_blank(),
                   panel.background = ggplot2::element_blank(),
                   legend.position = "none",
                   panel.border = ggplot2::element_blank(),
                   axis.text.y = ggplot2::element_blank(),
                   axis.title.y = ggplot2::element_blank(),
                   axis.ticks = ggplot2::element_blank(),
                   plot.margin = grid::unit(c(0,0,0.1,0), "lines"))

  rr <- exp(d$logRr)
  rrf <- formatC(rr,  digits = 2, format = "f")
  lo <- exp(d$logLb95Ci)
  lof <- formatC(lo,  digits = 2, format = "f")
  up <- exp(d$logUb95Ci)
  upf <- ifelse(up > 10,
                formatC(up,  digits = 1, format = "f"),
                formatC(up,  digits = 2, format = "f"))


  labels <- ifelse(d$type == "header", paste(xLabel, "(95% CI)"),
                   ifelse(is.na(d$logRr), " -- ",
                          paste0(
                            rrf, # formatC(rr,  digits = 2, format = "f")
                            " (",
                            lof, # formatC(lo, digits = 2, format = "f"),
                            "-",
                            upf, # formatC(up, digits = digits, format = "f"),
                            ")")))

  labels <- data.frame(y = rep(d$name, 2),
                       x = rep(1:2, each = nrow(d)),
                       label = c(as.character(d$name), labels))

  # levels(labels$label)[1] <-  paste(xLabel,"(95% CI)")

  data_table <- ggplot2::ggplot(labels,
                                ggplot2::aes(x = x, y = y, label = label,
                                             parse = TRUE)) +
    ggplot2::geom_text(size = 4, hjust=0, vjust=0.5) +
    ggplot2::geom_hline(ggplot2::aes(yintercept=nrow(d) - 0.5)) +
    ggplot2::theme(panel.grid.major = ggplot2::element_blank(),
                   panel.grid.minor = ggplot2::element_blank(),
                   legend.position = "none",
                   panel.border = ggplot2::element_blank(),
                   panel.background = ggplot2::element_blank(),
                   axis.text.x = ggplot2::element_text(colour="white"),#element_blank(),
                   axis.text.y = ggplot2::element_blank(),
                   axis.ticks = ggplot2::element_line(colour="white"),#element_blank(),
                   plot.margin = grid::unit(c(0,0,0.1,0), "lines")) +
    ggplot2::labs(x="",y="") +
    ggplot2::coord_cartesian(xlim=c(1,3))

  #  plot <- gridExtra::grid.arrange(data_table, p, ncol=2)

  if (!is.null(fileName))
    ggplot2::ggsave(fileName,
                    gridExtra::grid.arrange(data_table, p, ncol=2),
                    width = 7, height = 1 + length(logRr) * 0.4, dpi = 400)

  return(list(data_table = data_table,
              p = p,
              meta = meta))
}

buildOutcomeResults <- function(folders, analysisId, cohort, summaryExclude, allExclude = c()) {

  # analysisId <- 2
  # cohort <- "HipFracture"
  # allExclude <- c()

  allResults <- getData(folders)
  results <- allResults
  results <- results[results$analysisId == analysisId, ]
  results <- results[results$outcomeName == cohort, ]
  results <- results[results$db %in% folders, ]
  results$labels <- label3
  results <- results[!(results$db %in% allExclude),]

  results$logRr <- ifelse(abs(results$logRr) > 10, NA, results$logRr)

  results$order1 <- getOrder(results$db, folders)
  results <- results[order(results$order1,
                           decreasing = FALSE),]

  forest <- plotForest(logRr = results$logRr,
                       logLb95Ci = log(results$ci95lb),
                       logUb95Ci = log(results$ci95ub),
                       names = results$db,
                       labels = results$labels,
                       xLabel = "Hazard Ratio",
                       breaks = c(0.25, 0.5, 1, 2, 4),
                       summaryExclude = summaryExclude,
                       fileName = NULL)
}

.truncRight <- function(x, n) {
  nc <- nchar(x)
  x[nc > (n - 3)] <- paste("...",
                           substr(x[nc > (n - 3)], nc[nc > (n - 3)] - n + 1, nc[nc > (n - 3)]),
                           sep = "")
  x
}

plotCovariateBalanceOfTopVariables <- function(balance,
                                               n = 20,
                                               maxNameWidth = 100,
                                               fileName = NULL,
                                               beforeLabel = "before matching",
                                               afterLabel = "after matching") {
  topBefore <- balance[order(-abs(balance$beforeMatchingStdDiff)), ]
  topBefore <- topBefore[1:n, ]
  topBefore$facet <- paste("Top", n, beforeLabel)
  topAfter <- balance[order(-abs(balance$afterMatchingStdDiff)), ]
  topAfter <- topAfter[1:n, ]
  topAfter$facet <- paste("Top", n, afterLabel)
  filtered <- rbind(topBefore, topAfter)

  data <- data.frame(covariateId = rep(filtered$covariateId, 2),
                     covariate = rep(filtered$covariateName, 2),
                     difference = c(filtered$beforeMatchingStdDiff, filtered$afterMatchingStdDiff),
                     group = rep(c(beforeLabel, afterLabel), each = nrow(filtered)),
                     facet = rep(filtered$facet, 2),
                     rowId = rep(nrow(filtered):1, 2))
  filtered$covariateName <- .truncRight(as.character(filtered$covariateName), maxNameWidth)
  data$facet <- factor(data$facet, levels = rev(levels(data$facet)))
  data$group <- factor(data$group, levels = rev(levels(data$group)))
  plot <- ggplot2::ggplot(data, ggplot2::aes(x = difference,
                                             y = rowId,
                                             color = group,
                                             group = group,
                                             fill = group,
                                             shape = group)) +
    ggplot2::geom_point() +
    ggplot2::geom_vline(xintercept = 0) +
    ggplot2::scale_fill_manual(values = c(rgb(0.8, 0, 0, alpha = 0.5),
                                          rgb(0, 0, 0.8, alpha = 0.5))) +
    ggplot2::scale_color_manual(values = c(rgb(0.8, 0, 0, alpha = 0.5),
                                           rgb(0, 0, 0.8, alpha = 0.5))) +
    ggplot2::scale_x_continuous("Standardized difference of mean") +
    ggplot2::scale_y_continuous(breaks = nrow(filtered):1, labels = filtered$covariateName) +
    ggplot2::facet_grid(facet ~ ., scales = "free", space = "free") +
    ggplot2::theme(axis.text.y = ggplot2::element_text(size = 7),
                   axis.title.y = ggplot2::element_blank(),
                   legend.position = "top",
                   legend.direction = "horizontal",
                   legend.title = ggplot2::element_blank())
  if (!is.null(fileName))
    ggplot2::ggsave(fileName, plot, width = 10, height = max(2 + n * 0.2, 5), dpi = 400)
  return(plot)
}

plotCovariateBalanceOfTopVariables1 <- function(balance,
                                                n = 20,
                                                maxNameWidth = 100,
                                                fileName = NULL,
                                                beforeLabel = "before matching",
                                                afterLabel = "after matching") {
  topBefore <- balance[order(-abs(balance$beforeMatchingStdDiff)), ]
  topBefore <- topBefore[1:n, ]
  topBefore$facet <- paste("Top", n, beforeLabel)
  # topAfter <- balance[order(-abs(balance$afterMatchingStdDiff)), ]
  # topAfter <- topAfter[1:n, ]
  # topAfter$facet <- paste("Top", n, afterLabel)
  # filtered <- rbind(topBefore, topAfter)
  filtered <- topBefore

  data <- data.frame(covariateId = rep(filtered$covariateId, 2),
                     covariate = rep(filtered$covariateName, 2),
                     difference = c(filtered$beforeMatchingStdDiff, filtered$afterMatchingStdDiff),
                     group = rep(c(beforeLabel, afterLabel), each = nrow(filtered)),
                     #facet = rep(filtered$facet, 2),
                     rowId = rep(nrow(filtered):1, 2))
  filtered$covariateName <- .truncRight(as.character(filtered$covariateName), maxNameWidth)
  #data$facet <- factor(data$facet, levels = rev(levels(data$facet)))
  data$group <- factor(data$group, levels = rev(levels(data$group)))
  plot <- ggplot2::ggplot(data, ggplot2::aes(x = difference,
                                             y = rowId,
                                             color = group,
                                             group = group,
                                             fill = group,
                                             shape = group)) +
    ggplot2::geom_point() +
    ggplot2::geom_vline(xintercept = 0) +
    ggplot2::scale_fill_manual(values = c(rgb(0.8, 0, 0, alpha = 0.5),
                                          rgb(0, 0, 0.8, alpha = 0.5))) +
    ggplot2::scale_color_manual(values = c(rgb(0.8, 0, 0, alpha = 0.5),
                                           rgb(0, 0, 0.8, alpha = 0.5))) +
    ggplot2::scale_x_continuous("Standardized difference of mean") +
    ggplot2::scale_y_continuous(breaks = nrow(filtered):1, labels = filtered$covariateName) +
    #ggplot2::facet_grid(facet ~ ., scales = "free", space = "free") +
    ggplot2::theme(axis.text.y = ggplot2::element_text(size = 7),
                   axis.title.y = ggplot2::element_blank(),
                   legend.position = "top",
                   legend.direction = "horizontal",
                   legend.title = ggplot2::element_blank())
  if (!is.null(fileName))
    ggplot2::ggsave(fileName, plot, width = 10, height = max(2 + n * 0.2, 5), dpi = 400)
  return(plot)
}

printPValueTable <- function(analysisId,
                             folders,
                             labels,
                             cohortOrder,
                             prettyCohortNames,
                             singleTable = FALSE,
                             caption = NULL,
                             floating = FALSE) {

  allCohorts <- read.csv(system.file("settings",
                                     "CohortsToCreate.csv",
                                     package = "AlendronateVsRaloxifene"))
  allCohorts <- allCohorts[3:nrow(allCohorts),]

  allCohorts <- allCohorts[allCohorts$name %in% cohortOrder, ]

  allResults <- getData(folders)

  results <- allResults
  results <- results[results$analysisId == analysisId, ]
  results <- results[results$outcomeName %in% cohortOrder, ]
  results <- results[results$db %in% folders, ]

  results$space1 <- ""
  results$space2 <- ""
  results$space3 <- ""
  results$space4 <- ""
  results$space5 <- ""

  results$order1 <- getOrder(results$db, folders)
  results$order2 <- getOrder(as.character(results$outcomeName), cohortOrder)
  results <- results[order(results$order2,
                           results$order1,
                           decreasing = FALSE),]

  crude <- results[,c("db", "space3", "null_mean", "null_sd", "space1", "logRr", "seLogRr", "p", "space2",
                      "calibratedP", "calibratedP_lb95ci", "calibratedP_ub95ci")]

  crude$logRr[which(is.na(crude$seLogRr))] = NA

  for (column in c("null_mean", "null_sd", "logRr", "seLogRr", "p",
                   "calibratedP", "calibratedP_lb95ci", "calibratedP_ub95ci")) {
    crude[,column] <- formatC(crude[,column], digits = 3)
  }

  crude$db <- c(labels)

  addtorow = list()
  addtorow$pos <- c(list(0, 0, 0),
                    nrow(crude)
  )

  if (singleTable) {
    addtorow$pos = c(addtorow$pos, nrow(crude) - 1)
  } else {
    addtorow$pos = c(addtorow$pos, as.list((length(folders)) * (0:(length(cohortOrder) - 1))))
  }

  getNameInBox <- function(name) {
    paste0("\\parbox[t]{2mm}{\\multirow{",
           round(length(folders) / 2),
           "}{*}{\\rotatebox[origin=c]{90}{",
           name,
           "}}}")
  }

  getNameAcross <- function(name) {
    paste0("\\\\[-0.5em]\n \\multicolumn{8}{c}{",
           name,
           "} \\\\\n")
  }

  addtorow$command <- c("&& \\multicolumn{2}{c}{Empirical Null Dist.} & & \\multicolumn{3}{c}{Original Estimate} & & \\multicolumn{3}{c}{Calibrated P-value} \\\\\n",
                        "\\cline{3-4} \\cline{6-8} \\cline{10-12} \\\\[-1em]\n",
                        "Data source & & Mean & SD & & Mean & SD & p-value & & p-value & 95\\% lb & 95\\% ub \\\\\n \\hline \n",
                        "[0.4em]\\hline "
  )
  if (singleTable) {
    addtorow$command = c(addtorow$command, "")
  } else {
    addtorow$command = c(addtorow$command, getNameAcross(prettyCohortNames))
  }

  tab <- xtable(crude, caption = caption)

  # align(tab) <- "clp{0.1cm}rrrrrrrrrr"
  align(tab) <- "clp{0.1cm}RRrRRRrRRR"
  print.xtable(tab, include.rownames = FALSE, include.colnames = FALSE,
               add.to.row = addtorow,
               hline.after = c(-1),
               sanitize.text.function=function(x) { x },
               floating = floating)
}

multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL, byrow = FALSE) {
  library(grid)

  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)

  numPlots = length(plots)

  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols), byrow = byrow)
  }

  if (numPlots==1) {
    print(plots[[1]])

  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))

    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))

      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

plotCalibrationEffect <- function(logRrNegatives,
                                  seLogRrNegatives,
                                  logRrPositives,
                                  seLogRrPositives,
                                  null = NULL,
                                  alpha = 0.05,
                                  xLabel = "Hazard Ratio",
                                  title,
                                  showCis = FALSE,
                                  fileName = NULL) {

  logRrNegatives = logRrNegatives[which(!is.na(seLogRrNegatives))]
  seLogRrNegatives = seLogRrNegatives[which(!is.na(seLogRrNegatives))]

  if (is.null(null)) {
    if (showCis) {
      null <- EmpiricalCalibration::fitMcmcNull(logRrNegatives, seLogRrNegatives)
    } else {
      null <- EmpiricalCalibration::fitNull(logRrNegatives, seLogRrNegatives)
    }
  }
  if (showCis && is(null, "null"))
    stop("Cannot show credible intervals when using asymptotic null. Please use 'fitMcmcNull' to fit the null")

  # x <- exp(seq(log(0.25), log(10), by = 0.01))
  x <- exp(seq(min(log(0.25),min(logRrNegatives)), max(log(4),max(logRrNegatives)), by = 0.01))
  if (is(null, "null")) {
    y <- logRrtoSE(log(x), alpha, null[1], null[2])
  } else {
    chain <- attr(null, "mcmc")$chain
    matrix <- apply(chain, 1, function(null) logRrtoSE(log(x), alpha, null[1], 1/sqrt(null[2])))
    ys <- apply(matrix, 1, function(se) quantile(se, c(0.025, 0.50, 0.975), na.rm=TRUE))
    rm(matrix)
    y <- ys[2, ]
    yLb <- ys[1, ]
    yUb <- ys[3, ]
  }
  seTheoretical <- sapply(x, FUN = function(x) {
    abs(log(x))/qnorm(1-alpha/2)
  })
  breaks <- c(0.05, 0.1, 0.25, 0.5, 1, 2, 4)
  #if (!is.null(which(breaks<min(logRrNegatives)))) breaks <- breaks[max(which(breaks<min(logRrNegatives))):length(breaks)]
  #if (!is.null(which(breaks>max(logRrNegatives)))) breaks <- breaks[1:min(which(breaks>max(logRrNegatives)))]
  #if (min(logRrNegatives) < log(0.25)) breaks <- c(0.05, 0.1, breaks)
  theme <- ggplot2::element_text(colour = "#000000", size = 12)
  themeRA <- ggplot2::element_text(colour = "#000000", size = 12, hjust = 1)
  plot <- ggplot2::ggplot(data.frame(x, y, seTheoretical),
                          ggplot2::aes(x = x, y = y),
                          environment = environment()) +
    ggplot2::geom_vline(xintercept = breaks, colour = "#AAAAAA", lty = 1, size = 0.5) +
    ggplot2::geom_vline(xintercept = 1, size = 1) +
    ggplot2::geom_area(fill = rgb(1, 0.5, 0, alpha = 0.5),
                       color = rgb(1, 0.5, 0),
                       size = 1,
                       alpha = 0.5)

  if (showCis) {
    plot <- plot +
      ggplot2::geom_ribbon(ggplot2::aes(ymin = yLb,
                                        ymax = yUb), fill = rgb(0.8, 0.2, 0.2), alpha = 0.3) +
      ggplot2::geom_line(ggplot2::aes(y = yLb),
                         colour = rgb(0.8, 0.2, 0.2, alpha = 0.2),
                         size = 1) +
      ggplot2::geom_line(ggplot2::aes(y = yUb),
                         colour = rgb(0.8, 0.2, 0.2, alpha = 0.2),
                         size = 1)
  }
  plot <- plot +
    ggplot2::geom_area(ggplot2::aes(y = seTheoretical),
                       fill = rgb(0, 0, 0),
                       colour = rgb(0, 0, 0, alpha = 0.1),
                       alpha = 0.1) +
    ggplot2::geom_line(ggplot2::aes(y = seTheoretical),
                       colour = rgb(0, 0, 0),
                       linetype = "dashed",
                       size = 1,
                       alpha = 0.5) +
    ggplot2::geom_point(shape = 21,
                        ggplot2::aes(x, y),
                        data = data.frame(x = exp(logRrNegatives), y = seLogRrNegatives),
                        size = 2,
                        fill = rgb(0, 0, 1, alpha = 0.5),
                        colour = rgb(0, 0, 0.8)) +
    ggplot2::geom_hline(yintercept = 0) +
    ggplot2::scale_x_continuous(xLabel,
                                trans = "log10",
                                #limits = c(0.25, 10),
                                limits = c(min(exp(min(logRrNegatives)), 0.25), max(exp(max(logRrNegatives)), 4)),
                                breaks = breaks,
                                labels = breaks) +
    ggplot2::scale_y_continuous("Standard Error", limits = c(0, max(1.5, max(seLogRrNegatives)))) +
    ggplot2::theme(panel.grid.minor = ggplot2::element_blank(),
                   panel.background = ggplot2::element_rect(fill = "#FAFAFA", colour = NA),
                   panel.grid.major = ggplot2::element_blank(),
                   axis.ticks = ggplot2::element_blank(),
                   axis.text.y = themeRA,
                   axis.text.x = theme,
                   legend.key = ggplot2::element_blank(),
                   strip.text.x = theme,
                   strip.background = ggplot2::element_blank(),
                   legend.position = "none")
  if (!missing(logRrPositives)) {
    plot <- plot + ggplot2::geom_point(shape = 23,
                                       ggplot2::aes(x, y),
                                       data = data.frame(x = exp(logRrPositives),
                                                         y = seLogRrPositives),
                                       size = 4,
                                       fill = rgb(1, 1, 0),
                                       alpha = 0.8)
  }
  if (!missing(title)) {
    plot <- plot + ggplot2::ggtitle(title)
  }
  if (!is.null(fileName))
    ggplot2::ggsave(fileName, plot, width = 6, height = 4.5, dpi = 400)
  return(plot)
}

logRrtoSE <- function(logRr, alpha, mu, sigma) {
  phi <- (mu-logRr)^2/qnorm(alpha/2)^2-sigma^2
  phi[phi<0] <- 0
  se <- sqrt(phi)
  return(se)
}

plotCalibration <- function(logRr,
                            seLogRr,
                            useMcmc = FALSE,
                            legendPosition = "right",
                            title,
                            fileName = NULL) {
  if (any(is.infinite(seLogRr))) {
    warning("Estimate(s) with infinite standard error detected. Removing before fitting null distribution")
    logRr <- logRr[!is.infinite(seLogRr)]
    seLogRr <- seLogRr[!is.infinite(seLogRr)]
  }
  if (any(is.infinite(logRr))) {
    warning("Estimate(s) with infinite logRr detected. Removing before fitting null distribution")
    seLogRr <- seLogRr[!is.infinite(logRr)]
    logRr <- logRr[!is.infinite(logRr)]
  }
  if (any(is.na(seLogRr))) {
    warning("Estimate(s) with NA standard error detected. Removing before fitting null distribution")
    logRr <- logRr[!is.na(seLogRr)]
    seLogRr <- seLogRr[!is.na(seLogRr)]
  }
  if (any(is.na(logRr))) {
    warning("Estimate(s) with NA logRr detected. Removing before fitting null distribution")
    seLogRr <- seLogRr[!is.na(logRr)]
    logRr <- logRr[!is.na(logRr)]
  }

  data <- data.frame(logRr = logRr, SE = seLogRr)
  data$Z <- data$logRr/data$SE
  data$P <- 2 * pmin(pnorm(data$Z), 1 - pnorm(data$Z))  # 2-sided p-value
  data$Y <- sapply(data$P, function(x) {
    sum(data$P < x)/nrow(data)
  })

  data$calibratedP <- vector(length = nrow(data))
  for (i in 1:nrow(data)) {
    dataLeaveOneOut <- data[seq(1, nrow(data)) != i, ]
    if (useMcmc) {
      null <- EmpiricalCalibration::fitMcmcNull(dataLeaveOneOut$logRr, dataLeaveOneOut$SE)
    } else {
      null <- EmpiricalCalibration::fitNull(dataLeaveOneOut$logRr, dataLeaveOneOut$SE)
    }
    data$calibratedP[i] <- EmpiricalCalibration::calibrateP(null, data$logRr[i], data$SE[i], pValueOnly = TRUE)
  }
  data <- data[!is.na(data$calibratedP), ]
  data$AdjustedY <- sapply(data$calibratedP, function(x) {
    sum(data$calibratedP < x)/nrow(data)
  })

  catData <- data.frame(x = c(data$P, data$calibratedP),
                        y = c(data$Y, data$AdjustedY),
                        label = factor(c(rep("Theoretical", times = nrow(data)),
                                         rep("Empirical", times = nrow(data)))))
  catData$label <- factor(catData$label, levels = c("Empirical", "Theoretical"))

  names(catData) <- c("x", "y", "P-value calculation")

  breaks <- c(0, 0.25, 0.5, 0.75, 1)
  theme <- ggplot2::element_text(colour = "#000000", size = 10)
  themeRA <- ggplot2::element_text(colour = "#000000", size = 10, hjust = 1)
  plot <- with(catData, {
    ggplot2::ggplot(catData,
                    ggplot2::aes(x = x,
                                 y = y,
                                 colour = `P-value calculation`,
                                 linetype = `P-value calculation`),
                    environment = environment()) +
      ggplot2::geom_vline(xintercept = breaks,
                          colour = "#AAAAAA",
                          lty = 1,
                          size = 0.3) +
      ggplot2::geom_vline(xintercept = 0.05, colour = "#888888", linetype = "dashed", size = 1) +
      ggplot2::geom_hline(yintercept = breaks, colour = "#AAAAAA", lty = 1, size = 0.3) +
      ggplot2::geom_abline(colour = "#AAAAAA", lty = 1, size = 0.3) +
      ggplot2::geom_step(direction = "hv", size = 1) +
      ggplot2::scale_colour_manual(values = c(rgb(0, 0, 0), rgb(0, 0, 0), rgb(0.5, 0.5, 0.5))) +
      ggplot2::scale_linetype_manual(values = c("solid", "twodash")) +
      ggplot2::scale_x_continuous(expression(alpha), limits = c(0, 1), breaks = c(breaks, 0.05), labels = c("", ".25", ".50", ".75", "1", ".05")) +
      ggplot2::scale_y_continuous(expression(paste("Fraction with p < ", alpha)), limits = c(0, 1), breaks = breaks, labels = c("0", ".25", ".50", ".75", "1")) +
      ggplot2::theme(panel.grid.minor = ggplot2::element_blank(),
                     panel.background = ggplot2::element_rect(fill = "#FAFAFA", colour = NA),
                     panel.grid.major = ggplot2::element_blank(),
                     axis.ticks = ggplot2::element_blank(),
                     axis.text.y = themeRA,
                     axis.text.x = theme,
                     strip.text.x = theme,
                     strip.background = ggplot2::element_blank(),
                     legend.position = legendPosition)
  })
  if (!missing(title)) {
    plot <- plot + ggplot2::ggtitle(title)
  }
  if (!is.null(fileName))
    ggplot2::ggsave(fileName, plot, width = 6, height = 4.5, dpi = 400)
  return(plot)
}

printCovariateStats <- function(folders,
                                caption = NULL,
                                floating = FALSE) {
  table <-
    do.call(rbind,
            lapply(folders, FUN = function(file) {
              table <- read.csv(file.path(studyFolder,
                                          file,
                                          "Balance.csv"), as.is = TRUE)
              idx <- which(folders == file)
              name <- paste0(label1[idx], " ", label2[idx])
              name <-  sub("\\^(\\d)", "$^\\1$", name)
              table <- data.frame(db = name,
                                  totalCovariates = nrow(table),
                                  space0 = "",
                                  meanBefore = max(abs(table$beforeMatchingStdDiff),
                                                   na.rm = TRUE),
                                  unbalancedBefore = length(which(abs(table$beforeMatchingStdDiff)>0.05))/nrow(table),
                                  space1 = "",
                                  meanAfter = max(abs(table$afterMatchingStdDiff),
                                                  na.rm = TRUE),
                                  unbalancedAfter = length(which(abs(table$afterMatchingStdDiff)>0.05))/nrow(table))
              table
            }))

  for (column in c("unbalancedBefore", "unbalancedAfter")) {
    table[,column] <- paste0(formatC(100 * table[,column], digits = 2), "\\%")
  }

  addtorow <- list()
  addtorow$pos <- list(0,0,0,nrow(table))
  #addtorow$pos[[1]] <- 0
  #addtorow$pos[[2]] <- nrow(table)
  addtorow$command <- c("&&&\\multicolumn{2}{c}{Before PS} & & \\multicolumn{2}{c}{After PS} \\\\\n",
                        "\\cline{4-5} \\cline{7-8} \\\\[-1em]\n",
                        "Data source & Covariates && mean & \\textgreater 0.05 & & mean & \\textgreater 0.05 \\\\\n \\hline \\\\[-0.5em]",
                        "[0.4em]\\hline \n")

  output <- xtable(table, caption = caption)

  align(output) <- "clrrrrrrr"
  print.xtable(output, include.rownames = FALSE, include.colnames = FALSE,
               add.to.row = addtorow,
               hline.after = c(-1),
               sanitize.text.function=function(x) { x },
               floating = floating
  )
}

printAdjustedP <- function(analysisId,
                           folders,
                           labels,
                           cohortOrder,
                           prettyCohortNames,
                           singleTable = FALSE,
                           caption = NULL,
                           floating = FALSE) {

  allCohorts <- read.csv(system.file("settings",
                                     "CohortsToCreate.csv",
                                     package = "AlendronateVsRaloxifene"))
  allCohorts <- allCohorts[3:nrow(allCohorts),]

  allCohorts <- allCohorts[allCohorts$name %in% cohortOrder, ]

  allResults <- getData(folders)

  results <- allResults
  results <- results[results$analysisId == analysisId, ]
  results <- results[results$outcomeName %in% cohortOrder, ]
  results <- results[results$db %in% folders, ]

  results$space1 <- ""
  results$space2 <- ""

  results$order1 <- getOrder(results$db, folders)
  results$order2 <- getOrder(as.character(results$outcomeName), cohortOrder)
  results <- results[order(results$order2,
                           results$order1,
                           decreasing = FALSE),]

  crude <- results[,c("db", "space1", "logRr", "seLogRr", "p", "space2",
                      "calibratedP", "calibratedP_lb95ci", "calibratedP_ub95ci")]

  crude$logRr[which(is.na(crude$seLogRr))] = NA

  for (column in c("logRr", "seLogRr", "p",
                   "calibratedP", "calibratedP_lb95ci", "calibratedP_ub95ci")) {
    crude[,column] <- formatC(crude[,column], digits = 3)
  }

  crude$db <- c(labels)

  addtorow = list()
  addtorow$pos <- c(list(0, 0, 0),
                    nrow(crude)
  )

  if (singleTable) {
    addtorow$pos = c(addtorow$pos, nrow(crude) - 1)
  } else {
    addtorow$pos = c(addtorow$pos, as.list((length(folders)) * (0:(length(cohortOrder) - 1))))
  }

  getNameInBox <- function(name) {
    paste0("\\parbox[t]{2mm}{\\multirow{",
           round(length(folders) / 2),
           "}{*}{\\rotatebox[origin=c]{90}{",
           name,
           "}}}")
  }

  getNameAcross <- function(name) {
    paste0("\\\\[-0.5em]\n \\multicolumn{8}{c}{",
           name,
           "} \\\\\n")
  }

  addtorow$command <- c("& & \\multicolumn{3}{c}{Original Estimate} & & \\multicolumn{3}{c}{Calibrated P-value} \\\\\n",
                        " \\cline{3-5} \\cline{7-9} \\\\[-1em]\n",
                        "Data source & & Mean & SD & p-value & & p-value & 95\\% lb & 95\\% ub \\\\\n \\hline \n",
                        "[0.4em]\\hline "
  )
  if (singleTable) {
    addtorow$command = c(addtorow$command, "")
  } else {
    addtorow$command = c(addtorow$command, getNameAcross(prettyCohortNames))
  }

  tab <- xtable(crude, caption = caption)

  align(tab) <- "crrrrrrrrr"
  print.xtable(tab, include.rownames = FALSE, include.colnames = FALSE,
               add.to.row = addtorow,
               hline.after = c(-1),
               sanitize.text.function=function(x) { x },
               floating = floating)
}

printNegControlCoverage <- function(analysisId,
                                    folders,
                                    labels,
                                    caption = NULL,
                                    floating = FALSE) {

  pathToCsv <- system.file("settings", "NegativeControls.csv", package = "AlendronateVsRaloxifene")
  negativeControls <- read.csv(pathToCsv)
  negControlCohortIds <- negativeControls$conceptId

  allResults <- getData(folders)

  allResults <- allResults[allResults$analysisId == analysisId & allResults$outcomeId %in% negControlCohortIds, ]

  allResults$validOutcome = allResults$coverageCI = allResults$coverageP = allResults$coverageCalibratedP = 1
  allResults$validOutcome[which(is.na(allResults$seLogRr))] = 0
  allResults$coverageCI[which(allResults$ci95lb > 1 | allResults$ci95ub < 1)] = 0
  allResults$coverageP[which(allResults$p < 0.05)] = 0
  allResults$coverageCalibratedP[which(allResults$calibratedP < 0.05)] = 0
  allResults$coverageCI = allResults$coverageCI * allResults$validOutcome
  allResults$coverageP = allResults$coverageP * allResults$validOutcome
  allResults$coverageCalibratedP = allResults$coverageCalibratedP * allResults$validOutcome

  temp = aggregate(allResults[,c("validOutcome", "coverageCI", "coverageP", "coverageCalibratedP")],
                   by = list(db = allResults$db),
                   sum)

  temp1 = aggregate(allResults[,c("null_mean", "null_sd")],
                    by = list(db = allResults$db),
                    mean)

  crude = merge(temp1, temp)
  crude = crude[match(folders, crude$db),]
  crude$db = c(labels)
  crude = crude[,c("db","null_mean","null_sd","validOutcome","coverageCI","coverageP","coverageCalibratedP")]

  for (column in c("null_mean", "null_sd")) {
    crude[,column] <- formatC(crude[,column], digits = 3)
  }

  for (column in c("coverageCI","coverageP","coverageCalibratedP")) {
    crude[,column] <- paste0(crude[,column]," (",round(100*crude[,column]/crude[,c("validOutcome")]),"\\%)")
  }

  crude[,"validOutcome"] = formatC(crude[,"validOutcome"])

  addtorow = list()
  addtorow$pos <- c(list(0, 0, 0),
                    nrow(crude)
  )

  getNameInBox <- function(name) {
    paste0("\\parbox[t]{2mm}{\\multirow{",
           round(length(folders) / 2),
           "}{*}{\\rotatebox[origin=c]{90}{",
           name,
           "}}}")
  }

  getNameAcross <- function(name) {
    paste0("\\\\[-0.5em]\n \\multicolumn{8}{c}{",
           name,
           "} \\\\\n")
  }

  addtorow$command <- c("& \\multicolumn{2}{c}{Empirical Null Dist.} & & \\multicolumn{3}{c}{Coverage of Null Effect} \\\\\n",
                        "\\cline{2-3} \\cline{5-7} \\\\[-1em]\n",
                        "Data source & Mean & SD & Controls & Empirical CI & Theoretical p & Calibrated p \\\\\n \\hline \n",
                        "[0.4em]\\hline "
  )

  tab <- xtable(crude, caption = caption)

  align(tab) <- "crrrrrrr"
  print.xtable(tab, include.rownames = FALSE, include.colnames = FALSE,
               add.to.row = addtorow,
               hline.after = c(-1),
               sanitize.text.function=function(x) { x },
               floating = floating)
}

# ciCalibrationModelsIIT <- lapply(1:length(folders), FUN = function(idx) {
#   file <- folders[idx]
#   #print(file)
#   emp <-  read.csv(file.path(studyFolder,
#                              file, "tablesAndFigures",
#                              "EmpiricalCalibration.csv"))
#
#   emp <- emp[emp$analysisId == 1 & emp$outcomeId %in% negControlCohortIds, ]
#   model = EmpiricalCalibration::fitSystematicErrorModel(emp$logRr, emp$seLogRr, rep(0,nrow(emp)),
#                                                         estimateCovarianceMatrix = FALSE)
# })
#
# ciCalibrationModelsPP <- lapply(1:length(folders), FUN = function(idx) {
#   file <- folders[idx]
#   #print(file)
#   emp <-  read.csv(file.path(studyFolder,
#                              file, "tablesAndFigures",
#                              "EmpiricalCalibration.csv"))
#
#   emp <- emp[emp$analysisId == 2 & emp$outcomeId %in% negControlCohortIds, ]
#   model = EmpiricalCalibration::fitSystematicErrorModel(emp$logRr, emp$seLogRr, rep(0,nrow(emp)),
#                                                         estimateCovarianceMatrix = FALSE)
# })
#
# buildCalibratedOutcomeResults <- function(folders, analysisId, cohort, ciCalibrationModels, summaryExclude = c()) {
#   allResults <- getData(folders)
#   results <- allResults
#   results <- results[results$analysisId == analysisId, ]
#   results <- results[results$outcomeName == cohort, ]
#   results <- results[results$db %in% folders, ]
#   results$labels <- label3
#
#   results$logRr <- ifelse(abs(results$logRr) > 10, NA, results$logRr)
#
#   results$order1 <- getOrder(results$db, folders)
#   results <- results[order(results$order1,
#                            decreasing = FALSE),]
#
#   #a = calibrateConfidenceInterval(logRr, seLogRr, model, ciWidth = 0.95)
#   a = mapply(function(logRr, seLogRr, model, ciWidth) {
#     return(EmpiricalCalibration::calibrateConfidenceInterval(logRr, seLogRr, model, ciWidth))
#   }, results$logRr, results$seLogRr, ciCalibrationModels, rep(0.95, nrow(results)))
#
#   results$newlogRr = unlist(a[c("logRr"),])
#   results$newlogLb95Ci = unlist(a[c("logLb95Rr"),])
#   results$newlogUb95Ci = unlist(a[c("logUb95Rr"),])
#
#   forest <- plotForest(logRr = results$newlogRr,
#                        logLb95Ci = results$newlogLb95Ci,
#                        logUb95Ci = results$newlogUb95Ci,
#                        names = results$db,
#                        labels = results$labels,
#                        xLabel = "Hazard Ratio",
#                        breaks = c(0.25, 0.5, 1, 2, 4),
#                        summaryExclude = summaryExclude,
#                        fileName = NULL)
# }
