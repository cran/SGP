`visualizeSGP` <- 
  function(sgp_object,
           state,
           years,
           content_areas,
           districts,
           schools,
           grades,
           plot.types=c("bubblePlot", "studentGrowthPlot", "growthAchievementPlot"),
           bubblePlot.config=list(summary.data=sgp_object@Summary[["SCHOOL_NUMBER"]][["SCHOOL_NUMBER__CONTENT_AREA__YEAR__SCHOOL_ENROLLMENT_STATUS"]][
             SCHOOL_ENROLLMENT_STATUS=="Enrolled Full Academic Year in School"],
             use.school.names=TRUE,
             x.variable = "MEDIAN_SGP", 
             y.variable = list(CURRENT="PERCENT_AT_ABOVE_PROFICIENT", PRIOR="PERCENT_AT_ABOVE_PROFICIENT_PRIOR"),
             size.variable = "MEDIAN_SGP_COUNT",
             subset.factor = NULL,
             main.title = list(CURRENT="Growth and Current Achievement", PRIOR="Growth and Prior Achievement"),
             sub1.title = "Demonstration State School Performance",
             plot.extras = c("grid.text(x=unit(50, 'native'), y=unit(50, 'native'), 
			'DRAFT - DO NOT DISTRIBUTE', rot=-30, gp=gpar(col='grey80', cex=2.9, alpha=0.8, fontface=2))",
			"grid.lines(x=unit(50, 'native'), y=c(0,1), gp=gpar(col='grey40', lwd=1.5, lty=2, alpha=0.5))"),
             pdf.folder="Visualizations/Summary"),
           studentGrowthPlot.config=list(
             header.footer.color="goldenrod3",
             front.page=NULL,
             pdf.folder="Visualizations/Individual",
             folder.names="number",
             student.growth.projection.fan=TRUE,
             anonymize=FALSE,
             remove.auxillary.files=TRUE)) {

    ### Setting variables to NULL to prevent R CMD check warnings

    SCHOOL_NUMBER <- SCHOOL_NAME <- YEAR <- CONTENT_AREA <- NULL ## To prevent R CMD check warnings
    ETHNICITY <- GENDER <- ID <- NULL ## To prevent R CMD check warnings
    TEST_LEVEL <- SUBJECT_CODE <- SCALE_SCORE <- GRADE <- NULL ## To prevent R CMD check Warnings
    SCHOOL_ENROLLMENT_STATUS <- CUTLEVEL <- NULL


    ### Utility functions	

    "%w/o%" <- function(x,y) x[!x %in% y]

    capwords <- function(x) {
      special.words <- c("ELA", "II", "III", "IV")
      if (x %in% special.words) return(x)
      s <- strsplit(x, split=" ")[[1]]
      s <- paste(toupper(substring(s, 1,1)), tolower(substring(s, 2)), sep="", collapse=" ")
      s <- strsplit(s, split="-")[[1]]
      paste(toupper(substring(s, 1,1)), substring(s, 2), sep="", collapse="-")
    }


    #### Define/Calculate relevant quantities for bubblePlot and studentGrowthPlot

    # Year stuff

    if (missing(years)) {
      tmp.years <- tail(sort(unique(sgp_object@Data$YEAR)), 5)
      tmp.last.year <- tail(tmp.years, 1)
    } else {
      tmp.all.years <- sort(unique(sgp_object@Data$YEAR))
      tmp.years <- tail(tmp.all.years[1:which(tmp.years==year)], 5)
      tmp.last.year <- tail(tmp.years, 1)
    }

    # Grade stuff

    if (missing(grades)) {
      tmp.grades <- stateData[[state]][["Student_Report_Information"]]$Grades_Reported
    } else {
      tmp.grades <- grades
    }

    # District stuff

    if (missing(districts)) {
      tmp.districts <- sort(unique(sgp_object@Data[YEAR==tmp.last.year]$DISTRICT_NUMBER)) %w/o% NA
    } else {
      tmp.districts <- districts 
      if (is.factor(sgp_object@Data$DISTRICT_NUMBER)) tmp.districts <- as.factor(tmp.districts)
    }

    # School stuff

    if (missing(schools)) {
      tmp.schools <- sort(unique(sgp_object@Data[YEAR==tmp.last.year]$SCHOOL_NUMBER)) %w/o% NA
    } else {
      tmp.schools <- schools
      if (is.factor(sgp_object@Data$SCHOOL_NUMBER)) tmp.schools <- as.factor(tmp.schools)
    }

    # Content area stuff

    if ("studentGrowthPlot" %in% plot.types) { 
      tmp.content.areas.studentGrowthPlot <- sort(unique(sgp_object@Data[YEAR==tmp.last.year]$CONTENT_AREA)) %w/o% NA
    } 
    if (any(c("bubblePlot", "growthAchievementPlot") %in% plot.types)) {
	if (!missing(content_areas)) {
           tmp.content.areas.other <- content_areas
           if (is.factor(sgp_object@Data$CONTENT_AREA)) tmp.content.areas.other <- as.factor(tmp.content.areas.other) ## Factor joins to Factor
        } else {
           tmp.content.areas.other <- sort(unique(sgp_object@Data[YEAR==tmp.last.year]$CONTENT_AREA)) %w/o% NA
        }
    }


    ##############################################################################################################
    ########### bubblePlot
    ##############################################################################################################

    if ("bubblePlot" %in% plot.types) {

      if (bubblePlot.config[["use.school.names"]]) {
        tmp.names <- data.table(sgp_object@Data[, list(SCHOOL_NUMBER, SCHOOL_NAME, YEAR)], key="SCHOOL_NUMBER, YEAR")
        if ("YEAR" %in% names(bubblePlot.config[["summary.data"]])) {
          key(bubblePlot.config[["summary.data"]]) <- c("SCHOOL_NUMBER", "YEAR") 
        } else key(bubblePlot.config[["summary.data"]]) <- "SCHOOL_NUMBER"
        bubblePlot.config[["summary.data"]] <- tmp.names[bubblePlot.config[["summary.data"]], mult="last"]
        BUBBLE_TITLES <- "SCHOOL_NAME"
      } else BUBBLE_TITLES <- "SCHOOL_NUMBER"

      for (y in names(bubblePlot.config[["y.variable"]])) {
        if (y=="CURRENT") ach="yrs" else ach = "yrs-1"
        for (c in tmp.content.areas.other){
          for (yrs in 2:length(years)) {
            #						for (s in unique(bubblePlot.config[["subset.factor"]])[!is.na(unique(bubblePlot.config[["subset.factor"]]))]) 
            #	if (!is.null(s)) SUBSET <- "which(bubblePlot.config[["subset.factor"]]==s)" else SUBSET <- NULL
            tmp <- subset(bubblePlot.config[["summary.data"]], CONTENT_AREA==c & YEAR==years[[yrs]] & !is.na(eval(parse(text=paste(bubblePlot.config[["x.variable"]])))))
            attach(tmp)
            bubblePlot(
                       bubble_plot_data.X=eval(parse(text=paste(bubblePlot.config[["x.variable"]]))),
                       bubble_plot_data.Y=eval(parse(text=paste(bubblePlot.config[["y.variable"]][[y]]))),
                       bubble_plot_data.SUBSET=NULL, #eval(parse(text=SUBSET)),
                       bubble_plot_data.INDICATE=NULL,
                       bubble_plot_data.BUBBLE_CENTER_LABEL=NULL,
                       bubble_plot_data.SIZE=eval(parse(text=paste(bubblePlot.config[["size.variable"]]))),
                       bubble_plot_data.LEVELS=NULL, #eval(parse(text=paste(bubblePlot.config[["subset.factor"]]))),
                       bubble_plot_data.BUBBLE_TIPS_LINES=list(paste(eval(parse(text=paste(bubblePlot.config[["x.variable"]]))), 
                         " (", eval(parse(text=paste(bubblePlot.config[["size.variable"]]))), ")", sep=""),
                         paste(round(eval(parse(text=paste(bubblePlot.config[["y.variable"]][[y]])))), 
                               " (", eval(parse(text=paste(bubblePlot.config[["y.variable"]][[y]], "_COUNT", sep=""))), ")", sep="")),

                       bubble_plot_labels.X=c("Growth", paste(years[[yrs]], "Median Student Growth Percentile")),
                       bubble_plot_labels.Y=c("Achievement", paste(years[[eval(parse(text=(ach)))]], capwords(bubblePlot.config[["y.variable"]][[y]]))),
                       bubble_plot_labels.SIZE=c(50, 100, 250, 500),
                       bubble_plot_labels.LEVELS=NULL, #levels(bubblePlot.config[["subset.factor"]]),
                       bubble_plot_labels.BUBBLE_TIPS_LINES=list(paste(years[[yrs]], "Median SGP (Count)"),
                         paste(years[[eval(parse(text=(ach)))]], capwords(bubblePlot.config[["y.variable"]][[y]])), " (Count)"),
                       bubble_plot_labels.BUBBLE_TITLES=eval(parse(text=paste(BUBBLE_TITLES))),
                       bubble_plot_titles.MAIN=bubblePlot.config[["main.title"]][[y]],
                       bubble_plot_titles.SUB1=bubblePlot.config[["sub1.title"]],
                       bubble_plot_titles.SUB2=paste(years[[yrs]], capwords(c)),
                       bubble_plot_titles.LEGEND1="School Size",
                       bubble_plot_titles.LEGEND2_P1=NULL,
                       bubble_plot_titles.LEGEND2_P2=NULL,

                       bubble_plot_configs.BUBBLE_MIN_MAX=c(0.04, 0.07),
                       bubble_plot_configs.BUBBLE_X_TICKS=seq(0,100,10),
                       bubble_plot_configs.BUBBLE_X_TICKS_SIZE=c(rep(0.6, 5), 1, rep(0.6, 5)),
                       bubble_plot_configs.BUBBLE_Y_TICKS=seq(0,100,10),
                       bubble_plot_configs.BUBBLE_SUBSET_INCREASE=0.01,
                       bubble_plot_configs.BUBBLE_COLOR="deeppink2",
                       bubble_plot_configs.BUBBLE_SUBSET_ALPHA=list(Transparent=0.3, Opaque=0.9),
                       bubble_plot_configs.BUBBLE_TIPS="TRUE",
                       bubble_plot_configs.BUBBLE_PLOT_DEVICE="PDF",
                       bubble_plot_configs.BUBBLE_PLOT_FORMAT="print",
                       bubble_plot_configs.BUBBLE_PLOT_LEGEND="TRUE",
                       bubble_plot_configs.BUBBLE_PLOT_TITLE="TRUE",
                       bubble_plot_configs.BUBBLE_PLOT_EXTRAS=bubblePlot.config[["plot.extras"]],
                       bubble_plot_configs.BUBBLE_PLOT_NAME=paste(years[[yrs]], capwords(c), "Growth_and", capwords(y),"Achievement.pdf", sep="_"),
                       bubble_plot_configs.BUBBLE_PLOT_PATH=bubblePlot.config[["pdf.folder"]],
                       bubble_plot_pdftk.CREATE_CATALOG=FALSE)
            detach(tmp)
          } # END years loop
        } # END c - content area loop
      } # END y.var loop			
    } # END "if ("bubblePlot" %in% plot.types)"


    ####################################################################################################################
    ########### studentGrowthPlot
    ####################################################################################################################

    if ("studentGrowthPlot" %in% plot.types) {

      ### Utility functions

      pretty_year <- function(x) sub("_", "-", x)

      rbind.all <- function(.list, ...) {
        if (length(.list)==1) return (.list[[1]])
        Recall(c(list(rbind(.list[[1]], .list[[2]], ...)), .list[-(1:2)]), ...)
      }

      piecewise.transform <- function(scale_score, state, content_area, grade, output.digits=1) {
        if (is.null(stateData[[state]][["Student_Report_Information"]][["Modulo_Score_Transformation"]])) {
          tmp.loss.hoss <- c(max(0, min(scale_score, na.rm=TRUE)), min(600, max(scale_score, na.rm=TRUE)))
          tmp.old.cuts <- c(tmp.loss.hoss[1], stateData[[state]][["Achievement"]][["Cutscores"]][[as.character(content_area)]][[paste("GRADE_", grade, sep="")]], 
                            tmp.loss.hoss[2])
        } else {
          tmp.modulo <- stateData[[state]][["Student_Report_Information"]][["Modulo_Score_Transformation"]]
          scale_score <- scale_score %% tmp.modulo
          tmp.old.cuts <- stateData[[state]][["Achievement"]][["Cutscores"]][[as.character(content_area)]][[paste("GRADE_", grade, sep="")]] %% tmp.modulo
        }
        tmp.new.cuts <- stateData[[state]][["Student_Report_Information"]][["Transformed_Achievement_Level_Cutscores"]]
        tmp.index <- findInterval(scale_score, tmp.old.cuts, rightmost.closed=TRUE)
        tmp.diff <- diff(tmp.new.cuts)/diff(tmp.old.cuts)
        round(tmp.new.cuts[tmp.index] + (scale_score - tmp.old.cuts[tmp.index]) * (diff(tmp.new.cuts)/diff(tmp.old.cuts))[tmp.index], digits=output.digits)
      }

      create.long.cutscores <- function(state, content_area) {
        number.achievement.level.regions <- length(stateData[[state]][["Student_Report_Information"]][["Achievement_Level_Labels"]])
        if (is.null(stateData[[state]][["Student_Report_Information"]][["Transformed_Achievement_Level_Cutscores"]])) {
          tmp.grades <- as.numeric(matrix(unlist(strsplit(names(stateData[[state]][["Achievement"]][["Cutscores"]][[content_area]]), "_")),
                                          ncol=2, byrow=TRUE)[,2])
          tmp.cutscores <- matrix(unlist(stateData[[state]][["Achievement"]][["Cutscores"]][[content_area]]),
                                  ncol=number.achievement.level.regions-1, byrow=TRUE)
          tmp.list <- list()
          for (i in seq(number.achievement.level.regions-1)) {
            tmp.list[[i]] <- data.frame(GRADE=c(min(tmp.grades,na.rm=TRUE)-1, tmp.grades, max(tmp.grades,na.rm=TRUE)+1),
                                        CUTLEVEL=rep(i, length(tmp.grades)+2),
                                        CUTSCORES=c(extendrange(tmp.cutscores[,i], f=0.15)[1], tmp.cutscores[,i], extendrange(tmp.cutscores[,i], f=0.15)[2]))
          }
          subset(do.call(rbind, tmp.list), CUTLEVEL %in% 1:(number.achievement.level.regions-1))
        } else {
          tmp.grades <- as.numeric(matrix(unlist(strsplit(names(stateData[[state]][["Achievement"]][["Cutscores"]][[content_area]]), "_")),
                                          ncol=2, byrow=TRUE)[,2])
          tmp.list <- list()
          for (i in seq(number.achievement.level.regions-1)) {
            tmp.list[[i]] <- data.frame(GRADE=c(min(tmp.grades,na.rm=TRUE)-1, tmp.grades, max(tmp.grades,na.rm=TRUE)+1),
                                        CUTLEVEL=rep(i, length(tmp.grades)+2),
                                        CUTSCORES=rep(stateData[[state]][["Student_Report_Information"]][["Transformed_Achievement_Level_Cutscores"]][i+1],
                                          length(tmp.grades)+2))
          }
          do.call(rbind, tmp.list)
        }
      }

      ### Define quantities/variables

      test.abbreviation <- stateData[[state]][["Assessment_Program_Information"]][["Assessment_Abbreviation"]]
      Cutscores <- list()
      for (i in tmp.content.areas.studentGrowthPlot) {
        Cutscores[[i]] <- create.long.cutscores(state, i)
      }

      # State stuff

      if (state %in% c(state.abb, "DEMO")) {
        tmp.state <- paste(state.name[state==state.abb], stateData[[state]][["Assessment_Program_Information"]][["Assessment_Abbreviation"]])
        tmp.organization <- stateData[[state]][["Assessment_Program_Information"]][["Organization"]]
      } else {
        tmp.state <- state
        tmp.organization <- list(name=paste(state, "Department of Education"), URL=paste("http://www", state, "gov", sep="."), Phone_Number="123-456-7890")
      }	


      ### Subset data

      key(sgp_object@Data) <- c("VALID_CASE", "CONTENT_AREA", "YEAR", "DISTRICT_NUMBER", "SCHOOL_NUMBER")
      report.ids <- unique(sgp_object@Data[CJ("VALID_CASE", tmp.content.areas.studentGrowthPlot, tmp.last.year, tmp.districts, tmp.schools), mult="all", nomatch=0]$ID)
      key(sgp_object@Data) <- c("ID", "CONTENT_AREA", "YEAR")
      tmp.table <- CJ(report.ids, tmp.content.areas.studentGrowthPlot, tmp.years) 
      key(tmp.table) <- names(tmp.table) <- key(sgp_object@Data)
      tmp.table <- sgp_object@Data[tmp.table]

      ### Anonymize (if requested)
      
      if (studentGrowthPlot.config$anonymize) {
        require(randomNames)
        tmp.dt <- tmp.table[,list(ID, ETHNICITY, GENDER)]
        key(tmp.dt) <- "ID"
        tmp.dt <- tmp.dt[!duplicated(tmp.dt),]

        tmp.dt$LAST_NAME <- randomNames(gender=tmp.dt$GENDER, ethnicity=tmp.dt$ETHNICITY, which.names="last")
        tmp.dt$FIRST_NAME <- randomNames(gender=tmp.dt$GENDER, ethnicity=tmp.dt$ETHNICITY, which.names="first")

        names.dt <- tmp.dt[,list(ID, LAST_NAME, FIRST_NAME)]
        key(names.dt) <- "ID"

        key(tmp.table) <- "ID"
        tmp.table <- names.dt[tmp.table]

      } ## END create.random.names


      ### Transformed scale score stuff

      if (stateData[[state]][["Student_Report_Information"]][["Vertical_Scale"]]=="No") {
        tmp.table$TRANSFORMED_SCALE_SCORE <- tmp.table[,
                                                       piecewise.transform(SCALE_SCORE, state, SUBJECT_CODE[1], TEST_LEVEL[1]), by=list(CONTENT_AREA, GRADE)]$V1
      } else {
        tmp.table$TRANSFORMED_SCALE_SCORE <- tmp.table$SCALE_SCORE
      }


      ### Reshape data

      variables.to.keep <- c("VALID_CASE", "ID", "LAST_NAME", "FIRST_NAME", "CONTENT_AREA", "YEAR", "GRADE", "SCALE_SCORE", "TRANSFORMED_SCALE_SCORE",
                             "ACHIEVEMENT_LEVEL", "SGP", "SCHOOL_NAME", "SCHOOL_NUMBER", "DISTRICT_NAME", "DISTRICT_NUMBER")

      isr_data <- reshape(tmp.table[,variables.to.keep, with=FALSE],
                          idvar=c("ID", "CONTENT_AREA"),
                          timevar="YEAR",
                          drop=c("VALID_CASE"),
                          direction="wide")

      variables.to.keep <- c("ID", "CONTENT_AREA", paste("LAST_NAME", tmp.last.year, sep="."), paste("FIRST_NAME", tmp.last.year, sep="."), paste("GRADE", tmp.years, sep="."), 
                             paste("SGP", tmp.years, sep="."), paste("SCALE_SCORE", tmp.years, sep="."), paste("TRANSFORMED_SCALE_SCORE", tmp.years, sep="."), 
                             paste("ACHIEVEMENT_LEVEL", tmp.years, sep="."),
                             paste("SCHOOL_NAME", tmp.last.year, sep="."), paste("SCHOOL_NUMBER", tmp.last.year, sep="."), 
                             paste("DISTRICT_NAME", tmp.last.year, sep="."), paste("DISTRICT_NUMBER", tmp.last.year, sep="."))  

      key(isr_data) <- c("ID", "CONTENT_AREA")
      
      isr_data <- isr_data[, variables.to.keep, with=FALSE, mult="all", nomatch=0]


      ### Merge in 1 year projections (if requested & available)

      tmp.proj.names <- paste(tmp.content.areas.studentGrowthPlot, tmp.last.year, sep=".")
      if (studentGrowthPlot.config$student.growth.projection.fan & all(tmp.proj.names %in% names(sgp_object@SGP[["SGProjections"]]))) {
        tmp.list <- list()
        for (i in tmp.proj.names) {
          tmp.list[[i]] <- data.table(CONTENT_AREA=unlist(strsplit(i, "[.]"))[1],
                                      sgp_object@SGP[["SGProjections"]][[i]][,c(1, grep("PROJ_YEAR_1", names(sgp_object@SGP[["SGProjections"]][[i]])))])
        }
        isr_data <- data.table(rbind.all(tmp.list), key=paste(key(isr_data), collapse=","))[isr_data]
      }

      ## Big object removal and garbage collection

      rm(sgp_object); gc()


      #####
      ##### STUDENT REPORT PRODUCTION
      #####


      ### Define relevant variables 

      year_folder <- tmp.last.year

      ### Loop over unique DISTRICTS, SCHOOLS, GRADES and then STUDENTS

      tmp.keys <- paste(c("DISTRICT_NUMBER", "SCHOOL_NUMBER", "GRADE", "LAST_NAME", "FIRST_NAME"), tmp.last.year, sep=".")

      ## Districts

      key(isr_data) <- tmp.keys[1]

      for (i in tmp.districts) {

        if (studentGrowthPlot.config$anonymize) {
          tmp_district_name <- "Sample District"; district_folder <- paste("Sample_District_", which(i==tmp.districts), sep="")
        } else {
          tmp_district_name <- as.character(isr_data[J(i)]$DISTRICT_NAME[1])
          if (studentGrowthPlot.config$folder.names=="name") {
            district_folder <- gsub(" ", "_", paste(tmp_district_name, " (", i, ")", sep=""))
          } else {
            district_folder <- as.character(i)
          }
        }

        tmp_district_ids <- unique(isr_data[J(i)]$ID)
        tmp_district_data <- subset(isr_data, ID %in% tmp_district_ids)

        ## Schools

        key(tmp_district_data) <- tmp.keys[1]
        schools <- data.table(unique(data.table(tmp_district_data[J(i),  paste(c("SCHOOL_NUMBER", "SCHOOL_NAME"), tmp.last.year, sep="."), with=FALSE], 
                                                key=paste("SCHOOL_NUMBER", tmp.last.year, sep="."))), key=paste("SCHOOL_NAME", tmp.last.year, sep="."))[[paste("SCHOOL_NUMBER", tmp.last.year, sep=".")]]

        key(tmp_district_data) <- tmp.keys[2]

        for (j in schools) {
          if (studentGrowthPlot.config$anonymize) {
            tmp_school_name <- "Sample School"; school_folder <- paste("Sample_School_", which(j==schools), sep="")
          } else {
            tmp_school_name <- as.character(tmp_district_data[J(j)]$SCHOOL_NAME[1])
            if (studentGrowthPlot.config$folder.names=="name") {
              school_folder <- gsub(" ", "_", paste(tmp_school_name, " (", j, ")", sep=""))
            } else {
              school_folder <- as.character(j)
            }
          }

          tmp_school_ids <- unique(tmp_district_data[J(j)]$ID)
          tmp_school_data <- subset(tmp_district_data, ID %in% tmp_school_ids)


          ######################## SCHOOL Report Catalog LaTeX Header ##############################################
          cat("\\documentclass[pdftex]{book}
\\usepackage{hyperref,pdfpages}
\\hypersetup{%\n", file=paste("school_catalog_", j, ".tex", sep=""))
          cat(paste("pdftitle={", tmp_school_name, ": ", pretty_year(tmp.last.year), " ", tmp.state, " Growth and Achievement Reports},\n", sep=""), 
              file=paste("school_catalog_", j, ".tex", sep=""), append=TRUE)
          cat(paste("pdfauthor={", tmp.organization$Name, "/Center for Assessment Inc.},\n", sep=""), 
              "pdfcreator={pdfLaTeX},\n", 
              paste("pdfproducer={", tmp.organization$Name, "/Center for Assessment Inc.}}\n", sep=""), 
              "\\pdfminorversion=6
\\pdfobjcompresslevel=3
\\pdfcompresslevel=9
\\pdfmapfile{}
\\begin{document}\n", sep="", file=paste("school_catalog_", j, ".tex", sep=""), append=TRUE)
          cat(paste("\\pdfbookmark[-1]{", tmp_district_name, "}{", i, "}\n", sep=""), file=paste("school_catalog_", j, ".tex", sep=""), append=TRUE)
          cat(paste("\\pdfbookmark[0]{", tmp_school_name, "}{", j, "}\n", sep=""), file=paste("school_catalog_", j, ".tex", sep=""), append=TRUE)
          ###############################################################################################


          ## Grades

          key(tmp_school_data) <- tmp.keys[2]
          grades <- sort(unique(unlist(tmp_school_data[J(j), tmp.keys[3], with=FALSE]))) %w/o% NA
          key(tmp_school_data) <- tmp.keys[3]

          for (k in grades) {
            if (studentGrowthPlot.config$folder.names=="name") {
              grade_folder <- paste("Grade", k, sep="_")
            } else {
              grade_folder <- substr(paste("0", as.character(k), sep=""), nchar(k), nchar(k)+1)
            }
            path.to.pdfs <- file.path(studentGrowthPlot.config$pdf.folder, year_folder, district_folder, school_folder, grade_folder)
            dir.create(path.to.pdfs, recursive=TRUE, showWarnings=FALSE)

            tmp_grade_ids <- unique(tmp_school_data[J(k)]$ID)
            tmp_grade_data <- subset(tmp_school_data, ID %in% tmp_grade_ids)
            key(tmp_grade_data) <- tmp.keys[4:5]


            ################################ SCHOOL Report Catalog LaTeX Code ###########################
            cat(paste("\\pdfbookmark[1]{Grade ", k, "}{", j, k, "}\n", sep=""), file=paste("school_catalog_", j, ".tex", sep=""), append=TRUE) ## NOTE: j, k included in anchor for uniqueness
            ###############################################################################################


            ## Students

            for (n in unique(tmp_grade_data[["ID"]])) {
              tmp_student_data <- tmp_grade_data[ID==n]
              FIRST_NAME <- gsub(" ", "-", tmp_student_data[[tmp.keys[5]]][1]) 
              LAST_NAME <- gsub(" ", "-", tmp_student_data[[tmp.keys[4]]][1])
              if (studentGrowthPlot.config$anonymize) {
                student_number <- 1234567890
              } else {
                student_number <- n
              }


              ################################ SCHOOL Report Catalog LaTeX Code ###########################
              if (is.null(studentGrowthPlot.config$front.page)) {
                cat(paste("\\pdfbookmark[2]{", paste(LAST_NAME, ", ", FIRST_NAME, " (", student_number, ")", sep=""), "}{", n , "}
\\includepdf[fitpaper=true]{", path.to.pdfs, "/", FIRST_NAME, "_", LAST_NAME, "_", student_number, "_", year_folder, ".pdf}\n", sep=""), 
                    file=paste("school_catalog_", j, ".tex", sep=""), append=TRUE)
              } else {
                cat(paste("\\include{front_page.pdf}\n\\pdfbookmark[2]{", paste(LAST_NAME, ", ", FIRST_NAME, " (", student_number, ")", sep=""), "}{", n , "}
\\includepdf[fitpaper=true,pages=2]{", path.to.pdfs, "/", FIRST_NAME, "_", LAST_NAME, "_", student_number, "_", year_folder, ".pdf}\n", sep=""), 
                    file=paste("school_catalog_", j, ".tex", sep=""), append=TRUE)
              }
              ################################################################################################

              ################################ Front Page Attach LaTeX Code ###########################
              cat("\\documentclass[pdftex]{article}
\\usepackage{hyperref,pdfpages}
\\hypersetup{%\n", file=paste("student_report_", j, ".tex", sep=""))
              cat(paste("pdftitle={", FIRST_NAME, " ", LAST_NAME, " (", student_number, ")", ": ", pretty_year(tmp.last.year), " ", tmp.state, " Growth and Achievement Report},\n", sep=""), 
                  file=paste("student_report_", j, ".tex", sep=""), append=TRUE)
              cat(paste("pdfauthor={", tmp.organization$Name, "/Center for Assessment Inc.},\n", sep=""), 
                  "pdfcreator={pdfLaTeX},\n", 
                  paste("pdfproducer={", tmp.organization$Name, "/Center for Assessment Inc.}}\n", sep=""), 
                  "\\pdfminorversion=6
\\pdfobjcompresslevel=3
\\pdfcompresslevel=9
\\pdfmapfile{}
\\begin{document}\n", file=paste("student_report_", j, ".tex", sep=""), append=TRUE)
              if (!is.null(studentGrowthPlot.config$front.page)) {
                cat("\\includepdf[fitpaper=true]{front_page.pdf}\n", file=paste("student_report_", j, ".tex", sep=""), append=TRUE)
              }
              cat(paste("\\includepdf[fitpaper=true]{", path.to.pdfs, "/", FIRST_NAME, "_", LAST_NAME, "_", student_number, "_", year_folder, ".pdf}\n", sep=""), 
                  file=paste("student_report_", j, ".tex", sep=""), append=TRUE)

              cat("\\end{document}", file=paste("student_report_", j, ".tex", sep=""), append=TRUE)
              ########################################################################################

              ## Start pdf device
              
              if (length(tmp.content.areas.studentGrowthPlot)==2) {
                report.width=11
                report.height=8.5
              }
              if (length(tmp.content.areas.studentGrowthPlot)==3) {
                report.width=8.5
                report.height=11
              }
              if (!length(tmp.content.areas.studentGrowthPlot) %in% c(2,3)) {
                stop("Individual Student Report Templates currently only available for situations with 2 or 3 content areas.")
              }

              pdf(paste(path.to.pdfs, "/", FIRST_NAME, "_", LAST_NAME, "_", student_number, "_", year_folder, ".pdf", sep=""), 
                  width=report.width, height=report.height, version="1.4", encoding="ISOLatin2", family="URWHelvetica")


              ########################################################################################################
              ###
              ### Overall Report viewport creation
              ###
              ########################################################################################################

              if (length(tmp.content.areas.studentGrowthPlot)==2) {
                report.vp <- viewport(layout = grid.layout(4, 2, widths = unit(c(2.5, 8.5), rep("inches", 2)), 
                                        heights = unit(c(0.35, rep(3.8, 2), 0.55), rep("inches", 4))))

                content_area_1.vp <- viewport(layout.pos.row=2, layout.pos.col=2)
                content_area_2.vp <- viewport(layout.pos.row=3, layout.pos.col=2)
                top.border.vp <- viewport(layout.pos.row=1, layout.pos.col=1:2, xscale=c(0,1), yscale=c(0,1))
                bottom.border.vp <- viewport(layout.pos.row=4, layout.pos.col=1:2, xscale=c(0,1), yscale=c(0,1))
                left.legend.vp <- viewport(layout.pos.row=2:3, layout.pos.col=1, xscale=c(0,1), yscale=c(0,1))
              }

              if (length(tmp.content.areas.studentGrowthPlot)==3) {
                report.vp <- viewport(layout = grid.layout(6, 3, widths = unit(c(0.125, 8.3, 0.075), rep("inches", 3)), 
                                        heights = unit(c(0.27, 0.03, rep(3.54, 3), 0.08), rep("inches", 6))))

                content_area_1.vp <- viewport(layout.pos.row=3, layout.pos.col=2)
                content_area_2.vp <- viewport(layout.pos.row=4, layout.pos.col=2)
                content_area_3.vp <- viewport(layout.pos.row=5, layout.pos.col=2)
                top.border.vp <- viewport(layout.pos.row=1, layout.pos.col=1:3, xscale=c(0,3), yscale=c(0,1))
                bottom.border.vp <- viewport(layout.pos.row=6, layout.pos.col=1:3, xscale=c(0,3), yscale=c(0,1))
              }

              pushViewport(report.vp)

              for (vp in seq_along(tmp.content.areas.studentGrowthPlot)) {

		tmp_student_data <- as.data.frame(tmp_grade_data[ID==n & CONTENT_AREA==tmp.content.areas.studentGrowthPlot[vp]])
		pushViewport(get(paste("content_area_", vp, ".vp", sep="")))

                studentGrowthPlot(
                                  Scale_Scores=as.numeric(subset(tmp_student_data, select=paste("SCALE_SCORE", rev(tmp.years), sep="."))),
                                  Plotting_Scale_Scores=as.numeric(subset(tmp_student_data, select=paste("TRANSFORMED_SCALE_SCORE", rev(tmp.years), sep="."))),
                                  Achievement_Levels=as.character(unlist(subset(tmp_student_data, select=paste("ACHIEVEMENT_LEVEL", rev(tmp.years), sep=".")))),
                                  SGP=as.numeric(subset(tmp_student_data, select=paste("SGP", rev(tmp.years), sep="."))),
                                  Grades=as.numeric(subset(tmp_student_data, select=paste("GRADE", rev(tmp.years), sep="."))),
                                  Cuts_NY1=as.numeric(subset(tmp_student_data, select=grep("PROJ", names(tmp_student_data)))),
                                  Cutscores=Cutscores[[tmp.content.areas.studentGrowthPlot[vp]]],
                                  Report_Parameters=list(Current_Year=tmp.last.year, Content_Area=as.character(tmp.content.areas.studentGrowthPlot[vp]), State=state))

		popViewport()

              } ## END loop over tmp.content.areas.studentGrowthPlot


              ## Top Legend

              pushViewport(top.border.vp)
              grid.rect(gp=gpar(fill=studentGrowthPlot.config$header.footer.color, col=studentGrowthPlot.config$header.footer.color))
              grid.text(x=0.025, y=0.5, paste(FIRST_NAME, " ", LAST_NAME, sep="") , 
                        gp=gpar(fontface="bold", col="white", cex=1.5), just="left", default.units="native")
              grid.text(x=0.975, y=0.5, tmp_school_name, gp=gpar(fontface="bold", col="white", cex=1.5), just="right", default.units="native")
              popViewport()


              ## Bottom Legend

              pushViewport(bottom.border.vp)
              grid.rect(gp=gpar(fill=studentGrowthPlot.config$header.footer.color, col=studentGrowthPlot.config$header.footer.color))
              grid.text(x=0.02, y=0.75, paste("For more information please visit", tmp.organization$Name, "at", tmp.organization$URL, "or call", tmp.organization$Phone_Number), 
                        gp=gpar(cex=0.95, col="white"), default.units="native", just="left")
              grid.text(x=0.02, y=0.40, paste("Produced and distributed by the ", tmp.organization$Name, "/Center for Assessment, Inc.", sep=""), 
                        gp=gpar(cex=0.95, col="white"), default.units="native", just="left")

              copyright.text <- paste("Copyright 2011, ", tmp.organization$Name, "/Center for Assessment, Inc.", sep="")
              grid.text(x=0.975, y=0.18, paste("Copyright 2011, ", tmp.organization$Name, "/Center for Assessment, Inc.", sep=""), 
                        gp=gpar(col="white", cex=0.6), default.units="native", just="right")
              grid.text(x=unit(1.115, "native")-stringWidth(copyright.text), y=0.185, "\\co", gp=gpar(col="white", cex=0.7, fontfamily="HersheySymbol"), default.units="native")
              popViewport()


              ## Left Legend (Only with two content areas depicted)

              if (length(tmp.content.areas.studentGrowthPlot)==2) {

		pushViewport(left.legend.vp)

		# Interpretation

                interpretation.y <- 0.93

                grid.roundrect(x=unit(0.5, "native"), y=unit(interpretation.y, "native"), width=unit(0.9, "native"), height=unit(0.06, "native"), 
                               gp=gpar(fill=studentGrowthPlot.config$header.footer.color, col="black"))
                grid.text(x=0.5, y=interpretation.y+0.01, "How to interpret this growth", gp=gpar(fontface="bold", cex=0.95, col="white"))
                grid.text(x=0.5, y=interpretation.y-0.01, "and achievement report", gp=gpar(fontface="bold", cex=0.95, col="white"))

                grid.circle(x=0.2, y=interpretation.y-0.07, r=0.02, default.units="native")
                grid.text(x=0.275, y=interpretation.y-0.07, paste(test.abbreviation, "Test Score"), gp=gpar(cex=0.9), default.units="native", just="left")

                grid.polygon(x=c(0.1875, 0.1875, 0.17, 0.2, 0.23, 0.2125, 0.2125), y=interpretation.y-c(0.18, 0.13, 0.14, 0.10, 0.14, 0.13, 0.18), default.units="native",
                             gp=gpar(fill="grey50"))
                grid.text(x=0.275, y=interpretation.y-0.14, "Student's rate of growth", gp=gpar(cex=0.9), default.units="native", just="left")

		# Suggested uses

                suggested.y <- 0.52

                grid.roundrect(x=unit(0.5, "native"), y=unit(suggested.y, "native"), width=unit(0.9, "native"), height=unit(0.06, "native"), 
                               gp=gpar(fill=studentGrowthPlot.config$header.footer.color, col="black"))
                grid.text(x=0.5, y=suggested.y, "Suggested Uses", gp=gpar(fontface="bold", cex=0.95, col="white"))

                grid.circle(x=0.075, y=suggested.y-0.07, r=0.01, gp=gpar(fill="black"), default.units="native")
                grid.text(x=0.12, y=suggested.y-0.07, "Identify the rate of progress", gp=gpar(cex=0.8), default.units="native", just="left")
                grid.text(x=0.12, y=suggested.y-0.09, "needed in order to reach or", gp=gpar(cex=0.8), default.units="native", just="left")
                grid.text(x=0.12, y=suggested.y-0.11, "maintain proficient status", gp=gpar(cex=0.8), default.units="native", just="left")
                grid.text(x=0.12, y=suggested.y-0.13, paste("on the", test.abbreviation, "next year."), gp=gpar(cex=0.8), default.units="native", just="left")

                grid.circle(x=0.075, y=suggested.y-0.16, r=0.01, gp=gpar(fill="black"), default.units="native")
                grid.text(x=0.12, y=suggested.y-0.16, "Review past growth to assess", gp=gpar(cex=0.8), default.units="native", just="left")
                grid.text(x=0.12, y=suggested.y-0.18, paste("student progress toward", test.abbreviation), gp=gpar(cex=0.8), default.units="native", just="left")
                grid.text(x=0.12, y=suggested.y-0.20, "achievement goals", gp=gpar(cex=0.8), default.units="native", just="left")

                grid.circle(x=0.075, y=suggested.y-0.23, r=0.01, gp=gpar(fill="black"), default.units="native")
                grid.text(x=0.12, y=suggested.y-0.23, "Development of remediation or", gp=gpar(cex=0.8), default.units="native", just="left")
                grid.text(x=0.12, y=suggested.y-0.25, "enrichment plans based on rate of", gp=gpar(cex=0.8), default.units="native", just="left")
                grid.text(x=0.12, y=suggested.y-0.27, "growth needed to reach higher", gp=gpar(cex=0.8), default.units="native", just="left")
                grid.text(x=0.12, y=suggested.y-0.29, paste(test.abbreviation, "achievement levels"), gp=gpar(cex=0.8), default.units="native", just="left")

		# Extra stuff

                grid.lines(x=1.0, y=c(0.025,0.975), gp=gpar(lwd=1.8), default.units="native")

		popViewport()
                
              } ## END Left legend


              ## Turn pdf device off

              dev.off()

              ## Code to LaTeX document attaching first page

              system(paste("pdflatex -interaction=batchmode student_report_", j, ".tex", sep=""))
              file.rename(paste("student_report_", j, ".pdf", sep=""), paste(path.to.pdfs, "/", FIRST_NAME, "_", LAST_NAME, "_", student_number, "_", year_folder, ".pdf", sep=""))


            } ## END for loop for STUDENTS (n)
          } ## END for loop for GRADES (k)
          cat("\\end{document}", file=paste("school_catalog_", j, ".tex", sep=""), append=TRUE)
          system(paste("pdflatex -interaction=batchmode school_catalog_", j, ".tex", sep=""))
          system(paste("pdflatex -interaction=batchmode school_catalog_", j, ".tex", sep=""))
          file.rename(paste("school_catalog_", j, ".pdf", sep=""), file.path(studentGrowthPlot.config$pdf.folder, year_folder, district_folder, 
                                                                             paste(year_folder, "_", district_folder, "_", school_folder, "_Individual_SGP_Report_Catalog.pdf", sep="")))

          if (studentGrowthPlot.config$remove.auxillary.files) {
            files.to.remove <- list.files(pattern="tex|aux|log|out", all.files=TRUE)
            lapply(files.to.remove, file.remove)
          }

        } ## END for loop for SCHOOLS (j)
        #system(paste("zip -r ", pdf.folder, "/", year_folder, "/", district_folder, "_", tmp.last.year, ".zip ", pdf.folder, year_folder, district_folder, sep=""))
        print(paste("Finished with District", district_folder))
      } ## END for loop for DISTRICTS (i)
      
      if (studentGrowthPlot.config$remove.auxillary.files) {
        files.to.remove <- list.files(pattern="school_catalog|student_report")
        lapply(files.to.remove, file.remove)
      }

    } ## END "if ("studentGrowthPlot" %in% plot.types)"

  } ## END visualizeSGP Function
