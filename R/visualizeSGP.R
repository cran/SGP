`visualizeSGP` <- 
  function(sgp_object,
           plot.types=c("bubblePlot", "studentGrowthPlot", "growthAchievementPlot"),
           state,
           bPlot.years=NULL,
           bPlot.content_areas=NULL,
           bPlot.districts=NULL,
           bPlot.schools=NULL,
           bPlot.styles=c(1),
           bPlot.levels=NULL, 
           bPlot.full.academic.year=TRUE,
           bPlot.minimum.n=10,
           bPlot.anonymize=FALSE,
           bPlot.prior.achievement=TRUE, 
           bPlot.draft=FALSE,
           bPlot.format="print",
           bPlot.folder="Visualizations/bubblePlots",
           sgPlot.years=NULL,
           sgPlot.districts=NULL,
           sgPlot.schools=NULL,
           sgPlot.students=NULL,
           sgPlot.header.footer.color="#4CB9CC",
           sgPlot.front.page=NULL,
           sgPlot.folder="Visualizations/studentGrowthPlots",
           sgPlot.folder.names="number",
           sgPlot.fan=TRUE, 
           sgPlot.anonymize=FALSE,
           sgPlot.cleanup=TRUE,
           sgPlot.demo.report=TRUE,
           gaPlot.years=NULL,
           gaPlot.content_areas=NULL, 
           gaPlot.students=NULL,
           gaPlot.format="print",
           gaPlot.folder="Visualizations/growthAchievementPlots") {

    started.at <- proc.time()
    message(paste("Started visualizeSGP", date()))

    ### Setting variables to NULL to prevent R CMD check warnings

    DISTRICT_NUMBER <- DISTRICT_NAME <- SCHOOL_NUMBER <- SCHOOL_NAME <- YEAR <- CONTENT_AREA <- NULL ## To prevent R CMD check warnings
    ETHNICITY <- GENDER <- ID <- NULL ## To prevent R CMD check warnings
    TEST_LEVEL <- SUBJECT_CODE <- SCALE_SCORE <- GRADE <- NULL ## To prevent R CMD check warnings
    SCHOOL_ENROLLMENT_STATUS <- CUTLEVEL <- NULL ## To prevent R CMD check warnings
    MEDIAN_SGP <- MEDIAN_SGP_COUNT <- VALID_CASE <- NULL ## To prevent R CMD check warnings


   ### Create state (if missing) from sgp_object (if possible)

        if (missing(state)) {
                tmp.name <- gsub("_", " ", deparse(substitute(sgp_object)))
                if (any(sapply(c(state.name, "Demonstration"), function(x) regexpr(x, tmp.name)))==1) {
                        state <- c(state.abb, "DEMO")[which(sapply(c(state.name, "Demonstration"), function(x) regexpr(x, tmp.name))==1)]
                }
        }

    ### Utility functions	

    "%w/o%" <- function(x,y) x[!x %in% y]

    pretty_year <- function(x) sub("_", "-", x)

    .year.increment <- function(year, increment) {
         paste(as.numeric(unlist(strsplit(as.character(year), "_")))+increment, collapse="_")
    }

    capwords <- function(x) {
      special.words <- c("ELA", "EMH", "II", "III", "IV")
      if (x %in% special.words) return(x)
      s <- sub("_", " ", x)
      s <- strsplit(s, split=" ")[[1]]
      s <- paste(toupper(substring(s, 1,1)), tolower(substring(s, 2)), sep="", collapse=" ")
      s <- strsplit(s, split="-")[[1]]
      paste(toupper(substring(s, 1,1)), substring(s, 2), sep="", collapse="-")
    }


##############################################################################################################
#### bubblePlot
##############################################################################################################

	if ("bubblePlot" %in% plot.types) {

	started.at <- proc.time()
	message(paste("Started bubblePlot in visualizeSGP", date()))

	bubblePlot_Styles(sgp_object=sgp_object,
		state=state,
		bPlot.years=bPlot.years,
		bPlot.content_areas=bPlot.content_areas,
		bPlot.districts=bPlot.districts,
		bPlot.schools=bPlot.schools,
		bPlot.styles=bPlot.styles,
		bPlot.levels=bPlot.levels, 
		bPlot.full.academic.year=bPlot.full.academic.year,
		bPlot.minimum.n=bPlot.minimum.n,
		bPlot.anonymize=bPlot.anonymize,
		bPlot.prior.achievement=bPlot.prior.achievement, 
		bPlot.draft=bPlot.draft,
		bPlot.format=bPlot.format,
		bPlot.folder=bPlot.folder)

	message(paste("Finished bubblePlot in visualizeSGP", date(), "in", timetaken(started.at), "\n"))
	} ## END bubblePlot %in% plot.types	

####################################################################################################################
#### studentGrowthPlot
####################################################################################################################

    if ("studentGrowthPlot" %in% plot.types) {

	started.at <- proc.time()
	message(paste("Started studentGrowthPlot in visualizeSGP", date()))

       #### Define groups for whom studentGrowthPlots are produced

       # Year stuff
        
       if (is.null(sgPlot.years)) {
         tmp.years <- tail(sort(unique(sgp_object@Data$YEAR)), 5)
         tmp.last.year <- tail(tmp.years, 1)
       } else {
         tmp.all.years <- sort(unique(sgp_object@Data$YEAR))
         tmp.years <- tail(tmp.all.years[1:which(tmp.all.years==tail(sort(sgPlot.years), 1))], 5)
         tmp.last.year <- tail(tmp.years, 1)
       }

        
       # Content area stuff

       tmp.content_areas <- sort(unique(sgp_object@Data[YEAR==tmp.last.year]$CONTENT_AREA)) %w/o% NA

       # Demo report school selector

	if (sgPlot.demo.report) {
		sgPlot.anonymize <- TRUE
		tmp.ids <- list()
		key(sgp_object@Data) <- c("VALID_CASE", "YEAR", "GRADE")
		for (i in stateData[[state]][["Student_Report_Information"]]$Grades_Reported) {
			tmp.ids[[i]] <- as.character(sample(unique(sgp_object@Data[J("VALID_CASE", tmp.last.year, i)]$ID), 10))
		}
		if (is.factor(sgp_object@Data$ID)) {
			sgPlot.students <- as.factor(unlist(tmp.ids)) 
		} else {
			sgPlot.students <- as.integer(unlist(tmp.ids))
		}
	}

       if (is.null(sgPlot.students)) {

	# District stuff

	if (is.null(sgPlot.districts)) {
		tmp.districts <- sort(unique(sgp_object@Data[YEAR==tmp.last.year]$DISTRICT_NUMBER))
	} else {
		tmp.districts <- sgPlot.districts
		if (is.factor(sgp_object@Data$DISTRICT_NUMBER)) tmp.districts <- as.factor(tmp.districts)
	}
        
	# School stuff

	if (is.null(sgPlot.schools)) {
		tmp.schools <- sort(unique(sgp_object@Data[YEAR==tmp.last.year]$SCHOOL_NUMBER))
	} else {
		tmp.schools <- sgPlot.schools
		if (is.factor(sgp_object@Data$SCHOOL_NUMBER)) tmp.schools <- as.factor(tmp.schools)
	}

	# Reconcile choice of District and Schools

	if (is.null(sgPlot.schools) & is.null(sgPlot.districts)) {
		tmp.districts <- sort(unique(sgp_object@Data[YEAR==tmp.last.year]$DISTRICT_NUMBER)) %w/o% NA
		tmp.schools <- sort(unique(sgp_object@Data[YEAR==tmp.last.year]$SCHOOL_NUMBER)) %w/o% NA
	}

	if (is.null(sgPlot.schools) & !is.null(sgPlot.districts)) {
         	tmp.districts <- sgPlot.districts
         	if (is.factor(sgp_object@Data$DISTRICT_NUMBER)) tmp.districts <- as.factor(tmp.districts)
		tmp.schools <- unique(sgp_object@Data$SCHOOL_NUMBER[sgp_object@Data$DISTRICT_NUMBER %in% tmp.districts]) %w/o% NA
	}

	if (!is.null(sgPlot.schools) & is.null(sgPlot.districts)) {
         	tmp.schools <- sgPlot.schools 
		if (is.factor(sgp_object@Data$SCHOOL_NUMBER)) tmp.schools <- as.factor(tmp.schools)
		tmp.districts <- unique(sgp_object@Data$DISTRICT_NUMBER[sgp_object@Data$SCHOOL_NUMBER %in% tmp.schools]) %w/o% NA
	}

	if (!is.null(sgPlot.schools) & !is.null(sgPlot.districts)) {
         	tmp.districts <- sgPlot.districts
         	tmp.schools <- sgPlot.schools 
		tmp.schools <- unique(c(tmp.schools, sgp_object@Data$SCHOOL_NUMBER[sgp_object@Data$DISTRICT_NUMBER %in% tmp.districts])) %w/o% NA
		tmp.districts <- unique(c(tmp.districts, sgp_object@Data$DISTRICT_NUMBER[sgp_object@Data$SCHOOL_NUMBER %in% tmp.schools])) %w/o% NA
         	if (is.factor(sgp_object@Data$DISTRICT_NUMBER)) tmp.districts <- as.factor(tmp.districts)
		if (is.factor(sgp_object@Data$SCHOOL_NUMBER)) tmp.schools <- as.factor(tmp.schools)
	}
     } ## END if(is.null(sgPlot.students | sgPlot.demo.report))
 

      ### Utility functions

      rbind.all <- function(.list, ...) {
        if (length(.list)==1) return (.list[[1]])
        Recall(c(list(rbind(.list[[1]], .list[[2]], ...)), .list[-(1:2)]), ...)
      }

      piecewise.transform <- function(scale_score, state, content_area, year, grade, output.digits=1) {
        if (content_area %in% names(stateData[[state]][["Student_Report_Information"]][["Transformed_Achievement_Level_Cutscores"]]) &
            grade %in% as.numeric(matrix(unlist(strsplit(names(stateData[[state]][["Achievement"]][["Knots_Boundaries"]][[content_area]]), "_")), ncol=2, byrow=TRUE)[,2])) {

           if (is.null(stateData[[state]][["Student_Report_Information"]][["Modulo_Score_Transformation"]])) {
             tmp.loss.hoss <- stateData[[state]][["Achievement"]][["Knots_Boundaries"]][[as.character(content_area)]][[paste("loss.hoss_", grade, sep="")]]
             if (year %in% unlist(strsplit(names(stateData[[state]][["Achievement"]][["Cutscores"]])[grep(content_area, names(stateData[[state]][["Achievement"]][["Cutscores"]]))], "[.]"))) {
                  tmp.old.cuts <- c(tmp.loss.hoss[1], stateData[[state]][["Achievement"]][["Cutscores"]][[paste(content_area, year, sep=".")]][[paste("GRADE_", grade, sep="")]],
                         tmp.loss.hoss[2])
             } else {
                  tmp.old.cuts <- c(tmp.loss.hoss[1], stateData[[state]][["Achievement"]][["Cutscores"]][[as.character(content_area)]][[paste("GRADE_", grade, sep="")]],
                         tmp.loss.hoss[2])
             }
           } else {
             tmp.modulo <- stateData[[state]][["Student_Report_Information"]][["Modulo_Score_Transformation"]]
             tmp.loss.hoss <- stateData[[state]][["Achievement"]][["Knots_Boundaries"]][[as.character(content_area)]][[paste("loss.hoss_", grade, sep="")]]
             scale_score <- scale_score %% tmp.modulo
             if (year %in% unlist(strsplit(names(stateData[[state]][["Achievement"]][["Cutscores"]])[grep(content_area, names(stateData[[state]][["Achievement"]][["Cutscores"]]))], "[.]"))) {
                  tmp.old.cuts <- c(tmp.loss.hoss[1], stateData[[state]][["Achievement"]][["Cutscores"]][[paste(content_area, year, sep=".")]][[paste("GRADE_", grade, sep="")]],
                          tmp.loss.hoss[2]) %% tmp.modulo
             } else {
                  tmp.old.cuts <- c(tmp.loss.hoss[1], stateData[[state]][["Achievement"]][["Cutscores"]][[as.character(content_area)]][[paste("GRADE_", grade, sep="")]],
                          tmp.loss.hoss[2]) %% tmp.modulo
             }
           }
           tmp.new.cuts <- stateData[[state]][["Student_Report_Information"]][["Transformed_Achievement_Level_Cutscores"]][[as.character(content_area)]]
           tmp.index <- findInterval(scale_score, tmp.old.cuts, rightmost.closed=TRUE)
           tmp.diff <- diff(tmp.new.cuts)/diff(tmp.old.cuts)
           round(tmp.new.cuts[tmp.index] + (scale_score - tmp.old.cuts[tmp.index]) * (diff(tmp.new.cuts)/diff(tmp.old.cuts))[tmp.index], digits=output.digits)
        } else {
           as.numeric(scale_score)
        }
      } ## END piecewise.transform

      create.long.cutscores.sgPlot <- function(state, content_area) {
        number.achievement.level.regions <- length(stateData[[state]][["Student_Report_Information"]][["Achievement_Level_Labels"]])
        if (!content_area %in% names(stateData[[state]][["Student_Report_Information"]][["Transformed_Achievement_Level_Cutscores"]])) {
          tmp.list <- list()
          for (i in grep(content_area, names(stateData[[state]][["Achievement"]][["Cutscores"]]))) {
            tmp.grades <- as.numeric(matrix(unlist(strsplit(names(stateData[[state]][["Achievement"]][["Cutscores"]][[i]]), "_")),
                                          ncol=2, byrow=TRUE)[,2])
            tmp.cutscores <- matrix(unlist(stateData[[state]][["Achievement"]][["Cutscores"]][[i]]),
                                  ncol=number.achievement.level.regions-1, byrow=TRUE)
            tmp.year <- unlist(strsplit(names(stateData[[state]][["Achievement"]][["Cutscores"]])[i], "[.]"))[2]
          for (j in seq(number.achievement.level.regions-1)) {
            tmp.list[[paste(i, j, sep="_")]] <- data.frame(GRADE=c(min(tmp.grades,na.rm=TRUE)-1, tmp.grades, max(tmp.grades,na.rm=TRUE)+1),
                                        CUTLEVEL=rep(j, length(tmp.grades)+2),
                                        CUTSCORES=c(extendrange(tmp.cutscores[,j], f=0.15)[1], tmp.cutscores[,j], extendrange(tmp.cutscores[,j], f=0.15)[2]),
                                        YEAR=rep(tmp.year, length(tmp.grades)+2))
          }
          }
          subset(do.call(rbind, tmp.list), CUTLEVEL %in% 1:(number.achievement.level.regions-1))
        } else {
          tmp.grades <- as.numeric(matrix(unlist(strsplit(names(stateData[[state]][["Achievement"]][["Cutscores"]][[content_area]]), "_")),
                                          ncol=2, byrow=TRUE)[,2])
          tmp.list <- list()
          for (i in seq(number.achievement.level.regions-1)) {
            tmp.list[[i]] <- data.frame(GRADE=c(min(tmp.grades,na.rm=TRUE)-1, tmp.grades, max(tmp.grades,na.rm=TRUE)+1),
                                        CUTLEVEL=rep(i, length(tmp.grades)+2),
                                        CUTSCORES=rep(stateData[[state]][["Student_Report_Information"]][["Transformed_Achievement_Level_Cutscores"]][[content_area]][i+1],
                                          length(tmp.grades)+2))
          }
          do.call(rbind, tmp.list)
        }
      } ## END create.long.cutscores.sgPlot


      ### Define quantities/variables related to state

      if (state %in% names(stateData)) {
        tmp.abbreviation <- stateData[[state]][["Assessment_Program_Information"]][["Assessment_Abbreviation"]]
        tmp.state <- paste(state.name[state==state.abb], tmp.abbreviation)
        tmp.organization <- stateData[[state]][["Assessment_Program_Information"]][["Organization"]]
        Cutscores <- list()
        for (i in tmp.content_areas) {
          Cutscores[[i]] <- create.long.cutscores.sgPlot(state, i)
        }
        number.achievement.level.regions <- length(stateData[[state]][["Student_Report_Information"]][["Achievement_Level_Labels"]])

      } else {
        tmp.state <- state
        tmp.organization <- list(name=paste(state, "Department of Education"), URL=paste("http://www", state, "gov", sep="."), Phone_Number="123-456-7890")
      }	


      ### Create transformed scale scores (Necessary before Subset)

        key(sgp_object@Data) <- c("CONTENT_AREA", "YEAR", "GRADE")
        sgp_object@Data$TRANSFORMED_SCALE_SCORE <- sgp_object@Data[,piecewise.transform(SCALE_SCORE, state, CONTENT_AREA[1], YEAR[1], GRADE[1]), by=list(CONTENT_AREA, YEAR, GRADE)]$V1


      ### Subset data

	if (is.null(sgPlot.students)) {
		key(sgp_object@Data) <- c("VALID_CASE", "CONTENT_AREA", "YEAR", "DISTRICT_NUMBER", "SCHOOL_NUMBER")
		report.ids <- unique(sgp_object@Data[CJ("VALID_CASE", tmp.content_areas, tmp.last.year, tmp.districts, tmp.schools), mult="all", nomatch=0]$ID)
	} else {
		report.ids <- sgPlot.students
		if (is.factor(sgp_object@Data$ID)) report.ids <- as.factor(report.ids)
	}
	key(sgp_object@Data) <- c("ID", "CONTENT_AREA", "YEAR")
	tmp.table <- CJ(report.ids, tmp.content_areas, tmp.years) 
	key(tmp.table) <- key(sgp_object@Data) <- names(tmp.table) <- c("ID", "CONTENT_AREA", "YEAR")
	tmp.table <- sgp_object@Data[tmp.table]
	if (sgPlot.demo.report) {
			tmp.table$SCHOOL_NUMBER <- as.integer(tmp.table$SCHOOL_NUMBER) 
			tmp.table$SCHOOL_NUMBER <- tmp.schools <- -99L
			tmp.table$DISTRICT_NUMBER <- as.integer(tmp.table$DISTRICT_NUMBER) 
			tmp.table$DISTRICT_NUMBER <- tmp.districts <- -999L
	}

      ### Anonymize (if requested)
     
      if (sgPlot.anonymize) {
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
	if (sgPlot.demo.report) {
		tmp.table$DISTRICT_NAME <- as.factor("Sample District")
		tmp.table$SCHOOL_NAME <- as.factor("Sample School")
	} else {
		key(tmp.table) <- "DISTRICT_NUMBER"
		tmp.district.number <- J(DISTRICT_NUMBER=unique(tmp.table$DISTRICT_NUMBER) %w/o% NA, seq_along(unique(tmp.table$DISTRICT_NUMBER) %w/o% NA), key="DISTRICT_NUMBER")[tmp.table]$V2
		tmp.table$DISTRICT_NAME <- as.character(tmp.table$DISTRICT_NAME)
		tmp.table$DISTRICT_NAME[!is.na(tmp.table$DISTRICT_NUMBER)] <- paste("Sample District", tmp.district.number[!is.na(tmp.table$DISTRICT_NUMBER)])
		tmp.table$DISTRICT_NAME <- as.factor(tmp.table$DISTRICT_NAME)
	
		key(tmp.table) <- "SCHOOL_NUMBER"
		tmp.school.number <- J(SCHOOL_NUMBER=unique(tmp.table$SCHOOL_NUMBER) %w/o% NA, seq_along(unique(tmp.table$SCHOOL_NUMBER) %w/o% NA), key="SCHOOL_NUMBER")[tmp.table]$V2
		tmp.table$SCHOOL_NAME <- as.character(tmp.table$SCHOOL_NAME)
		tmp.table$SCHOOL_NAME[!is.na(tmp.table$SCHOOL_NUMBER)] <- paste("Sample School", tmp.school.number[!is.na(tmp.table$SCHOOL_NUMBER)])
		tmp.table$SCHOOL_NAME <- as.factor(tmp.table$SCHOOL_NAME)
	}

      } ## END create.random.names


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

      isr_data <<- isr_data[, variables.to.keep, with=FALSE]
      isr_data <- isr_data[, variables.to.keep, with=FALSE]
      
      ### Merge in 1 year projections (if requested & available) and transform using piecewise.tranform (if required)

      tmp.proj.names <- paste(tmp.content_areas, tmp.last.year, sep=".")
      if (sgPlot.fan & all(tmp.proj.names %in% names(sgp_object@SGP[["SGProjections"]]))) {
        key(isr_data) <- c("ID", "CONTENT_AREA")
        tmp.list <- list()
        for (i in tmp.proj.names) {
          tmp.list[[i]] <- data.table(CONTENT_AREA=unlist(strsplit(i, "[.]"))[1],
                                      sgp_object@SGP[["SGProjections"]][[i]][,c(1, grep("PROJ_YEAR_1", names(sgp_object@SGP[["SGProjections"]][[i]])))])
        }
        isr_data <- data.table(rbind.all(tmp.list), key=paste(key(isr_data), collapse=","))[isr_data]
	tmp.grade.name <- paste("GRADE", tmp.last.year, sep=".")
	tmp.year.name <- .year.increment(tmp.last.year, 1)
	key(isr_data) <- c("CONTENT_AREA", tmp.grade.name)
	for (proj.iter in grep("PROJ_YEAR_1", names(isr_data))) {
		tmp.scale_score.name <- names(isr_data)[proj.iter]
		isr_data[[proj.iter]] <- isr_data[,piecewise.transform(get(tmp.scale_score.name), state, CONTENT_AREA[1], tmp.year.name, get(tmp.grade.name)[1]+1), 
			by=list(CONTENT_AREA, isr_data[[tmp.grade.name]])]$V1 
	}
      }


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

          if (sgPlot.demo.report) {
            tmp_district_name <- "Sample District"
            district_folder <- "Sample_District"
          } else {
            if (sgPlot.folder.names=="name") {
              tmp_district_name <- as.character(isr_data[J(i)][[paste("DISTRICT_NAME", tmp.last.year, sep=".")]][1])
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

            if (sgPlot.demo.report) {
              tmp_school_name <- "Sample School"
              school_folder <- "Sample_School"
            } else {
              if (sgPlot.folder.names=="name") {
                tmp_school_name <- as.character(tmp_district_data[J(j)][[paste("SCHOOL_NAME", tmp.last.year, sep=".")]][1])
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
            if (sgPlot.folder.names=="name") {
              grade_folder <- paste("Grade", k, sep="_")
            } else {
              grade_folder <- substr(paste("0", as.character(k), sep=""), nchar(k), nchar(k)+1)
            }
            path.to.pdfs <- file.path(sgPlot.folder, year_folder, district_folder, school_folder, grade_folder)
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
              if (sgPlot.anonymize) {
                student_number <- 1234567890
              } else {
                student_number <- n
              }


              ################################ SCHOOL Report Catalog LaTeX Code ###########################
              if (is.null(sgPlot.front.page)) {
                cat(paste("\\pdfbookmark[2]{", paste(LAST_NAME, ", ", FIRST_NAME, " (", student_number, ")", sep=""), "}{", n , "}
\\includepdf[fitpaper=true]{", path.to.pdfs, "/", FIRST_NAME, "_", LAST_NAME, "_", student_number, "_", year_folder, ".pdf}\n", sep=""), 
                    file=paste("school_catalog_", j, ".tex", sep=""), append=TRUE)
              } else {
                cat(paste("\\include{", sgPlot.front.page, "}\n\\pdfbookmark[2]{", paste(LAST_NAME, ", ", FIRST_NAME, " (", student_number, ")", sep=""), "}{", n , "}
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
              if (!is.null(sgPlot.front.page)) {
                cat("\\includepdf[fitpaper=true]{", sgPlot.front.page, "}\n", file=paste("student_report_", j, ".tex", sep=""), append=TRUE)
              }
              cat(paste("\\includepdf[fitpaper=true]{", path.to.pdfs, "/", FIRST_NAME, "_", LAST_NAME, "_", student_number, "_", year_folder, ".pdf}\n", sep=""), 
                  file=paste("student_report_", j, ".tex", sep=""), append=TRUE)

              cat("\\end{document}", file=paste("student_report_", j, ".tex", sep=""), append=TRUE)
              ########################################################################################

              ## Start pdf device
              
              if (length(tmp.content_areas)==2) {
                report.width=11
                report.height=8.5
              }
              if (length(tmp.content_areas)==3) {
                report.width=8.5
                report.height=11
              }
              if (!length(tmp.content_areas) %in% c(2,3)) {
                stop("Individual Student Report Templates currently only available for situations with 2 or 3 content areas.")
              }

              pdf(paste(path.to.pdfs, "/", FIRST_NAME, "_", LAST_NAME, "_", student_number, "_", year_folder, ".pdf", sep=""), 
                  width=report.width, height=report.height, version="1.4", encoding="ISOLatin2", family="URWHelvetica")


              ########################################################################################################
              ###
              ### Overall Report viewport creation
              ###
              ########################################################################################################

              if (length(tmp.content_areas)==2) {
                report.vp <- viewport(layout = grid.layout(7, 4, widths = unit(c(2.5, 0.1, 8.3, 0.1), rep("inches", 4)), 
                                        heights = unit(c(0.35, 0.2, 3.55, 0.25, 3.55, 0.2, 0.4), rep("inches", 7))))

                content_area_1.vp <- viewport(layout.pos.row=3, layout.pos.col=3)
                content_area_2.vp <- viewport(layout.pos.row=5, layout.pos.col=3)
                top.border.vp <- viewport(layout.pos.row=1, layout.pos.col=1:4)
                bottom.border.vp <- viewport(layout.pos.row=7, layout.pos.col=1:4)
                left.legend.vp <- viewport(layout.pos.row=2:6, layout.pos.col=1)
              }

              if (length(tmp.content_areas)==3) {
                report.vp <- viewport(layout = grid.layout(9, 3, widths = unit(c(0.125, 8.3, 0.075), rep("inches", 3)), 
                                        heights = unit(c(0.35, 0.1, 3.256, 0.14, 3.256, 0.14, 3.256, 0.1, 0.4), rep("inches", 9))))

                content_area_1.vp <- viewport(layout.pos.row=3, layout.pos.col=2)
                content_area_2.vp <- viewport(layout.pos.row=5, layout.pos.col=2)
                content_area_3.vp <- viewport(layout.pos.row=7, layout.pos.col=2)
                top.border.vp <- viewport(layout.pos.row=1, layout.pos.col=1:3)
                bottom.border.vp <- viewport(layout.pos.row=9, layout.pos.col=1:3)
              }

              pushViewport(report.vp)

              for (vp in seq_along(tmp.content_areas)) {

		tmp_student_data <- as.data.frame(tmp_grade_data[ID==n & CONTENT_AREA==tmp.content_areas[vp]])
		pushViewport(get(paste("content_area_", vp, ".vp", sep="")))

                studentGrowthPlot(
                                  Scale_Scores=as.numeric(subset(tmp_student_data, select=paste("SCALE_SCORE", rev(tmp.years), sep="."))),
                                  Plotting_Scale_Scores=as.numeric(subset(tmp_student_data, select=paste("TRANSFORMED_SCALE_SCORE", rev(tmp.years), sep="."))),
                                  Achievement_Levels=as.character(unlist(subset(tmp_student_data, select=paste("ACHIEVEMENT_LEVEL", rev(tmp.years), sep=".")))),
                                  SGP=as.numeric(subset(tmp_student_data, select=paste("SGP", rev(tmp.years), sep="."))),
                                  Grades=as.numeric(subset(tmp_student_data, select=paste("GRADE", rev(tmp.years), sep="."))),
                                  Cuts_NY1=as.numeric(subset(tmp_student_data, select=grep("PROJ", names(tmp_student_data)))),
                                  Cutscores=Cutscores[[tmp.content_areas[vp]]],
                                  Report_Parameters=list(Current_Year=tmp.last.year, Content_Area=as.character(tmp.content_areas[vp]), State=state))

		popViewport()

              } ## END loop over tmp.content_areas


              ## Top Legend

              pushViewport(top.border.vp)
              grid.rect(gp=gpar(fill=sgPlot.header.footer.color, col=sgPlot.header.footer.color))
              grid.text(x=0.025, y=0.5, paste(FIRST_NAME, " ", LAST_NAME, sep="") , 
                        gp=gpar(fontface="bold", col="white", cex=1.5), just="left", default.units="native")
              grid.text(x=0.975, y=0.5, tmp_school_name, gp=gpar(fontface="bold", col="white", cex=1.5), just="right", default.units="native")
              popViewport()


              ## Bottom Legend

              pushViewport(bottom.border.vp)
              grid.rect(gp=gpar(fill=sgPlot.header.footer.color, col=sgPlot.header.footer.color))
              grid.text(x=0.02, y=0.70, paste("For more information please visit", tmp.organization$Name, "at", tmp.organization$URL, "or call", tmp.organization$Phone_Number), 
                        gp=gpar(cex=0.8, col="white"), default.units="native", just="left")
              copyright.text <- paste("Cooperatively developed by the ", tmp.organization$Name, " & the Center for Assessment, Inc.", sep="")
              grid.text(x=0.02, y=0.30, paste(copyright.text, " Distributed by the ", tmp.organization$Name, ".", sep=""), 
                        gp=gpar(cex=0.8, col="white"), default.units="native", just="left")

#              grid.text(x=0.995, y=0.18, copyright.text, gp=gpar(col="white", cex=0.45), default.units="native", just="right")
#              grid.text(x=unit(0.992, "native")-convertWidth(grobWidth(textGrob(copyright.text, gp=gpar(cex=0.45))), "native"), y=0.19, "\\co", 
#                        gp=gpar(col="white", cex=0.55, fontfamily="HersheySymbol"), default.units="native", just="right")
              popViewport()


              ## Left Legend (Only with two content areas depicted)

              if (length(tmp.content_areas)==2) {

		pushViewport(left.legend.vp)

		# Interpretation

                interpretation.y <- 0.93
                achievement.level.region.colors <- paste("grey", round(seq(62, 91, length=number.achievement.level.regions)), sep="")

                grid.roundrect(x=unit(0.5, "native"), y=unit(interpretation.y, "native"), width=unit(0.9, "native"), height=unit(0.06, "native"), 
                               gp=gpar(fill=sgPlot.header.footer.color, col="black"))
                grid.text(x=0.5, y=interpretation.y+0.011, "How to interpret this student", gp=gpar(fontface="bold", cex=0.95, col="white"))
                grid.text(x=0.5, y=interpretation.y-0.011, "growth & achievement report", gp=gpar(fontface="bold", cex=0.95, col="white"))

                grid.roundrect(x=unit(0.2, "native"), y=unit(interpretation.y-0.08, "native"), width=unit(0.1, "native"), height=unit(0.05, "native"), r=unit(0.02, "inches"), 
                               gp=gpar(fill=achievement.level.region.colors[1], lwd=1))
                grid.circle(x=0.2, y=interpretation.y-0.08, r=0.02, default.units="native", gp=gpar(fill="white"))
                grid.text(x=0.325, y=interpretation.y-0.0675, tmp.abbreviation, gp=gpar(cex=0.9), default.units="native", just="left")
                grid.text(x=0.325, y=interpretation.y-0.0925, "Scale Score", gp=gpar(cex=0.9), default.units="native", just="left")

                tmp.rect.height <- 0.125/number.achievement.level.regions
                for (i in seq(number.achievement.level.regions)) {
                    grid.rect(x=unit(0.2, "native"), y=unit(interpretation.y-0.125-(i-1)*tmp.rect.height, "native"), width=unit(0.1, "native"), height=unit(tmp.rect.height, "native"),
                               gp=gpar(fill=rev(achievement.level.region.colors)[i], col="white", lwd=1), just=c("center", "top"))
                } 
                grid.roundrect(x=unit(0.2, "native"), y=interpretation.y-0.125, width=unit(0.1, "native"), height=unit(0.125, "native"), r=unit(0.02, "inches"),
                               gp=gpar(col="black", lwd=1.5), just=c("center", "top"))
                grid.text(x=0.325, y=interpretation.y-0.1625, tmp.abbreviation, default.units="native", just="left")
                grid.text(x=0.325, y=interpretation.y-0.1875, "Achievement", default.units="native", just="left")
                grid.text(x=0.325, y=interpretation.y-0.2125, "Levels", default.units="native", just="left")

                grid.polygon(x=c(0.1875, 0.1875, 0.17, 0.2, 0.23, 0.2125, 0.2125), y=interpretation.y-c(0.35, 0.30, 0.31, 0.27, 0.31, 0.30, 0.35), default.units="native",
                             gp=gpar(fill="grey50"))
                grid.text(x=0.325, y=interpretation.y-0.285, "Student", gp=gpar(cex=0.9), default.units="native", just="left")
                grid.text(x=0.325, y=interpretation.y-0.31, "Growth", gp=gpar(cex=0.9), default.units="native", just="left")
                grid.text(x=0.325, y=interpretation.y-0.335, "Percentile", gp=gpar(cex=0.9), default.units="native", just="left")

		# Suggested uses

                suggested.y <- 0.52

                grid.roundrect(x=unit(0.5, "native"), y=unit(suggested.y, "native"), width=unit(0.9, "native"), height=unit(0.06, "native"), 
                               gp=gpar(fill=sgPlot.header.footer.color, col="black"))
                grid.text(x=0.5, y=suggested.y, "Suggested Uses", gp=gpar(fontface="bold", cex=0.95, col="white"))

                grid.circle(x=0.075, y=suggested.y-0.07, r=0.01, gp=gpar(fill="black"), default.units="native")
                grid.text(x=0.12, y=suggested.y-0.07, "Review past growth to assess", gp=gpar(cex=0.8), default.units="native", just="left")
                grid.text(x=0.12, y=suggested.y-0.09, "student academic progress toward", gp=gpar(cex=0.8), default.units="native", just="left")
                grid.text(x=0.12, y=suggested.y-0.11, paste(tmp.abbreviation, "achievement goals."), gp=gpar(cex=0.8), default.units="native", just="left")

                grid.circle(x=0.075, y=suggested.y-0.14, r=0.01, gp=gpar(fill="black"), default.units="native")
                grid.text(x=0.12, y=suggested.y-0.14, "Develop remediation or enrich-", gp=gpar(cex=0.8), default.units="native", just="left")
                grid.text(x=0.12, y=suggested.y-0.16, "ment plans based on rate of", gp=gpar(cex=0.8), default.units="native", just="left")
                grid.text(x=0.12, y=suggested.y-0.18, "growth needed to reach higher", gp=gpar(cex=0.8), default.units="native", just="left")
                grid.text(x=0.12, y=suggested.y-0.20, paste(tmp.abbreviation, "achievement levels."), gp=gpar(cex=0.8), default.units="native", just="left")

                if (sgPlot.fan) {
                   grid.circle(x=0.075, y=suggested.y-0.23, r=0.01, gp=gpar(fill="black"), default.units="native")
                   grid.text(x=0.12, y=suggested.y-0.23, "Identify the rate of progress", gp=gpar(cex=0.8), default.units="native", just="left")
                   grid.text(x=0.12, y=suggested.y-0.25, "needed in order to reach or", gp=gpar(cex=0.8), default.units="native", just="left")
                   grid.text(x=0.12, y=suggested.y-0.27, "maintain proficient status", gp=gpar(cex=0.8), default.units="native", just="left")
                   grid.text(x=0.12, y=suggested.y-0.29, paste("on the", tmp.abbreviation, "next year."), gp=gpar(cex=0.8), default.units="native", just="left")
                }


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
          file.rename(paste("school_catalog_", j, ".pdf", sep=""), file.path(sgPlot.folder, year_folder, district_folder, 
                                                                             paste(year_folder, "_", district_folder, "_", school_folder, "_Individual_SGP_Report_Catalog.pdf", sep="")))

          if (sgPlot.cleanup) {
            files.to.remove <- list.files(pattern="tex|aux|log|out", all.files=TRUE)
            lapply(files.to.remove, file.remove)
          }

        } ## END for loop for SCHOOLS (j)
        #system(paste("zip -r ", pdf.folder, "/", year_folder, "/", district_folder, "_", tmp.last.year, ".zip ", pdf.folder, year_folder, district_folder, sep=""))
        print(paste("Finished with District", district_folder))
      } ## END for loop for DISTRICTS (i)
      
      if (sgPlot.cleanup) {
        files.to.remove <- list.files(pattern="school_catalog|student_report")
        lapply(files.to.remove, file.remove)
      }

    message(paste("Finished studentGrowthPlot in visualizeSGP", date(), "in", timetaken(started.at), "\n"))
    } ## END "if ("studentGrowthPlot" %in% plot.types)"


####################################################################################################################
#### growthAchievementPlot
####################################################################################################################

    if ("growthAchievementPlot" %in% plot.types) {

	started.at <- proc.time()
	message(paste("Started growthAchievementPlot in visualizeSGP", date()))

       #### Define/Calculate relevant quantities for growthAchievementPlot

       # Year stuff 

       if (is.null(gaPlot.years)) {
         tmp.years <- tail(sort(unique(sgp_object@Data$YEAR)), 1)
       } else {
         tmp.years <- gaPlot.years
       }

       ## Loop over content areas and years

       for (year.iter in tmp.years) {

       if (!is.null(gaPlot.content_areas)) {
          tmp.content_areas <- gaPlot.content_areas
          if (is.factor(sgp_object@Data$CONTENT_AREA)) tmp.content_areas <- as.factor(tmp.content_areas) ## Factor joins to Factor
       } else {
          tmp.content_areas <- sort(unique(sgp_object@Data[YEAR==year.iter]$CONTENT_AREA)) %w/o% NA
       }

       for (content_area.iter in tmp.content_areas) {
      
          if (is.null(gaPlot.students)) {
              tmp.students <- NULL
          } else {
              tmp.students <- gaPlot.students
          }

	key(sgp_object@Data) <- c("VALID_CASE", "CONTENT_AREA")

          growthAchievementPlot(
             gaPlot.sgp_object=sgp_object,
             gaPlot.students=tmp.students,
             state=state,
             content_area=content_area.iter,
             year=year.iter, 
             format=gaPlot.format,
             pdf.folder=file.path(gaPlot.folder, year.iter))

        } ## END for loop content_area.iter
     } ## END for loop year.iter
    message(paste("Finished growthAchievementPlot in visualizeSGP", date(), "in", timetaken(started.at), "\n"))
   } ## END if (growthAchievementPlot %in% plot.types)

   message(paste("Finished visualizeSGP", date(), "in", timetaken(started.at), "\n"))
} ## END visualizeSGP Function
