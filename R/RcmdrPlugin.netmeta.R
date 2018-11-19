# R MTC Jags
# Copyright (C) 2017. Marcelo Goulart Correia

# This program is free software; you can redistribute it and/or
# modify it under the terms of the GNU General Public License
# as published by the Free Software Foundation; either version 2
# of the License, or (at your option) any later version.

# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.

# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software
# Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.

# Marcelo Goulart Correia
# Rua das Laranjeiras, 374 - 5o. Andar
# Laranjeiras - Rio de Janeiro - RJ
# Zip code: 22240-005
# marcelo.goulart@inc.saude.gov.br
# mgoulart.inc@gmail.com

# last modified: 2018-11-01 by M.G. Correia

.onAttach <- function(libname, pkgname){
    if (!interactive()) return()
    putRcmdr("slider.env", new.env())    
    Rcmdr <- options()$Rcmdr
    plugins <- Rcmdr$plugins
	require('netmeta')
	require('hasseDiagram')
	source("https://bioconductor.org/biocLite.R")
	if (is.installed('Rgraphviz') == "FALSE") {biocLite("Rgraphviz")}
    if (!pkgname %in% plugins) {
        Rcmdr$plugins <- c(plugins, pkgname)
        options(Rcmdr=Rcmdr)
        if("package:Rcmdr" %in% search()) {
            if(!getRcmdr("autoRestart")) {
                closeCommander(ask=FALSE, ask.save=TRUE)
                Commander()
            }
        }
        else {
            Commander()
        }
    }
}

#------------//------------//------------//------------//------------//------------//------------//------------//------------//------------//------------//------------//------------

#---- Data conversion routines ----

ConvertDataA <- function(){
    Library('netmeta')
    defaults <- list(treatlab=NULL, event=NULL, n=NULL, Studlab=NULL, SM=NULL)
    dialog.values <- getDialog("ConvertDataA", defaults)
    initializeDialog(title=gettextRcmdr("Data conversion - Binary outcome"))
	variablesFrame <- tkframe(top)
	studBox <- variableListBox(top, Factors(), 
                          title = gettextRcmdr("Study label (pick one)"),
                          initialSelection = varPosn (dialog.values$treatlab, "factor"))
    treatBox <- variableListBox(top, Factors(), selectmode = "multiple", 
                          title = gettextRcmdr("Treatments columns (pick two or more)"),
                          initialSelection = varPosn (dialog.values$Studlab, "factor"))
	eventBox <- variableListBox(top, Numeric(), selectmode="multiple", title=gettextRcmdr("Cases columns (pick two or more)"),
                            initialSelection=varPosn(dialog.values$event, "numeric"))
	nBox <- variableListBox(top, Numeric(), selectmode="multiple", title=gettextRcmdr("n columns (pick two or more)"),
                            initialSelection=varPosn(dialog.values$n, "numeric"))
	newbaseName <- tclVar(paste("NewBase", sep = ""))
    newbaseFrame <- tkframe(top)
	newbase <- ttkentry(newbaseFrame, width = "20", textvariable = newbaseName)
	radioButtons(name="sm", buttons=c("OR", "RR", "RD", "ASD"), labels=gettextRcmdr(c("Odds Ratio", "Risk Ratio", "Risk difference", "Arcsine difference")), title=gettextRcmdr("SM"))
    
    onOK <- function(){       
		newbaseValue <- trim.blanks(tclvalue(newbaseName))
		sm <- trim.blanks(tclvalue(smVariable))
		event <- getSelection(eventBox)
		n <- getSelection(nBox)
		treat <- getSelection(treatBox)
		Studlab <- getSelection(studBox)
        closeDialog()      
		if (!is.valid.name(newbaseValue)){
      	    errorCondition(recall=ConvertDataA, message=sprintf(gettextRcmdr('"%s" is not a valid name.'), newbaseValue), model=TRUE)
      	    return()} 
		if (length(event) < 2){
            errorCondition(recall=ConvertDataA, message=gettextRcmdr("You must select at least two variables (event)"))
            return()}
		if (length(n) < 2){
            errorCondition(recall=ConvertDataA, message=gettextRcmdr("You must select at least two variables (n)"))
            return()}			
		if (length(treat) < 2){
            errorCondition(recall=ConvertDataA, message=gettextRcmdr("You must select at least two variables (treatment label)"))
            return()}
			
		varsE <- if (length(event) == 1) paste(event, sep="") else paste(event, collapse=", ", sep="")
		varsN <- if (length(n) == 1) paste(n, sep="") else paste(n, collapse=", ", sep="")
		varsT <- if (length(treat) == 1) paste(treat, sep="") else paste(treat, collapse=", ", sep="")		         
		varsS <- if (length(Studlab) == 1) {paste(Studlab, sep="")} else if (length(Studlab) == 0) {paste("NULL", sep="")} else {paste(Studlab, collapse=", ", sep="")}			
			
		#if (length(Studlab) !=1){
        #    errorCondition(recall=ConvertDataA, message=gettextRcmdr("You must select only one variable (study label)"))
        #    return()}		
		
		doItAndPrint(paste(newbaseValue," <- pairwise(treat = list(",varsT,"), n=list(",varsN,"), event=list(",varsE,"), data=",ActiveDataSet(),", studlab=",varsS,", sm=c('",sm,"'))", sep =""))
		activeDataSet(newbaseValue)
}
	OKCancelHelp(helpSubject="pairwise", reset="ConvertDataA")
    tkgrid(labelRcmdr(newbaseFrame, text = gettextRcmdr("Enter name for the new base:")), newbase, sticky = "w")
    tkgrid(newbaseFrame, sticky = "nw")
	tkgrid(getFrame(studBox), sticky="nw") 
	tkgrid(getFrame(treatBox), sticky="nw") 
	tkgrid(getFrame(eventBox), sticky="nw") 
	tkgrid(getFrame(nBox), sticky="nw")
	tkgrid(smFrame, sticky="w")
	tkgrid(buttonsFrame, sticky="w") 
    dialogSuffix()
}

ConvertDataB <- function(){
    Library('netmeta')
    defaults <- list(treat=NULL, Mean=NULL, SD=NULL, n=NULL, Studlab=NULL, SM=NULL)
    dialog.values <- getDialog("ConvertDataB", defaults)
    initializeDialog(top,title=gettextRcmdr("Data conversion - Continuous outcome"))
	variablesFrame <- tkframe(top)
	studBox <- variableListBox(top, Factors(), 
                          title = gettextRcmdr("Study label (pick one)"),
                          initialSelection = varPosn (dialog.values$Studlab, "factor"))
    treatBox <- variableListBox(top, Factors(), selectmode = "multiple", 
                          title = gettextRcmdr("Treatments columns (pick two or more)"),
                          initialSelection = varPosn (dialog.values$treat, "factor"))
	MeanBox <- variableListBox(top, Numeric(), selectmode="multiple", title=gettextRcmdr("Mean columns (pick two or more)"),
                            initialSelection=varPosn(dialog.values$Mean, "numeric"))
	SDBox <- variableListBox(top, Numeric(), selectmode="multiple", title=gettextRcmdr("SD columns (pick two or more)"),
                            initialSelection=varPosn(dialog.values$SD, "numeric"))
	nBox <- variableListBox(top, Numeric(), selectmode="multiple", title=gettextRcmdr("n columns (pick two or more)"),
                            initialSelection=varPosn(dialog.values$n, "numeric"))
	newbaseName <- tclVar(paste("NewBase", sep = ""))
    newbaseFrame <- tkframe(top)
	newbase <- ttkentry(newbaseFrame, width = "20", textvariable = newbaseName) 
    radioButtons(name="sm", buttons=c("MD", "SMD", "ROM"), labels=gettextRcmdr(c("Mean Difference", "Standardised Mean Difference", "Ratio of Means")), title=gettextRcmdr("SM"))
	
    onOK <- function(){       
		newbaseValue <- trim.blanks(tclvalue(newbaseName))        
		sm <- trim.blanks(tclvalue(smVariable))
		Mean <- getSelection(MeanBox)
		SD <- getSelection(SDBox)
		n <- getSelection(nBox)
		treat <- getSelection(treatBox)
		Studlab <- getSelection(studBox)
		closeDialog()
		if (!is.valid.name(newbaseValue)){
      	    errorCondition(recall=ConvertDataB, message=sprintf(gettextRcmdr('"%s" is not a valid name.'), newbaseValue), model=TRUE)
      	    return()}  		
		if (length(Mean) < 2){
            errorCondition(recall=ConvertDataB, message=gettextRcmdr("You must select at least two variables (mean)"))
            return()}
		if (length(SD) < 2){
            errorCondition(recall=ConvertDataB, message=gettextRcmdr("You must select at least two variables (SD)"))
            return()}
		if (length(n) < 2){
            errorCondition(recall=ConvertDataB, message=gettextRcmdr("You must select at least two variables (n)"))
            return()}			
		if (length(treat) < 2){
            errorCondition(recall=ConvertDataB, message=gettextRcmdr("You must select at least two variables (treatment label)"))
            return()}
			
		
		varsM <- if (length(Mean) == 1) paste(Mean, sep="") else paste(Mean, collapse=", ", sep="")
		varsN <- if (length(n) == 1) paste(n, sep="") else paste(n, collapse=", ", sep="")
	   varsSD <- if (length(SD) == 1) paste(SD, sep="") else paste(SD, collapse=", ", sep="")
		varsT <- if (length(treat) == 1) paste(treat, sep="") else paste(treat, collapse=", ", sep="")	
		varsS <- if (length(Studlab) == 1) {paste(Studlab, sep="")} else if (length(Studlab) == 0) {paste("NULL", sep="")} else {paste(Studlab, collapse=", ", sep="")}		
		
		doItAndPrint(paste(newbaseValue," <- pairwise(treat = list(",varsT,"), n=list(",varsN,"), mean=list(",varsM,"), sd=list(",varsSD,"), data=",ActiveDataSet(),", studlab=",varsS,", sm=c('",sm,"'))", sep =""))
		activeDataSet(newbaseValue)
}
	OKCancelHelp(helpSubject="pairwise", reset="ConvertDataB")
    tkgrid(labelRcmdr(newbaseFrame, text = gettextRcmdr("Enter name for the new base:")), newbase, sticky = "w")
    tkgrid(newbaseFrame, sticky = "nw")
	tkgrid(getFrame(studBox), sticky="nw") 
	tkgrid(getFrame(treatBox), sticky="nw") 
	tkgrid(getFrame(MeanBox), sticky="nw") 
	tkgrid(getFrame(SDBox), sticky="nw") 
	tkgrid(getFrame(nBox), sticky="nw")
	tkgrid(smFrame, sticky="w")	
	tkgrid(buttonsFrame, sticky="w") 
    dialogSuffix()
}

ConvertDataC <- function(){
    Library('netmeta')
    defaults <- list(treat=NULL, event=NULL, Time=NULL, Studlab=NULL, SM=NULL)
    dialog.values <- getDialog("ConvertDataC", defaults)
    initializeDialog(top,title=gettextRcmdr("Data conversion - Incidence rate"))
	variablesFrame <- tkframe(top)
	studBox <- variableListBox(top, Factors(), 
                          title = gettextRcmdr("Study label (pick one)"),
                          initialSelection = varPosn (dialog.values$Studlab, "factor"))
    treatBox <- variableListBox(top, Factors(), selectmode = "multiple", 
                          title = gettextRcmdr("Treatments columns (pick two or more)"),
                          initialSelection = varPosn (dialog.values$treat, "factor"))
	eventBox <- variableListBox(top, Numeric(), selectmode="multiple", title=gettextRcmdr("Cases columns (pick two or more)"),
                            initialSelection=varPosn(dialog.values$event, "numeric"))
	TimeBox <- variableListBox(top, Numeric(), selectmode="multiple", title=gettextRcmdr("Time columns (pick two or more)"),
                            initialSelection=varPosn(dialog.values$Time, "numeric"))
	newbaseName <- tclVar(paste("NewBase", sep = ""))
    newbaseFrame <- tkframe(top)
	newbase <- ttkentry(newbaseFrame, width = "20", textvariable = newbaseName)
	radioButtons(name="sm", buttons=c("IRR", "IRD"), labels=gettextRcmdr(c("Incidence Rate Ratio", "Incidence Rate Difference")), title=gettextRcmdr("SM"))
    
    onOK <- function(){       
		newbaseValue <- trim.blanks(tclvalue(newbaseName))
		sm <- trim.blanks(tclvalue(smVariable))
		event <- getSelection(eventBox)
		Time <- getSelection(TimeBox)
		treat <- getSelection(treatBox)
		Studlab <- getSelection(studBox)
        closeDialog()      
		if (!is.valid.name(newbaseValue)){
      	    errorCondition(recall=ConvertDataC, message=sprintf(gettextRcmdr('"%s" is not a valid name.'), newbaseValue), model=TRUE)
      	    return()}  		
		if (length(event) < 2){
            errorCondition(recall=ConvertDataC, message=gettextRcmdr("You must select at least two variables (event)"))
            return()}
		if (length(Time) < 2){
            errorCondition(recall=ConvertDataC, message=gettextRcmdr("You must select at least two variables (time)"))
            return()}
		if (length(treat) < 2){
            errorCondition(recall=ConvertDataC, message=gettextRcmdr("You must select at least two variables (treatment label)"))
            return()}
			
		varsE <- if (length(event) == 1) paste(event, sep="") else paste(event, collapse=", ", sep="")
	   varsTi <- if (length(Time) == 1) paste(Time, sep="") else paste(Time, collapse=", ", sep="")
		varsT <- if (length(treat) == 1) paste(treat, sep="") else paste(treat, collapse=", ", sep="")		         
		varsS <- if (length(Studlab) == 1) {paste(Studlab, sep="")} else if (length(Studlab) == 0) {paste("NULL", sep="")} else {paste(Studlab, collapse=", ", sep="")}			
		
		doItAndPrint(paste(newbaseValue," <- pairwise(treat = list(",varsT,"), time=list(",varsTi,"), event=list(",varsE,"), data=",ActiveDataSet(),", studlab=",varsS,", sm=c('",sm,"'))", sep =""))
		activeDataSet(newbaseValue)
}
	OKCancelHelp(helpSubject="pairwise", reset="ConvertDataC")
    tkgrid(labelRcmdr(newbaseFrame, text = gettextRcmdr("Enter name for the new base:")), newbase, sticky = "w")
    tkgrid(newbaseFrame, sticky = "nw")
	tkgrid(getFrame(studBox), sticky="nw") 
	tkgrid(getFrame(treatBox), sticky="nw") 
	tkgrid(getFrame(eventBox), sticky="nw") 
	tkgrid(getFrame(TimeBox), sticky="nw")
	tkgrid(smFrame, sticky="w")
	tkgrid(buttonsFrame, sticky="w") 
    dialogSuffix()
}

ConvertDataD <- function(){
    Library('netmeta')
    defaults <- list(treat=NULL, te=NULL, SETE=NULL, Studlab=NULL, SM=NULL)
    dialog.values <- getDialog("ConvertDataD", defaults)
    initializeDialog(top,title=gettextRcmdr("Data conversion - Generic outcome"))
	variablesFrame <- tkframe(top)
	studBox <- variableListBox(top, Factors(), 
                          title = gettextRcmdr("Study label (pick one)"),
                          initialSelection = varPosn (dialog.values$Studlab, "factor"))
    treatBox <- variableListBox(top, Factors(), selectmode = "multiple", 
                          title = gettextRcmdr("Treatments columns (pick two or more)"),
                          initialSelection = varPosn (dialog.values$treat, "factor"))
	teBox <- variableListBox(top, Numeric(), selectmode="multiple", title=gettextRcmdr("Treatment effects columns (pick two or more)"),
                            initialSelection=varPosn(dialog.values$te, "numeric"))
	SETEBox <- variableListBox(top, Numeric(), selectmode="multiple", title=gettextRcmdr("Standard errors columns (pick two or more)"),
                            initialSelection=varPosn(dialog.values$SETE, "numeric"))
	newbaseName <- tclVar(paste("NewBase", sep = ""))
    newbaseFrame <- tkframe(top)
	newbase <- ttkentry(newbaseFrame, width = "20", textvariable = newbaseName) 
    radioButtons(name="sm", buttons=c("RD", "RR", "OR", "ASD", "HR", "MD", "SMD", "ROM"), labels=gettextRcmdr(c("Risk Difference", "Risk Ratio", "Odds Ratio", "Arcsine Difference", "Hazard Ratio", "Mean Difference", "Standardised Mean Difference", "Ratio of Means")), title=gettextRcmdr("SM"))
	
    onOK <- function(){       
		newbaseValue <- trim.blanks(tclvalue(newbaseName))
		sm <- trim.blanks(tclvalue(smVariable))
		te <- getSelection(teBox)
		SETE <- getSelection(SETEBox)
		treat <- getSelection(treatBox)
		Studlab <- getSelection(studBox)
        closeDialog()      
		if (!is.valid.name(newbaseValue)){
      	    errorCondition(recall=ConvertDataD, message=sprintf(gettextRcmdr('"%s" is not a valid name.'), newbaseValue), model=TRUE)
      	    return()}  		
		if (length(te) < 2){
            errorCondition(recall=ConvertDataD, message=gettextRcmdr("You must select at least two variables (TE)"))
            return()}
		if (length(SETE) < 2){
            errorCondition(recall=ConvertDataD, message=gettextRcmdr("You must select at least two variables (SE)"))
            return()}			
		if (length(treat) < 2){
            errorCondition(recall=ConvertDataD, message=gettextRcmdr("You must select at least two variables (treatment label)"))
            return()}
			
	   varsTE <- if (length(te) == 1) paste(te, sep="") else paste(te, collapse=", ", sep="")
	   varsSE <- if (length(SETE) == 1) paste(SETE, sep="") else paste(SETE, collapse=", ", sep="")
		varsT <- if (length(treat) == 1) paste(treat, sep="") else paste(treat, collapse=", ", sep="")		         
		varsS <- if (length(Studlab) == 1) {paste(Studlab, sep="")} else if (length(Studlab) == 0) {paste("NULL", sep="")} else {paste(Studlab, collapse=", ", sep="")}			
		
		doItAndPrint(paste(newbaseValue," <- pairwise(treat = list(",varsT,"), seTE=list(",varsSE,"), TE=list(",varsTE,"), data=",ActiveDataSet(),", studlab=",varsS,", sm=c('",sm,"'))", sep =""))
		activeDataSet(newbaseValue)
}
	OKCancelHelp(helpSubject="pairwise", reset="ConvertDataD")
    tkgrid(labelRcmdr(newbaseFrame, text = gettextRcmdr("Enter name for the new base:")), newbase, sticky = "w")
    tkgrid(newbaseFrame, sticky = "nw")
	tkgrid(getFrame(studBox), sticky="nw") 
	tkgrid(getFrame(treatBox), sticky="nw") 
	tkgrid(getFrame(teBox), sticky="nw") 
	tkgrid(getFrame(SETEBox), sticky="nw")
	tkgrid(smFrame, sticky="w")
	tkgrid(buttonsFrame, sticky="w") 	
    dialogSuffix()
}

#------------//------------//------------//------------//------------//------------//------------//------------//------------//------------//------------//------------//------------

#---- Model creation routines ----

netmetaModel <- function(){
	Library('netmeta')
	defaults <- list(model = "", Level="", LevelComb="")
	dialog.values <- getDialog("netmetaModel", defaults)
	initializeDialog(title=gettextRcmdr("Run Network Meta-Analysis"))
	modelName <- tclVar(paste("Model", sep = ""))
    modelFrame <- tkframe(top)
    model <- tkentry(modelFrame, width = "20", textvariable = modelName)
	LevelName <- tclVar(paste("0.95", sep = ""))
    LevelFrame <- tkframe(top)
    Level <- tkentry(LevelFrame, width = "20", textvariable = LevelName)
	LevelCombName <- tclVar(paste("0.95", sep = ""))
    LevelCombFrame <- tkframe(top)
    LevelComb <- tkentry(LevelCombFrame, width = "20", textvariable = LevelCombName)	
	#LevelVar <- tclVar(dialog.values$LevelVar)
    #LevelEntry <- tkentry(top, width="6", textvariable=LevelVar)
    #LevelCombVar <- tclVar(dialog.values$LevelCombVar)
    #LevelCombEntry <- tkentry(top, width="6", textvariable=LevelCombVar)
	radioButtons(name="outcome", buttons=c("RD", "RR", "OR", "ASD", "HR", "MD", "SMD", "ROM", "IRD", "IRR"), 
	labels=gettextRcmdr(c("Risk Difference", "Risk Ratio", "Odds Ratio", "Arcsine Difference", "Hazard Ratio", "Mean Difference", "Standardised Mean Difference", "Ratio of Means", "Incidence Rate Differences
", "Incidence Rate Ratios")), 
	title=gettextRcmdr("Outcome"))
	
	onOK <- function(){       
		modelValue <- trim.blanks(tclvalue(modelName))
	    LevelVal <- as.numeric(tclvalue(LevelName))
		LevelCombVal <- as.numeric(tclvalue(LevelCombName))		
		outcome <- tclvalue(outcomeVariable)
        closeDialog()      
		if (!is.valid.name(modelValue)){
      	    errorCondition(recall=netmetaModel, message=sprintf(gettextRcmdr('"%s" is not a valid name.'), modelValue), model=TRUE)
      	    return()}  		
		if (LevelVal > 1 | LevelVal < 0){
            errorCondition(recall=netmetaModel, message="Value must be between 0 and 1")
            return()}			
		if (LevelCombVal < 0 | LevelCombVal > 1){
            errorCondition(recall=netmetaModel, message="Value must be between 0 and 1")
            return()}
			
		doItAndPrint(paste(modelValue,"<- netmeta(TE, seTE, treat1, treat2, studlab, data=",ActiveDataSet(),", subset=NULL, sm=c('",outcome,"'), level=",LevelVal,", level.comb=",LevelCombVal,")", sep =""))
		tkfocus(CommanderWindow())
			}

OKCancelHelp(helpSubject="netmeta", reset="netmetaModel")
tkgrid(labelRcmdr(modelFrame, text = gettextRcmdr("Enter name for model:")), model, sticky = "w")
tkgrid(modelFrame, sticky = "e")
tkgrid(labelRcmdr(LevelFrame, text = gettextRcmdr("Confidence intervel:")), Level, sticky = "w")
tkgrid(LevelFrame, sticky = "e")
tkgrid(labelRcmdr(LevelCombFrame, text = gettextRcmdr("Combined condifence intervel:")), LevelComb, sticky = "w")
tkgrid(LevelCombFrame, sticky = "e")
#tkgrid(tklabel(top, text="Confidence Interval"), LevelEntry, sticky="w")
#tkgrid(tklabel(top, text="Combined Confidence Interval"), LevelCombEntry, sticky="w")
#tkgrid.configure(LevelEntry, sticky="w")
#tkgrid.configure(LevelCombEntry, sticky="w")
tkgrid(outcomeFrame, sticky="w")
tkgrid(buttonsFrame, sticky="w")
dialogSuffix()
}



# netmetaModel <- function(){
#    Library('netmeta')
#    netmeta(TE, seTE, treat1, treat2, studlab, data=NULL, subset=NULL, sm, level=0.95, level.comb=0.95)	
#    defaults <- list(model = NULL, Level=NULL, LevelComb=NULL, Type=NULL)
#    dialog.values <- getDialog("netmetaModel", defaults)
#    initializeDialog(title=gettextRcmdr("Run Network Meta-Analysis"))
#    modelName <- tclVar(paste("", sep = ""))
#    modelFrame <- tkframe(top)
#	model <- ttkentry(modelFrame, width = "20", textvariable = modelName)
	
#	LevelVar <- tclVar(dialog.values$LevelVar)
#	LevelEntry <- ttkentry(top, width="6", textvariable=LevelEntry)
#	LevelCombVar <- tclVar(dialog.values$LevelCombVar)
#	LevelCombEntry <- ttkentry(top, width="6", textvariable=LevelCombEntry)

 # radioButtons(name="SM", buttons=c("RD", "RR", "OR", "ASD", "HR", "MD", "SMD", "ROM"), labels=gettextRcmdr(c("Risk Difference", "Risk Ratio", "Odds Ratio", "Arcsine Difference", "Hazard Ratio", "Mean Difference", "Standardised Mean Difference", "Ratio of Means")), title=gettextRcmdr("SM"))  

	#LevelName <- tclVar(paste("", sep = ""))
    #LevelFrame <- tkframe(top)
	#level <- ttkentry(LevelFrame, width = "20", textvariable = LevelName)
	#LevelCombName <- tclVar(paste("", sep = ""))
    #LevelCombFrame <- tkframe(top)
	#levelcomb <- ttkentry(LevelCombFrame, width = "20", textvariable = LevelCombName)	
		
#    onOK <- function(){       
#		modelValue <- trim.blanks(tclvalue(modelName))
		#subsetValue <- trim.blanks(tclvalue(subsetName))
		#LevelValue <- trim.blanks(tclvalue(LevelName))
		#LevelCombValue <- trim.blanks(tclvalue(LevelCombName))
#		SM <- tclvalue(smVariable)
#       closeDialog()      
#		if (!is.valid.name(modelValue)){
#      	    errorCondition(recall=netmetaModel, message=sprintf(gettextRcmdr('"%s" is not a valid name.'), newbaseValue), model=TRUE)
#      	    return()}  		
#		if (!is.valid.name(subsetValue)){
#            errorCondition(recall=netmetaModel, message=gettextRcmdr("Subset function incorrect"))
#            return()}
#		if (LevelValue > 1 | LevelValue < 0){
#            errorCondition(recall=netmetaModel, message=gettextRcmdr("Value must be between 0 and 1"))
#            return()}			
#		if (LevelCombValue < 0 | LevelCombValue > 1){
#            errorCondition(recall=netmetaModel, message=gettextRcmdr("Value must be between 0 and 1"))
#            return()}		
				
		#netmeta(TE, seTE, treat1, treat2, studlab, data=NULL, subset=NULL, sm, level=0.95, level.comb=0.95)
#		doItAndPrint(paste(modelValue,"<- netmeta(TE, seTE, treat1, treat2, studlab, data=",ActiveDataSet(),", subset=NULL, sm=",SM," level=",Level,", level.comb=",LevelComb,")", sep =""))
#}
#	OKCancelHelp(helpSubject="netmeta", reset="netmetaModel")
#    tkgrid(labelRcmdr(modelFrame, text = gettextRcmdr("Enter name for the model:")), model, sticky = "w")	
#    tkgrid(modelFrame, sticky = "w")
#	tkgrid(tklabel(top, text="Confidence Intervel"), LevelEntry, sticky="e")
#	tkgrid(tklabel(top, text="Combined Confidence Interval"), LevelCombEntry, sticky="e")
#	tkgrid.configure(LevelEntry, sticky="w") 
#	tkgrid.configure(LevelCombEntry, sticky="w")
#	tkgrid(SMFrame, sticky="w")	
#    dialogSuffix()
#}

netcombModel <- function(){
    
    initializeDialog(title=gettextRcmdr("Run Additive Network Meta-Analysis"))
    modelName <- tclVar(paste("Model", sep = ""))
    modelFrame <- tkframe(top)    
    model <- ttkentry(modelFrame, width = "20", textvariable = modelName)
	modelAddName <- tclVar(paste("ModelAdd", sep = ""))
    modelAddFrame <- tkframe(top)    
    modelAdd <- ttkentry(modelAddFrame, width = "20", textvariable = modelAddName)

    onOK <- function(){
    modelValue <- trim.blanks(tclvalue(modelName))
	modelAddValue <- trim.blanks(tclvalue(modelAddName))
    closeDialog()      
	if (!is.valid.name(modelValue)) {
      	    errorCondition(recall=netcombModel, message=sprintf(gettextRcmdr('"%s" is not a valid name.'), modelValue), model=TRUE)
      	    return()
									} 
	if (!is.valid.name(modelAddValue)) {
      	    errorCondition(recall=netcombModel, message=sprintf(gettextRcmdr('"%s" is not a valid name.'), modelAddValue), model=TRUE)
      	    return()
									} 
	command <- paste(modelAddValue,' <- netcomb(',modelValue,')', sep="")
        doItAndPrint(command)
	tkfocus(CommanderWindow())
	               }
    OKCancelHelp(helpSubject="netcomb", reset="netcombModel")
    tkgrid(labelRcmdr(modelFrame, text = gettextRcmdr("Enter a name for existing model:")), model, sticky = "w")
    tkgrid(modelFrame, sticky = "w")    
	tkgrid(labelRcmdr(modelAddFrame, text = gettextRcmdr("Enter a name for new addtive model:")), modelAdd, sticky = "w")
    tkgrid(modelAddFrame, sticky = "w")    
    tkgrid(buttonsFrame, sticky="w")
    dialogSuffix()
	                   }

#------------//------------//------------//------------//------------//------------//------------//------------//------------//------------//------------//------------//------------

#---- Results routines ----

Connection <- function(){
    
    initializeDialog(title=gettextRcmdr("Verify network connection"))
    modelName <- tclVar(paste("Database", sep = ""))
    modelFrame <- tkframe(top)    
    model <- ttkentry(modelFrame, width = "20", textvariable = modelName) 
    UpdateModelNumber()

    onOK <- function(){
        modelValue <- trim.blanks(tclvalue(modelName))
        closeDialog()      
	if (!is.valid.name(modelValue)){
      	    errorCondition(recall=Connection, message=sprintf(gettextRcmdr('"%s" is not a valid name.'), modelValue), model=TRUE)
      	    return()
    	    } 
			
	#netconnection(treat1, treat2, studlab, data = modelValue)
	command <- paste('netconnection(treat1, treat2, studlab, data = ',modelValue,')', sep="")
    doItAndPrint(command)
	tkfocus(CommanderWindow())
	               }
    OKCancelHelp(helpSubject="netconnection", reset="Connection")
    tkgrid(labelRcmdr(modelFrame, text = gettextRcmdr("Enter a name for existing database:")), model, sticky = "w")
    tkgrid(modelFrame, sticky = "w")    
    tkgrid(buttonsFrame, sticky="w")
    dialogSuffix()
	                   }
					   
Results <- function(){

    
    initializeDialog(title=gettextRcmdr("Network results"))
    modelName <- tclVar(paste("Model", sep = ""))
    modelFrame <- tkframe(top)    
    model <- ttkentry(modelFrame, width = "20", textvariable = modelName) 
    UpdateModelNumber()

    onOK <- function(){
        modelValue <- trim.blanks(tclvalue(modelName))
        closeDialog()      
	if (!is.valid.name(modelValue)){
      	    errorCondition(recall=Results, message=sprintf(gettextRcmdr('"%s" is not a valid name.'), modelValue), model=TRUE)
      	    return()
    	    } 
			
	command <- paste('print(summary(',modelValue,'))', sep="")
    doItAndPrint(command)
	tkfocus(CommanderWindow())
	               }
    OKCancelHelp(helpSubject="netmeta", reset="Results")
    tkgrid(labelRcmdr(modelFrame, text = gettextRcmdr("Enter a name for existing model:")), model, sticky = "w")
    tkgrid(modelFrame, sticky = "w")    
    tkgrid(buttonsFrame, sticky="w")
    dialogSuffix()
	               }

SplitEvid <- function(){

    
    initializeDialog(title=gettextRcmdr("Pairwise comparison"))
    modelName <- tclVar(paste("Model", sep = ""))
    modelFrame <- tkframe(top)    
    model <- ttkentry(modelFrame, width = "20", textvariable = modelName) 
    UpdateModelNumber()

    onOK <- function(){
        modelValue <- trim.blanks(tclvalue(modelName))
        closeDialog()      
	if (!is.valid.name(modelValue)){
      	    errorCondition(recall=SplitEvid, message=sprintf(gettextRcmdr('"%s" is not a valid name.'), modelValue), model=TRUE)
      	    return()
    	    } 
	
	command <- paste('print(netsplit(',modelValue,'))', sep="")
    doItAndPrint(command)
	tkfocus(CommanderWindow())
	               }
    OKCancelHelp(helpSubject="netsplit", reset="SplitEvid")
    tkgrid(labelRcmdr(modelFrame, text = gettextRcmdr("Enter a name for existing model:")), model, sticky = "w")
    tkgrid(modelFrame, sticky = "w")    
    tkgrid(buttonsFrame, sticky="w")
    dialogSuffix()
	               }

Ranking <- function(){
    
    initializeDialog(title=gettextRcmdr("Ranking treatments")) 
    modelName <- tclVar(paste("Model", sep = ""))
    modelFrame <- tkframe(top)    
    model <- ttkentry(modelFrame, width = "20", textvariable = modelName)
    radioButtons(name="outcome", buttons=c("good", "bad"), values=c("good", "bad"),
	labels=gettext(domain="R-RcmdrPlugin.EZR",c("Good", "Bad")), title=gettext(domain="R-RcmdrPlugin.netmeta","Outcome:"))

    onOK <- function(){
        modelValue <- trim.blanks(tclvalue(modelName))
		outcome <- tclvalue(outcomeVariable)
        closeDialog()      
	if (!is.valid.name(modelValue)){
      	    errorCondition(recall=Ranking, message=sprintf(gettextRcmdr('"%s" is not a valid name.'), modelValue), model=TRUE)
      	    return()
    	    }
	command <- paste('print(netrank(',modelValue,',small.values=c("',outcome,'")))', sep="")
        doItAndPrint(command)
	tkfocus(CommanderWindow())
	               }
    OKCancelHelp(helpSubject="netrank", reset="Ranking")
    tkgrid(labelRcmdr(modelFrame, text = gettextRcmdr("Enter a name for existing model:")), model, sticky = "w")
    tkgrid(modelFrame, sticky = "w")
	tkgrid(outcomeFrame, sticky="w")
    tkgrid(buttonsFrame, sticky="w")
    dialogSuffix()
			}

PairwiseComp <- function(){

    initializeDialog(title=gettextRcmdr("Pairwise comparison"))
    modelName <- tclVar(paste("Model", sep = ""))
    modelFrame <- tkframe(top)    
    model <- ttkentry(modelFrame, width = "20", textvariable = modelName) 	
    UpdateModelNumber()

    onOK <- function(){
        modelValue <- trim.blanks(tclvalue(modelName))
        closeDialog()      
	if (!is.valid.name(modelValue)){
      	    errorCondition(recall=PairwiseComp, message=sprintf(gettextRcmdr('"%s" is not a valid name.'), modelValue), model=TRUE)
      	    return()
    	    }
			
	command <- paste('netleague(',modelValue,')', sep="")
    doItAndPrint(command)
	tkfocus(CommanderWindow())
	               }
    OKCancelHelp(helpSubject="netleague", reset="PairwiseComp")
    tkgrid(labelRcmdr(modelFrame, text = gettextRcmdr("Enter a name for existing model:")), model, sticky = "w")	
    tkgrid(modelFrame, sticky = "w")   	
    tkgrid(buttonsFrame, sticky="w")
    dialogSuffix()
	               }

#------------//------------//------------//------------//------------//------------//------------//------------//------------//------------//------------//------------//------------

#---- Graphics routines ----

NetworkGraph <- function(){

    
    initializeDialog(title=gettextRcmdr("Network graph"))
    modelName <- tclVar(paste("Model", sep = ""))
    modelFrame <- tkframe(top)    
    model <- ttkentry(modelFrame, width = "20", textvariable = modelName) 
    UpdateModelNumber()

    onOK <- function(){
        modelValue <- trim.blanks(tclvalue(modelName))
        closeDialog()      
	if (!is.valid.name(modelValue)){
      	    errorCondition(recall=NetworkGraph, message=sprintf(gettextRcmdr('"%s" is not a valid name.'), modelValue), model=TRUE)
      	    return()
    	    } 
			
	command <- paste('netgraph(',modelValue,')', sep="")
    doItAndPrint(command)
	tkfocus(CommanderWindow())
	               }
    OKCancelHelp(helpSubject="netgraph", reset="NetworkGraph")
    tkgrid(labelRcmdr(modelFrame, text = gettextRcmdr("Enter a name for existing model:")), model, sticky = "w")
    tkgrid(modelFrame, sticky = "w")    
    tkgrid(buttonsFrame, sticky="w")
    dialogSuffix()
	               }

ForestPlot <- function(){

    initializeDialog(title=gettextRcmdr("Forest Plot"))
    modelName <- tclVar(paste("Model", sep = ""))
    modelFrame <- tkframe(top)    
    model <- ttkentry(modelFrame, width = "20", textvariable = modelName) 
	referName <- tclVar(paste("", sep = ""))
    referFrame <- tkframe(top)    
    refer <- ttkentry(referFrame, width = "20", textvariable = referName) 
    UpdateModelNumber()

    onOK <- function(){
        modelValue <- trim.blanks(tclvalue(modelName))
		referValue <- trim.blanks(tclvalue(referName))
        closeDialog()      
	if (!is.valid.name(modelValue)){
      	    errorCondition(recall=ForestPlot, message=sprintf(gettextRcmdr('"%s" is not a valid name.'), modelValue), model=TRUE)
      	    return()
    	    }
#	if (!is.valid.name(referValue)){
#      	    errorCondition(recall=ForestPlot, message=sprintf(gettextRcmdr('"%s" is not a valid name.'), referValue), model=TRUE)
#      	    return()
#    	    } 
			
	command <- paste('forest(',modelValue,', ref=c("',referValue,'"))', sep="")
    doItAndPrint(command)
	tkfocus(CommanderWindow())
	               }
    OKCancelHelp(helpSubject="netmeta", reset="ForestPlot")
    tkgrid(labelRcmdr(modelFrame, text = gettextRcmdr("Enter a name for existing model:")), model, sticky = "w")
	tkgrid(modelFrame, sticky = "w")   
	tkgrid(labelRcmdr(referFrame, text = gettextRcmdr("Enter the name of baseline treatment:")), refer, sticky = "w")    
	tkgrid(referFrame, sticky = "w")  	
    tkgrid(buttonsFrame, sticky="w")
    dialogSuffix()
	               }

SplitForestPlot <- function(){

    initializeDialog(title=gettextRcmdr("Forest Plot"))
    modelName <- tclVar(paste("Model", sep = ""))
    modelFrame <- tkframe(top)    
    model <- ttkentry(modelFrame, width = "20", textvariable = modelName) 
    UpdateModelNumber()

    onOK <- function(){
        modelValue <- trim.blanks(tclvalue(modelName))
        closeDialog()      
	if (!is.valid.name(modelValue)){
      	    errorCondition(recall=SplitForestPlot, message=sprintf(gettextRcmdr('"%s" is not a valid name.'), modelValue), model=TRUE)
      	    return()
    	    }
			
	command <- paste('forest(netsplit(',modelValue,', baseline.reference=F), overall = F, indirect = F)', sep="")
    doItAndPrint(command)
	tkfocus(CommanderWindow())
	               }
    OKCancelHelp(helpSubject="netsplit", reset="SplitForestPlot")
    tkgrid(labelRcmdr(modelFrame, text = gettextRcmdr("Enter a name for existing model:")), model, sticky = "w")
	tkgrid(modelFrame, sticky = "w")   
	tkgrid(buttonsFrame, sticky="w")
    dialogSuffix()
	               }

HasseDiagram <- function(){

    initializeDialog(title=gettextRcmdr("Hasse diagram"))
    
	modelName <- tclVar(paste("Model", sep = ""))
    modelFrame <- tkframe(top)    
    model <- ttkentry(modelFrame, width = "20", textvariable = modelName) 
	
	m1Name <- tclVar(paste("Comp1", sep = ""))
    m1Frame <- tkframe(top)    
    m1 <- ttkentry(m1Frame, width = "20", textvariable = m1Name)
	
	o1Name <- tclVar(paste("Out1", sep = ""))
    o1Frame <- tkframe(top)    
    o1 <- ttkentry(o1Frame, width = "20", textvariable = o1Name)
	
	m2Name <- tclVar(paste("Comp2", sep = ""))
    m2Frame <- tkframe(top)    
    m2 <- ttkentry(m2Frame, width = "20", textvariable = m2Name)
	
	o2Name <- tclVar(paste("Out2", sep = ""))
    o2Frame <- tkframe(top)    
    o2 <- ttkentry(o2Frame, width = "20", textvariable = o2Name)
	
	Resp1Name <- tclVar(paste("Resp1", sep = ""))
    Resp1Frame <- tkframe(top)    
    Resp1 <- ttkentry(Resp1Frame, width = "20", textvariable = Resp1Name)
	
	Resp2Name <- tclVar(paste("Resp2", sep = ""))
    Resp2Frame <- tkframe(top)    
    Resp2 <- ttkentry(Resp2Frame, width = "20", textvariable = Resp2Name)
	
    UpdateModelNumber()

    onOK <- function(){
        modelValue <- trim.blanks(tclvalue(modelName))
		m1Value <- trim.blanks(tclvalue(m1Name))
		m2Value <- trim.blanks(tclvalue(m2Name))
		o1Value <- trim.blanks(tclvalue(o1Name))
		o2Value <- trim.blanks(tclvalue(o2Name))
		Resp1Value <- trim.blanks(tclvalue(Resp1Name))
		Resp2Value <- trim.blanks(tclvalue(Resp2Name))		
        closeDialog()      
	if (!is.valid.name(modelValue)){
      	    errorCondition(recall=HasseDiagram, message=sprintf(gettextRcmdr('"%s" is not a valid name.'), modelValue), model=TRUE)
      	    return()
    	    }
	if (!is.valid.name(m1Value)){
      	    errorCondition(recall=HasseDiagram, message=sprintf(gettextRcmdr('"%s" is not a valid name.'), m1Value), model=TRUE)
      	    return()
    	    }
	if (!is.valid.name(m2Value)){
      	    errorCondition(recall=HasseDiagram, message=sprintf(gettextRcmdr('"%s" is not a valid name.'), m2Value), model=TRUE)
      	    return()
    	    }		
    if (Resp1Value == ""){
      	    errorCondition(recall=HasseDiagram, message=sprintf(gettextRcmdr('"%s" is not a valid name.'), Resp1Value), model=TRUE)
      	    return()
    	    }
	if (Resp2Value == ""){
      	    errorCondition(recall=HasseDiagram, message=sprintf(gettextRcmdr('"%s" is not a valid name.'), Resp2Value), model=TRUE)
      	    return()
    	    }
	#po1 <- netposet(nr4.1, nr4.2, outcomes=c("Resp1","Resp2"))
	#plot(po1)
	#hasse(po1)
	#po <- netposet(netrank(Modelo1, small.values = "bad"), netrank(Modelo2, small.values = "bad"), outcomes = outcomes)
	
	command <- paste(modelValue,'<- netposet(netrank(',m1Value,', small.values = "',o1Value,'"), netrank(',m2Value,', small.values = "',o2Value,'"), outcomes=c("',Resp1Value,'","',Resp2Value,'"))', sep="")
    doItAndPrint(command)
	command2 <- paste("plot(",modelValue,")", sep="")
	doItAndPrint(command2)
	command3 <- paste("hasse(",modelValue,")", sep="")
	doItAndPrint(command3)
	tkfocus(CommanderWindow())
	               }
    OKCancelHelp(helpSubject="netposet", reset="HasseDiagram")
    tkgrid(labelRcmdr(modelFrame, text = gettextRcmdr("Enter a name for the graph:")), model, sticky = "w")
	tkgrid(modelFrame, sticky = "w")
	tkgrid(labelRcmdr(m1Frame, text = gettextRcmdr("Enter a name for 1st model:")), m1, sticky = "w")
	tkgrid(m1Frame, sticky = "w") 
	tkgrid(labelRcmdr(m2Frame, text = gettextRcmdr("Enter a name for 2nd model:")), m2, sticky = "w")
	tkgrid(m2Frame, sticky = "w") 
	tkgrid(labelRcmdr(o1Frame, text = gettextRcmdr("Outcome on 1st model (good/bad):")), o1, sticky = "w")
	tkgrid(o1Frame, sticky = "w") 
	tkgrid(labelRcmdr(o2Frame, text = gettextRcmdr("Outcome on 2nd model (good/bad):")), o2, sticky = "w")
	tkgrid(o2Frame, sticky = "w") 
	tkgrid(labelRcmdr(Resp1Frame, text = gettextRcmdr("Enter the outcome name on 1st model:")), Resp1, sticky = "w")
	tkgrid(Resp1Frame, sticky = "w") 
	tkgrid(labelRcmdr(Resp2Frame, text = gettextRcmdr("Enter the outcome name on 2nd model:")), Resp2, sticky = "w")
	tkgrid(Resp2Frame, sticky = "w") 
	tkgrid(buttonsFrame, sticky="w")
    dialogSuffix()
	               }
		
HeatPlot <- function(){

    
    initializeDialog(title=gettextRcmdr("Heat Plot"))
    modelName <- tclVar(paste("Model", sep = ""))
    modelFrame <- tkframe(top)    
    model <- ttkentry(modelFrame, width = "20", textvariable = modelName) 
    UpdateModelNumber()

    onOK <- function(){
        modelValue <- trim.blanks(tclvalue(modelName))
        closeDialog()      
	if (!is.valid.name(modelValue)){
      	    errorCondition(recall=HeatPlot, message=sprintf(gettextRcmdr('"%s" is not a valid name.'), modelValue), model=TRUE)
      	    return()
    	    } 
			
	command <- paste('print(netheat(',modelValue,'))', sep="")
    doItAndPrint(command)
	tkfocus(CommanderWindow())
	               }
    OKCancelHelp(helpSubject="netmeta", reset="HeatPlot")
    tkgrid(labelRcmdr(modelFrame, text = gettextRcmdr("Enter a name for existing model:")), model, sticky = "w")
    tkgrid(modelFrame, sticky = "w")    
    tkgrid(buttonsFrame, sticky="w")
    dialogSuffix()
	               }
		
#------------//------------//------------//------------//------------//------------//------------//------------//------------//------------//------------//------------//------------
