################################################################################
# AppStat Exercises from week 14
################################################################################
# Exercises from the script
#*------------------------------------------------------------------------------

rm(list=ls(all=TRUE))

#   load R-function to do residual analysis (written by A. Ruckstuhl)
source(file.path("02_R-Scripts", "RFn_Plot-lmSim.R"))

#   load package
require(daewr)

#*******************************************************************************
#* Problem 14.3.2 (Filling Variability, cf. [23], Problem 14-51)
#* A factorial experiment is used to study the ﬁlling variability of dry soup 
#* mix packages. The factors are
#* • the number of mixing ports through which the vegetable oil was added (A) 
#*    with levels one or two ports,
#* • the temperature surrounding the mixer (B) with levels cooled and ambient,
#* • the mixing time (C) with levels 60 or 80 seconds,
#* • the batch weight (D) with levels 1500 or 2000 lb and
#* • the number of days of delay between mixing and packaging (E) with levels 
#*    one or seven days.
#*    
#*                          *Data Table in Script*
#*      
#*******************************************************************************
#* a. The design generator is E = −ABCD. Describe the design used in the experiment:
#*    • Name of the design?
#*    • Deﬁning relations?
#*    • Alias structure?
#* b. Calculate by hand the estimate for the main eﬀect A as well as for the 
#*    interactions C :D and A:B :E.
#* c. Use a model without interactions. Which eﬀects are statistically signiﬁcant?
#* d. Perform an analysis of residuals. Report your ﬁndings.
#* e. Fit a model with all main eﬀects and all second-order interaction eﬀects. 
#*    Find all signiﬁcant eﬀects and ﬁt a reduced model. Note that for signiﬁcant 
#*    interaction eﬀects, the corresponding main eﬀects are always added in the 
#*    reduced model.
#* f. Report your results of your analysis of the data from the experiment.
#*------------------------------------------------------------------------------
