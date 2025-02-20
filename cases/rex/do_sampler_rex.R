# rex sole call to make the input data
GitDir<-"C:/GitProjects/sampler/R/"
source(file.path(GitDir,"CallQueriesForSamplerData.R"))
info<-call_queries(GitDir = "C:/GitProjects/sampler/R/",outdir="C:/Users/carey.mcgilliard/Work/FlatfishAssessments/2025/rex_cie_review/data/fishery",StrataMap = data.frame(STRATA = rep(1,12),MONTH = seq(from = 1,to = 12, by =1)), FmpArea = "600 and 650",SpeciesCode = "105", CatchSpeciesCode = "'REXS'" , CatchFmpSubArea =  "('CG','WG','SE','WY')", minage = 0, maxage = 20, minlen = 9, maxlen = 65)

