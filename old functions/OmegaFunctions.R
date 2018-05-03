
#' calculateOmega
#'
#' This function displays full and partial omega squared for different types
#' different types of analysis of variance analyses up to two factors.  
#' 
#'
#' @param design type of research design used
#' @param partno participant number associated with responses
#' @param dv dependent variable 
#' @param bw1 name of the first between-subjects factor used
#' @param bw2 name of the second between-subjects factor used
#' @param rm1 name of the first within-subjects factor used
#' @param rm2 name of the second within-subjects factor used
#' @param data name of the dataset
#' @keywords effect size, omega squared
#' @export
#' @examples
#' calculateOmega(design = "BN2", partno = "subject", dv = "Rating", bw1 = "Group", bw2 = "Sex", data = dataset)

calculateOmega = function(design, partno, dv, bw1, bw2, rm1,rm2, data){
  # This function displays full and partial omega squared for different types
  # different types of analysis of variance analyses up to two factors.
  #
  # Args:
  #   design: type of research design used. Options are: "BN1" for a one way 
  #           between-subjects design, "BN2" for a two-way between-subjects
  #           design, "RM1" for a one way repeated measures design, "RM2" for
  #           a two way repeated measures design, and "MIX" for a mixed design.
  #   partno: Name of a participant number column in quotes.
  #   dv    : Name of the dependent variable column in quotes.
  #   bw1   : Name of the first between-subjects factor in quotes. This argument
  #           is not necessary if type of design is "RM1" or "RM2"
  #   bw2   : Name of the second between-subjects factor in quotes. This argument
  #           is not necessary if type of design is "BN1", "RM1", "RM2", or "MIX"
  #   rm1   : Name of the first repeated measures factor in quotes. This argument
  #           is not necessary if type of design is "BN1" or "BN2"
  #   rm2   : Name of the second repeated measures factor in quotes. This argument
  #           is not necessary if type of design is "BN1", "BN2", "RM1", or "MIX"
  #   data  : Name of the dataset
  #
  # Returns:
  #   If using a one factor design, returns Omega squared for that factor. If using
  #   a two factor design, returns Omega squared for both factors, and the interaction
  #   term.
  
  library(ez)
  if(design=="BN1"){ 
    ##BN1
    tempdv = which(colnames(data)==dv)
    colnames(data)[tempdv] = "dvx"
    temppartno = which(colnames(data)==partno)
    colnames(data)[temppartno] = "partnox"
    tempbw = which(colnames(data)==bw1)
    colnames(data)[tempbw] = "bw1x"
    data$partnox = factor(data$partnox)
    output =  ezANOVA(data = data,
                      wid = partnox,
                      between = bw1x,
                      dv = dvx,
                      type = 3,
                      detailed = T)
    
    bw1FOS = (output$ANOVA$DFn[2]*((output$ANOVA$SSn[2]/output$ANOVA$DFn[2])-(output$ANOVA$SSd[2]/output$ANOVA$DFd[2])))/
      ((output$ANOVA$SSn[2] + output$ANOVA$SSd[2])+(output$ANOVA$SSd[2]/output$ANOVA$DFd[2]))
    tempy = setNames(bw1FOS, "bw1 Full Omega Squared")
    return(tempy)
  } else if(design == "RM1"){
    ##RM1
    tempdv = which(colnames(data)==dv)
    colnames(data)[tempdv] = "dvx"
    temppartno = which(colnames(data)==partno)
    colnames(data)[temppartno] = "partnox"
    temprm = which(colnames(data)==rm1)
    colnames(data)[temprm] = "rm1x"
    data$partnox = factor(data$partnox)
    output = ezANOVA(data = data,
                     wid = partnox,
                     within = rm1x,
                     dv = dvx,
                     type = 3,
                     detailed = T)
    rm1FOS = (output$ANOVA$DFn[2]*((output$ANOVA$SSn[2]/output$ANOVA$DFn[2])-(output$ANOVA$SSd[2]/output$ANOVA$DFd[2])))/
      ((output$ANOVA$SSn[2]+output$ANOVA$SSd[2]+output$ANOVA$SSd[1])+(output$ANOVA$SSn[1]/(output$ANOVA$DFd[2]/output$ANOVA$DFn[2])))
    tempy = setNames(rm1FOS, "rm1 Full Omega Squared")
    return(tempy)
  } else if(design == "BN2"){
    ##BN2
    tempdv = which(colnames(data)==dv)
    colnames(data)[tempdv] = "dvx"
    temppartno = which(colnames(data)==partno)
    colnames(data)[temppartno] = "partnox"
    tempbw1 = which(colnames(data)==bw1)
    colnames(data)[tempbw1] = "bw1x"
    tempbw2 = which(colnames(data)==bw2)
    colnames(data)[tempbw2] = "bw2x"
    data$partnox = factor(data$partnox)
    output = ezANOVA(data = data,
                     wid = partnox,
                     between = .(bw1x, bw2x),
                     dv = dvx,
                     type = 3,
                     detailed = T)
    #FOS variable 1
    bw1FullOmega = (output$ANOVA$DFn[2]*((output$ANOVA$SSn[2]/output$ANOVA$DFn[2])-(output$ANOVA$SSd[1]/output$ANOVA$DFd[2])))/
      ((output$ANOVA$SSn[2] + output$ANOVA$SSd[1] + output$ANOVA$SSn[3] + output$ANOVA$SSn[4])+(output$ANOVA$SSd[1]/output$ANOVA$DFd[2]))
    #FOS variable2
    bw2FullOmega = (output$ANOVA$DFn[3]*((output$ANOVA$SSn[3]/output$ANOVA$DFn[3])-(output$ANOVA$SSd[1]/output$ANOVA$DFd[3])))/
      ((output$ANOVA$SSn[3] + output$ANOVA$SSd[1] + output$ANOVA$SSn[2] + output$ANOVA$SSn[4])+(output$ANOVA$SSd[1]/output$ANOVA$DFd[3]))
    #FOS interaction
    interactionFullOmega = (output$ANOVA$DFn[4]*((output$ANOVA$SSn[4]/output$ANOVA$DFn[4])-(output$ANOVA$SSd[1]/output$ANOVA$DFd[4])))/
      ((output$ANOVA$SSn[3] + output$ANOVA$SSd[1] + output$ANOVA$SSn[2] + output$ANOVA$SSn[4])+(output$ANOVA$SSd[1]/output$ANOVA$DFd[3]))
    #POS variable 1
    bw1PartialOmega = (output$ANOVA$DFn[2]*((output$ANOVA$SSn[2]/output$ANOVA$DFn[2])-(output$ANOVA$SSd[1]/output$ANOVA$DFd[2])))/
      (output$ANOVA$SSn[2]+(((max(as.numeric(data$partno)))-output$ANOVA$DFn[2])*(output$ANOVA$SSd[1]/output$ANOVA$DFd[2])))
    #POS variable 2
    bw2PartialOmega = (output$ANOVA$DFn[3]*((output$ANOVA$SSn[3]/output$ANOVA$DFn[3])-(output$ANOVA$SSd[1]/output$ANOVA$DFd[3])))/
      (output$ANOVA$SSn[3]+(((max(as.numeric(data$partno)))-output$ANOVA$DFn[3])*(output$ANOVA$SSd[1]/output$ANOVA$DFd[3])))
    #POS interaction
    interactionPartialOmega = (output$ANOVA$DFn[4]*((output$ANOVA$SSn[4]/output$ANOVA$DFn[4])-(output$ANOVA$SSd[1]/output$ANOVA$DFd[4])))/
      (output$ANOVA$SSn[4]+(((max(as.numeric(data$partno)))-output$ANOVA$DFn[4])*(output$ANOVA$SSd[1]/output$ANOVA$DFd[4])))
    
    statvec = c(bw1FullOmega,bw1PartialOmega,bw2FullOmega,bw2PartialOmega,interactionFullOmega,interactionPartialOmega)
    names = c("bw1 Full Omega Squared", "bw1 Partial Omega Squared", "bw2 Full Omega Squared", "bw2 Partial Omega Squared",
              "Interaction Full Omega Squared", "Interaction Partial Omega Squared")
    tempy = setNames(statvec, names)
    return(tempy)

  } else if(design == "RM2"){
    ##RM2
    tempdv = which(colnames(data)==dv)
    colnames(data)[tempdv] = "dvx"
    temppartno = which(colnames(data)==partno)
    colnames(data)[temppartno] = "partnox"
    temprm1 = which(colnames(data)==rm1)
    colnames(data)[temprm1] = "rm1x"
    temprm2 = which(colnames(data)==rm2)
    colnames(data)[temprm2] = "rm2x"
    data$partnox = factor(data$partnox)
    output = ezANOVA(data = data,
                     wid = partnox,
                     between = .(rm1x, rm2x),
                     dv = dvx,
                     type = 3,
                     detailed = T)
    
    #FOS variable 1
    rm1FullOmega = (output$ANOVA$DFn[2]*((output$ANOVA$SSn[2]/output$ANOVA$DFn[2])-(output$ANOVA$SSd[2]/output$ANOVA$DFd[2])))/
      ((output$ANOVA$SSn[2]+output$ANOVA$SSd[2]+output$ANOVA$SSn[3]+
          output$ANOVA$SSd[3]+output$ANOVA$SSn[4]+output$ANOVA$SSd[4]+output$ANOVA$SSd[1])+
         (output$ANOVA$SSn[1]/(output$ANOVA$DFd[2]/output$ANOVA$DFn[2])))
    #FOS variable 2
    rm2FullOmega = (output$ANOVA$DFn[3]*((output$ANOVA$SSn[3]/output$ANOVA$DFn[3])-(output$ANOVA$SSd[3]/output$ANOVA$DFd[3])))/
      ((output$ANOVA$SSn[2]+output$ANOVA$SSd[2]+output$ANOVA$SSn[3]+
          output$ANOVA$SSd[3]+output$ANOVA$SSn[4]+output$ANOVA$SSd[4]+output$ANOVA$SSd[1])+
         (output$ANOVA$SSn[1]/(output$ANOVA$DFd[3]/output$ANOVA$DFn[3])))
    #FOS interaction
    interactionFullOmega = (output$ANOVA$DFn[4]*((output$ANOVA$SSn[4]/output$ANOVA$DFn[4])-(output$ANOVA$SSd[4]/output$ANOVA$DFd[4])))/
      ((output$ANOVA$SSn[2]+output$ANOVA$SSd[2]+output$ANOVA$SSn[3]+
          output$ANOVA$SSd[3]+output$ANOVA$SSn[4]+output$ANOVA$SSd[4]+output$ANOVA$SSd[1])+
         (output$ANOVA$SSn[1]/(output$ANOVA$DFd[4]/output$ANOVA$DFn[4])))
    #POS variable 1
    rm1PartialOmega = (output$ANOVA$DFn[2]*((output$ANOVA$SSn[2]/output$ANOVA$DFn[2])-(output$ANOVA$SSd[2]/output$ANOVA$DFd[2])))/
      (output$ANOVA$SSn[2]+output$ANOVA$SSd[2]+output$ANOVA$SSd[1]+(output$ANOVA$SSd[1]/(output$ANOVA$DFd[2]/output$ANOVA$DFn[2])))
    #POS variable 2
    rm2PartialOmega = (output$ANOVA$DFn[3]*((output$ANOVA$SSn[3]/output$ANOVA$DFn[3])-(output$ANOVA$SSd[3]/output$ANOVA$DFd[3])))/
      (output$ANOVA$SSn[3]+output$ANOVA$SSd[3]+output$ANOVA$SSd[1]+(output$ANOVA$SSd[1]/(output$ANOVA$DFd[3]/output$ANOVA$DFn[3])))
    #POS interaction
    interactionPartialOmega = (output$ANOVA$DFn[4]*((output$ANOVA$SSn[4]/output$ANOVA$DFn[4])-(output$ANOVA$SSd[4]/output$ANOVA$DFd[4])))/
      (output$ANOVA$SSn[4]+output$ANOVA$SSd[4]+output$ANOVA$SSd[1]+(output$ANOVA$SSd[1]/(output$ANOVA$DFd[4]/output$ANOVA$DFn[4])))
    
    statvec = c(rm1FullOmega,rm1PartialOmega,rm2FullOmega,rm2PartialOmega,interactionFullOmega,interactionPartialOmega)
    names = c("rm1 Full Omega Squared", "rm1 Partial Omega Squared", "rm2 Full Omega Squared", "rm2 Partial Omega Squared",
              "Interaction Full Omega Squared", "Interaction Partial Omega Squared")
    tempy = setNames(statvec, names)
    return(tempy)
    
  } else if(design == "MIX"){
    ##MIX
    tempdv = which(colnames(data)==dv)
    colnames(data)[tempdv] = "dvx"
    temppartno = which(colnames(data)==partno)
    colnames(data)[temppartno] = "partnox"
    temprm = which(colnames(data)==rm1)
    colnames(data)[temprm] = "rm1x"
    tempbw = which(colnames(data)==bw1)
    colnames(data)[tempbw] = "bw1x"
    data$partnox = factor(data$partnox)
    output = ezANOVA(data = data,
                     dv = dvx,
                     wid = partnox,
                     within = rm1x,
                     between = bw1x,
                     detailed = TRUE,
                     type = 3)
    #FOS variable 1 (between)
    bw1FullOmega = (output$ANOVA$DFn[2]*((output$ANOVA$SSn[2]/output$ANOVA$DFn[2])-(output$ANOVA$SSd[1]/output$ANOVA$DFd[2])))/
      ((output$ANOVA$SSn[2] + output$ANOVA$SSd[1] + output$ANOVA$SSn[3] + output$ANOVA$SSn[4])+(output$ANOVA$SSd[1]/output$ANOVA$DFd[2]))
    #FOS variable 2 (within)
    rm1FullOmega = (output$ANOVA$DFn[3]*((output$ANOVA$SSn[3]/output$ANOVA$DFn[3])-(output$ANOVA$SSd[3]/output$ANOVA$DFd[3])))/
      (output$ANOVA$SSn[3]+output$ANOVA$SSd[3]+output$ANOVA$SSd[1]+(output$ANOVA$SSd[3]/(output$ANOVA$DFd[3]/output$ANOVA$DFn[3])))
    #FOS interaction
    interactionFullOmega = (output$ANOVA$DFn[4]*((output$ANOVA$SSn[4]/output$ANOVA$DFn[4])-(output$ANOVA$SSd[4]/output$ANOVA$DFd[4])))/
      (output$ANOVA$SSn[4]+output$ANOVA$SSd[4]+output$ANOVA$SSd[1]+(output$ANOVA$SSd[4]/(output$ANOVA$DFd[4]/output$ANOVA$DFn[4])))
    
    #POS variable 1 (between)
    bw1PartialOmega = (output$ANOVA$DFn[2]*((output$ANOVA$SSn[2]/output$ANOVA$DFn[2])-(output$ANOVA$SSd[2]/output$ANOVA$DFd[2])))/
      (output$ANOVA$SSn[2]+output$ANOVA$SSd[1]+(output$ANOVA$SSd[1]/(output$ANOVA$DFd[2]/output$ANOVA$DFn[2])))
    #POS variable 2 (within)
    rm1PartialOmega = (output$ANOVA$DFn[3]*((output$ANOVA$SSn[3]/output$ANOVA$DFn[3])-(output$ANOVA$SSd[3]/output$ANOVA$DFd[3])))/
      (output$ANOVA$SSn[3]+output$ANOVA$SSd[3]+output$ANOVA$SSd[1]+(output$ANOVA$SSd[1]/(output$ANOVA$DFd[3]/output$ANOVA$DFn[3])))
    #POS interaction
    interactionPartialOmega = (output$ANOVA$DFn[4]*((output$ANOVA$SSn[4]/output$ANOVA$DFn[4])-(output$ANOVA$SSd[4]/output$ANOVA$DFd[4])))/
      (output$ANOVA$SSn[4]+output$ANOVA$SSd[4]+output$ANOVA$SSd[4]+(output$ANOVA$SSd[4]/(output$ANOVA$DFd[4]/output$ANOVA$DFn[4])))
    
    statvec = c(bw1FullOmega, bw1PartialOmega, rm1FullOmega, rm1PartialOmega, interactionFullOmega, interactionPartialOmega)
    names = c("bw1 Full Omega Squared", "bw1 Partial Omega Squared", "rm1 Full Omega Squared", "rm1 Partial Omega Squared",
              "Interaction Full Omega Squared", "Interaction Partial Omega Squared")
    tempy = setNames(statvec, names)
    return(tempy)
  }
}







