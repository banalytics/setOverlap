#all functions work on the same basis, they take a data frame with combination frames as a frequency as well as names of sets 
#in a combination (dubbed elementOne to elementFive). The data frame requires the form identical to output of the combination-Calculation
#function. The idea is that the overlaps are denoted by "," characters in the combination column and these functions use stri_count to 
#determine how many sets (or elements) are in a combination

vennTwoElements<-function(combinationFrame=data.frame(),elementOne,elementTwo){
  if(names(dev.cur()) != "null device"){dev.off()}
  draw.pairwise.venn(area1=combinationFrame[combinationFrame[,"combination"]==elementOne,"frequency"],
                     area2=combinationFrame[combinationFrame[, "combination"] == elementTwo, "frequency"],
                     cross.area=combinationFrame[grepl(elementOne,combinationFrame[,"combination"]) & grepl(elementTwo,combinationFrame[,"combination"]) & stringi::stri_count(combinationFrame[,"combination"],regex=",")==1,"frequency"],
                     category=c(elementOne,elementTwo),
                     col=c("darkred","darkblue"),
                     fill=c("red","blue"),
                     sep.dist=0.0125,
                     cat.dist=-0.03
                     )
}

vennThreeElements<-function(combinationFrame=data.frame(),elementOne,elementTwo,elementThree){
  if(names(dev.cur()) != "null device"){dev.off()}
  draw.triple.venn(area1=combinationFrame[combinationFrame[, "combination"]==elementOne, "frequency"],
                     area2=combinationFrame[combinationFrame[,"combination"]==elementTwo,"frequency"],
                     area3=combinationFrame[combinationFrame[,"combination"]==elementThree,"frequency"],
                     n12=combinationFrame[grepl(elementOne,combinationFrame[,"combination"]) 
                                          & grepl(elementTwo,combinationFrame[,"combination"]) 
                                          & stringi::stri_count(combinationFrame[,"combination"],regex=",")==1,"frequency"],
                     n13=combinationFrame[grepl(elementOne,combinationFrame[,"combination"]) 
                                          & grepl(elementThree,combinationFrame[,"combination"]) 
                                          & stringi::stri_count(combinationFrame[,"combination"],regex=",")==1,"frequency"],
                     n23=combinationFrame[grepl(elementTwo,combinationFrame[,"combination"]) 
                                          & grepl(elementThree,combinationFrame[,"combination"]) 
                                          & stringi::stri_count(combinationFrame[,"combination"],regex=",")==1,"frequency"],
                     n123=combinationFrame[grepl(elementOne,combinationFrame[,"combination"]) 
                                          & grepl(elementTwo,combinationFrame[,"combination"]) 
                                          & grepl(elementThree,combinationFrame[,"combination"]) 
                                          & stringi::stri_count(combinationFrame[,"combination"],regex=",")==2],
                     category=c(elementOne,elementTwo,elementThree),
                     col=c("darkred","darkblue","darkgreen"),
                     fill=c("red","blue","green"),
                     sep.dist=0.0125,
                     cat.dist=-0.03
                   )
  
}



vennFourElements<-function(combinationFrame=data.frame(),elementOne,elementTwo,elementThree,elementFour){
  if(names(dev.cur()) != "null device"){dev.off()}
  draw.quad.venn(area1=combinationFrame[combinationFrame[, "combination"]==elementOne, "frequency"],
                   area2=combinationFrame[combinationFrame[,"combination"]==elementTwo,"frequency"],
                   area3=combinationFrame[combinationFrame[,"combination"]==elementThree,"frequency"],
                   area4=combinationFrame[combinationFrame[,"combination"]==elementFour,"frequency"],
                   n12=combinationFrame[grepl(elementOne,combinationFrame[,"combination"]) 
                                        & grepl(elementTwo,combinationFrame[,"combination"]) 
                                        & stringi::stri_count(combinationFrame[,"combination"],regex=",")==1,"frequency"],
                   n13=combinationFrame[grepl(elementOne,combinationFrame[,"combination"]) 
                                        & grepl(elementThree,combinationFrame[,"combination"]) 
                                        & stringi::stri_count(combinationFrame[,"combination"],regex=",")==1,"frequency"],
                   n23=combinationFrame[grepl(elementTwo,combinationFrame[,"combination"]) 
                                        & grepl(elementThree,combinationFrame[,"combination"]) 
                                        & stringi::stri_count(combinationFrame[,"combination"],regex=",")==1,"frequency"],
                   n14=combinationFrame[grepl(elementOne,combinationFrame[,"combination"]) 
                                        & grepl(elementFour,combinationFrame[,"combination"]) 
                                        & stringi::stri_count(combinationFrame[,"combination"],regex=",")==1,"frequency"],
                   n24=combinationFrame[grepl(elementTwo,combinationFrame[,"combination"]) 
                                        & grepl(elementFour,combinationFrame[,"combination"]) 
                                        & stringi::stri_count(combinationFrame[,"combination"],regex=",")==1,"frequency"],
                   n34=combinationFrame[grepl(elementThree,combinationFrame[,"combination"]) 
                                        & grepl(elementFour,combinationFrame[,"combination"]) 
                                        & stringi::stri_count(combinationFrame[,"combination"],regex=",")==1,"frequency"],
                   n123=combinationFrame[grepl(elementOne,combinationFrame[,"combination"]) 
                                        & grepl(elementTwo,combinationFrame[,"combination"]) 
                                        & grepl(elementThree,combinationFrame[,"combination"]) 
                                        & stringi::stri_count(combinationFrame[,"combination"],regex=",")==2,"frequency"],
                   n124=combinationFrame[grepl(elementOne,combinationFrame[,"combination"]) 
                                        & grepl(elementTwo,combinationFrame[,"combination"]) 
                                        & grepl(elementFour,combinationFrame[,"combination"]) 
                                        & stringi::stri_count(combinationFrame[,"combination"],regex=",")==2,"frequency"],
                   n234=combinationFrame[grepl(elementThree,combinationFrame[,"combination"]) 
                                        & grepl(elementTwo,combinationFrame[,"combination"]) 
                                        & grepl(elementFour,combinationFrame[,"combination"]) 
                                        & stringi::stri_count(combinationFrame[,"combination"],regex=",")==2,"frequency"],
                   n134=combinationFrame[grepl(elementThree,combinationFrame[,"combination"]) 
                                        & grepl(elementOne,combinationFrame[,"combination"]) 
                                        & grepl(elementFour,combinationFrame[,"combination"]) 
                                        & stringi::stri_count(combinationFrame[,"combination"],regex=",")==2,"frequency"],
                   n1234=combinationFrame[grepl(elementThree,combinationFrame[,"combination"]) 
                                        & grepl(elementTwo,combinationFrame[,"combination"]) 
                                        & grepl(elementFour,combinationFrame[,"combination"]) 
                                        & grepl(elementOne,combinationFrame[,"combination"]) 
                                        & stringi::stri_count(combinationFrame[,"combination"],regex=",")==3,"frequency"],
                   category=c(elementOne,elementTwo,elementThree,elementFour),
                   col=c("darkred","darkblue","darkgreen","black"),
                   fill=c("red","blue","green","grey"),
                   sep.dist=0.0125,
                   cat.dist=-0.03
  )
  
}



vennFiveElements<-function(combinationFrame=data.frame(),elementOne,elementTwo,elementThree,elementFour,elementFive){
  if(names(dev.cur()) != "null device"){dev.off()}
  draw.quintuple.venn(
                 area1=combinationFrame[combinationFrame[, "combination"]==elementOne, "frequency"],
                 area2=combinationFrame[combinationFrame[,"combination"]==elementTwo,"frequency"],
                 area3=combinationFrame[combinationFrame[,"combination"]==elementThree,"frequency"],
                 area4=combinationFrame[combinationFrame[,"combination"]==elementFour,"frequency"],
                 area5=combinationFrame[combinationFrame[,"combination"]==elementFive,"frequency"],
                 n12=combinationFrame[grepl(elementOne,combinationFrame[,"combination"]) 
                                                & grepl(elementTwo,combinationFrame[,"combination"]) 
                                                & stringi::stri_count(combinationFrame[,"combination"],regex=",")==1,"frequency"],
                 n13=combinationFrame[grepl(elementOne,combinationFrame[,"combination"]) 
                                                & grepl(elementThree,combinationFrame[,"combination"]) 
                                                & stringi::stri_count(combinationFrame[,"combination"],regex=",")==1,"frequency"],
                 n23=combinationFrame[grepl(elementTwo,combinationFrame[,"combination"]) 
                                                & grepl(elementThree,combinationFrame[,"combination"]) 
                                                & stringi::stri_count(combinationFrame[,"combination"],regex=",")==1,"frequency"],
                 n14=combinationFrame[grepl(elementOne,combinationFrame[,"combination"]) 
                                                & grepl(elementFour,combinationFrame[,"combination"]) 
                                                & stringi::stri_count(combinationFrame[,"combination"],regex=",")==1,"frequency"],
                 n24=combinationFrame[grepl(elementTwo,combinationFrame[,"combination"]) 
                                                & grepl(elementFour,combinationFrame[,"combination"]) 
                                                & stringi::stri_count(combinationFrame[,"combination"],regex=",")==1,"frequency"],
                 n34=combinationFrame[grepl(elementThree,combinationFrame[,"combination"]) 
                                                & grepl(elementFour,combinationFrame[,"combination"]) 
                                                & stringi::stri_count(combinationFrame[,"combination"],regex=",")==1,"frequency"],
                 n15=combinationFrame[grepl(elementOne,combinationFrame[,"combination"]) 
                                                & grepl(elementFive,combinationFrame[,"combination"]) 
                                                & stringi::stri_count(combinationFrame[,"combination"],regex=",")==1,"frequency"],
                 n25=combinationFrame[grepl(elementTwo,combinationFrame[,"combination"]) 
                                                & grepl(elementFive,combinationFrame[,"combination"]) 
                                                & stringi::stri_count(combinationFrame[,"combination"],regex=",")==1,"frequency"],
                 n35=combinationFrame[grepl(elementThree,combinationFrame[,"combination"]) 
                                                & grepl(elementFive,combinationFrame[,"combination"]) 
                                                & stringi::stri_count(combinationFrame[,"combination"],regex=",")==1,"frequency"],
                 n45=combinationFrame[grepl(elementFive,combinationFrame[,"combination"]) 
                                                & grepl(elementFour,combinationFrame[,"combination"]) 
                                                & stringi::stri_count(combinationFrame[,"combination"],regex=",")==1,"frequency"],
                 n123=combinationFrame[grepl(elementOne,combinationFrame[,"combination"]) 
                                                & grepl(elementTwo,combinationFrame[,"combination"]) 
                                                & grepl(elementThree,combinationFrame[,"combination"]) 
                                                & stringi::stri_count(combinationFrame[,"combination"],regex=",")==2,"frequency"],
                 n124=combinationFrame[grepl(elementOne,combinationFrame[,"combination"]) 
                                                & grepl(elementTwo,combinationFrame[,"combination"]) 
                                                & grepl(elementFour,combinationFrame[,"combination"]) 
                                                & stringi::stri_count(combinationFrame[,"combination"],regex=",")==2,"frequency"],
                 n234=combinationFrame[grepl(elementThree,combinationFrame[,"combination"]) 
                                                & grepl(elementTwo,combinationFrame[,"combination"]) 
                                                & grepl(elementFour,combinationFrame[,"combination"]) 
                                                & stringi::stri_count(combinationFrame[,"combination"],regex=",")==2,"frequency"],
                 n134=combinationFrame[grepl(elementThree,combinationFrame[,"combination"]) 
                                                & grepl(elementOne,combinationFrame[,"combination"]) 
                                                & grepl(elementFour,combinationFrame[,"combination"]) 
                                                & stringi::stri_count(combinationFrame[,"combination"],regex=",")==2,"frequency"],
                 n125=combinationFrame[grepl(elementOne,combinationFrame[,"combination"]) 
                                                & grepl(elementTwo,combinationFrame[,"combination"]) 
                                                & grepl(elementFive,combinationFrame[,"combination"]) 
                                                & stringi::stri_count(combinationFrame[,"combination"],regex=",")==2,"frequency"],
                 n135=combinationFrame[grepl(elementOne,combinationFrame[,"combination"]) 
                                                & grepl(elementFive,combinationFrame[,"combination"]) 
                                                & grepl(elementThree,combinationFrame[,"combination"]) 
                                                & stringi::stri_count(combinationFrame[,"combination"],regex=",")==2,"frequency"],
                 n145=combinationFrame[grepl(elementOne,combinationFrame[,"combination"]) 
                                                & grepl(elementFive,combinationFrame[,"combination"]) 
                                                & grepl(elementFour,combinationFrame[,"combination"]) 
                                                & stringi::stri_count(combinationFrame[,"combination"],regex=",")==2,"frequency"],
                 n235=combinationFrame[grepl(elementThree,combinationFrame[,"combination"]) 
                                                & grepl(elementTwo,combinationFrame[,"combination"]) 
                                                & grepl(elementFive,combinationFrame[,"combination"]) 
                                                & stringi::stri_count(combinationFrame[,"combination"],regex=",")==2,"frequency"],
                 n245=combinationFrame[grepl(elementFour,combinationFrame[,"combination"]) 
                                                & grepl(elementTwo,combinationFrame[,"combination"]) 
                                                & grepl(elementFive,combinationFrame[,"combination"]) 
                                                & stringi::stri_count(combinationFrame[,"combination"],regex=",")==2,"frequency"],
                 n345=combinationFrame[grepl(elementFour,combinationFrame[,"combination"]) 
                                                & grepl(elementThree,combinationFrame[,"combination"]) 
                                                & grepl(elementFive,combinationFrame[,"combination"]) 
                                                & stringi::stri_count(combinationFrame[,"combination"],regex=",")==2,"frequency"],
                 n1234=combinationFrame[grepl(elementThree,combinationFrame[,"combination"]) 
                                                & grepl(elementTwo,combinationFrame[,"combination"]) 
                                                & grepl(elementFour,combinationFrame[,"combination"]) 
                                                & grepl(elementOne,combinationFrame[,"combination"]) 
                                                & stringi::stri_count(combinationFrame[,"combination"],regex=",")==3,"frequency"],
                 n1235=combinationFrame[grepl(elementThree,combinationFrame[,"combination"]) 
                                                & grepl(elementTwo,combinationFrame[,"combination"]) 
                                                & grepl(elementFive,combinationFrame[,"combination"]) 
                                                & grepl(elementOne,combinationFrame[,"combination"]) 
                                                & stringi::stri_count(combinationFrame[,"combination"],regex=",")==3,"frequency"],
                 n1245=combinationFrame[grepl(elementFour,combinationFrame[,"combination"]) 
                                                & grepl(elementTwo,combinationFrame[,"combination"]) 
                                                & grepl(elementFive,combinationFrame[,"combination"]) 
                                                & grepl(elementOne,combinationFrame[,"combination"]) 
                                                & stringi::stri_count(combinationFrame[,"combination"],regex=",")==3,"frequency"],
                 n1345=combinationFrame[grepl(elementFour,combinationFrame[,"combination"]) 
                                                & grepl(elementThree,combinationFrame[,"combination"]) 
                                                & grepl(elementFive,combinationFrame[,"combination"]) 
                                                & grepl(elementOne,combinationFrame[,"combination"]) 
                                                & stringi::stri_count(combinationFrame[,"combination"],regex=",")==3,"frequency"],
                 n2345=combinationFrame[grepl(elementFour,combinationFrame[,"combination"]) 
                                                & grepl(elementThree,combinationFrame[,"combination"]) 
                                                & grepl(elementFive,combinationFrame[,"combination"]) 
                                                & grepl(elementTwo,combinationFrame[,"combination"]) 
                                                & stringi::stri_count(combinationFrame[,"combination"],regex=",")==3,"frequency"],
                 n12345=combinationFrame[grepl(elementFour,combinationFrame[,"combination"]) 
                                                  & grepl(elementTwo,combinationFrame[,"combination"]) 
                                                  & grepl(elementOne,combinationFrame[,"combination"]) 
                                                  & grepl(elementThree,combinationFrame[,"combination"]) 
                                                  & grepl(elementFive,combinationFrame[,"combination"]) 
                                                  & stringi::stri_count(combinationFrame[,"combination"],regex=",")==4,"frequency"],
                 category=c(elementOne,elementTwo,elementThree,elementFour,elementFive),
                 col=c("dodgerblue", "goldenrod1", "darkorange1", "seagreen3", "orchid3"),
                 fill=c("dodgerblue", "goldenrod1", "darkorange1", "seagreen3", "orchid3"),
                 margin = 0.05,
                 ind = TRUE,
                 cat.dist=-0.03,
                 cat.pos=150,
                 cat.cex=c(rep(0.9,5))
  )
  
}