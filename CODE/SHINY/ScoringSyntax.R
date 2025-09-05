## Scoring function to be called per record
lg_scoring<-function(dat) {
  # Create auxiliary variables 
  if(is.na(dat$css_signs___2)) {
    css_signs___2_lg<-0
    css_signs___2_lg_m<-1
  }
  else {
    css_signs___2_lg<-dat$css_signs___2
    css_signs___2_lg_m<-0
  }
  if(is.na(dat$orebro_score)) {
    orebro_score_lg<-0
    orebro_score_lg_m<-1
  }
  else {
    orebro_score_lg<-dat$orebro_score
    orebro_score_lg_m<-0
  }
  if(is.na(dat$rbcrpsbpds_score)) {
    rbcrpsbpds_score_lg<-0
    rbcrpsbpds_score_lg_m<-1
  }
  else {
    rbcrpsbpds_score_lg<-dat$rbcrpsbpds_score
    rbcrpsbpds_score_lg_m<-0
  }
  if(is.na(dat$triggering_event_no0_no1)) {
    triggering_event_no0_no1_lg<-0
    triggering_event_no0_no1_lg_m<-1
  }
  else {
    triggering_event_no0_no1_lg<-dat$triggering_event_no0_no1
    triggering_event_no0_no1_lg_m<-0
  }
  if(is.na(dat$ppt_z_norm)) {
    ppt_z_norm_lg<-0
    ppt_z_norm_lg_m<-1
  }
  else {
    ppt_z_norm_lg<-dat$ppt_z_norm
    ppt_z_norm_lg_m<-0
  }

  # Compute classification logits 
  Cluster_lg_1<-(4.5188594)+
     (-0.59396254)*css_signs___2_lg+
     (-0.16227453)*orebro_score_lg+
     (0.4033855)*rbcrpsbpds_score_lg+
     (0.74837457)*triggering_event_no0_no1_lg+
     (0.277063)*ppt_z_norm_lg+
     (0.00085489678)*orebro_score_lg*orebro_score_lg+
     (-0.010732156)*rbcrpsbpds_score_lg*rbcrpsbpds_score_lg+
     (-0.11925119)*ppt_z_norm_lg*ppt_z_norm_lg+
     (-0.42008314)*css_signs___2_lg_m+
     (-6.6429874)*orebro_score_lg_m+
     (1.2273978)*rbcrpsbpds_score_lg_m+
     (2.1113198)*triggering_event_no0_no1_lg_m+
     (-0.36593027)*ppt_z_norm_lg_m
  Cluster_lg_2<-(18.784051)+
     (-1.4680407)*css_signs___2_lg+
     (-0.2917661)*orebro_score_lg+
     (-0.3933944)*rbcrpsbpds_score_lg+
     (0.45623914)*triggering_event_no0_no1_lg+
     (-0.20779313)*ppt_z_norm_lg+
     (0.0016569632)*orebro_score_lg*orebro_score_lg+
     (-0.019228013)*rbcrpsbpds_score_lg*rbcrpsbpds_score_lg+
     (-0.054071223)*ppt_z_norm_lg*ppt_z_norm_lg+
     (-0.68517065)*css_signs___2_lg_m+
     (-10.360489)*orebro_score_lg_m+
     (-8.1253055)*rbcrpsbpds_score_lg_m+
     (1.1429389)*triggering_event_no0_no1_lg_m+
     (-0.67164784)*ppt_z_norm_lg_m
  Cluster_lg_3<-(-25.091576)+
     (1.2892414)*css_signs___2_lg+
     (0.35587142)*orebro_score_lg+
     (0.8039539)*rbcrpsbpds_score_lg+
     (-0.066881621)*triggering_event_no0_no1_lg+
     (-0.037514539)*ppt_z_norm_lg+
     (-0.0026335818)*orebro_score_lg*orebro_score_lg+
     (0.00021162458)*rbcrpsbpds_score_lg*rbcrpsbpds_score_lg+
     (0.05038911)*ppt_z_norm_lg*ppt_z_norm_lg+
     (0.75162977)*css_signs___2_lg_m+
     (10.887568)*orebro_score_lg_m+
     (13.831902)*rbcrpsbpds_score_lg_m+
     (-0.42588956)*triggering_event_no0_no1_lg_m+
     (-0.016862815)*ppt_z_norm_lg_m
  Cluster_lg_4<-(1.7886653)+
     (0.77276184)*css_signs___2_lg+
     (0.098169212)*orebro_score_lg+
     (-0.813945)*rbcrpsbpds_score_lg+
     (-1.1377321)*triggering_event_no0_no1_lg+
     (-0.031755329)*ppt_z_norm_lg+
     (0.00012172176)*orebro_score_lg*orebro_score_lg+
     (0.029748545)*rbcrpsbpds_score_lg*rbcrpsbpds_score_lg+
     (0.1229333)*ppt_z_norm_lg*ppt_z_norm_lg+
     (0.35362402)*css_signs___2_lg_m+
     (6.1159092)*orebro_score_lg_m+
     (-6.9339944)*rbcrpsbpds_score_lg_m+
     (-2.8283691)*triggering_event_no0_no1_lg_m+
     (1.0544409)*ppt_z_norm_lg_m

  # Compute odds from logits 
  max_lg<-Cluster_lg_1
  if(Cluster_lg_2>max_lg) {
    max_lg<-Cluster_lg_2
  }
  if(Cluster_lg_3>max_lg) {
    max_lg<-Cluster_lg_3
  }
  if(Cluster_lg_4>max_lg) {
    max_lg<-Cluster_lg_4
  }
  Cluster_lg_1<-exp(Cluster_lg_1-max_lg)
  Cluster_lg_2<-exp(Cluster_lg_2-max_lg)
  Cluster_lg_3<-exp(Cluster_lg_3-max_lg)
  Cluster_lg_4<-exp(Cluster_lg_4-max_lg)

  # Compute modal class and probabilities from odds 
  max_lg<-Cluster_lg_1
  Cluster_lg_modal<-1
  if(Cluster_lg_2>max_lg) {
    max_lg<-Cluster_lg_2
    Cluster_lg_modal<-2
  }
  if(Cluster_lg_3>max_lg) {
    max_lg<-Cluster_lg_3
    Cluster_lg_modal<-3
  }
  if(Cluster_lg_4>max_lg) {
    max_lg<-Cluster_lg_4
    Cluster_lg_modal<-4
  }
  sum_lg<-Cluster_lg_1+Cluster_lg_2+Cluster_lg_3+Cluster_lg_4
  Cluster_lg_1<-Cluster_lg_1/sum_lg
  Cluster_lg_2<-Cluster_lg_2/sum_lg
  Cluster_lg_3<-Cluster_lg_3/sum_lg
  Cluster_lg_4<-Cluster_lg_4/sum_lg

  return(list(
    "Cluster_modal"=Cluster_lg_modal,
    "Cluster_1"=Cluster_lg_1,
    "Cluster_2"=Cluster_lg_2,
    "Cluster_3"=Cluster_lg_3,
    "Cluster_4"=Cluster_lg_4
  ))
}

## Example of call of scoring function in a loop over records 
outdata<-inpdata
for(i in 1:nrow(outdata)){
  scoring<-lg_scoring(outdata[i,])
  outdata[i,"Cluster_modal"]<-scoring$Cluster_modal
  outdata[i,"Cluster_1"]<-scoring$Cluster_1
  outdata[i,"Cluster_2"]<-scoring$Cluster_2
  outdata[i,"Cluster_3"]<-scoring$Cluster_3
  outdata[i,"Cluster_4"]<-scoring$Cluster_4
}
