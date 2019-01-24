############### load libraries

library(neuralnet) ## to fit and test nn
library(ggplot2) ## to plot results 


############## set directory 

setwd("C:/Users/mowrys/Desktop/nn_2")

############# import data

full<-read.csv("full.nn.data.csv")

#####MODEL FORMATION#####

    #create nn's with different input variable, and hidden node combinations
    #g<-c(9,11,14,16,19,21,23,24,26,28,30,32)
    g<-c(9,11,14,16,19,21,23,24,30,32)   ## adjust based on which variable to include 
    geats<-names(full[g])
    geats
    g_fun<-paste(geats, collapse = ' + ')
    g_fun<-paste("log_larvae~",g_fun)
    g_fun<-as.formula(g_fun)

    nn<-neuralnet(formula = g_fun, 
               data = full,
               hidden =10,  ## adjust based on number of hidden nodes desired
              threshold =5, ## adjust based on desired threshold
               linear.output = TRUE,
                stepmax = 1000000) 

    #variable selcetion 
AIC<-(nrow(full)-1)*log(nn$result.matrix[1]/(nrow(full)-1))+2*((length(g)+2)*9+1)

## save model with lowest AIC
bestnn<-nn 

## run 25 networks with optimal nodes/variables to obtain best intial variables

bestnn<-randnn11
## optimize threshold 

bestnn<-thresh.2nn

##Compute R^2 between actual and predicted values 
    
predicted.nn.values<-compute(bestnn,full[, g])
pred.nn <- predicted.nn.values$net.result
summary(lm(full$log_larvae ~ pred.nn))$r.squared

##append model predictions to original  data

full$pred<-pred.nn


#####MODEL PERFORMANCE#####

## graph actual vs. predicted values 

plot(full$log_larvae, pred.nn, main = "Actual v Predicted Values",
     xlab = "Actual", ylab = "Predicted",ylim=c(0, max(full$log_larvae)))
abline(lm(full$log_larvae ~ pred.nn))
plot(x, y, main="title", sub="subtitle",
     xlab="X-axis label", ylab="y-axis label",
     xlim=c(xmin, xmax), ylim=c(ymin, ymax))

## plot residuals 
plot(full$log_larvae, resid(lm(full$log_larvae ~ full$pred.nn)), 
     ylab="Residuals", xlab="Log larvae", cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5) 
abline(0, 0)   

## plot model architecture 
plot(bestnn)

## save model weights
results<-bestnn$result.matrix

## export model weights
write.csv(results, "weights.csv")

#####MODEL VALIDATION#####

##add CDD_cat to dataset for validation  
CDD_cat<-function(x){
  if (x< -.3) 
    {"1: very low"}
      else 
      {if(x < -.1)
      {"2: low"}
        else 
        {if (x < .1)
        {"3: moderate"}
          else 
          {if (x < .3) 
          {"4: high"}
            else {"5: very high"}}}}}


full$CDD_cat<-apply(full[,16,drop = F], 1, CDD_cat)


##export full data with model predictions and CDD_cat  
write.csv(full, "full_data_w_CDD_cat.csv")

##look at neural network data output (for validation )
ggplot(full, aes(day.of.year,predicted.log.larvae))+geom_smooth(aes
    (group = CDD_cat, color = CDD_cat)) + theme_grey(
      base_size = 20)  + scale_x_continuous(
        name ="Day of the year", limits=c(100, 350))  + scale_y_continuous(
          name ="Predicted log(larvae)", limits=c(0, 1.5))  

#####SIMULATED DATA######


#load in NN weights
int<-weights[114,2]
w1<-weights[115,2]
w2<-weights[116,2]
w3<-weights[117,2]
w4<-weights[118,2]
w5<-weights[119,2]
w6<-weights[120,2]
w7<-weights[121,2]
w8<-weights[122,2]
w9<-weights[123,2]
w10<-weights[124,2]

int1<-weights[4,2]
w11<-weights[5,2]
w21<-weights[6,2]
w31<-weights[7,2]
w41<-weights[8,2]
w51<-weights[9,2]
w61<-weights[10,2]
w71<-weights[11,2]
w81<-weights[12,2]
w91<-weights[13,2]
w101<-weights[14,2]

int2<-weights[15,2]
w12<-weights[16,2]
w22<-weights[17,2]
w32<-weights[18,2]
w42<-weights[19,2]
w52<-weights[20,2]
w62<-weights[21,2]
w72<-weights[22,2]
w82<-weights[23,2]
w92<-weights[24,2]
w102<-weights[25,2]


int3<-weights[26,2]
w13<-weights[27,2]
w23<-weights[28,2]
w33<-weights[29,2]
w43<-weights[30,2]
w53<-weights[31,2]
w63<-weights[32,2]
w73<-weights[33,2]
w83<-weights[34,2]
w93<-weights[35,2]
w103<-weights[36,2]

int4<-weights[37,2]
w14<-weights[38,2]
w24<-weights[39,2]
w34<-weights[40,2]
w44<-weights[41,2]
w54<-weights[42,2]
w64<-weights[43,2]
w74<-weights[44,2]
w84<-weights[45,2]
w94<-weights[46,2]
w104<-weights[47,2]

int5<-weights[48,2]
w15<-weights[49,2]
w25<-weights[50,2]
w35<-weights[51,2]
w45<-weights[52,2]
w55<-weights[53,2]
w65<-weights[54,2]
w75<-weights[55,2]
w85<-weights[56,2]
w95<-weights[57,2]
w105<-weights[58,2]

int6<-weights[59,2]
w16<-weights[60,2]
w26<-weights[61,2]
w36<-weights[62,2]
w46<-weights[63,2]
w56<-weights[64,2]
w66<-weights[65,2]
w76<-weights[66,2]
w86<-weights[67,2]
w96<-weights[68,2]
w106<-weights[69,2]

int7<-weights[70,2]
w17<-weights[71,2]
w27<-weights[72,2]
w37<-weights[73,2]
w47<-weights[74,2]
w57<-weights[75,2]
w67<-weights[76,2]
w77<-weights[77,2]
w87<-weights[78,2]
w97<-weights[79,2]
w107<-weights[80,2]

int8<-weights[81,2]
w18<-weights[82,2]
w28<-weights[83,2]
w38<-weights[84,2]
w48<-weights[85,2]
w58<-weights[86,2]
w68<-weights[87,2]
w78<-weights[88,2]
w88<-weights[89,2]
w98<-weights[90,2]
w108<-weights[91,2]

int9<-weights[92,2]
w19<-weights[93,2]
w29<-weights[94,2]
w39<-weights[95,2]
w49<-weights[96,2]
w59<-weights[97,2]
w69<-weights[98,2]
w79<-weights[99,2]
w89<-weights[100,2]
w99<-weights[101,2]
w109<-weights[102,2]

int10<-weights[103,2]
w110<-weights[104,2]
w210<-weights[105,2]
w310<-weights[106,2]
w410<-weights[107,2]
w510<-weights[108,2]
w610<-weights[109,2]
w710<-weights[110,2]
w810<-weights[111,2]
w910<-weights[112,2]
w1010<-weights[113,2]


## create data set of full combination of variables  
time<-c(rep(0,72600),rep(1,72600))
day.of.year_1_t<-c(rep(-0.328193833,72600),rep(0.059471366,72600)) ##only looking at single day for small peak
day.of.year_2_t<-c(rep(-0.328193833,72600),rep(0.063876652,72600))
day.of.year_3_t<-c(rep(-0.328193833,72600),rep(0.068281938,72600))
day.of.year_4_t<-c(rep(-0.328193833,72600),rep(0.072687225,72600))
day.of.year_5_t<-c(rep(-0.328193833,72600),rep(0.077092511,72600))
day.of.year_6_t<-c(rep(-0.328193833,72600),rep(0.081497797,72600))
day.of.year_7_t<-c(rep(-0.328193833,72600),rep(0.085903084,72600))
day.of.year_8_t<-c(rep(-0.328193833,72600),rep(0.09030837,72600))
day.of.year_9_t<-c(rep(-0.328193833,72600),rep(0.094713656,72600))
day.of.year_10_t<-c(rep(-0.328193833,72600),rep(0.099118943,72600))
day.of.year_11_t<-c(rep(-0.328193833,72600),rep(0.103524229,72600))
day.of.year_12_t<-c(rep(-0.328193833,72600),rep(0.107929515,72600))
day.of.year_13_t<-c(rep(-0.328193833,72600),rep(0.112334802,72600))
day.of.year_14_t<-c(rep(-0.328193833,72600),rep(0.116740088,72600))
day.of.year_15_t<-c(rep(-0.328193833,72600),rep(0.121145374,72600))
day.of.year_16_t<-c(rep(-0.328193833,72600),rep(0.125550661,72600))
day.of.year_17_t<-c(rep(-0.328193833,72600),rep(0.129955947,72600))



MNA_t<-rep(c(rep(-.5,14520),rep(-.25,14520),rep(0,14520), rep(.25,14520), rep(.5,14520)),2)

wt_t<-rep(c(rep(-.5,2904),rep(-.25,2904),rep(0,2904), rep(.25,2904), rep(.5,2904)),10)

CDD_t<-rep(c(rep(-.5,264), rep(-.4,264),rep(-.3,264),rep(-.2,264),rep(-.1,264),rep(0,264),
             rep(.1,264),rep(.2,264),rep(.3,264),rep(.4,264),rep(.5,264)) ,50)

PHDI_t<-rep(c(rep(-.5,24), rep(-.4,24),rep(-.3,24),rep(-.2,24),rep(-.1,24),rep(0,24),
              rep(.1,24),rep(.2,24),rep(.3,24),rep(.4,24),rep(.5,24)) ,550)

sex<-rep(c(rep(0,12), rep(1,12)) ,6050)
subadult<-rep(c(0,1,1) ,48400)
adult<-rep(c(0,0,1) ,48400)
pg<-rep(c(c(0,0,0,0,0,0,0,1,1,0,1,1) ,c(0,0,0,0,0,0,0,0,0,0,0,0)),6050)
lac<-rep(c(c(0,0,0,0,1,1,0,0,0,0,1,1) ,c(0,0,0,0,0,0,0,0,0,0,0,0)),6050)

graph_data<-data.frame(time, day.of.year_1_t,day.of.year_2_t,
                       day.of.year_3_t,day.of.year_4_t,
                       day.of.year_5_t,day.of.year_6_t,
                       day.of.year_7_t,day.of.year_8_t,
                       day.of.year_9_t,day.of.year_10_t,
                       day.of.year_11_t,day.of.year_12_t,
                       day.of.year_13_t,day.of.year_14_t,
                       day.of.year_15_t,day.of.year_16_t,day.of.year_17_t,
                       MNA_t, wt_t,CDD_t, PHDI_t, sex, subadult, adult, lac, pg)    



##add back-transformed data to graph_data 

transform<-function(x,y){((x+.5)*(max(y)-min(y)))+min(y)}


graph_data$MNA<-transform(graph_data$MNA_t, full$MNA)
graph_data$CDD<-transform(graph_data$CDD_t, full$delta.CDD)
graph_data$PHDI<-transform(graph_data$PHDI_t, full$PHDI)

## add NN predictions to simulation data

graph_data$pred.larvae_1<-(10^(int 
                                      +(w1/
                                        (1 + exp
                                          (-(int1 + w11*day.of.year_1_t + w21*MNA_t + w31*wt_t + w41*CDD_t 
                                          + w51*PHDI_t + w61*sex + w71*subadult + w81*adult + w91*lac
                                          + w101*pg))))+
                                        (w2/
                                          (1 + exp
                                            (-(int2 + w12*day.of.year_1_t + w22*MNA_t + w32*wt_t + w42*CDD_t
                                            + w52*PHDI_t + w62*sex + w72*subadult + w82*adult + w92*lac
                                            + w102*pg))))+
                                        (w3/
                                          (1 + exp
                                            (-(int3 + w13*day.of.year_1_t + w23*MNA_t + w33*wt_t + w43*CDD_t
                                            + w53*PHDI_t + w63*sex + w73*subadult + w83*adult + w93*lac
                                            + w103*pg))))+
                                        (w4/
                                          (1 + exp
                                            (-(int4 + w14*day.of.year_1_t + w24*MNA_t + w34*wt_t + w44*CDD_t 
                                            + w54*PHDI_t + w64*sex + w74*subadult + w84*adult + w94*lac
                                            + w104*pg))))+
                                        (w5/
                                          (1 + exp
                                            (-(int5 + w15*day.of.year_1_t + w25*MNA_t + w35*wt_t + w45*CDD_t 
                                            + w55*PHDI_t + w65*sex + w75*subadult + w85*adult + w95*lac
                                            + w105*pg))))+
                                        (w6/
                                          (1 + exp
                                            (-(int6 + w16*day.of.year_1_t + w26*MNA_t + w36*wt_t + w46*CDD_t 
                                             + w56*PHDI_t + w66*sex + w76*subadult + w86*adult + w96*lac
                                              + w106*pg))))+
                                        (w7/
                                          (1 + exp
                                            (-(int7 + w17*day.of.year_1_t + w27*MNA_t + w37*wt_t + w47*CDD_t
                                            + w57*PHDI_t + w67*sex + w77*subadult + w87*adult + w97*lac
                                            + w107*pg))))+
                                        (w8/
                                          (1 + exp
                                            (-(int8 + w18*day.of.year_1_t + w28*MNA_t + w38*wt_t + w48*CDD_t 
                                            + w58*PHDI_t + w68*sex + w78*subadult + w88*adult + w98*lac
                                            + w108*pg))))+
                                        (w9/
                                           (1 + exp
                                            (-(int9 + w19*day.of.year_1_t + w29*MNA_t + w39*wt_t + w49*CDD_t 
                                             + w59*PHDI_t + w69*sex + w79*subadult + w89*adult + w99*lac
                                            + w109*pg))))+
                                        (w10/
                                          (1 + exp
                                            (-(int10 + w110*day.of.year_1_t + w210*MNA_t + w310*wt_t + w410*CDD_t 
                                            + w510*PHDI_t + w610*sex + w710*subadult + w810*adult + w910*lac
                                            + w1010*pg))))))


graph_data$pred.larvae_2<-(10^(int 
                                          +(w1/
                                              (1 + exp
                                               (-(int1 + w11*day.of.year_2_t + w21*MNA_t + w31*wt_t + w41*CDD_t 
                                                  + w51*PHDI_t + w61*sex + w71*subadult + w81*adult + w91*lac
                                                  + w101*pg))))+
                                            (w2/
                                               (1 + exp
                                                (-(int2 + w12*day.of.year_2_t + w22*MNA_t + w32*wt_t + w42*CDD_t
                                                   + w52*PHDI_t + w62*sex + w72*subadult + w82*adult + w92*lac
                                                   + w102*pg))))+
                                            (w3/
                                               (1 + exp
                                                (-(int3 + w13*day.of.year_2_t + w23*MNA_t + w33*wt_t + w43*CDD_t
                                                   + w53*PHDI_t + w63*sex + w73*subadult + w83*adult + w93*lac
                                                   + w103*pg))))+
                                            (w4/
                                               (1 + exp
                                                (-(int4 + w14*day.of.year_2_t + w24*MNA_t + w34*wt_t + w44*CDD_t 
                                                   + w54*PHDI_t + w64*sex + w74*subadult + w84*adult + w94*lac
                                                   + w104*pg))))+
                                            (w5/
                                               (1 + exp
                                                (-(int5 + w15*day.of.year_2_t + w25*MNA_t + w35*wt_t + w45*CDD_t 
                                                   + w55*PHDI_t + w65*sex + w75*subadult + w85*adult + w95*lac
                                                   + w105*pg))))+
                                            (w6/
                                               (1 + exp
                                                (-(int6 + w16*day.of.year_2_t + w26*MNA_t + w36*wt_t + w46*CDD_t 
                                                   + w56*PHDI_t + w66*sex + w76*subadult + w86*adult + w96*lac
                                                   + w106*pg))))+
                                            (w7/
                                               (1 + exp
                                                (-(int7 + w17*day.of.year_2_t + w27*MNA_t + w37*wt_t + w47*CDD_t
                                                   + w57*PHDI_t + w67*sex + w77*subadult + w87*adult + w97*lac
                                                   + w107*pg))))+
                                            (w8/
                                               (1 + exp
                                                (-(int8 + w18*day.of.year_2_t + w28*MNA_t + w38*wt_t + w48*CDD_t 
                                                   + w58*PHDI_t + w68*sex + w78*subadult + w88*adult + w98*lac
                                                   + w108*pg))))+
                                            (w9/
                                               (1 + exp
                                                (-(int9 + w19*day.of.year_2_t + w29*MNA_t + w39*wt_t + w49*CDD_t 
                                                   + w59*PHDI_t + w69*sex + w79*subadult + w89*adult + w99*lac
                                                   + w109*pg))))+
                                            (w10/
                                               (1 + exp
                                                (-(int10 + w110*day.of.year_2_t + w210*MNA_t + w310*wt_t + w410*CDD_t 
                                                   + w510*PHDI_t + w610*sex + w710*subadult + w810*adult + w910*lac
                                                   + w1010*pg))))))

graph_data$pred.larvae_3<-(10^(int 
                                          +(w1/
                                              (1 + exp
                                               (-(int1 + w11*day.of.year_3_t + w21*MNA_t + w31*wt_t + w41*CDD_t 
                                                  + w51*PHDI_t + w61*sex + w71*subadult + w81*adult + w91*lac
                                                  + w101*pg))))+
                                            (w2/
                                               (1 + exp
                                                (-(int2 + w12*day.of.year_3_t + w22*MNA_t + w32*wt_t + w42*CDD_t
                                                   + w52*PHDI_t + w62*sex + w72*subadult + w82*adult + w92*lac
                                                   + w102*pg))))+
                                            (w3/
                                               (1 + exp
                                                (-(int3 + w13*day.of.year_3_t + w23*MNA_t + w33*wt_t + w43*CDD_t
                                                   + w53*PHDI_t + w63*sex + w73*subadult + w83*adult + w93*lac
                                                   + w103*pg))))+
                                            (w4/
                                               (1 + exp
                                                (-(int4 + w14*day.of.year_3_t + w24*MNA_t + w34*wt_t + w44*CDD_t 
                                                   + w54*PHDI_t + w64*sex + w74*subadult + w84*adult + w94*lac
                                                   + w104*pg))))+
                                            (w5/
                                               (1 + exp
                                                (-(int5 + w15*day.of.year_3_t + w25*MNA_t + w35*wt_t + w45*CDD_t 
                                                   + w55*PHDI_t + w65*sex + w75*subadult + w85*adult + w95*lac
                                                   + w105*pg))))+
                                            (w6/
                                               (1 + exp
                                                (-(int6 + w16*day.of.year_3_t + w26*MNA_t + w36*wt_t + w46*CDD_t 
                                                   + w56*PHDI_t + w66*sex + w76*subadult + w86*adult + w96*lac
                                                   + w106*pg))))+
                                            (w7/
                                               (1 + exp
                                                (-(int7 + w17*day.of.year_3_t + w27*MNA_t + w37*wt_t + w47*CDD_t
                                                   + w57*PHDI_t + w67*sex + w77*subadult + w87*adult + w97*lac
                                                   + w107*pg))))+
                                            (w8/
                                               (1 + exp
                                                (-(int8 + w18*day.of.year_3_t + w28*MNA_t + w38*wt_t + w48*CDD_t 
                                                   + w58*PHDI_t + w68*sex + w78*subadult + w88*adult + w98*lac
                                                   + w108*pg))))+
                                            (w9/
                                               (1 + exp
                                                (-(int9 + w19*day.of.year_3_t + w29*MNA_t + w39*wt_t + w49*CDD_t 
                                                   + w59*PHDI_t + w69*sex + w79*subadult + w89*adult + w99*lac
                                                   + w109*pg))))+
                                            (w10/
                                               (1 + exp
                                                (-(int10 + w110*day.of.year_3_t + w210*MNA_t + w310*wt_t + w410*CDD_t 
                                                   + w510*PHDI_t + w610*sex + w710*subadult + w810*adult + w910*lac
                                                   + w1010*pg))))))

graph_data$pred.larvae_4<-(10^(int 
                                          +(w1/
                                              (1 + exp
                                               (-(int1 + w11*day.of.year_4_t + w21*MNA_t + w31*wt_t + w41*CDD_t 
                                                  + w51*PHDI_t + w61*sex + w71*subadult + w81*adult + w91*lac
                                                  + w101*pg))))+
                                            (w2/
                                               (1 + exp
                                                (-(int2 + w12*day.of.year_4_t + w22*MNA_t + w32*wt_t + w42*CDD_t
                                                   + w52*PHDI_t + w62*sex + w72*subadult + w82*adult + w92*lac
                                                   + w102*pg))))+
                                            (w3/
                                               (1 + exp
                                                (-(int3 + w13*day.of.year_4_t + w23*MNA_t + w33*wt_t + w43*CDD_t
                                                   + w53*PHDI_t + w63*sex + w73*subadult + w83*adult + w93*lac
                                                   + w103*pg))))+
                                            (w4/
                                               (1 + exp
                                                (-(int4 + w14*day.of.year_4_t + w24*MNA_t + w34*wt_t + w44*CDD_t 
                                                   + w54*PHDI_t + w64*sex + w74*subadult + w84*adult + w94*lac
                                                   + w104*pg))))+
                                            (w5/
                                               (1 + exp
                                                (-(int5 + w15*day.of.year_4_t + w25*MNA_t + w35*wt_t + w45*CDD_t 
                                                   + w55*PHDI_t + w65*sex + w75*subadult + w85*adult + w95*lac
                                                   + w105*pg))))+
                                            (w6/
                                               (1 + exp
                                                (-(int6 + w16*day.of.year_4_t + w26*MNA_t + w36*wt_t + w46*CDD_t 
                                                   + w56*PHDI_t + w66*sex + w76*subadult + w86*adult + w96*lac
                                                   + w106*pg))))+
                                            (w7/
                                               (1 + exp
                                                (-(int7 + w17*day.of.year_4_t + w27*MNA_t + w37*wt_t + w47*CDD_t
                                                   + w57*PHDI_t + w67*sex + w77*subadult + w87*adult + w97*lac
                                                   + w107*pg))))+
                                            (w8/
                                               (1 + exp
                                                (-(int8 + w18*day.of.year_4_t + w28*MNA_t + w38*wt_t + w48*CDD_t 
                                                   + w58*PHDI_t + w68*sex + w78*subadult + w88*adult + w98*lac
                                                   + w108*pg))))+
                                            (w9/
                                               (1 + exp
                                                (-(int9 + w19*day.of.year_4_t + w29*MNA_t + w39*wt_t + w49*CDD_t 
                                                   + w59*PHDI_t + w69*sex + w79*subadult + w89*adult + w99*lac
                                                   + w109*pg))))+
                                            (w10/
                                               (1 + exp
                                                (-(int10 + w110*day.of.year_4_t + w210*MNA_t + w310*wt_t + w410*CDD_t 
                                                   + w510*PHDI_t + w610*sex + w710*subadult + w810*adult + w910*lac
                                                   + w1010*pg))))))


graph_data$pred.larvae_5<-(10^(int 
                                          +(w1/
                                              (1 + exp
                                               (-(int1 + w11*day.of.year_5_t + w21*MNA_t + w31*wt_t + w41*CDD_t 
                                                  + w51*PHDI_t + w61*sex + w71*subadult + w81*adult + w91*lac
                                                  + w101*pg))))+
                                            (w2/
                                               (1 + exp
                                                (-(int2 + w12*day.of.year_5_t + w22*MNA_t + w32*wt_t + w42*CDD_t
                                                   + w52*PHDI_t + w62*sex + w72*subadult + w82*adult + w92*lac
                                                   + w102*pg))))+
                                            (w3/
                                               (1 + exp
                                                (-(int3 + w13*day.of.year_5_t + w23*MNA_t + w33*wt_t + w43*CDD_t
                                                   + w53*PHDI_t + w63*sex + w73*subadult + w83*adult + w93*lac
                                                   + w103*pg))))+
                                            (w4/
                                               (1 + exp
                                                (-(int4 + w14*day.of.year_5_t + w24*MNA_t + w34*wt_t + w44*CDD_t 
                                                   + w54*PHDI_t + w64*sex + w74*subadult + w84*adult + w94*lac
                                                   + w104*pg))))+
                                            (w5/
                                               (1 + exp
                                                (-(int5 + w15*day.of.year_5_t + w25*MNA_t + w35*wt_t + w45*CDD_t 
                                                   + w55*PHDI_t + w65*sex + w75*subadult + w85*adult + w95*lac
                                                   + w105*pg))))+
                                            (w6/
                                               (1 + exp
                                                (-(int6 + w16*day.of.year_5_t + w26*MNA_t + w36*wt_t + w46*CDD_t 
                                                   + w56*PHDI_t + w66*sex + w76*subadult + w86*adult + w96*lac
                                                   + w106*pg))))+
                                            (w7/
                                               (1 + exp
                                                (-(int7 + w17*day.of.year_5_t + w27*MNA_t + w37*wt_t + w47*CDD_t
                                                   + w57*PHDI_t + w67*sex + w77*subadult + w87*adult + w97*lac
                                                   + w107*pg))))+
                                            (w8/
                                               (1 + exp
                                                (-(int8 + w18*day.of.year_5_t + w28*MNA_t + w38*wt_t + w48*CDD_t 
                                                   + w58*PHDI_t + w68*sex + w78*subadult + w88*adult + w98*lac
                                                   + w108*pg))))+
                                            (w9/
                                               (1 + exp
                                                (-(int9 + w19*day.of.year_5_t + w29*MNA_t + w39*wt_t + w49*CDD_t 
                                                   + w59*PHDI_t + w69*sex + w79*subadult + w89*adult + w99*lac
                                                   + w109*pg))))+
                                            (w10/
                                               (1 + exp
                                                (-(int10 + w110*day.of.year_5_t + w210*MNA_t + w310*wt_t + w410*CDD_t 
                                                   + w510*PHDI_t + w610*sex + w710*subadult + w810*adult + w910*lac
                                                   + w1010*pg))))))
graph_data$pred.larvae_6<-(10^(int 
                                          +(w1/
                                              (1 + exp
                                               (-(int1 + w11*day.of.year_6_t + w21*MNA_t + w31*wt_t + w41*CDD_t 
                                                  + w51*PHDI_t + w61*sex + w71*subadult + w81*adult + w91*lac
                                                  + w101*pg))))+
                                            (w2/
                                               (1 + exp
                                                (-(int2 + w12*day.of.year_6_t + w22*MNA_t + w32*wt_t + w42*CDD_t
                                                   + w52*PHDI_t + w62*sex + w72*subadult + w82*adult + w92*lac
                                                   + w102*pg))))+
                                            (w3/
                                               (1 + exp
                                                (-(int3 + w13*day.of.year_6_t + w23*MNA_t + w33*wt_t + w43*CDD_t
                                                   + w53*PHDI_t + w63*sex + w73*subadult + w83*adult + w93*lac
                                                   + w103*pg))))+
                                            (w4/
                                               (1 + exp
                                                (-(int4 + w14*day.of.year_6_t + w24*MNA_t + w34*wt_t + w44*CDD_t 
                                                   + w54*PHDI_t + w64*sex + w74*subadult + w84*adult + w94*lac
                                                   + w104*pg))))+
                                            (w5/
                                               (1 + exp
                                                (-(int5 + w15*day.of.year_6_t + w25*MNA_t + w35*wt_t + w45*CDD_t 
                                                   + w55*PHDI_t + w65*sex + w75*subadult + w85*adult + w95*lac
                                                   + w105*pg))))+
                                            (w6/
                                               (1 + exp
                                                (-(int6 + w16*day.of.year_6_t + w26*MNA_t + w36*wt_t + w46*CDD_t 
                                                   + w56*PHDI_t + w66*sex + w76*subadult + w86*adult + w96*lac
                                                   + w106*pg))))+
                                            (w7/
                                               (1 + exp
                                                (-(int7 + w17*day.of.year_6_t + w27*MNA_t + w37*wt_t + w47*CDD_t
                                                   + w57*PHDI_t + w67*sex + w77*subadult + w87*adult + w97*lac
                                                   + w107*pg))))+
                                            (w8/
                                               (1 + exp
                                                (-(int8 + w18*day.of.year_6_t + w28*MNA_t + w38*wt_t + w48*CDD_t 
                                                   + w58*PHDI_t + w68*sex + w78*subadult + w88*adult + w98*lac
                                                   + w108*pg))))+
                                            (w9/
                                               (1 + exp
                                                (-(int9 + w19*day.of.year_6_t + w29*MNA_t + w39*wt_t + w49*CDD_t 
                                                   + w59*PHDI_t + w69*sex + w79*subadult + w89*adult + w99*lac
                                                   + w109*pg))))+
                                            (w10/
                                               (1 + exp
                                                (-(int10 + w110*day.of.year_6_t + w210*MNA_t + w310*wt_t + w410*CDD_t 
                                                   + w510*PHDI_t + w610*sex + w710*subadult + w810*adult + w910*lac
                                                   + w1010*pg))))))

graph_data$pred.larvae_7<-(10^(int 
                                          +(w1/
                                              (1 + exp
                                               (-(int1 + w11*day.of.year_7_t + w21*MNA_t + w31*wt_t + w41*CDD_t 
                                                  + w51*PHDI_t + w61*sex + w71*subadult + w81*adult + w91*lac
                                                  + w101*pg))))+
                                            (w2/
                                               (1 + exp
                                                (-(int2 + w12*day.of.year_7_t + w22*MNA_t + w32*wt_t + w42*CDD_t
                                                   + w52*PHDI_t + w62*sex + w72*subadult + w82*adult + w92*lac
                                                   + w102*pg))))+
                                            (w3/
                                               (1 + exp
                                                (-(int3 + w13*day.of.year_7_t + w23*MNA_t + w33*wt_t + w43*CDD_t
                                                   + w53*PHDI_t + w63*sex + w73*subadult + w83*adult + w93*lac
                                                   + w103*pg))))+
                                            (w4/
                                               (1 + exp
                                                (-(int4 + w14*day.of.year_7_t + w24*MNA_t + w34*wt_t + w44*CDD_t 
                                                   + w54*PHDI_t + w64*sex + w74*subadult + w84*adult + w94*lac
                                                   + w104*pg))))+
                                            (w5/
                                               (1 + exp
                                                (-(int5 + w15*day.of.year_7_t + w25*MNA_t + w35*wt_t + w45*CDD_t 
                                                   + w55*PHDI_t + w65*sex + w75*subadult + w85*adult + w95*lac
                                                   + w105*pg))))+
                                            (w6/
                                               (1 + exp
                                                (-(int6 + w16*day.of.year_7_t + w26*MNA_t + w36*wt_t + w46*CDD_t 
                                                   + w56*PHDI_t + w66*sex + w76*subadult + w86*adult + w96*lac
                                                   + w106*pg))))+
                                            (w7/
                                               (1 + exp
                                                (-(int7 + w17*day.of.year_7_t + w27*MNA_t + w37*wt_t + w47*CDD_t
                                                   + w57*PHDI_t + w67*sex + w77*subadult + w87*adult + w97*lac
                                                   + w107*pg))))+
                                            (w8/
                                               (1 + exp
                                                (-(int8 + w18*day.of.year_7_t + w28*MNA_t + w38*wt_t + w48*CDD_t 
                                                   + w58*PHDI_t + w68*sex + w78*subadult + w88*adult + w98*lac
                                                   + w108*pg))))+
                                            (w9/
                                               (1 + exp
                                                (-(int9 + w19*day.of.year_7_t + w29*MNA_t + w39*wt_t + w49*CDD_t 
                                                   + w59*PHDI_t + w69*sex + w79*subadult + w89*adult + w99*lac
                                                   + w109*pg))))+
                                            (w10/
                                               (1 + exp
                                                (-(int10 + w110*day.of.year_7_t + w210*MNA_t + w310*wt_t + w410*CDD_t 
                                                   + w510*PHDI_t + w610*sex + w710*subadult + w810*adult + w910*lac
                                                   + w1010*pg))))))

graph_data$pred.larvae_8<-(10^(int 
                                          +(w1/
                                              (1 + exp
                                               (-(int1 + w11*day.of.year_8_t + w21*MNA_t + w31*wt_t + w41*CDD_t 
                                                  + w51*PHDI_t + w61*sex + w71*subadult + w81*adult + w91*lac
                                                  + w101*pg))))+
                                            (w2/
                                               (1 + exp
                                                (-(int2 + w12*day.of.year_8_t + w22*MNA_t + w32*wt_t + w42*CDD_t
                                                   + w52*PHDI_t + w62*sex + w72*subadult + w82*adult + w92*lac
                                                   + w102*pg))))+
                                            (w3/
                                               (1 + exp
                                                (-(int3 + w13*day.of.year_8_t + w23*MNA_t + w33*wt_t + w43*CDD_t
                                                   + w53*PHDI_t + w63*sex + w73*subadult + w83*adult + w93*lac
                                                   + w103*pg))))+
                                            (w4/
                                               (1 + exp
                                                (-(int4 + w14*day.of.year_8_t + w24*MNA_t + w34*wt_t + w44*CDD_t 
                                                   + w54*PHDI_t + w64*sex + w74*subadult + w84*adult + w94*lac
                                                   + w104*pg))))+
                                            (w5/
                                               (1 + exp
                                                (-(int5 + w15*day.of.year_8_t + w25*MNA_t + w35*wt_t + w45*CDD_t 
                                                   + w55*PHDI_t + w65*sex + w75*subadult + w85*adult + w95*lac
                                                   + w105*pg))))+
                                            (w6/
                                               (1 + exp
                                                (-(int6 + w16*day.of.year_8_t + w26*MNA_t + w36*wt_t + w46*CDD_t 
                                                   + w56*PHDI_t + w66*sex + w76*subadult + w86*adult + w96*lac
                                                   + w106*pg))))+
                                            (w7/
                                               (1 + exp
                                                (-(int7 + w17*day.of.year_8_t + w27*MNA_t + w37*wt_t + w47*CDD_t
                                                   + w57*PHDI_t + w67*sex + w77*subadult + w87*adult + w97*lac
                                                   + w107*pg))))+
                                            (w8/
                                               (1 + exp
                                                (-(int8 + w18*day.of.year_8_t + w28*MNA_t + w38*wt_t + w48*CDD_t 
                                                   + w58*PHDI_t + w68*sex + w78*subadult + w88*adult + w98*lac
                                                   + w108*pg))))+
                                            (w9/
                                               (1 + exp
                                                (-(int9 + w19*day.of.year_8_t + w29*MNA_t + w39*wt_t + w49*CDD_t 
                                                   + w59*PHDI_t + w69*sex + w79*subadult + w89*adult + w99*lac
                                                   + w109*pg))))+
                                            (w10/
                                               (1 + exp
                                                (-(int10 + w110*day.of.year_8_t + w210*MNA_t + w310*wt_t + w410*CDD_t 
                                                   + w510*PHDI_t + w610*sex + w710*subadult + w810*adult + w910*lac
                                                   + w1010*pg))))))


graph_data$pred.larvae_9<-(10^(int 
                                          +(w1/
                                              (1 + exp
                                               (-(int1 + w11*day.of.year_9_t + w21*MNA_t + w31*wt_t + w41*CDD_t 
                                                  + w51*PHDI_t + w61*sex + w71*subadult + w81*adult + w91*lac
                                                  + w101*pg))))+
                                            (w2/
                                               (1 + exp
                                                (-(int2 + w12*day.of.year_9_t + w22*MNA_t + w32*wt_t + w42*CDD_t
                                                   + w52*PHDI_t + w62*sex + w72*subadult + w82*adult + w92*lac
                                                   + w102*pg))))+
                                            (w3/
                                               (1 + exp
                                                (-(int3 + w13*day.of.year_9_t + w23*MNA_t + w33*wt_t + w43*CDD_t
                                                   + w53*PHDI_t + w63*sex + w73*subadult + w83*adult + w93*lac
                                                   + w103*pg))))+
                                            (w4/
                                               (1 + exp
                                                (-(int4 + w14*day.of.year_9_t + w24*MNA_t + w34*wt_t + w44*CDD_t 
                                                   + w54*PHDI_t + w64*sex + w74*subadult + w84*adult + w94*lac
                                                   + w104*pg))))+
                                            (w5/
                                               (1 + exp
                                                (-(int5 + w15*day.of.year_9_t + w25*MNA_t + w35*wt_t + w45*CDD_t 
                                                   + w55*PHDI_t + w65*sex + w75*subadult + w85*adult + w95*lac
                                                   + w105*pg))))+
                                            (w6/
                                               (1 + exp
                                                (-(int6 + w16*day.of.year_9_t + w26*MNA_t + w36*wt_t + w46*CDD_t 
                                                   + w56*PHDI_t + w66*sex + w76*subadult + w86*adult + w96*lac
                                                   + w106*pg))))+
                                            (w7/
                                               (1 + exp
                                                (-(int7 + w17*day.of.year_9_t + w27*MNA_t + w37*wt_t + w47*CDD_t
                                                   + w57*PHDI_t + w67*sex + w77*subadult + w87*adult + w97*lac
                                                   + w107*pg))))+
                                            (w8/
                                               (1 + exp
                                                (-(int8 + w18*day.of.year_9_t + w28*MNA_t + w38*wt_t + w48*CDD_t 
                                                   + w58*PHDI_t + w68*sex + w78*subadult + w88*adult + w98*lac
                                                   + w108*pg))))+
                                            (w9/
                                               (1 + exp
                                                (-(int9 + w19*day.of.year_9_t + w29*MNA_t + w39*wt_t + w49*CDD_t 
                                                   + w59*PHDI_t + w69*sex + w79*subadult + w89*adult + w99*lac
                                                   + w109*pg))))+
                                            (w10/
                                               (1 + exp
                                                (-(int10 + w110*day.of.year_9_t + w210*MNA_t + w310*wt_t + w410*CDD_t 
                                                   + w510*PHDI_t + w610*sex + w710*subadult + w810*adult + w910*lac
                                                   + w1010*pg))))))


graph_data$pred.larvae_10<-(10^(int 
                                          +(w1/
                                              (1 + exp
                                               (-(int1 + w11*day.of.year_10_t + w21*MNA_t + w31*wt_t + w41*CDD_t 
                                                  + w51*PHDI_t + w61*sex + w71*subadult + w81*adult + w91*lac
                                                  + w101*pg))))+
                                            (w2/
                                               (1 + exp
                                                (-(int2 + w12*day.of.year_10_t + w22*MNA_t + w32*wt_t + w42*CDD_t
                                                   + w52*PHDI_t + w62*sex + w72*subadult + w82*adult + w92*lac
                                                   + w102*pg))))+
                                            (w3/
                                               (1 + exp
                                                (-(int3 + w13*day.of.year_10_t + w23*MNA_t + w33*wt_t + w43*CDD_t
                                                   + w53*PHDI_t + w63*sex + w73*subadult + w83*adult + w93*lac
                                                   + w103*pg))))+
                                            (w4/
                                               (1 + exp
                                                (-(int4 + w14*day.of.year_10_t + w24*MNA_t + w34*wt_t + w44*CDD_t 
                                                   + w54*PHDI_t + w64*sex + w74*subadult + w84*adult + w94*lac
                                                   + w104*pg))))+
                                            (w5/
                                               (1 + exp
                                                (-(int5 + w15*day.of.year_10_t + w25*MNA_t + w35*wt_t + w45*CDD_t 
                                                   + w55*PHDI_t + w65*sex + w75*subadult + w85*adult + w95*lac
                                                   + w105*pg))))+
                                            (w6/
                                               (1 + exp
                                                (-(int6 + w16*day.of.year_10_t + w26*MNA_t + w36*wt_t + w46*CDD_t 
                                                   + w56*PHDI_t + w66*sex + w76*subadult + w86*adult + w96*lac
                                                   + w106*pg))))+
                                            (w7/
                                               (1 + exp
                                                (-(int7 + w17*day.of.year_10_t + w27*MNA_t + w37*wt_t + w47*CDD_t
                                                   + w57*PHDI_t + w67*sex + w77*subadult + w87*adult + w97*lac
                                                   + w107*pg))))+
                                            (w8/
                                               (1 + exp
                                                (-(int8 + w18*day.of.year_10_t + w28*MNA_t + w38*wt_t + w48*CDD_t 
                                                   + w58*PHDI_t + w68*sex + w78*subadult + w88*adult + w98*lac
                                                   + w108*pg))))+
                                            (w9/
                                               (1 + exp
                                                (-(int9 + w19*day.of.year_10_t + w29*MNA_t + w39*wt_t + w49*CDD_t 
                                                   + w59*PHDI_t + w69*sex + w79*subadult + w89*adult + w99*lac
                                                   + w109*pg))))+
                                            (w10/
                                               (1 + exp
                                                (-(int10 + w110*day.of.year_10_t + w210*MNA_t + w310*wt_t + w410*CDD_t 
                                                   + w510*PHDI_t + w610*sex + w710*subadult + w810*adult + w910*lac
                                                   + w1010*pg))))))

graph_data$pred.larvae_11<-(10^(int 
                                          +(w1/
                                              (1 + exp
                                               (-(int1 + w11*day.of.year_11_t + w21*MNA_t + w31*wt_t + w41*CDD_t 
                                                  + w51*PHDI_t + w61*sex + w71*subadult + w81*adult + w91*lac
                                                  + w101*pg))))+
                                            (w2/
                                               (1 + exp
                                                (-(int2 + w12*day.of.year_11_t + w22*MNA_t + w32*wt_t + w42*CDD_t
                                                   + w52*PHDI_t + w62*sex + w72*subadult + w82*adult + w92*lac
                                                   + w102*pg))))+
                                            (w3/
                                               (1 + exp
                                                (-(int3 + w13*day.of.year_11_t + w23*MNA_t + w33*wt_t + w43*CDD_t
                                                   + w53*PHDI_t + w63*sex + w73*subadult + w83*adult + w93*lac
                                                   + w103*pg))))+
                                            (w4/
                                               (1 + exp
                                                (-(int4 + w14*day.of.year_11_t + w24*MNA_t + w34*wt_t + w44*CDD_t 
                                                   + w54*PHDI_t + w64*sex + w74*subadult + w84*adult + w94*lac
                                                   + w104*pg))))+
                                            (w5/
                                               (1 + exp
                                                (-(int5 + w15*day.of.year_11_t + w25*MNA_t + w35*wt_t + w45*CDD_t 
                                                   + w55*PHDI_t + w65*sex + w75*subadult + w85*adult + w95*lac
                                                   + w105*pg))))+
                                            (w6/
                                               (1 + exp
                                                (-(int6 + w16*day.of.year_11_t + w26*MNA_t + w36*wt_t + w46*CDD_t 
                                                   + w56*PHDI_t + w66*sex + w76*subadult + w86*adult + w96*lac
                                                   + w106*pg))))+
                                            (w7/
                                               (1 + exp
                                                (-(int7 + w17*day.of.year_11_t + w27*MNA_t + w37*wt_t + w47*CDD_t
                                                   + w57*PHDI_t + w67*sex + w77*subadult + w87*adult + w97*lac
                                                   + w107*pg))))+
                                            (w8/
                                               (1 + exp
                                                (-(int8 + w18*day.of.year_11_t + w28*MNA_t + w38*wt_t + w48*CDD_t 
                                                   + w58*PHDI_t + w68*sex + w78*subadult + w88*adult + w98*lac
                                                   + w108*pg))))+
                                            (w9/
                                               (1 + exp
                                                (-(int9 + w19*day.of.year_11_t + w29*MNA_t + w39*wt_t + w49*CDD_t 
                                                   + w59*PHDI_t + w69*sex + w79*subadult + w89*adult + w99*lac
                                                   + w109*pg))))+
                                            (w10/
                                               (1 + exp
                                                (-(int10 + w110*day.of.year_11_t + w210*MNA_t + w310*wt_t + w410*CDD_t 
                                                   + w510*PHDI_t + w610*sex + w710*subadult + w810*adult + w910*lac
                                                   + w1010*pg))))))



graph_data$pred.larvae_12<-(10^(int 
                                          +(w1/
                                              (1 + exp
                                               (-(int1 + w11*day.of.year_12_t + w21*MNA_t + w31*wt_t + w41*CDD_t 
                                                  + w51*PHDI_t + w61*sex + w71*subadult + w81*adult + w91*lac
                                                  + w101*pg))))+
                                            (w2/
                                               (1 + exp
                                                (-(int2 + w12*day.of.year_12_t + w22*MNA_t + w32*wt_t + w42*CDD_t
                                                   + w52*PHDI_t + w62*sex + w72*subadult + w82*adult + w92*lac
                                                   + w102*pg))))+
                                            (w3/
                                               (1 + exp
                                                (-(int3 + w13*day.of.year_12_t + w23*MNA_t + w33*wt_t + w43*CDD_t
                                                   + w53*PHDI_t + w63*sex + w73*subadult + w83*adult + w93*lac
                                                   + w103*pg))))+
                                            (w4/
                                               (1 + exp
                                                (-(int4 + w14*day.of.year_12_t + w24*MNA_t + w34*wt_t + w44*CDD_t 
                                                   + w54*PHDI_t + w64*sex + w74*subadult + w84*adult + w94*lac
                                                   + w104*pg))))+
                                            (w5/
                                               (1 + exp
                                                (-(int5 + w15*day.of.year_12_t + w25*MNA_t + w35*wt_t + w45*CDD_t 
                                                   + w55*PHDI_t + w65*sex + w75*subadult + w85*adult + w95*lac
                                                   + w105*pg))))+
                                            (w6/
                                               (1 + exp
                                                (-(int6 + w16*day.of.year_12_t + w26*MNA_t + w36*wt_t + w46*CDD_t 
                                                   + w56*PHDI_t + w66*sex + w76*subadult + w86*adult + w96*lac
                                                   + w106*pg))))+
                                            (w7/
                                               (1 + exp
                                                (-(int7 + w17*day.of.year_12_t + w27*MNA_t + w37*wt_t + w47*CDD_t
                                                   + w57*PHDI_t + w67*sex + w77*subadult + w87*adult + w97*lac
                                                   + w107*pg))))+
                                            (w8/
                                               (1 + exp
                                                (-(int8 + w18*day.of.year_12_t + w28*MNA_t + w38*wt_t + w48*CDD_t 
                                                   + w58*PHDI_t + w68*sex + w78*subadult + w88*adult + w98*lac
                                                   + w108*pg))))+
                                            (w9/
                                               (1 + exp
                                                (-(int9 + w19*day.of.year_12_t + w29*MNA_t + w39*wt_t + w49*CDD_t 
                                                   + w59*PHDI_t + w69*sex + w79*subadult + w89*adult + w99*lac
                                                   + w109*pg))))+
                                            (w10/
                                               (1 + exp
                                                (-(int10 + w110*day.of.year_12_t + w210*MNA_t + w310*wt_t + w410*CDD_t 
                                                   + w510*PHDI_t + w610*sex + w710*subadult + w810*adult + w910*lac
                                                   + w1010*pg))))))


graph_data$pred.larvae_13<-(10^(int 
                                          +(w1/
                                              (1 + exp
                                               (-(int1 + w11*day.of.year_13_t + w21*MNA_t + w31*wt_t + w41*CDD_t 
                                                  + w51*PHDI_t + w61*sex + w71*subadult + w81*adult + w91*lac
                                                  + w101*pg))))+
                                            (w2/
                                               (1 + exp
                                                (-(int2 + w12*day.of.year_13_t + w22*MNA_t + w32*wt_t + w42*CDD_t
                                                   + w52*PHDI_t + w62*sex + w72*subadult + w82*adult + w92*lac
                                                   + w102*pg))))+
                                            (w3/
                                               (1 + exp
                                                (-(int3 + w13*day.of.year_13_t + w23*MNA_t + w33*wt_t + w43*CDD_t
                                                   + w53*PHDI_t + w63*sex + w73*subadult + w83*adult + w93*lac
                                                   + w103*pg))))+
                                            (w4/
                                               (1 + exp
                                                (-(int4 + w14*day.of.year_13_t + w24*MNA_t + w34*wt_t + w44*CDD_t 
                                                   + w54*PHDI_t + w64*sex + w74*subadult + w84*adult + w94*lac
                                                   + w104*pg))))+
                                            (w5/
                                               (1 + exp
                                                (-(int5 + w15*day.of.year_13_t + w25*MNA_t + w35*wt_t + w45*CDD_t 
                                                   + w55*PHDI_t + w65*sex + w75*subadult + w85*adult + w95*lac
                                                   + w105*pg))))+
                                            (w6/
                                               (1 + exp
                                                (-(int6 + w16*day.of.year_13_t + w26*MNA_t + w36*wt_t + w46*CDD_t 
                                                   + w56*PHDI_t + w66*sex + w76*subadult + w86*adult + w96*lac
                                                   + w106*pg))))+
                                            (w7/
                                               (1 + exp
                                                (-(int7 + w17*day.of.year_13_t + w27*MNA_t + w37*wt_t + w47*CDD_t
                                                   + w57*PHDI_t + w67*sex + w77*subadult + w87*adult + w97*lac
                                                   + w107*pg))))+
                                            (w8/
                                               (1 + exp
                                                (-(int8 + w18*day.of.year_13_t + w28*MNA_t + w38*wt_t + w48*CDD_t 
                                                   + w58*PHDI_t + w68*sex + w78*subadult + w88*adult + w98*lac
                                                   + w108*pg))))+
                                            (w9/
                                               (1 + exp
                                                (-(int9 + w19*day.of.year_13_t + w29*MNA_t + w39*wt_t + w49*CDD_t 
                                                   + w59*PHDI_t + w69*sex + w79*subadult + w89*adult + w99*lac
                                                   + w109*pg))))+
                                            (w10/
                                               (1 + exp
                                                (-(int10 + w110*day.of.year_13_t + w210*MNA_t + w310*wt_t + w410*CDD_t 
                                                   + w510*PHDI_t + w610*sex + w710*subadult + w810*adult + w910*lac
                                                   + w1010*pg))))))


graph_data$pred.larvae_14<-(10^(int 
                                          +(w1/
                                              (1 + exp
                                               (-(int1 + w11*day.of.year_14_t + w21*MNA_t + w31*wt_t + w41*CDD_t 
                                                  + w51*PHDI_t + w61*sex + w71*subadult + w81*adult + w91*lac
                                                  + w101*pg))))+
                                            (w2/
                                               (1 + exp
                                                (-(int2 + w12*day.of.year_14_t + w22*MNA_t + w32*wt_t + w42*CDD_t
                                                   + w52*PHDI_t + w62*sex + w72*subadult + w82*adult + w92*lac
                                                   + w102*pg))))+
                                            (w3/
                                               (1 + exp
                                                (-(int3 + w13*day.of.year_14_t + w23*MNA_t + w33*wt_t + w43*CDD_t
                                                   + w53*PHDI_t + w63*sex + w73*subadult + w83*adult + w93*lac
                                                   + w103*pg))))+
                                            (w4/
                                               (1 + exp
                                                (-(int4 + w14*day.of.year_14_t + w24*MNA_t + w34*wt_t + w44*CDD_t 
                                                   + w54*PHDI_t + w64*sex + w74*subadult + w84*adult + w94*lac
                                                   + w104*pg))))+
                                            (w5/
                                               (1 + exp
                                                (-(int5 + w15*day.of.year_14_t + w25*MNA_t + w35*wt_t + w45*CDD_t 
                                                   + w55*PHDI_t + w65*sex + w75*subadult + w85*adult + w95*lac
                                                   + w105*pg))))+
                                            (w6/
                                               (1 + exp
                                                (-(int6 + w16*day.of.year_14_t + w26*MNA_t + w36*wt_t + w46*CDD_t 
                                                   + w56*PHDI_t + w66*sex + w76*subadult + w86*adult + w96*lac
                                                   + w106*pg))))+
                                            (w7/
                                               (1 + exp
                                                (-(int7 + w17*day.of.year_14_t + w27*MNA_t + w37*wt_t + w47*CDD_t
                                                   + w57*PHDI_t + w67*sex + w77*subadult + w87*adult + w97*lac
                                                   + w107*pg))))+
                                            (w8/
                                               (1 + exp
                                                (-(int8 + w18*day.of.year_14_t + w28*MNA_t + w38*wt_t + w48*CDD_t 
                                                   + w58*PHDI_t + w68*sex + w78*subadult + w88*adult + w98*lac
                                                   + w108*pg))))+
                                            (w9/
                                               (1 + exp
                                                (-(int9 + w19*day.of.year_14_t + w29*MNA_t + w39*wt_t + w49*CDD_t 
                                                   + w59*PHDI_t + w69*sex + w79*subadult + w89*adult + w99*lac
                                                   + w109*pg))))+
                                            (w10/
                                               (1 + exp
                                                (-(int10 + w110*day.of.year_14_t + w210*MNA_t + w310*wt_t + w410*CDD_t 
                                                   + w510*PHDI_t + w610*sex + w710*subadult + w810*adult + w910*lac
                                                   + w1010*pg))))))
graph_data$pred.larvae_15<-(10^(int 
                                          +(w1/
                                              (1 + exp
                                               (-(int1 + w11*day.of.year_15_t + w21*MNA_t + w31*wt_t + w41*CDD_t 
                                                  + w51*PHDI_t + w61*sex + w71*subadult + w81*adult + w91*lac
                                                  + w101*pg))))+
                                            (w2/
                                               (1 + exp
                                                (-(int2 + w12*day.of.year_15_t + w22*MNA_t + w32*wt_t + w42*CDD_t
                                                   + w52*PHDI_t + w62*sex + w72*subadult + w82*adult + w92*lac
                                                   + w102*pg))))+
                                            (w3/
                                               (1 + exp
                                                (-(int3 + w13*day.of.year_15_t + w23*MNA_t + w33*wt_t + w43*CDD_t
                                                   + w53*PHDI_t + w63*sex + w73*subadult + w83*adult + w93*lac
                                                   + w103*pg))))+
                                            (w4/
                                               (1 + exp
                                                (-(int4 + w14*day.of.year_15_t + w24*MNA_t + w34*wt_t + w44*CDD_t 
                                                   + w54*PHDI_t + w64*sex + w74*subadult + w84*adult + w94*lac
                                                   + w104*pg))))+
                                            (w5/
                                               (1 + exp
                                                (-(int5 + w15*day.of.year_15_t + w25*MNA_t + w35*wt_t + w45*CDD_t 
                                                   + w55*PHDI_t + w65*sex + w75*subadult + w85*adult + w95*lac
                                                   + w105*pg))))+
                                            (w6/
                                               (1 + exp
                                                (-(int6 + w16*day.of.year_15_t + w26*MNA_t + w36*wt_t + w46*CDD_t 
                                                   + w56*PHDI_t + w66*sex + w76*subadult + w86*adult + w96*lac
                                                   + w106*pg))))+
                                            (w7/
                                               (1 + exp
                                                (-(int7 + w17*day.of.year_15_t + w27*MNA_t + w37*wt_t + w47*CDD_t
                                                   + w57*PHDI_t + w67*sex + w77*subadult + w87*adult + w97*lac
                                                   + w107*pg))))+
                                            (w8/
                                               (1 + exp
                                                (-(int8 + w18*day.of.year_15_t + w28*MNA_t + w38*wt_t + w48*CDD_t 
                                                   + w58*PHDI_t + w68*sex + w78*subadult + w88*adult + w98*lac
                                                   + w108*pg))))+
                                            (w9/
                                               (1 + exp
                                                (-(int9 + w19*day.of.year_15_t + w29*MNA_t + w39*wt_t + w49*CDD_t 
                                                   + w59*PHDI_t + w69*sex + w79*subadult + w89*adult + w99*lac
                                                   + w109*pg))))+
                                            (w10/
                                               (1 + exp
                                                (-(int10 + w110*day.of.year_15_t + w210*MNA_t + w310*wt_t + w410*CDD_t 
                                                   + w510*PHDI_t + w610*sex + w710*subadult + w810*adult + w910*lac
                                                   + w1010*pg))))))


graph_data$pred.larvae_16<-(10^(int 
                                          +(w1/
                                              (1 + exp
                                               (-(int1 + w11*day.of.year_16_t + w21*MNA_t + w31*wt_t + w41*CDD_t 
                                                  + w51*PHDI_t + w61*sex + w71*subadult + w81*adult + w91*lac
                                                  + w101*pg))))+
                                            (w2/
                                               (1 + exp
                                                (-(int2 + w12*day.of.year_16_t + w22*MNA_t + w32*wt_t + w42*CDD_t
                                                   + w52*PHDI_t + w62*sex + w72*subadult + w82*adult + w92*lac
                                                   + w102*pg))))+
                                            (w3/
                                               (1 + exp
                                                (-(int3 + w13*day.of.year_16_t + w23*MNA_t + w33*wt_t + w43*CDD_t
                                                   + w53*PHDI_t + w63*sex + w73*subadult + w83*adult + w93*lac
                                                   + w103*pg))))+
                                            (w4/
                                               (1 + exp
                                                (-(int4 + w14*day.of.year_16_t + w24*MNA_t + w34*wt_t + w44*CDD_t 
                                                   + w54*PHDI_t + w64*sex + w74*subadult + w84*adult + w94*lac
                                                   + w104*pg))))+
                                            (w5/
                                               (1 + exp
                                                (-(int5 + w15*day.of.year_16_t + w25*MNA_t + w35*wt_t + w45*CDD_t 
                                                   + w55*PHDI_t + w65*sex + w75*subadult + w85*adult + w95*lac
                                                   + w105*pg))))+
                                            (w6/
                                               (1 + exp
                                                (-(int6 + w16*day.of.year_16_t + w26*MNA_t + w36*wt_t + w46*CDD_t 
                                                   + w56*PHDI_t + w66*sex + w76*subadult + w86*adult + w96*lac
                                                   + w106*pg))))+
                                            (w7/
                                               (1 + exp
                                                (-(int7 + w17*day.of.year_16_t + w27*MNA_t + w37*wt_t + w47*CDD_t
                                                   + w57*PHDI_t + w67*sex + w77*subadult + w87*adult + w97*lac
                                                   + w107*pg))))+
                                            (w8/
                                               (1 + exp
                                                (-(int8 + w18*day.of.year_16_t + w28*MNA_t + w38*wt_t + w48*CDD_t 
                                                   + w58*PHDI_t + w68*sex + w78*subadult + w88*adult + w98*lac
                                                   + w108*pg))))+
                                            (w9/
                                               (1 + exp
                                                (-(int9 + w19*day.of.year_16_t + w29*MNA_t + w39*wt_t + w49*CDD_t 
                                                   + w59*PHDI_t + w69*sex + w79*subadult + w89*adult + w99*lac
                                                   + w109*pg))))+
                                            (w10/
                                               (1 + exp
                                                (-(int10 + w110*day.of.year_16_t + w210*MNA_t + w310*wt_t + w410*CDD_t 
                                                   + w510*PHDI_t + w610*sex + w710*subadult + w810*adult + w910*lac
                                                   + w1010*pg))))))


graph_data$pred.larvae_17<-(10^(int 
                                          +(w1/
                                              (1 + exp
                                               (-(int1 + w11*day.of.year_17_t + w21*MNA_t + w31*wt_t + w41*CDD_t 
                                                  + w51*PHDI_t + w61*sex + w71*subadult + w81*adult + w91*lac
                                                  + w101*pg))))+
                                            (w2/
                                               (1 + exp
                                                (-(int2 + w12*day.of.year_17_t + w22*MNA_t + w32*wt_t + w42*CDD_t
                                                   + w52*PHDI_t + w62*sex + w72*subadult + w82*adult + w92*lac
                                                   + w102*pg))))+
                                            (w3/
                                               (1 + exp
                                                (-(int3 + w13*day.of.year_17_t + w23*MNA_t + w33*wt_t + w43*CDD_t
                                                   + w53*PHDI_t + w63*sex + w73*subadult + w83*adult + w93*lac
                                                   + w103*pg))))+
                                            (w4/
                                               (1 + exp
                                                (-(int4 + w14*day.of.year_17_t + w24*MNA_t + w34*wt_t + w44*CDD_t 
                                                   + w54*PHDI_t + w64*sex + w74*subadult + w84*adult + w94*lac
                                                   + w104*pg))))+
                                            (w5/
                                               (1 + exp
                                                (-(int5 + w15*day.of.year_17_t + w25*MNA_t + w35*wt_t + w45*CDD_t 
                                                   + w55*PHDI_t + w65*sex + w75*subadult + w85*adult + w95*lac
                                                   + w105*pg))))+
                                            (w6/
                                               (1 + exp
                                                (-(int6 + w16*day.of.year_17_t + w26*MNA_t + w36*wt_t + w46*CDD_t 
                                                   + w56*PHDI_t + w66*sex + w76*subadult + w86*adult + w96*lac
                                                   + w106*pg))))+
                                            (w7/
                                               (1 + exp
                                                (-(int7 + w17*day.of.year_17_t + w27*MNA_t + w37*wt_t + w47*CDD_t
                                                   + w57*PHDI_t + w67*sex + w77*subadult + w87*adult + w97*lac
                                                   + w107*pg))))+
                                            (w8/
                                               (1 + exp
                                                (-(int8 + w18*day.of.year_17_t + w28*MNA_t + w38*wt_t + w48*CDD_t 
                                                   + w58*PHDI_t + w68*sex + w78*subadult + w88*adult + w98*lac
                                                   + w108*pg))))+
                                            (w9/
                                               (1 + exp
                                                (-(int9 + w19*day.of.year_17_t + w29*MNA_t + w39*wt_t + w49*CDD_t 
                                                   + w59*PHDI_t + w69*sex + w79*subadult + w89*adult + w99*lac
                                                   + w109*pg))))+
                                            (w10/
                                               (1 + exp
                                                (-(int10 + w110*day.of.year_17_t + w210*MNA_t + w310*wt_t + w410*CDD_t 
                                                   + w510*PHDI_t + w610*sex + w710*subadult + w810*adult + w910*lac
                                                   + w1010*pg))))))


##average of pred.larvae over s large larval peak
graph_data$pred.larvae<-apply(graph_data[,31:47],1,mean)


##export simulated data

write.csv(graph_data, "simulated_data.csv")


#####HEAT MAPS###### 

##set subset you want to graph 
sub<-subset(graph_data, sex == 1&  adult ==1 & pg == 0 & subadult ==1  & lac == 0 & time == 1  )


ggplot(sub, aes(PHDI, CDD)) + geom_tile(aes(fill = pred.larvae),
    colour = "white") + scale_fill_distiller( palette = "Spectral",
      limits = c(min(10),
        max(90)))+facet_grid(MNA
          ~ wt_t)+xlab("Lg: weight / Sm: PHDI")+ylab("Lg : MNA / Sm: CDD")+ theme_grey(base_size = 20)

#####BINARY VARIABLE EFFECTS#####
##create data sets to graph effects of binary data
## binary effect = effect of changing one feature (i.e. difference in predicted larvae between male adults and sub adults )
##subset full graph data based on sub group

j_m_o<-subset(graph_data,sex ==1 &  adult == 0 & pg == 0 & subadult ==0  & lac == 0 & time == 1 ) ## juvenile, male, off peak 
s_m_o<-subset(graph_data,sex ==1 &  adult == 0 & pg == 0 & subadult ==1  & lac == 0 & time == 1 ) ## subadult, male, off peak
a_m_o<-subset(graph_data,sex ==1 &  adult == 1 & pg == 0 & subadult ==1  & lac == 0 & time == 1 ) ## adult, male, off peak

j_f_o<-subset(graph_data,sex ==0 &  adult == 0 & pg == 0 & subadult ==0  & lac == 0 & time == 1) ## juvenile, female, off peak
s_f_o<-subset(graph_data,sex ==0 &  adult == 0 & pg == 0 & subadult ==1  & lac == 0 & time == 1 ) ## subadult, female, off peak
a_f_o<-subset(graph_data,sex ==0 &  adult == 1 & pg == 0 & subadult ==1  & lac == 0 & time == 1 ) ## adult, female, off peak

s_p_f_o<-subset(graph_data,sex ==0 &  adult == 0 & pg == 1 & subadult ==1  & lac == 0 & time == 1 ) ## subadult, preg, female, off peak
s_l_f_o<-subset(graph_data,sex ==0 &  adult == 0 & pg == 0 & subadult ==1  & lac == 1 & time == 1 ) ## subadult, lac, female, off peak  

a_p_f_o<-subset(graph_data,sex ==0 &  adult == 1 & pg == 1 & subadult ==1  & lac == 0 & time == 1 ) ## adult, preg, female, off peak
a_l_f_o<-subset(graph_data,sex ==0 &  adult == 1 & pg == 0 & subadult ==1  & lac == 1 & time == 1 ) ## adult, lac, female, off peak

j_m_p<-subset(graph_data,sex ==1 &  adult == 0 & pg == 0 & subadult ==0  & lac == 0 & time == 1) ## juvenile, male, peak
s_m_p<-subset(graph_data,sex ==1 &  adult == 0 & pg == 0 & subadult ==1  & lac == 0 & time == 1 ) ## subadult, male, peak
a_m_p<-subset(graph_data,sex ==1 &  adult == 1 & pg == 0 & subadult ==1  & lac == 0 & time == 1 ) ## adult, male, peak


j_f_p<-subset(graph_data,sex ==0 &  adult == 0 & pg == 0 & subadult ==0  & lac == 0 & time == 1) ## juvenile, female, peak
s_f_p<-subset(graph_data,sex ==0 &  adult == 0 & pg == 0 & subadult ==1  & lac == 0 & time == 1 ) ## subadult, female, peak
a_f_p<-subset(graph_data,sex ==0 &  adult == 1 & pg == 0 & subadult ==1  & lac == 0 & time == 1 ) ## adult, female, peak

write.csv(j_f_p, "j_f_p.csv")

## calculate binary effects 
O_J_S_M<-(s_m_o$pred.larvae-j_m_o$pred.larvae) ## off peak, juv->sub, male
O_S_A_M<-(a_m_o$pred.larvae-s_m_o$pred.larvae) ## off peak, sub->adult, male
O_J_A_M<-(a_m_o$pred.larvae-j_m_o$pred.larvae) ## off peak, juv->adult, male

O_J_S_F<-(s_f_o$pred.larvae-j_f_o$pred.larvae) ## off peak, juv->sub, female
O_S_A_F<-(a_f_o$pred.larvae-s_f_o$pred.larvae) ## off peak, sub->adult, female
O_J_A_F<-(a_f_o$pred.larvae-j_f_o$pred.larvae) ## off peak, juv->adult, female 

O_M_F_J<-(j_m_o$pred.larvae-j_f_o$pred.larvae) ## off peak, male_fem, juv
O_M_F_S<-(s_m_o$pred.larvae-s_f_o$pred.larvae) ## off peak, male->fem, sub
O_M_F_A<-(a_m_o$pred.larvae-a_f_o$pred.larvae) ## off peak, male->fem, adult

O_S_PG<-(s_p_f_o$pred.larvae-s_f_o$pred.larvae) ## off peak, sub, pg
O_S_L<-(s_l_f_o$pred.larvae-s_f_o$pred.larvae) ## off peak, sub, lac
O_A_PG<-(a_p_f_o$pred.larvae-a_f_o$pred.larvae) ## off peak, adult, pg
O_A_L<-(a_l_f_o$pred.larvae-a_f_o$pred.larvae) ## off peak, adult, lac

P_J_S_M<-(s_m_p$pred.larvae-j_m_p$pred.larvae) ## peak, juv->sub, male
P_S_A_M<-(a_m_p$pred.larvae-s_m_p$pred.larvae) ## peak, sub->adult, male
P_J_A_M<-(a_m_p$pred.larvae-j_m_p$pred.larvae) ## peak, juv->adult, male

P_J_S_F<-(s_f_p$pred.larvae-j_f_p$pred.larvae) ## peak, juv->sub, fem
P_S_A_F<-(a_f_p$pred.larvae-s_f_p$pred.larvae) ## peak, sub->adult, fem
P_J_A_F<-(a_f_p$pred.larvae-j_f_p$pred.larvae) ## peak, juv->adult, fem

P_M_F_J<-(j_m_p$pred.larvae-j_f_p$pred.larvae) ## peak, male->fem, juv
P_M_F_A<-(a_m_p$pred.larvae-a_f_p$pred.larvae) ## peak, male->fem, adult
P_M_F_S<-(s_m_p$pred.larvae-s_f_p$pred.larvae) ## peak, male_.fem, sub

 binary_effect<-data.frame(#O_J_S_M, O_S_A_M, O_J_A_M, O_J_S_F, O_S_A_F, O_J_A_F, 	
                           #O_M_F_J, O_M_F_S, O_M_F_A,
                           #O_S_PG, O_S_L, O_A_PG, O_A_L, 
                           P_J_S_M, P_S_A_M, P_J_A_M,
                           P_J_S_F, P_S_A_F,P_J_A_F,
                           P_M_F_J, P_M_F_S, P_M_F_A)

 ##create binary effects dataset 
binary_effect_graph<-data.frame(check[1:7],binary_effect)
 

## export binary effects dataset
write.csv(binary_effect_graph, "binary_effects_2.csv")

##plot binary effects, same as original graph with differnce as dependent variable 
ggplot(binary_effect_graph, aes(PHDI, CDD)) + geom_tile(aes(fill = P_M_F_A), ##adjust fill based on effect you want to graph 
              colour = "white") + scale_fill_distiller( palette = "Spectral",
              limits = c(min(-10), max(30)))+facet_grid(MNA_t
              ~ wt_t)+xlab("Lg: weight / Sm: PHDI")+ylab("Lg : MNA 
            / Sm: CDD") + ggtitle("Effect of pregnancy (off peak)")  ##adjust titles based on subset 


##save peak variable effects (average of effect over all conditions)
subadult_effect<-mean(c(mean(P_J_S_M),mean(P_J_S_F))) ## average over male and female
adult_effect<-mean(c(mean(P_S_A_M),mean(P_S_A_F))) ## average over male and female
sex_effect<-mean(c(mean(P_M_F_J), mean(P_M_F_S), mean(P_M_F_A))) ## average over all age classes
lactation_effect<-mean(c(mean(O_A_L), mean(O_S_L))) ## average over subadults and adults
pregnancy_effect<-mean(c(mean(O_A_PG), mean(O_S_PG))) ## average over subadults and adults


#####CONTINUOUS VARIABLE EFFECTS#####

## create datasets to evaluate continuous variable effects

  ##CDD effect (ranging over peak range based on delta.CDD)
day.of.year_1_t<-rep(0.059471366,1650)
day.of.year_2_t<-rep(0.063876652,1650)
day.of.year_3_t<-rep(0.068281938,1650)
day.of.year_4_t<-rep(0.072687225,1650)
day.of.year_5_t<-rep(0.077092511,1650)
day.of.year_6_t<-rep(0.081497797,1650)
day.of.year_7_t<-rep(0.085903084,1650)
day.of.year_8_t<-rep(0.09030837,1650)
day.of.year_9_t<-rep(0.094713656,1650)
day.of.year_10_t<-rep(0.099118943,1650)
day.of.year_11_t<-rep(0.103524229,1650)
day.of.year_12_t<-rep(0.107929515,1650)
day.of.year_13_t<-rep(0.112334802,1650)
day.of.year_14_t<-rep(0.116740088,1650)
day.of.year_15_t<-rep(0.121145374,1650)
day.of.year_16_t<-rep(0.125550661,1650)
day.of.year_17_t<-rep(0.129955947,1650)


MNA_t<-c(rep(-.5,330),rep(-.25,330),rep(0,330), rep(.25,330), rep(.5,330))  ##
wt_t<-rep(c(rep(-.5,66),rep(-.25,66),rep(0,66), rep(.25,66), rep(.5,66)),5)
PHDI_t<-rep(c(rep(-.5,6), rep(-.4,6),rep(-.3,6),rep(-.2,6),rep(-.1,6),rep(0,6),
              rep(.1,6),rep(.2,6),rep(.3,6),rep(.4,6),rep(.5,6)) ,25)
sex<-rep(c(rep(0,3), rep(1,3)) ,275)
subadult<-rep(c(0,1,1) ,550)
adult<-rep(c(0,0,1) ,550)
preg<-rep(0,1650)
lac<-rep(0,1650)

CDD_matrix<-as.matrix(data.frame(day.of.year_1_t,day.of.year_2_t,day.of.year_3_t,day.of.year_4_t,
                                 day.of.year_5_t,day.of.year_6_t,day.of.year_7_t,day.of.year_8_t,
                                 day.of.year_9_t,day.of.year_10_t,day.of.year_11_t,day.of.year_12_t,
                                 day.of.year_13_t,day.of.year_14_t,day.of.year_15_t,day.of.year_16_t,
                                 day.of.year_17_t,MNA_t, wt_t, PHDI_t, sex, subadult, adult, preg, lac))

## create min/max function 
minmax <- function(P,D1,D2,D3,D4,
                   D5,D6,D7,D8,D9,
                   D10,D11,D12,D13,D14,
                   D15,D16,D17,M,W,x,X,S,A,G,L) {
  
                                                pred_1<-(10^(int 
                                              +(w1/
                                                  (1 + exp
                                                   (-(int1 + w11*D1 + w21*M + w31*W + w41*x 
                                                      + w51*P + w61*X + w71*S + w81*A + w91*L
                                                      + w101*G))))+
                                                (w2/
                                                   (1 + exp
                                                    (-(int2 + w12*D1 + w22*M + w32*W + w42*x
                                                       + w52*P + w62*X + w72*S + w82*A + w92*L
                                                       + w102*G))))+
                                                (w3/
                                                   (1 + exp
                                                    (-(int3 + w13*D1 + w23*M + w33*W + w43*x
                                                       + w53*P + w63*X + w73*S + w83*A + w93*L
                                                       + w103*G))))+
                                                (w4/
                                                   (1 + exp
                                                    (-(int4 + w14*D1 + w24*M + w34*W + w44*x 
                                                       + w54*P + w64*X + w74*S + w84*A + w94*L
                                                       + w104*G))))+
                                                (w5/
                                                   (1 + exp
                                                    (-(int5 + w15*D1 + w25*M + w35*W + w45*x 
                                                       + w55*P + w65*X + w75*S + w85*A + w95*L
                                                       + w105*G))))+
                                                (w6/
                                                   (1 + exp
                                                    (-(int6 + w16*D1 + w26*M + w36*W + w46*x 
                                                       + w56*P + w66*X + w76*S + w86*A + w96*L
                                                       + w106*G))))+
                                                (w7/
                                                   (1 + exp
                                                    (-(int7 + w17*D1 + w27*M + w37*W + w47*x
                                                       + w57*P + w67*X + w77*S + w87*A + w97*L
                                                       + w107*G))))+
                                                (w8/
                                                   (1 + exp
                                                    (-(int8 + w18*D1 + w28*M + w38*W + w48*x 
                                                       + w58*P + w68*X + w78*S + w88*A + w98*L
                                                       + w108*G))))+
                                                (w9/
                                                   (1 + exp
                                                    (-(int9 + w19*D1 + w29*M + w39*W + w49*x 
                                                       + w59*P + w69*X + w79*S + w89*A + w99*L
                                                       + w109*G))))+
                                                (w10/
                                                   (1 + exp
                                                    (-(int10 + w110*D1 + w210*M + w310*W + w410*x 
                                                       + w510*P + w610*X + w710*S + w810*A + w910*L
                                                       + w1010*G))))))
                                                
                                                pred_2<-(10^(int 
                                                             +(w1/
                                                                 (1 + exp
                                                                  (-(int1 + w11*D2 + w21*M + w31*W + w41*x 
                                                                     + w51*P + w61*X + w71*S + w81*A + w91*L
                                                                     + w101*G))))+
                                                               (w2/
                                                                  (1 + exp
                                                                   (-(int2 + w12*D2 + w22*M + w32*W + w42*x
                                                                      + w52*P + w62*X + w72*S + w82*A + w92*L
                                                                      + w102*G))))+
                                                               (w3/
                                                                  (1 + exp
                                                                   (-(int3 + w13*D2 + w23*M + w33*W + w43*x
                                                                      + w53*P + w63*X + w73*S + w83*A + w93*L
                                                                      + w103*G))))+
                                                               (w4/
                                                                  (1 + exp
                                                                   (-(int4 + w14*D2 + w24*M + w34*W + w44*x 
                                                                      + w54*P + w64*X + w74*S + w84*A + w94*L
                                                                      + w104*G))))+
                                                               (w5/
                                                                  (1 + exp
                                                                   (-(int5 + w15*D2 + w25*M + w35*W + w45*x 
                                                                      + w55*P + w65*X + w75*S + w85*A + w95*L
                                                                      + w105*G))))+
                                                               (w6/
                                                                  (1 + exp
                                                                   (-(int6 + w16*D2 + w26*M + w36*W + w46*x 
                                                                      + w56*P + w66*X + w76*S + w86*A + w96*L
                                                                      + w106*G))))+
                                                               (w7/
                                                                  (1 + exp
                                                                   (-(int7 + w17*D2 + w27*M + w37*W + w47*x
                                                                      + w57*P + w67*X + w77*S + w87*A + w97*L
                                                                      + w107*G))))+
                                                               (w8/
                                                                  (1 + exp
                                                                   (-(int8 + w18*D2 + w28*M + w38*W + w48*x 
                                                                      + w58*P + w68*X + w78*S + w88*A + w98*L
                                                                      + w108*G))))+
                                                               (w9/
                                                                  (1 + exp
                                                                   (-(int9 + w19*D2 + w29*M + w39*W + w49*x 
                                                                      + w59*P + w69*X + w79*S + w89*A + w99*L
                                                                      + w109*G))))+
                                                               (w10/
                                                                  (1 + exp
                                                                   (-(int10 + w110*D2 + w210*M + w310*W + w410*x 
                                                                      + w510*P + w610*X + w710*S + w810*A + w910*L
                                                                      + w1010*G))))))
                                                
                                                pred_3<-(10^(int 
                                                             +(w1/
                                                                 (1 + exp
                                                                  (-(int1 + w11*D3 + w21*M + w31*W + w41*x 
                                                                     + w51*P + w61*X + w71*S + w81*A + w91*L
                                                                     + w101*G))))+
                                                               (w2/
                                                                  (1 + exp
                                                                   (-(int2 + w12*D3 + w22*M + w32*W + w42*x
                                                                      + w52*P + w62*X + w72*S + w82*A + w92*L
                                                                      + w102*G))))+
                                                               (w3/
                                                                  (1 + exp
                                                                   (-(int3 + w13*D3 + w23*M + w33*W + w43*x
                                                                      + w53*P + w63*X + w73*S + w83*A + w93*L
                                                                      + w103*G))))+
                                                               (w4/
                                                                  (1 + exp
                                                                   (-(int4 + w14*D3 + w24*M + w34*W + w44*x 
                                                                      + w54*P + w64*X + w74*S + w84*A + w94*L
                                                                      + w104*G))))+
                                                               (w5/
                                                                  (1 + exp
                                                                   (-(int5 + w15*D3 + w25*M + w35*W + w45*x 
                                                                      + w55*P + w65*X + w75*S + w85*A + w95*L
                                                                      + w105*G))))+
                                                               (w6/
                                                                  (1 + exp
                                                                   (-(int6 + w16*D3 + w26*M + w36*W + w46*x 
                                                                      + w56*P + w66*X + w76*S + w86*A + w96*L
                                                                      + w106*G))))+
                                                               (w7/
                                                                  (1 + exp
                                                                   (-(int7 + w17*D3 + w27*M + w37*W + w47*x
                                                                      + w57*P + w67*X + w77*S + w87*A + w97*L
                                                                      + w107*G))))+
                                                               (w8/
                                                                  (1 + exp
                                                                   (-(int8 + w18*D3 + w28*M + w38*W + w48*x 
                                                                      + w58*P + w68*X + w78*S + w88*A + w98*L
                                                                      + w108*G))))+
                                                               (w9/
                                                                  (1 + exp
                                                                   (-(int9 + w19*D3 + w29*M + w39*W + w49*x 
                                                                      + w59*P + w69*X + w79*S + w89*A + w99*L
                                                                      + w109*G))))+
                                                               (w10/
                                                                  (1 + exp
                                                                   (-(int10 + w110*D3 + w210*M + w310*W + w410*x 
                                                                      + w510*P + w610*X + w710*S + w810*A + w910*L
                                                                      + w1010*G))))))
                                                
                                                pred_4<-(10^(int 
                                                             +(w1/
                                                                 (1 + exp
                                                                  (-(int1 + w11*D4 + w21*M + w31*W + w41*x 
                                                                     + w51*P + w61*X + w71*S + w81*A + w91*L
                                                                     + w101*G))))+
                                                               (w2/
                                                                  (1 + exp
                                                                   (-(int2 + w12*D4 + w22*M + w32*W + w42*x
                                                                      + w52*P + w62*X + w72*S + w82*A + w92*L
                                                                      + w102*G))))+
                                                               (w3/
                                                                  (1 + exp
                                                                   (-(int3 + w13*D4 + w23*M + w33*W + w43*x
                                                                      + w53*P + w63*X + w73*S + w83*A + w93*L
                                                                      + w103*G))))+
                                                               (w4/
                                                                  (1 + exp
                                                                   (-(int4 + w14*D4 + w24*M + w34*W + w44*x 
                                                                      + w54*P + w64*X + w74*S + w84*A + w94*L
                                                                      + w104*G))))+
                                                               (w5/
                                                                  (1 + exp
                                                                   (-(int5 + w15*D4 + w25*M + w35*W + w45*x 
                                                                      + w55*P + w65*X + w75*S + w85*A + w95*L
                                                                      + w105*G))))+
                                                               (w6/
                                                                  (1 + exp
                                                                   (-(int6 + w16*D4 + w26*M + w36*W + w46*x 
                                                                      + w56*P + w66*X + w76*S + w86*A + w96*L
                                                                      + w106*G))))+
                                                               (w7/
                                                                  (1 + exp
                                                                   (-(int7 + w17*D4 + w27*M + w37*W + w47*x
                                                                      + w57*P + w67*X + w77*S + w87*A + w97*L
                                                                      + w107*G))))+
                                                               (w8/
                                                                  (1 + exp
                                                                   (-(int8 + w18*D4 + w28*M + w38*W + w48*x 
                                                                      + w58*P + w68*X + w78*S + w88*A + w98*L
                                                                      + w108*G))))+
                                                               (w9/
                                                                  (1 + exp
                                                                   (-(int9 + w19*D4 + w29*M + w39*W + w49*x 
                                                                      + w59*P + w69*X + w79*S + w89*A + w99*L
                                                                      + w109*G))))+
                                                               (w10/
                                                                  (1 + exp
                                                                   (-(int10 + w110*D4 + w210*M + w310*W + w410*x 
                                                                      + w510*P + w610*X + w710*S + w810*A + w910*L
                                                                      + w1010*G))))))
                                                
                                                pred_5<-(10^(int 
                                                             +(w1/
                                                                 (1 + exp
                                                                  (-(int1 + w11*D5 + w21*M + w31*W + w41*x 
                                                                     + w51*P + w61*X + w71*S + w81*A + w91*L
                                                                     + w101*G))))+
                                                               (w2/
                                                                  (1 + exp
                                                                   (-(int2 + w12*D5 + w22*M + w32*W + w42*x
                                                                      + w52*P + w62*X + w72*S + w82*A + w92*L
                                                                      + w102*G))))+
                                                               (w3/
                                                                  (1 + exp
                                                                   (-(int3 + w13*D5 + w23*M + w33*W + w43*x
                                                                      + w53*P + w63*X + w73*S + w83*A + w93*L
                                                                      + w103*G))))+
                                                               (w4/
                                                                  (1 + exp
                                                                   (-(int4 + w14*D5 + w24*M + w34*W + w44*x 
                                                                      + w54*P + w64*X + w74*S + w84*A + w94*L
                                                                      + w104*G))))+
                                                               (w5/
                                                                  (1 + exp
                                                                   (-(int5 + w15*D5 + w25*M + w35*W + w45*x 
                                                                      + w55*P + w65*X + w75*S + w85*A + w95*L
                                                                      + w105*G))))+
                                                               (w6/
                                                                  (1 + exp
                                                                   (-(int6 + w16*D5 + w26*M + w36*W + w46*x 
                                                                      + w56*P + w66*X + w76*S + w86*A + w96*L
                                                                      + w106*G))))+
                                                               (w7/
                                                                  (1 + exp
                                                                   (-(int7 + w17*D5 + w27*M + w37*W + w47*x
                                                                      + w57*P + w67*X + w77*S + w87*A + w97*L
                                                                      + w107*G))))+
                                                               (w8/
                                                                  (1 + exp
                                                                   (-(int8 + w18*D5 + w28*M + w38*W + w48*x 
                                                                      + w58*P + w68*X + w78*S + w88*A + w98*L
                                                                      + w108*G))))+
                                                               (w9/
                                                                  (1 + exp
                                                                   (-(int9 + w19*D5 + w29*M + w39*W + w49*x 
                                                                      + w59*P + w69*X + w79*S + w89*A + w99*L
                                                                      + w109*G))))+
                                                               (w10/
                                                                  (1 + exp
                                                                   (-(int10 + w110*D5 + w210*M + w310*W + w410*x 
                                                                      + w510*P + w610*X + w710*S + w810*A + w910*L
                                                                      + w1010*G))))))
                                                
                                                pred_6<-(10^(int 
                                                             +(w1/
                                                                 (1 + exp
                                                                  (-(int1 + w11*D6 + w21*M + w31*W + w41*x 
                                                                     + w51*P + w61*X + w71*S + w81*A + w91*L
                                                                     + w101*G))))+
                                                               (w2/
                                                                  (1 + exp
                                                                   (-(int2 + w12*D6 + w22*M + w32*W + w42*x
                                                                      + w52*P + w62*X + w72*S + w82*A + w92*L
                                                                      + w102*G))))+
                                                               (w3/
                                                                  (1 + exp
                                                                   (-(int3 + w13*D6 + w23*M + w33*W + w43*x
                                                                      + w53*P + w63*X + w73*S + w83*A + w93*L
                                                                      + w103*G))))+
                                                               (w4/
                                                                  (1 + exp
                                                                   (-(int4 + w14*D6 + w24*M + w34*W + w44*x 
                                                                      + w54*P + w64*X + w74*S + w84*A + w94*L
                                                                      + w104*G))))+
                                                               (w5/
                                                                  (1 + exp
                                                                   (-(int5 + w15*D6 + w25*M + w35*W + w45*x 
                                                                      + w55*P + w65*X + w75*S + w85*A + w95*L
                                                                      + w105*G))))+
                                                               (w6/
                                                                  (1 + exp
                                                                   (-(int6 + w16*D6 + w26*M + w36*W + w46*x 
                                                                      + w56*P + w66*X + w76*S + w86*A + w96*L
                                                                      + w106*G))))+
                                                               (w7/
                                                                  (1 + exp
                                                                   (-(int7 + w17*D6 + w27*M + w37*W + w47*x
                                                                      + w57*P + w67*X + w77*S + w87*A + w97*L
                                                                      + w107*G))))+
                                                               (w8/
                                                                  (1 + exp
                                                                   (-(int8 + w18*D6 + w28*M + w38*W + w48*x 
                                                                      + w58*P + w68*X + w78*S + w88*A + w98*L
                                                                      + w108*G))))+
                                                               (w9/
                                                                  (1 + exp
                                                                   (-(int9 + w19*D6 + w29*M + w39*W + w49*x 
                                                                      + w59*P + w69*X + w79*S + w89*A + w99*L
                                                                      + w109*G))))+
                                                               (w10/
                                                                  (1 + exp
                                                                   (-(int10 + w110*D6 + w210*M + w310*W + w410*x 
                                                                      + w510*P + w610*X + w710*S + w810*A + w910*L
                                                                      + w1010*G))))))
                                                
                                                pred_7<-(10^(int 
                                                             +(w1/
                                                                 (1 + exp
                                                                  (-(int1 + w11*D7 + w21*M + w31*W + w41*x 
                                                                     + w51*P + w61*X + w71*S + w81*A + w91*L
                                                                     + w101*G))))+
                                                               (w2/
                                                                  (1 + exp
                                                                   (-(int2 + w12*D7 + w22*M + w32*W + w42*x
                                                                      + w52*P + w62*X + w72*S + w82*A + w92*L
                                                                      + w102*G))))+
                                                               (w3/
                                                                  (1 + exp
                                                                   (-(int3 + w13*D7 + w23*M + w33*W + w43*x
                                                                      + w53*P + w63*X + w73*S + w83*A + w93*L
                                                                      + w103*G))))+
                                                               (w4/
                                                                  (1 + exp
                                                                   (-(int4 + w14*D7 + w24*M + w34*W + w44*x 
                                                                      + w54*P + w64*X + w74*S + w84*A + w94*L
                                                                      + w104*G))))+
                                                               (w5/
                                                                  (1 + exp
                                                                   (-(int5 + w15*D7 + w25*M + w35*W + w45*x 
                                                                      + w55*P + w65*X + w75*S + w85*A + w95*L
                                                                      + w105*G))))+
                                                               (w6/
                                                                  (1 + exp
                                                                   (-(int6 + w16*D7 + w26*M + w36*W + w46*x 
                                                                      + w56*P + w66*X + w76*S + w86*A + w96*L
                                                                      + w106*G))))+
                                                               (w7/
                                                                  (1 + exp
                                                                   (-(int7 + w17*D7 + w27*M + w37*W + w47*x
                                                                      + w57*P + w67*X + w77*S + w87*A + w97*L
                                                                      + w107*G))))+
                                                               (w8/
                                                                  (1 + exp
                                                                   (-(int8 + w18*D7 + w28*M + w38*W + w48*x 
                                                                      + w58*P + w68*X + w78*S + w88*A + w98*L
                                                                      + w108*G))))+
                                                               (w9/
                                                                  (1 + exp
                                                                   (-(int9 + w19*D7 + w29*M + w39*W + w49*x 
                                                                      + w59*P + w69*X + w79*S + w89*A + w99*L
                                                                      + w109*G))))+
                                                               (w10/
                                                                  (1 + exp
                                                                   (-(int10 + w110*D7 + w210*M + w310*W + w410*x 
                                                                      + w510*P + w610*X + w710*S + w810*A + w910*L
                                                                      + w1010*G))))))
                                                
                                                pred_8<-(10^(int 
                                                             +(w1/
                                                                 (1 + exp
                                                                  (-(int1 + w11*D8 + w21*M + w31*W + w41*x 
                                                                     + w51*P + w61*X + w71*S + w81*A + w91*L
                                                                     + w101*G))))+
                                                               (w2/
                                                                  (1 + exp
                                                                   (-(int2 + w12*D8 + w22*M + w32*W + w42*x
                                                                      + w52*P + w62*X + w72*S + w82*A + w92*L
                                                                      + w102*G))))+
                                                               (w3/
                                                                  (1 + exp
                                                                   (-(int3 + w13*D8 + w23*M + w33*W + w43*x
                                                                      + w53*P + w63*X + w73*S + w83*A + w93*L
                                                                      + w103*G))))+
                                                               (w4/
                                                                  (1 + exp
                                                                   (-(int4 + w14*D8 + w24*M + w34*W + w44*x 
                                                                      + w54*P + w64*X + w74*S + w84*A + w94*L
                                                                      + w104*G))))+
                                                               (w5/
                                                                  (1 + exp
                                                                   (-(int5 + w15*D8 + w25*M + w35*W + w45*x 
                                                                      + w55*P + w65*X + w75*S + w85*A + w95*L
                                                                      + w105*G))))+
                                                               (w6/
                                                                  (1 + exp
                                                                   (-(int6 + w16*D8 + w26*M + w36*W + w46*x 
                                                                      + w56*P + w66*X + w76*S + w86*A + w96*L
                                                                      + w106*G))))+
                                                               (w7/
                                                                  (1 + exp
                                                                   (-(int7 + w17*D8 + w27*M + w37*W + w47*x
                                                                      + w57*P + w67*X + w77*S + w87*A + w97*L
                                                                      + w107*G))))+
                                                               (w8/
                                                                  (1 + exp
                                                                   (-(int8 + w18*D8 + w28*M + w38*W + w48*x 
                                                                      + w58*P + w68*X + w78*S + w88*A + w98*L
                                                                      + w108*G))))+
                                                               (w9/
                                                                  (1 + exp
                                                                   (-(int9 + w19*D8 + w29*M + w39*W + w49*x 
                                                                      + w59*P + w69*X + w79*S + w89*A + w99*L
                                                                      + w109*G))))+
                                                               (w10/
                                                                  (1 + exp
                                                                   (-(int10 + w110*D8 + w210*M + w310*W + w410*x 
                                                                      + w510*P + w610*X + w710*S + w810*A + w910*L
                                                                      + w1010*G))))))
                                                
                                                pred_9<-(10^(int 
                                                             +(w1/
                                                                 (1 + exp
                                                                  (-(int1 + w11*D9 + w21*M + w31*W + w41*x 
                                                                     + w51*P + w61*X + w71*S + w81*A + w91*L
                                                                     + w101*G))))+
                                                               (w2/
                                                                  (1 + exp
                                                                   (-(int2 + w12*D9 + w22*M + w32*W + w42*x
                                                                      + w52*P + w62*X + w72*S + w82*A + w92*L
                                                                      + w102*G))))+
                                                               (w3/
                                                                  (1 + exp
                                                                   (-(int3 + w13*D9 + w23*M + w33*W + w43*x
                                                                      + w53*P + w63*X + w73*S + w83*A + w93*L
                                                                      + w103*G))))+
                                                               (w4/
                                                                  (1 + exp
                                                                   (-(int4 + w14*D9 + w24*M + w34*W + w44*x 
                                                                      + w54*P + w64*X + w74*S + w84*A + w94*L
                                                                      + w104*G))))+
                                                               (w5/
                                                                  (1 + exp
                                                                   (-(int5 + w15*D9 + w25*M + w35*W + w45*x 
                                                                      + w55*P + w65*X + w75*S + w85*A + w95*L
                                                                      + w105*G))))+
                                                               (w6/
                                                                  (1 + exp
                                                                   (-(int6 + w16*D9 + w26*M + w36*W + w46*x 
                                                                      + w56*P + w66*X + w76*S + w86*A + w96*L
                                                                      + w106*G))))+
                                                               (w7/
                                                                  (1 + exp
                                                                   (-(int7 + w17*D9 + w27*M + w37*W + w47*x
                                                                      + w57*P + w67*X + w77*S + w87*A + w97*L
                                                                      + w107*G))))+
                                                               (w8/
                                                                  (1 + exp
                                                                   (-(int8 + w18*D9 + w28*M + w38*W + w48*x 
                                                                      + w58*P + w68*X + w78*S + w88*A + w98*L
                                                                      + w108*G))))+
                                                               (w9/
                                                                  (1 + exp
                                                                   (-(int9 + w19*D9 + w29*M + w39*W + w49*x 
                                                                      + w59*P + w69*X + w79*S + w89*A + w99*L
                                                                      + w109*G))))+
                                                               (w10/
                                                                  (1 + exp
                                                                   (-(int10 + w110*D9 + w210*M + w310*W + w410*x 
                                                                      + w510*P + w610*X + w710*S + w810*A + w910*L
                                                                      + w1010*G))))))
                                                
                                                pred_10<-(10^(int 
                                                             +(w1/
                                                                 (1 + exp
                                                                  (-(int1 + w11*D10 + w21*M + w31*W + w41*x 
                                                                     + w51*P + w61*X + w71*S + w81*A + w91*L
                                                                     + w101*G))))+
                                                               (w2/
                                                                  (1 + exp
                                                                   (-(int2 + w12*D10 + w22*M + w32*W + w42*x
                                                                      + w52*P + w62*X + w72*S + w82*A + w92*L
                                                                      + w102*G))))+
                                                               (w3/
                                                                  (1 + exp
                                                                   (-(int3 + w13*D10 + w23*M + w33*W + w43*x
                                                                      + w53*P + w63*X + w73*S + w83*A + w93*L
                                                                      + w103*G))))+
                                                               (w4/
                                                                  (1 + exp
                                                                   (-(int4 + w14*D10 + w24*M + w34*W + w44*x 
                                                                      + w54*P + w64*X + w74*S + w84*A + w94*L
                                                                      + w104*G))))+
                                                               (w5/
                                                                  (1 + exp
                                                                   (-(int5 + w15*D10 + w25*M + w35*W + w45*x 
                                                                      + w55*P + w65*X + w75*S + w85*A + w95*L
                                                                      + w105*G))))+
                                                               (w6/
                                                                  (1 + exp
                                                                   (-(int6 + w16*D10 + w26*M + w36*W + w46*x 
                                                                      + w56*P + w66*X + w76*S + w86*A + w96*L
                                                                      + w106*G))))+
                                                               (w7/
                                                                  (1 + exp
                                                                   (-(int7 + w17*D10 + w27*M + w37*W + w47*x
                                                                      + w57*P + w67*X + w77*S + w87*A + w97*L
                                                                      + w107*G))))+
                                                               (w8/
                                                                  (1 + exp
                                                                   (-(int8 + w18*D10 + w28*M + w38*W + w48*x 
                                                                      + w58*P + w68*X + w78*S + w88*A + w98*L
                                                                      + w108*G))))+
                                                               (w9/
                                                                  (1 + exp
                                                                   (-(int9 + w19*D10 + w29*M + w39*W + w49*x 
                                                                      + w59*P + w69*X + w79*S + w89*A + w99*L
                                                                      + w109*G))))+
                                                               (w10/
                                                                  (1 + exp
                                                                   (-(int10 + w110*D10 + w210*M + w310*W + w410*x 
                                                                      + w510*P + w610*X + w710*S + w810*A + w910*L
                                                                      + w1010*G))))))
                                                
                                                pred_11<-(10^(int 
                                                             +(w1/
                                                                 (1 + exp
                                                                  (-(int1 + w11*D11 + w21*M + w31*W + w41*x 
                                                                     + w51*P + w61*X + w71*S + w81*A + w91*L
                                                                     + w101*G))))+
                                                               (w2/
                                                                  (1 + exp
                                                                   (-(int2 + w12*D11 + w22*M + w32*W + w42*x
                                                                      + w52*P + w62*X + w72*S + w82*A + w92*L
                                                                      + w102*G))))+
                                                               (w3/
                                                                  (1 + exp
                                                                   (-(int3 + w13*D11 + w23*M + w33*W + w43*x
                                                                      + w53*P + w63*X + w73*S + w83*A + w93*L
                                                                      + w103*G))))+
                                                               (w4/
                                                                  (1 + exp
                                                                   (-(int4 + w14*D11 + w24*M + w34*W + w44*x 
                                                                      + w54*P + w64*X + w74*S + w84*A + w94*L
                                                                      + w104*G))))+
                                                               (w5/
                                                                  (1 + exp
                                                                   (-(int5 + w15*D11 + w25*M + w35*W + w45*x 
                                                                      + w55*P + w65*X + w75*S + w85*A + w95*L
                                                                      + w105*G))))+
                                                               (w6/
                                                                  (1 + exp
                                                                   (-(int6 + w16*D11 + w26*M + w36*W + w46*x 
                                                                      + w56*P + w66*X + w76*S + w86*A + w96*L
                                                                      + w106*G))))+
                                                               (w7/
                                                                  (1 + exp
                                                                   (-(int7 + w17*D11 + w27*M + w37*W + w47*x
                                                                      + w57*P + w67*X + w77*S + w87*A + w97*L
                                                                      + w107*G))))+
                                                               (w8/
                                                                  (1 + exp
                                                                   (-(int8 + w18*D11 + w28*M + w38*W + w48*x 
                                                                      + w58*P + w68*X + w78*S + w88*A + w98*L
                                                                      + w108*G))))+
                                                               (w9/
                                                                  (1 + exp
                                                                   (-(int9 + w19*D11 + w29*M + w39*W + w49*x 
                                                                      + w59*P + w69*X + w79*S + w89*A + w99*L
                                                                      + w109*G))))+
                                                               (w10/
                                                                  (1 + exp
                                                                   (-(int10 + w110*D11 + w210*M + w310*W + w410*x 
                                                                      + w510*P + w610*X + w710*S + w810*A + w910*L
                                                                      + w1010*G))))))
                                                
                                                pred_12<-(10^(int 
                                                             +(w1/
                                                                 (1 + exp
                                                                  (-(int1 + w11*D12 + w21*M + w31*W + w41*x 
                                                                     + w51*P + w61*X + w71*S + w81*A + w91*L
                                                                     + w101*G))))+
                                                               (w2/
                                                                  (1 + exp
                                                                   (-(int2 + w12*D12 + w22*M + w32*W + w42*x
                                                                      + w52*P + w62*X + w72*S + w82*A + w92*L
                                                                      + w102*G))))+
                                                               (w3/
                                                                  (1 + exp
                                                                   (-(int3 + w13*D12 + w23*M + w33*W + w43*x
                                                                      + w53*P + w63*X + w73*S + w83*A + w93*L
                                                                      + w103*G))))+
                                                               (w4/
                                                                  (1 + exp
                                                                   (-(int4 + w14*D12 + w24*M + w34*W + w44*x 
                                                                      + w54*P + w64*X + w74*S + w84*A + w94*L
                                                                      + w104*G))))+
                                                               (w5/
                                                                  (1 + exp
                                                                   (-(int5 + w15*D12 + w25*M + w35*W + w45*x 
                                                                      + w55*P + w65*X + w75*S + w85*A + w95*L
                                                                      + w105*G))))+
                                                               (w6/
                                                                  (1 + exp
                                                                   (-(int6 + w16*D12 + w26*M + w36*W + w46*x 
                                                                      + w56*P + w66*X + w76*S + w86*A + w96*L
                                                                      + w106*G))))+
                                                               (w7/
                                                                  (1 + exp
                                                                   (-(int7 + w17*D12 + w27*M + w37*W + w47*x
                                                                      + w57*P + w67*X + w77*S + w87*A + w97*L
                                                                      + w107*G))))+
                                                               (w8/
                                                                  (1 + exp
                                                                   (-(int8 + w18*D12 + w28*M + w38*W + w48*x 
                                                                      + w58*P + w68*X + w78*S + w88*A + w98*L
                                                                      + w108*G))))+
                                                               (w9/
                                                                  (1 + exp
                                                                   (-(int9 + w19*D12 + w29*M + w39*W + w49*x 
                                                                      + w59*P + w69*X + w79*S + w89*A + w99*L
                                                                      + w109*G))))+
                                                               (w10/
                                                                  (1 + exp
                                                                   (-(int10 + w110*D12 + w210*M + w310*W + w410*x 
                                                                      + w510*P + w610*X + w710*S + w810*A + w910*L
                                                                      + w1010*G))))))
                                                
                                                pred_13<-(10^(int 
                                                             +(w1/
                                                                 (1 + exp
                                                                  (-(int1 + w11*D13 + w21*M + w31*W + w41*x 
                                                                     + w51*P + w61*X + w71*S + w81*A + w91*L
                                                                     + w101*G))))+
                                                               (w2/
                                                                  (1 + exp
                                                                   (-(int2 + w12*D13 + w22*M + w32*W + w42*x
                                                                      + w52*P + w62*X + w72*S + w82*A + w92*L
                                                                      + w102*G))))+
                                                               (w3/
                                                                  (1 + exp
                                                                   (-(int3 + w13*D13 + w23*M + w33*W + w43*x
                                                                      + w53*P + w63*X + w73*S + w83*A + w93*L
                                                                      + w103*G))))+
                                                               (w4/
                                                                  (1 + exp
                                                                   (-(int4 + w14*D13 + w24*M + w34*W + w44*x 
                                                                      + w54*P + w64*X + w74*S + w84*A + w94*L
                                                                      + w104*G))))+
                                                               (w5/
                                                                  (1 + exp
                                                                   (-(int5 + w15*D13 + w25*M + w35*W + w45*x 
                                                                      + w55*P + w65*X + w75*S + w85*A + w95*L
                                                                      + w105*G))))+
                                                               (w6/
                                                                  (1 + exp
                                                                   (-(int6 + w16*D13 + w26*M + w36*W + w46*x 
                                                                      + w56*P + w66*X + w76*S + w86*A + w96*L
                                                                      + w106*G))))+
                                                               (w7/
                                                                  (1 + exp
                                                                   (-(int7 + w17*D13 + w27*M + w37*W + w47*x
                                                                      + w57*P + w67*X + w77*S + w87*A + w97*L
                                                                      + w107*G))))+
                                                               (w8/
                                                                  (1 + exp
                                                                   (-(int8 + w18*D13 + w28*M + w38*W + w48*x 
                                                                      + w58*P + w68*X + w78*S + w88*A + w98*L
                                                                      + w108*G))))+
                                                               (w9/
                                                                  (1 + exp
                                                                   (-(int9 + w19*D13 + w29*M + w39*W + w49*x 
                                                                      + w59*P + w69*X + w79*S + w89*A + w99*L
                                                                      + w109*G))))+
                                                               (w10/
                                                                  (1 + exp
                                                                   (-(int10 + w110*D13 + w210*M + w310*W + w410*x 
                                                                      + w510*P + w610*X + w710*S + w810*A + w910*L
                                                                      + w1010*G))))))
                                                
                                        
                                                
                                                pred_14<-(10^(int 
                                                             +(w1/
                                                                 (1 + exp
                                                                  (-(int1 + w11*D14 + w21*M + w31*W + w41*x 
                                                                     + w51*P + w61*X + w71*S + w81*A + w91*L
                                                                     + w101*G))))+
                                                               (w2/
                                                                  (1 + exp
                                                                   (-(int2 + w12*D14 + w22*M + w32*W + w42*x
                                                                      + w52*P + w62*X + w72*S + w82*A + w92*L
                                                                      + w102*G))))+
                                                               (w3/
                                                                  (1 + exp
                                                                   (-(int3 + w13*D14 + w23*M + w33*W + w43*x
                                                                      + w53*P + w63*X + w73*S + w83*A + w93*L
                                                                      + w103*G))))+
                                                               (w4/
                                                                  (1 + exp
                                                                   (-(int4 + w14*D14 + w24*M + w34*W + w44*x 
                                                                      + w54*P + w64*X + w74*S + w84*A + w94*L
                                                                      + w104*G))))+
                                                               (w5/
                                                                  (1 + exp
                                                                   (-(int5 + w15*D14 + w25*M + w35*W + w45*x 
                                                                      + w55*P + w65*X + w75*S + w85*A + w95*L
                                                                      + w105*G))))+
                                                               (w6/
                                                                  (1 + exp
                                                                   (-(int6 + w16*D14 + w26*M + w36*W + w46*x 
                                                                      + w56*P + w66*X + w76*S + w86*A + w96*L
                                                                      + w106*G))))+
                                                               (w7/
                                                                  (1 + exp
                                                                   (-(int7 + w17*D14 + w27*M + w37*W + w47*x
                                                                      + w57*P + w67*X + w77*S + w87*A + w97*L
                                                                      + w107*G))))+
                                                               (w8/
                                                                  (1 + exp
                                                                   (-(int8 + w18*D14 + w28*M + w38*W + w48*x 
                                                                      + w58*P + w68*X + w78*S + w88*A + w98*L
                                                                      + w108*G))))+
                                                               (w9/
                                                                  (1 + exp
                                                                   (-(int9 + w19*D14 + w29*M + w39*W + w49*x 
                                                                      + w59*P + w69*X + w79*S + w89*A + w99*L
                                                                      + w109*G))))+
                                                               (w10/
                                                                  (1 + exp
                                                                   (-(int10 + w110*D14 + w210*M + w310*W + w410*x 
                                                                      + w510*P + w610*X + w710*S + w810*A + w910*L
                                                                      + w1010*G))))))
                                                
                                                pred_15<-(10^(int 
                                                             +(w1/
                                                                 (1 + exp
                                                                  (-(int1 + w11*D15 + w21*M + w31*W + w41*x 
                                                                     + w51*P + w61*X + w71*S + w81*A + w91*L
                                                                     + w101*G))))+
                                                               (w2/
                                                                  (1 + exp
                                                                   (-(int2 + w12*D15 + w22*M + w32*W + w42*x
                                                                      + w52*P + w62*X + w72*S + w82*A + w92*L
                                                                      + w102*G))))+
                                                               (w3/
                                                                  (1 + exp
                                                                   (-(int3 + w13*D15 + w23*M + w33*W + w43*x
                                                                      + w53*P + w63*X + w73*S + w83*A + w93*L
                                                                      + w103*G))))+
                                                               (w4/
                                                                  (1 + exp
                                                                   (-(int4 + w14*D15 + w24*M + w34*W + w44*x 
                                                                      + w54*P + w64*X + w74*S + w84*A + w94*L
                                                                      + w104*G))))+
                                                               (w5/
                                                                  (1 + exp
                                                                   (-(int5 + w15*D15 + w25*M + w35*W + w45*x 
                                                                      + w55*P + w65*X + w75*S + w85*A + w95*L
                                                                      + w105*G))))+
                                                               (w6/
                                                                  (1 + exp
                                                                   (-(int6 + w16*D15 + w26*M + w36*W + w46*x 
                                                                      + w56*P + w66*X + w76*S + w86*A + w96*L
                                                                      + w106*G))))+
                                                               (w7/
                                                                  (1 + exp
                                                                   (-(int7 + w17*D15 + w27*M + w37*W + w47*x
                                                                      + w57*P + w67*X + w77*S + w87*A + w97*L
                                                                      + w107*G))))+
                                                               (w8/
                                                                  (1 + exp
                                                                   (-(int8 + w18*D15 + w28*M + w38*W + w48*x 
                                                                      + w58*P + w68*X + w78*S + w88*A + w98*L
                                                                      + w108*G))))+
                                                               (w9/
                                                                  (1 + exp
                                                                   (-(int9 + w19*D15 + w29*M + w39*W + w49*x 
                                                                      + w59*P + w69*X + w79*S + w89*A + w99*L
                                                                      + w109*G))))+
                                                               (w10/
                                                                  (1 + exp
                                                                   (-(int10 + w110*D15 + w210*M + w310*W + w410*x 
                                                                      + w510*P + w610*X + w710*S + w810*A + w910*L
                                                                      + w1010*G))))))
                                                
                                                pred_16<-(10^(int 
                                                             +(w1/
                                                                 (1 + exp
                                                                  (-(int1 + w11*D16 + w21*M + w31*W + w41*x 
                                                                     + w51*P + w61*X + w71*S + w81*A + w91*L
                                                                     + w101*G))))+
                                                               (w2/
                                                                  (1 + exp
                                                                   (-(int2 + w12*D16 + w22*M + w32*W + w42*x
                                                                      + w52*P + w62*X + w72*S + w82*A + w92*L
                                                                      + w102*G))))+
                                                               (w3/
                                                                  (1 + exp
                                                                   (-(int3 + w13*D16 + w23*M + w33*W + w43*x
                                                                      + w53*P + w63*X + w73*S + w83*A + w93*L
                                                                      + w103*G))))+
                                                               (w4/
                                                                  (1 + exp
                                                                   (-(int4 + w14*D16 + w24*M + w34*W + w44*x 
                                                                      + w54*P + w64*X + w74*S + w84*A + w94*L
                                                                      + w104*G))))+
                                                               (w5/
                                                                  (1 + exp
                                                                   (-(int5 + w15*D16 + w25*M + w35*W + w45*x 
                                                                      + w55*P + w65*X + w75*S + w85*A + w95*L
                                                                      + w105*G))))+
                                                               (w6/
                                                                  (1 + exp
                                                                   (-(int6 + w16*D16 + w26*M + w36*W + w46*x 
                                                                      + w56*P + w66*X + w76*S + w86*A + w96*L
                                                                      + w106*G))))+
                                                               (w7/
                                                                  (1 + exp
                                                                   (-(int7 + w17*D16 + w27*M + w37*W + w47*x
                                                                      + w57*P + w67*X + w77*S + w87*A + w97*L
                                                                      + w107*G))))+
                                                               (w8/
                                                                  (1 + exp
                                                                   (-(int8 + w18*D16 + w28*M + w38*W + w48*x 
                                                                      + w58*P + w68*X + w78*S + w88*A + w98*L
                                                                      + w108*G))))+
                                                               (w9/
                                                                  (1 + exp
                                                                   (-(int9 + w19*D16 + w29*M + w39*W + w49*x 
                                                                      + w59*P + w69*X + w79*S + w89*A + w99*L
                                                                      + w109*G))))+
                                                               (w10/
                                                                  (1 + exp
                                                                   (-(int10 + w110*D16 + w210*M + w310*W + w410*x 
                                                                      + w510*P + w610*X + w710*S + w810*A + w910*L
                                                                      + w1010*G))))))
                                                
                                                pred_17<-(10^(int 
                                                              +(w1/
                                                                  (1 + exp
                                                                   (-(int1 + w11*D17 + w21*M + w31*W + w41*x 
                                                                      + w51*P + w61*X + w71*S + w81*A + w91*L
                                                                      + w101*G))))+
                                                                (w2/
                                                                   (1 + exp
                                                                    (-(int2 + w12*D17 + w22*M + w32*W + w42*x
                                                                       + w52*P + w62*X + w72*S + w82*A + w92*L
                                                                       + w102*G))))+
                                                                (w3/
                                                                   (1 + exp
                                                                    (-(int3 + w13*D17 + w23*M + w33*W + w43*x
                                                                       + w53*P + w63*X + w73*S + w83*A + w93*L
                                                                       + w103*G))))+
                                                                (w4/
                                                                   (1 + exp
                                                                    (-(int4 + w14*D17 + w24*M + w34*W + w44*x 
                                                                       + w54*P + w64*X + w74*S + w84*A + w94*L
                                                                       + w104*G))))+
                                                                (w5/
                                                                   (1 + exp
                                                                    (-(int5 + w15*D17 + w25*M + w35*W + w45*x 
                                                                       + w55*P + w65*X + w75*S + w85*A + w95*L
                                                                       + w105*G))))+
                                                                (w6/
                                                                   (1 + exp
                                                                    (-(int6 + w16*D17 + w26*M + w36*W + w46*x 
                                                                       + w56*P + w66*X + w76*S + w86*A + w96*L
                                                                       + w106*G))))+
                                                                (w7/
                                                                   (1 + exp
                                                                    (-(int7 + w17*D17 + w27*M + w37*W + w47*x
                                                                       + w57*P + w67*X + w77*S + w87*A + w97*L
                                                                       + w107*G))))+
                                                                (w8/
                                                                   (1 + exp
                                                                    (-(int8 + w18*D17 + w28*M + w38*W + w48*x 
                                                                       + w58*P + w68*X + w78*S + w88*A + w98*L
                                                                       + w108*G))))+
                                                                (w9/
                                                                   (1 + exp
                                                                    (-(int9 + w19*D17 + w29*M + w39*W + w49*x 
                                                                       + w59*P + w69*X + w79*S + w89*A + w99*L
                                                                       + w109*G))))+
                                                                (w10/
                                                                   (1 + exp
                                                                    (-(int10 + w110*D17 + w210*M + w310*W + w410*x 
                                                                       + w510*P + w610*X + w710*S + w810*A + w910*L
                                                                       + w1010*G))))))
                                                
                                                mean(pred_1, pred_2, pred_3, pred_4, pred_5, pred_6, pred_7,
                                                     pred_8, pred_9, pred_10, pred_11, pred_12, pred_13,
                                                     pred_14, pred_15, pred_16, pred_17)}


## apply min/max fucntion 

max1<- apply(CDD_matrix, 1, function(i) {
  optimize(minmax, c(-.5, -.3),  D1 = i[["day.of.year_1_t"]], D2 = i[["day.of.year_2_t"]], D3 = i[["day.of.year_3_t"]],
           D4 = i[["day.of.year_4_t"]], D5 = i[["day.of.year_5_t"]], D6 = i[["day.of.year_6_t"]], D7 = i[["day.of.year_7_t"]],
           D8 = i[["day.of.year_8_t"]], D9 = i[["day.of.year_9_t"]], D10 = i[["day.of.year_10_t"]], D11 = i[["day.of.year_11_t"]],
           D12 = i[["day.of.year_12_t"]], D13 = i[["day.of.year_13_t"]], D14 = i[["day.of.year_14_t"]], D15 = i[["day.of.year_15_t"]], 
           D16 = i[["day.of.year_16_t"]], D17 = i[["day.of.year_17_t"]], M=i[["MNA_t"]], W=i[["wt_t"]], P=i[["PHDI_t"]], X=i[["sex"]],
           S = i[["subadult"]], A=i[["adult"]], G=i[["preg"]], L=i[["lac"]],maximum=T)$objective})

max2<- apply(CDD_matrix, 1, function(i) {
  optimize(minmax, c(-.3, -.1),   D1 = i[["day.of.year_1_t"]], D2 = i[["day.of.year_2_t"]], D3 = i[["day.of.year_3_t"]],
           D4 = i[["day.of.year_4_t"]], D5 = i[["day.of.year_5_t"]], D6 = i[["day.of.year_6_t"]], D7 = i[["day.of.year_7_t"]],
           D8 = i[["day.of.year_8_t"]], D9 = i[["day.of.year_9_t"]], D10 = i[["day.of.year_10_t"]], D11 = i[["day.of.year_11_t"]],
           D12 = i[["day.of.year_12_t"]], D13 = i[["day.of.year_13_t"]], D14 = i[["day.of.year_14_t"]], D15 = i[["day.of.year_15_t"]], 
           D16 = i[["day.of.year_16_t"]], D17 = i[["day.of.year_17_t"]], M=i[["MNA_t"]], W=i[["wt_t"]], P=i[["PHDI_t"]], X=i[["sex"]],
           S = i[["subadult"]], A=i[["adult"]], G=i[["preg"]], L=i[["lac"]],maximum=T)$objective})

max3<- apply(CDD_matrix, 1, function(i) {
  optimize(minmax, c(-.1, .1),   D1 = i[["day.of.year_1_t"]], D2 = i[["day.of.year_2_t"]], D3 = i[["day.of.year_3_t"]],
           D4 = i[["day.of.year_4_t"]], D5 = i[["day.of.year_5_t"]], D6 = i[["day.of.year_6_t"]], D7 = i[["day.of.year_7_t"]],
           D8 = i[["day.of.year_8_t"]], D9 = i[["day.of.year_9_t"]], D10 = i[["day.of.year_10_t"]], D11 = i[["day.of.year_11_t"]],
           D12 = i[["day.of.year_12_t"]], D13 = i[["day.of.year_13_t"]], D14 = i[["day.of.year_14_t"]], D15 = i[["day.of.year_15_t"]], 
           D16 = i[["day.of.year_16_t"]], D17 = i[["day.of.year_17_t"]], M=i[["MNA_t"]], W=i[["wt_t"]], P=i[["PHDI_t"]], X=i[["sex"]],
           S = i[["subadult"]], A=i[["adult"]], G=i[["preg"]], L=i[["lac"]],maximum=T)$objective})

max4<- apply(CDD_matrix, 1, function(i) {
  optimize(minmax, c(.1, .3),   D1 = i[["day.of.year_1_t"]], D2 = i[["day.of.year_2_t"]], D3 = i[["day.of.year_3_t"]],
           D4 = i[["day.of.year_4_t"]], D5 = i[["day.of.year_5_t"]], D6 = i[["day.of.year_6_t"]], D7 = i[["day.of.year_7_t"]],
           D8 = i[["day.of.year_8_t"]], D9 = i[["day.of.year_9_t"]], D10 = i[["day.of.year_10_t"]], D11 = i[["day.of.year_11_t"]],
           D12 = i[["day.of.year_12_t"]], D13 = i[["day.of.year_13_t"]], D14 = i[["day.of.year_14_t"]], D15 = i[["day.of.year_15_t"]], 
           D16 = i[["day.of.year_16_t"]], D17 = i[["day.of.year_17_t"]], M=i[["MNA_t"]], W=i[["wt_t"]], P=i[["PHDI_t"]], X=i[["sex"]],
           S = i[["subadult"]], A=i[["adult"]], G=i[["preg"]], L=i[["lac"]],maximum=T)$objective})

max5<- apply(CDD_matrix, 1, function(i) {
  optimize(minmax, c(.3, .5), D1 = i[["day.of.year_1_t"]], D2 = i[["day.of.year_2_t"]], D3 = i[["day.of.year_3_t"]],
           D4 = i[["day.of.year_4_t"]], D5 = i[["day.of.year_5_t"]], D6 = i[["day.of.year_6_t"]], D7 = i[["day.of.year_7_t"]],
           D8 = i[["day.of.year_8_t"]], D9 = i[["day.of.year_9_t"]], D10 = i[["day.of.year_10_t"]], D11 = i[["day.of.year_11_t"]],
           D12 = i[["day.of.year_12_t"]], D13 = i[["day.of.year_13_t"]], D14 = i[["day.of.year_14_t"]], D15 = i[["day.of.year_15_t"]], 
           D16 = i[["day.of.year_16_t"]], D17 = i[["day.of.year_17_t"]], M=i[["MNA_t"]], W=i[["wt_t"]], P=i[["PHDI_t"]], X=i[["sex"]],
           S = i[["subadult"]], A=i[["adult"]], G=i[["preg"]], L=i[["lac"]],maximum=T)$objective})

min1<- apply(CDD_matrix, 1, function(i) {
  optimize(minmax, c(-.5, -.3), D1 = i[["day.of.year_1_t"]], D2 = i[["day.of.year_2_t"]], D3 = i[["day.of.year_3_t"]],
           D4 = i[["day.of.year_4_t"]], D5 = i[["day.of.year_5_t"]], D6 = i[["day.of.year_6_t"]], D7 = i[["day.of.year_7_t"]],
           D8 = i[["day.of.year_8_t"]], D9 = i[["day.of.year_9_t"]], D10 = i[["day.of.year_10_t"]], D11 = i[["day.of.year_11_t"]],
           D12 = i[["day.of.year_12_t"]], D13 = i[["day.of.year_13_t"]], D14 = i[["day.of.year_14_t"]], D15 = i[["day.of.year_15_t"]], 
           D16 = i[["day.of.year_16_t"]], D17 = i[["day.of.year_17_t"]],M=i[["MNA_t"]], W=i[["wt_t"]], P=i[["PHDI_t"]], X=i[["sex"]],
           S = i[["subadult"]], A=i[["adult"]], G=i[["preg"]], L=i[["lac"]],maximum=F)$objective})

min2<- apply(CDD_matrix, 1, function(i) {
  optimize(minmax, c(-.3,-.1),  D1 = i[["day.of.year_1_t"]], D2 = i[["day.of.year_2_t"]], D3 = i[["day.of.year_3_t"]],
           D4 = i[["day.of.year_4_t"]], D5 = i[["day.of.year_5_t"]], D6 = i[["day.of.year_6_t"]], D7 = i[["day.of.year_7_t"]],
           D8 = i[["day.of.year_8_t"]], D9 = i[["day.of.year_9_t"]], D10 = i[["day.of.year_10_t"]], D11 = i[["day.of.year_11_t"]],
           D12 = i[["day.of.year_12_t"]], D13 = i[["day.of.year_13_t"]], D14 = i[["day.of.year_14_t"]], D15 = i[["day.of.year_15_t"]], 
           D16 = i[["day.of.year_16_t"]], D17 = i[["day.of.year_17_t"]], M=i[["MNA_t"]], W=i[["wt_t"]], P=i[["PHDI_t"]], X=i[["sex"]],
           S = i[["subadult"]], A=i[["adult"]], G=i[["preg"]], L=i[["lac"]],maximum=F)$objective})
min3<- 
  apply(CDD_matrix, 1, function(i) {
  optimize(minmax, c(-.1, .1), D1 = i[["day.of.year_1_t"]], D2 = i[["day.of.year_2_t"]], D3 = i[["day.of.year_3_t"]],
           D4 = i[["day.of.year_4_t"]], D5 = i[["day.of.year_5_t"]], D6 = i[["day.of.year_6_t"]], D7 = i[["day.of.year_7_t"]],
           D8 = i[["day.of.year_8_t"]], D9 = i[["day.of.year_9_t"]], D10 = i[["day.of.year_10_t"]], D11 = i[["day.of.year_11_t"]],
           D12 = i[["day.of.year_12_t"]], D13 = i[["day.of.year_13_t"]], D14 = i[["day.of.year_14_t"]], D15 = i[["day.of.year_15_t"]], 
           D16 = i[["day.of.year_16_t"]], D17 = i[["day.of.year_17_t"]], M=i[["MNA_t"]], W=i[["wt_t"]], P=i[["PHDI_t"]], X=i[["sex"]],
           S = i[["subadult"]], A=i[["adult"]], G=i[["preg"]], L=i[["lac"]],maximum=F)$objective})

min4<- apply(CDD_matrix, 1, function(i) {
  optimize(minmax, c(.1, .3), D1 = i[["day.of.year_1_t"]], D2 = i[["day.of.year_2_t"]], D3 = i[["day.of.year_3_t"]],
           D4 = i[["day.of.year_4_t"]], D5 = i[["day.of.year_5_t"]], D6 = i[["day.of.year_6_t"]], D7 = i[["day.of.year_7_t"]],
           D8 = i[["day.of.year_8_t"]], D9 = i[["day.of.year_9_t"]], D10 = i[["day.of.year_10_t"]], D11 = i[["day.of.year_11_t"]],
           D12 = i[["day.of.year_12_t"]], D13 = i[["day.of.year_13_t"]], D14 = i[["day.of.year_14_t"]], D15 = i[["day.of.year_15_t"]], 
           D16 = i[["day.of.year_16_t"]], D17 = i[["day.of.year_17_t"]], M=i[["MNA_t"]], W=i[["wt_t"]], P=i[["PHDI_t"]], X=i[["sex"]],
           S = i[["subadult"]], A=i[["adult"]], G=i[["preg"]], L=i[["lac"]],maximum=F)$objective})

min5<- apply(CDD_matrix, 1, function(i) {
  optimize(minmax, c(.3, .5),  D1 = i[["day.of.year_1_t"]], D2 = i[["day.of.year_2_t"]], D3 = i[["day.of.year_3_t"]],
           D4 = i[["day.of.year_4_t"]], D5 = i[["day.of.year_5_t"]], D6 = i[["day.of.year_6_t"]], D7 = i[["day.of.year_7_t"]],
           D8 = i[["day.of.year_8_t"]], D9 = i[["day.of.year_9_t"]], D10 = i[["day.of.year_10_t"]], D11 = i[["day.of.year_11_t"]],
           D12 = i[["day.of.year_12_t"]], D13 = i[["day.of.year_13_t"]], D14 = i[["day.of.year_14_t"]], D15 = i[["day.of.year_15_t"]], 
           D16 = i[["day.of.year_16_t"]], D17 = i[["day.of.year_17_t"]], M=i[["MNA_t"]], W=i[["wt_t"]], P=i[["PHDI_t"]], X=i[["sex"]],
           S = i[["subadult"]], A=i[["adult"]], G=i[["preg"]], L=i[["lac"]],maximum=F)$objective})


CDD_matrix_2 <- cbind(CDD_matrix, max1)
CDD_matrix_3<-cbind(CDD_matrix_2,max2)
CDD_matrix_4 <- cbind(CDD_matrix_3, max3)
CDD_matrix_5<-cbind(CDD_matrix_4,max4)
CDD_matrix_6<-cbind(CDD_matrix_5,max5)
CDD_matrix_7<-cbind(CDD_matrix_6, min1)
CDD_matrix_8<-cbind(CDD_matrix_7,min2)
CDD_matrix_9<-cbind(CDD_matrix_8,min3)
CDD_matrix_10<-cbind(CDD_matrix_9,min4)
CDD_matrix_11<-cbind(CDD_matrix_10,min5)

absmax<-apply(CDD_matrix_11[,26:30],1,max)
absmin<-apply(CDD_matrix_11[,31:35],1,min)
CDD_matrix_12<-cbind(CDD_matrix,absmax)
CDD_matrix_13<-cbind(CDD_matrix_12, absmin)
diff<-absmax-absmin
CDD_matrix_14<-cbind(CDD_matrix_13,diff )


## save CDD effect 
CDD_peak_effect<-mean(diff)

##export csv
write.csv(CDD_matrix_14,"CDD_effect.csv" )

## graph CDD effect over all conditions 
CDD_effect<-as.data.frame(CDD_matrix_14)

CDD_effect<-read.csv("CDD_effect.csv")
ggplot(CDD_effect, aes(PHDI, wt_t)) + geom_tile(aes(fill =  diff), 
            colour = "white") + scale_fill_distiller( palette = "Spectral",
              limits = c(min(0),max(CDD_effect$diff)))+facet_grid(.~MNA)+xlab("Lg: MNA, Sm: PHDI")+ylab("weight"
                  ) + theme_grey(base_size = 20)


## PHDI effect 
day.of.year_1_t<-rep(0.059471366,1650)
day.of.year_2_t<-rep(0.063876652,1650)
day.of.year_3_t<-rep(0.068281938,1650)
day.of.year_4_t<-rep(0.072687225,1650)
day.of.year_5_t<-rep(0.077092511,1650)
day.of.year_6_t<-rep(0.081497797,1650)
day.of.year_7_t<-rep(0.085903084,1650)
day.of.year_8_t<-rep(0.09030837,1650)
day.of.year_9_t<-rep(0.094713656,1650)
day.of.year_10_t<-rep(0.099118943,1650)
day.of.year_11_t<-rep(0.103524229,1650)
day.of.year_12_t<-rep(0.107929515,1650)
day.of.year_13_t<-rep(0.112334802,1650)
day.of.year_14_t<-rep(0.116740088,1650)
day.of.year_15_t<-rep(0.121145374,1650)
day.of.year_16_t<-rep(0.125550661,1650)
day.of.year_17_t<-rep(0.129955947,1650)
MNA_t<-c(rep(-.5,330),rep(-.25,330),rep(0,330), rep(.25,330), rep(.5,330))
wt_t<-rep(c(rep(-.5,66),rep(-.25,66),rep(0,66), rep(.25,66), rep(.5,66)),5)
CDD_t<-rep(c(rep(-.5,6), rep(-.4,6),rep(-.3,6),rep(-.2,6),rep(-.1,6),rep(0,6),
              rep(.1,6),rep(.2,6),rep(.3,6),rep(.4,6),rep(.5,6)) ,25)
sex<-rep(c(rep(0,3), rep(1,3)) ,275)
subadult<-rep(c(0,1,1) ,550)
adult<-rep(c(0,0,1) ,550)
preg<-rep(0,1650)
lac<-rep(0,1650)

PHDI_matrix<-as.matrix(data.frame(day.of.year_1_t,day.of.year_2_t,day.of.year_3_t,day.of.year_4_t,
                                  day.of.year_5_t,day.of.year_6_t,day.of.year_7_t,day.of.year_8_t,
                                  day.of.year_9_t,day.of.year_10_t,day.of.year_11_t,day.of.year_12_t,
                                  day.of.year_13_t,day.of.year_14_t,day.of.year_15_t,day.of.year_16_t,
                                  day.of.year_17_t, MNA_t, wt_t, delta.CDD_t, sex, subadult, adult, preg, lac))


##create minmax(PHDI)
minmax <- function(x,D1, D2,D3,D4,D5,
                   D6,D7,D8,D9,D10,D11,
                   D12,D13,D14,D15,D16,
                   D17,M,W,C,X,S,A,G,L) {pred_1<-(10^(int 
                                              +(w1/
                                                  (1 + exp
                                                   (-(int1 + w11*D1 + w21*M + w31*W + w41*C 
                                                      + w51*x + w61*X + w71*S + w81*A + w91*L
                                                      + w101*G))))+
                                                (w2/
                                                   (1 + exp
                                                    (-(int2 + w12*D1 + w22*M + w32*W + w42*C
                                                       + w52*x + w62*X + w72*S + w82*A + w92*L
                                                       + w102*G))))+
                                                (w3/
                                                   (1 + exp
                                                    (-(int3 + w13*D1 + w23*M + w33*W + w43*C
                                                       + w53*x + w63*X + w73*S + w83*A + w93*L
                                                       + w103*G))))+
                                                (w4/
                                                   (1 + exp
                                                    (-(int4 + w14*D1 + w24*M + w34*W + w44*C 
                                                       + w54*x + w64*X + w74*S + w84*A + w94*L
                                                       + w104*G))))+
                                                (w5/
                                                   (1 + exp
                                                    (-(int5 + w15*D1 + w25*M + w35*W + w45*C 
                                                       + w55*x + w65*X + w75*S + w85*A + w95*L
                                                       + w105*G))))+
                                                (w6/
                                                   (1 + exp
                                                    (-(int6 + w16*D1 + w26*M + w36*W + w46*C 
                                                       + w56*x + w66*X + w76*S + w86*A + w96*L
                                                       + w106*G))))+
                                                (w7/
                                                   (1 + exp
                                                    (-(int7 + w17*D1 + w27*M + w37*W + w47*C
                                                       + w57*x + w67*X + w77*S + w87*A + w97*L
                                                       + w107*G))))+
                                                (w8/
                                                   (1 + exp
                                                    (-(int8 + w18*D1 + w28*M + w38*W + w48*C 
                                                       + w58*x + w68*X + w78*S + w88*A + w98*L
                                                       + w108*G))))+
                                                (w9/
                                                   (1 + exp
                                                    (-(int9 + w19*D1 + w29*M + w39*W + w49*C 
                                                       + w59*x + w69*X + w79*S + w89*A + w99*L
                                                       + w109*G))))+
                                                (w10/
                                                   (1 + exp
                                                    (-(int10 + w110*D1 + w210*M + w310*W + w410*C 
                                                       + w510*x + w610*X + w710*S + w810*A + w910*L
                                                       + w1010*G))))))
                   pred_2<-(10^(int 
                                +(w1/
                                    (1 + exp
                                     (-(int1 + w11*D2 + w21*M + w31*W + w41*C 
                                        + w51*x + w61*X + w71*S + w81*A + w91*L
                                        + w101*G))))+
                                  (w2/
                                     (1 + exp
                                      (-(int2 + w12*D2 + w22*M + w32*W + w42*C
                                         + w52*x + w62*X + w72*S + w82*A + w92*L
                                         + w102*G))))+
                                  (w3/
                                     (1 + exp
                                      (-(int3 + w13*D2 + w23*M + w33*W + w43*C
                                         + w53*x + w63*X + w73*S + w83*A + w93*L
                                         + w103*G))))+
                                  (w4/
                                     (1 + exp
                                      (-(int4 + w14*D2 + w24*M + w34*W + w44*C 
                                         + w54*x + w64*X + w74*S + w84*A + w94*L
                                         + w104*G))))+
                                  (w5/
                                     (1 + exp
                                      (-(int5 + w15*D2 + w25*M + w35*W + w45*C 
                                         + w55*x + w65*X + w75*S + w85*A + w95*L
                                         + w105*G))))+
                                  (w6/
                                     (1 + exp
                                      (-(int6 + w16*D2 + w26*M + w36*W + w46*C 
                                         + w56*x + w66*X + w76*S + w86*A + w96*L
                                         + w106*G))))+
                                  (w7/
                                     (1 + exp
                                      (-(int7 + w17*D2 + w27*M + w37*W + w47*C
                                         + w57*x + w67*X + w77*S + w87*A + w97*L
                                         + w107*G))))+
                                  (w8/
                                     (1 + exp
                                      (-(int8 + w18*D2 + w28*M + w38*W + w48*C 
                                         + w58*x + w68*X + w78*S + w88*A + w98*L
                                         + w108*G))))+
                                  (w9/
                                     (1 + exp
                                      (-(int9 + w19*D2 + w29*M + w39*W + w49*C 
                                         + w59*x + w69*X + w79*S + w89*A + w99*L
                                         + w109*G))))+
                                  (w10/
                                     (1 + exp
                                      (-(int10 + w110*D2 + w210*M + w310*W + w410*C 
                                         + w510*x + w610*X + w710*S + w810*A + w910*L
                                         + w1010*G))))))
                   pred_3<-(10^(int 
                                +(w1/
                                    (1 + exp
                                     (-(int1 + w11*D3 + w21*M + w31*W + w41*C 
                                        + w51*x + w61*X + w71*S + w81*A + w91*L
                                        + w101*G))))+
                                  (w2/
                                     (1 + exp
                                      (-(int2 + w12*D3 + w22*M + w32*W + w42*C
                                         + w52*x + w62*X + w72*S + w82*A + w92*L
                                         + w102*G))))+
                                  (w3/
                                     (1 + exp
                                      (-(int3 + w13*D3 + w23*M + w33*W + w43*C
                                         + w53*x + w63*X + w73*S + w83*A + w93*L
                                         + w103*G))))+
                                  (w4/
                                     (1 + exp
                                      (-(int4 + w14*D3 + w24*M + w34*W + w44*C 
                                         + w54*x + w64*X + w74*S + w84*A + w94*L
                                         + w104*G))))+
                                  (w5/
                                     (1 + exp
                                      (-(int5 + w15*D3 + w25*M + w35*W + w45*C 
                                         + w55*x + w65*X + w75*S + w85*A + w95*L
                                         + w105*G))))+
                                  (w6/
                                     (1 + exp
                                      (-(int6 + w16*D3 + w26*M + w36*W + w46*C 
                                         + w56*x + w66*X + w76*S + w86*A + w96*L
                                         + w106*G))))+
                                  (w7/
                                     (1 + exp
                                      (-(int7 + w17*D3 + w27*M + w37*W + w47*C
                                         + w57*x + w67*X + w77*S + w87*A + w97*L
                                         + w107*G))))+
                                  (w8/
                                     (1 + exp
                                      (-(int8 + w18*D3 + w28*M + w38*W + w48*C 
                                         + w58*x + w68*X + w78*S + w88*A + w98*L
                                         + w108*G))))+
                                  (w9/
                                     (1 + exp
                                      (-(int9 + w19*D3 + w29*M + w39*W + w49*C 
                                         + w59*x + w69*X + w79*S + w89*A + w99*L
                                         + w109*G))))+
                                  (w10/
                                     (1 + exp
                                      (-(int10 + w110*D3 + w210*M + w310*W + w410*C 
                                         + w510*x + w610*X + w710*S + w810*A + w910*L
                                         + w1010*G))))))
                   pred_4<-(10^(int 
                                +(w1/
                                    (1 + exp
                                     (-(int1 + w11*D4 + w21*M + w31*W + w41*C 
                                        + w51*x + w61*X + w71*S + w81*A + w91*L
                                        + w101*G))))+
                                  (w2/
                                     (1 + exp
                                      (-(int2 + w12*D4 + w22*M + w32*W + w42*C
                                         + w52*x + w62*X + w72*S + w82*A + w92*L
                                         + w102*G))))+
                                  (w3/
                                     (1 + exp
                                      (-(int3 + w13*D4 + w23*M + w33*W + w43*C
                                         + w53*x + w63*X + w73*S + w83*A + w93*L
                                         + w103*G))))+
                                  (w4/
                                     (1 + exp
                                      (-(int4 + w14*D4 + w24*M + w34*W + w44*C 
                                         + w54*x + w64*X + w74*S + w84*A + w94*L
                                         + w104*G))))+
                                  (w5/
                                     (1 + exp
                                      (-(int5 + w15*D4 + w25*M + w35*W + w45*C 
                                         + w55*x + w65*X + w75*S + w85*A + w95*L
                                         + w105*G))))+
                                  (w6/
                                     (1 + exp
                                      (-(int6 + w16*D4 + w26*M + w36*W + w46*C 
                                         + w56*x + w66*X + w76*S + w86*A + w96*L
                                         + w106*G))))+
                                  (w7/
                                     (1 + exp
                                      (-(int7 + w17*D4 + w27*M + w37*W + w47*C
                                         + w57*x + w67*X + w77*S + w87*A + w97*L
                                         + w107*G))))+
                                  (w8/
                                     (1 + exp
                                      (-(int8 + w18*D4 + w28*M + w38*W + w48*C 
                                         + w58*x + w68*X + w78*S + w88*A + w98*L
                                         + w108*G))))+
                                  (w9/
                                     (1 + exp
                                      (-(int9 + w19*D4 + w29*M + w39*W + w49*C 
                                         + w59*x + w69*X + w79*S + w89*A + w99*L
                                         + w109*G))))+
                                  (w10/
                                     (1 + exp
                                      (-(int10 + w110*D4 + w210*M + w310*W + w410*C 
                                         + w510*x + w610*X + w710*S + w810*A + w910*L
                                         + w1010*G))))))
                   pred_5<-(10^(int 
                                +(w1/
                                    (1 + exp
                                     (-(int1 + w11*D5 + w21*M + w31*W + w41*C 
                                        + w51*x + w61*X + w71*S + w81*A + w91*L
                                        + w101*G))))+
                                  (w2/
                                     (1 + exp
                                      (-(int2 + w12*D5 + w22*M + w32*W + w42*C
                                         + w52*x + w62*X + w72*S + w82*A + w92*L
                                         + w102*G))))+
                                  (w3/
                                     (1 + exp
                                      (-(int3 + w13*D5 + w23*M + w33*W + w43*C
                                         + w53*x + w63*X + w73*S + w83*A + w93*L
                                         + w103*G))))+
                                  (w4/
                                     (1 + exp
                                      (-(int4 + w14*D5 + w24*M + w34*W + w44*C 
                                         + w54*x + w64*X + w74*S + w84*A + w94*L
                                         + w104*G))))+
                                  (w5/
                                     (1 + exp
                                      (-(int5 + w15*D5 + w25*M + w35*W + w45*C 
                                         + w55*x + w65*X + w75*S + w85*A + w95*L
                                         + w105*G))))+
                                  (w6/
                                     (1 + exp
                                      (-(int6 + w16*D5 + w26*M + w36*W + w46*C 
                                         + w56*x + w66*X + w76*S + w86*A + w96*L
                                         + w106*G))))+
                                  (w7/
                                     (1 + exp
                                      (-(int7 + w17*D5 + w27*M + w37*W + w47*C
                                         + w57*x + w67*X + w77*S + w87*A + w97*L
                                         + w107*G))))+
                                  (w8/
                                     (1 + exp
                                      (-(int8 + w18*D5 + w28*M + w38*W + w48*C 
                                         + w58*x + w68*X + w78*S + w88*A + w98*L
                                         + w108*G))))+
                                  (w9/
                                     (1 + exp
                                      (-(int9 + w19*D5 + w29*M + w39*W + w49*C 
                                         + w59*x + w69*X + w79*S + w89*A + w99*L
                                         + w109*G))))+
                                  (w10/
                                     (1 + exp
                                      (-(int10 + w110*D5 + w210*M + w310*W + w410*C 
                                         + w510*x + w610*X + w710*S + w810*A + w910*L
                                         + w1010*G))))))
                   pred_6<-(10^(int 
                                +(w1/
                                    (1 + exp
                                     (-(int1 + w11*D6 + w21*M + w31*W + w41*C 
                                        + w51*x + w61*X + w71*S + w81*A + w91*L
                                        + w101*G))))+
                                  (w2/
                                     (1 + exp
                                      (-(int2 + w12*D6 + w22*M + w32*W + w42*C
                                         + w52*x + w62*X + w72*S + w82*A + w92*L
                                         + w102*G))))+
                                  (w3/
                                     (1 + exp
                                      (-(int3 + w13*D6 + w23*M + w33*W + w43*C
                                         + w53*x + w63*X + w73*S + w83*A + w93*L
                                         + w103*G))))+
                                  (w4/
                                     (1 + exp
                                      (-(int4 + w14*D6 + w24*M + w34*W + w44*C 
                                         + w54*x + w64*X + w74*S + w84*A + w94*L
                                         + w104*G))))+
                                  (w5/
                                     (1 + exp
                                      (-(int5 + w15*D6 + w25*M + w35*W + w45*C 
                                         + w55*x + w65*X + w75*S + w85*A + w95*L
                                         + w105*G))))+
                                  (w6/
                                     (1 + exp
                                      (-(int6 + w16*D6+ w26*M + w36*W + w46*C 
                                         + w56*x + w66*X + w76*S + w86*A + w96*L
                                         + w106*G))))+
                                  (w7/
                                     (1 + exp
                                      (-(int7 + w17*D6 + w27*M + w37*W + w47*C
                                         + w57*x + w67*X + w77*S + w87*A + w97*L
                                         + w107*G))))+
                                  (w8/
                                     (1 + exp
                                      (-(int8 + w18*D6 + w28*M + w38*W + w48*C 
                                         + w58*x + w68*X + w78*S + w88*A + w98*L
                                         + w108*G))))+
                                  (w9/
                                     (1 + exp
                                      (-(int9 + w19*D6 + w29*M + w39*W + w49*C 
                                         + w59*x + w69*X + w79*S + w89*A + w99*L
                                         + w109*G))))+
                                  (w10/
                                     (1 + exp
                                      (-(int10 + w110*D6 + w210*M + w310*W + w410*C 
                                         + w510*x + w610*X + w710*S + w810*A + w910*L
                                         + w1010*G))))))
                   pred_7<-(10^(int 
                                +(w1/
                                    (1 + exp
                                     (-(int1 + w11*D7 + w21*M + w31*W + w41*C 
                                        + w51*x + w61*X + w71*S + w81*A + w91*L
                                        + w101*G))))+
                                  (w2/
                                     (1 + exp
                                      (-(int2 + w12*D7 + w22*M + w32*W + w42*C
                                         + w52*x + w62*X + w72*S + w82*A + w92*L
                                         + w102*G))))+
                                  (w3/
                                     (1 + exp
                                      (-(int3 + w13*D7 + w23*M + w33*W + w43*C
                                         + w53*x + w63*X + w73*S + w83*A + w93*L
                                         + w103*G))))+
                                  (w4/
                                     (1 + exp
                                      (-(int4 + w14*D7 + w24*M + w34*W + w44*C 
                                         + w54*x + w64*X + w74*S + w84*A + w94*L
                                         + w104*G))))+
                                  (w5/
                                     (1 + exp
                                      (-(int5 + w15*D7 + w25*M + w35*W + w45*C 
                                         + w55*x + w65*X + w75*S + w85*A + w95*L
                                         + w105*G))))+
                                  (w6/
                                     (1 + exp
                                      (-(int6 + w16*D7 + w26*M + w36*W + w46*C 
                                         + w56*x + w66*X + w76*S + w86*A + w96*L
                                         + w106*G))))+
                                  (w7/
                                     (1 + exp
                                      (-(int7 + w17*D7 + w27*M + w37*W + w47*C
                                         + w57*x + w67*X + w77*S + w87*A + w97*L
                                         + w107*G))))+
                                  (w8/
                                     (1 + exp
                                      (-(int8 + w18*D7 + w28*M + w38*W + w48*C 
                                         + w58*x + w68*X + w78*S + w88*A + w98*L
                                         + w108*G))))+
                                  (w9/
                                     (1 + exp
                                      (-(int9 + w19*D7 + w29*M + w39*W + w49*C 
                                         + w59*x + w69*X + w79*S + w89*A + w99*L
                                         + w109*G))))+
                                  (w10/
                                     (1 + exp
                                      (-(int10 + w110*D7 + w210*M + w310*W + w410*C 
                                         + w510*x + w610*X + w710*S + w810*A + w910*L
                                         + w1010*G))))))
                   pred_8<-(10^(int 
                                +(w1/
                                    (1 + exp
                                     (-(int1 + w11*D8 + w21*M + w31*W + w41*C 
                                        + w51*x + w61*X + w71*S + w81*A + w91*L
                                        + w101*G))))+
                                  (w2/
                                     (1 + exp
                                      (-(int2 + w12*D8 + w22*M + w32*W + w42*C
                                         + w52*x + w62*X + w72*S + w82*A + w92*L
                                         + w102*G))))+
                                  (w3/
                                     (1 + exp
                                      (-(int3 + w13*D8 + w23*M + w33*W + w43*C
                                         + w53*x + w63*X + w73*S + w83*A + w93*L
                                         + w103*G))))+
                                  (w4/
                                     (1 + exp
                                      (-(int4 + w14*D8 + w24*M + w34*W + w44*C 
                                         + w54*x + w64*X + w74*S + w84*A + w94*L
                                         + w104*G))))+
                                  (w5/
                                     (1 + exp
                                      (-(int5 + w15*D8 + w25*M + w35*W + w45*C 
                                         + w55*x + w65*X + w75*S + w85*A + w95*L
                                         + w105*G))))+
                                  (w6/
                                     (1 + exp
                                      (-(int6 + w16*D8 + w26*M + w36*W + w46*C 
                                         + w56*x + w66*X + w76*S + w86*A + w96*L
                                         + w106*G))))+
                                  (w7/
                                     (1 + exp
                                      (-(int7 + w17*D8 + w27*M + w37*W + w47*C
                                         + w57*x + w67*X + w77*S + w87*A + w97*L
                                         + w107*G))))+
                                  (w8/
                                     (1 + exp
                                      (-(int8 + w18*D8 + w28*M + w38*W + w48*C 
                                         + w58*x + w68*X + w78*S + w88*A + w98*L
                                         + w108*G))))+
                                  (w9/
                                     (1 + exp
                                      (-(int9 + w19*D8 + w29*M + w39*W + w49*C 
                                         + w59*x + w69*X + w79*S + w89*A + w99*L
                                         + w109*G))))+
                                  (w10/
                                     (1 + exp
                                      (-(int10 + w110*D8 + w210*M + w310*W + w410*C 
                                         + w510*x + w610*X + w710*S + w810*A + w910*L
                                         + w1010*G))))))
                   pred_9<-(10^(int 
                                +(w1/
                                    (1 + exp
                                     (-(int1 + w11*D9 + w21*M + w31*W + w41*C 
                                        + w51*x + w61*X + w71*S + w81*A + w91*L
                                        + w101*G))))+
                                  (w2/
                                     (1 + exp
                                      (-(int2 + w12*D9 + w22*M + w32*W + w42*C
                                         + w52*x + w62*X + w72*S + w82*A + w92*L
                                         + w102*G))))+
                                  (w3/
                                     (1 + exp
                                      (-(int3 + w13*D9 + w23*M + w33*W + w43*C
                                         + w53*x + w63*X + w73*S + w83*A + w93*L
                                         + w103*G))))+
                                  (w4/
                                     (1 + exp
                                      (-(int4 + w14*D9 + w24*M + w34*W + w44*C 
                                         + w54*x + w64*X + w74*S + w84*A + w94*L
                                         + w104*G))))+
                                  (w5/
                                     (1 + exp
                                      (-(int5 + w15*D9 + w25*M + w35*W + w45*C 
                                         + w55*x + w65*X + w75*S + w85*A + w95*L
                                         + w105*G))))+
                                  (w6/
                                     (1 + exp
                                      (-(int6 + w16*D9 + w26*M + w36*W + w46*C 
                                         + w56*x + w66*X + w76*S + w86*A + w96*L
                                         + w106*G))))+
                                  (w7/
                                     (1 + exp
                                      (-(int7 + w17*D9 + w27*M + w37*W + w47*C
                                         + w57*x + w67*X + w77*S + w87*A + w97*L
                                         + w107*G))))+
                                  (w8/
                                     (1 + exp
                                      (-(int8 + w18*D9 + w28*M + w38*W + w48*C 
                                         + w58*x + w68*X + w78*S + w88*A + w98*L
                                         + w108*G))))+
                                  (w9/
                                     (1 + exp
                                      (-(int9 + w19*D9 + w29*M + w39*W + w49*C 
                                         + w59*x + w69*X + w79*S + w89*A + w99*L
                                         + w109*G))))+
                                  (w10/
                                     (1 + exp
                                      (-(int10 + w110*D9 + w210*M + w310*W + w410*C 
                                         + w510*x + w610*X + w710*S + w810*A + w910*L
                                         + w1010*G))))))
                   pred_10<-(10^(int 
                                +(w1/
                                    (1 + exp
                                     (-(int1 + w11*D10 + w21*M + w31*W + w41*C 
                                        + w51*x + w61*X + w71*S + w81*A + w91*L
                                        + w101*G))))+
                                  (w2/
                                     (1 + exp
                                      (-(int2 + w12*D10 + w22*M + w32*W + w42*C
                                         + w52*x + w62*X + w72*S + w82*A + w92*L
                                         + w102*G))))+
                                  (w3/
                                     (1 + exp
                                      (-(int3 + w13*D10 + w23*M + w33*W + w43*C
                                         + w53*x + w63*X + w73*S + w83*A + w93*L
                                         + w103*G))))+
                                  (w4/
                                     (1 + exp
                                      (-(int4 + w14*D10 + w24*M + w34*W + w44*C 
                                         + w54*x + w64*X + w74*S + w84*A + w94*L
                                         + w104*G))))+
                                  (w5/
                                     (1 + exp
                                      (-(int5 + w15*D10 + w25*M + w35*W + w45*C 
                                         + w55*x + w65*X + w75*S + w85*A + w95*L
                                         + w105*G))))+
                                  (w6/
                                     (1 + exp
                                      (-(int6 + w16*D10 + w26*M + w36*W + w46*C 
                                         + w56*x + w66*X + w76*S + w86*A + w96*L
                                         + w106*G))))+
                                  (w7/
                                     (1 + exp
                                      (-(int7 + w17*D10 + w27*M + w37*W + w47*C
                                         + w57*x + w67*X + w77*S + w87*A + w97*L
                                         + w107*G))))+
                                  (w8/
                                     (1 + exp
                                      (-(int8 + w18*D10 + w28*M + w38*W + w48*C 
                                         + w58*x + w68*X + w78*S + w88*A + w98*L
                                         + w108*G))))+
                                  (w9/
                                     (1 + exp
                                      (-(int9 + w19*D10 + w29*M + w39*W + w49*C 
                                         + w59*x + w69*X + w79*S + w89*A + w99*L
                                         + w109*G))))+
                                  (w10/
                                     (1 + exp
                                      (-(int10 + w110*D10 + w210*M + w310*W + w410*C 
                                         + w510*x + w610*X + w710*S + w810*A + w910*L
                                         + w1010*G))))))
                   pred_11<-(10^(int 
                                +(w1/
                                    (1 + exp
                                     (-(int1 + w11*D11 + w21*M + w31*W + w41*C 
                                        + w51*x + w61*X + w71*S + w81*A + w91*L
                                        + w101*G))))+
                                  (w2/
                                     (1 + exp
                                      (-(int2 + w12*D11 + w22*M + w32*W + w42*C
                                         + w52*x + w62*X + w72*S + w82*A + w92*L
                                         + w102*G))))+
                                  (w3/
                                     (1 + exp
                                      (-(int3 + w13*D11 + w23*M + w33*W + w43*C
                                         + w53*x + w63*X + w73*S + w83*A + w93*L
                                         + w103*G))))+
                                  (w4/
                                     (1 + exp
                                      (-(int4 + w14*D11 + w24*M + w34*W + w44*C 
                                         + w54*x + w64*X + w74*S + w84*A + w94*L
                                         + w104*G))))+
                                  (w5/
                                     (1 + exp
                                      (-(int5 + w15*D11 + w25*M + w35*W + w45*C 
                                         + w55*x + w65*X + w75*S + w85*A + w95*L
                                         + w105*G))))+
                                  (w6/
                                     (1 + exp
                                      (-(int6 + w16*D11 + w26*M + w36*W + w46*C 
                                         + w56*x + w66*X + w76*S + w86*A + w96*L
                                         + w106*G))))+
                                  (w7/
                                     (1 + exp
                                      (-(int7 + w17*D11 + w27*M + w37*W + w47*C
                                         + w57*x + w67*X + w77*S + w87*A + w97*L
                                         + w107*G))))+
                                  (w8/
                                     (1 + exp
                                      (-(int8 + w18*D11 + w28*M + w38*W + w48*C 
                                         + w58*x + w68*X + w78*S + w88*A + w98*L
                                         + w108*G))))+
                                  (w9/
                                     (1 + exp
                                      (-(int9 + w19*D11 + w29*M + w39*W + w49*C 
                                         + w59*x + w69*X + w79*S + w89*A + w99*L
                                         + w109*G))))+
                                  (w10/
                                     (1 + exp
                                      (-(int10 + w110*D11 + w210*M + w310*W + w410*C 
                                         + w510*x + w610*X + w710*S + w810*A + w910*L
                                         + w1010*G))))))
                   pred_12<-(10^(int 
                                +(w1/
                                    (1 + exp
                                     (-(int1 + w11*D12 + w21*M + w31*W + w41*C 
                                        + w51*x + w61*X + w71*S + w81*A + w91*L
                                        + w101*G))))+
                                  (w2/
                                     (1 + exp
                                      (-(int2 + w12*D12 + w22*M + w32*W + w42*C
                                         + w52*x + w62*X + w72*S + w82*A + w92*L
                                         + w102*G))))+
                                  (w3/
                                     (1 + exp
                                      (-(int3 + w13*D12 + w23*M + w33*W + w43*C
                                         + w53*x + w63*X + w73*S + w83*A + w93*L
                                         + w103*G))))+
                                  (w4/
                                     (1 + exp
                                      (-(int4 + w14*D12 + w24*M + w34*W + w44*C 
                                         + w54*x + w64*X + w74*S + w84*A + w94*L
                                         + w104*G))))+
                                  (w5/
                                     (1 + exp
                                      (-(int5 + w15*D12 + w25*M + w35*W + w45*C 
                                         + w55*x + w65*X + w75*S + w85*A + w95*L
                                         + w105*G))))+
                                  (w6/
                                     (1 + exp
                                      (-(int6 + w16*D12 + w26*M + w36*W + w46*C 
                                         + w56*x + w66*X + w76*S + w86*A + w96*L
                                         + w106*G))))+
                                  (w7/
                                     (1 + exp
                                      (-(int7 + w17*D12 + w27*M + w37*W + w47*C
                                         + w57*x + w67*X + w77*S + w87*A + w97*L
                                         + w107*G))))+
                                  (w8/
                                     (1 + exp
                                      (-(int8 + w18*D12 + w28*M + w38*W + w48*C 
                                         + w58*x + w68*X + w78*S + w88*A + w98*L
                                         + w108*G))))+
                                  (w9/
                                     (1 + exp
                                      (-(int9 + w19*D12 + w29*M + w39*W + w49*C 
                                         + w59*x + w69*X + w79*S + w89*A + w99*L
                                         + w109*G))))+
                                  (w10/
                                     (1 + exp
                                      (-(int10 + w110*D12 + w210*M + w310*W + w410*C 
                                         + w510*x + w610*X + w710*S + w810*A + w910*L
                                         + w1010*G))))))
                   pred_13<-(10^(int 
                                +(w1/
                                    (1 + exp
                                     (-(int1 + w11*D13 + w21*M + w31*W + w41*C 
                                        + w51*x + w61*X + w71*S + w81*A + w91*L
                                        + w101*G))))+
                                  (w2/
                                     (1 + exp
                                      (-(int2 + w12*D13 + w22*M + w32*W + w42*C
                                         + w52*x + w62*X + w72*S + w82*A + w92*L
                                         + w102*G))))+
                                  (w3/
                                     (1 + exp
                                      (-(int3 + w13*D13 + w23*M + w33*W + w43*C
                                         + w53*x + w63*X + w73*S + w83*A + w93*L
                                         + w103*G))))+
                                  (w4/
                                     (1 + exp
                                      (-(int4 + w14*D13 + w24*M + w34*W + w44*C 
                                         + w54*x + w64*X + w74*S + w84*A + w94*L
                                         + w104*G))))+
                                  (w5/
                                     (1 + exp
                                      (-(int5 + w15*D13 + w25*M + w35*W + w45*C 
                                         + w55*x + w65*X + w75*S + w85*A + w95*L
                                         + w105*G))))+
                                  (w6/
                                     (1 + exp
                                      (-(int6 + w16*D13 + w26*M + w36*W + w46*C 
                                         + w56*x + w66*X + w76*S + w86*A + w96*L
                                         + w106*G))))+
                                  (w7/
                                     (1 + exp
                                      (-(int7 + w17*D13 + w27*M + w37*W + w47*C
                                         + w57*x + w67*X + w77*S + w87*A + w97*L
                                         + w107*G))))+
                                  (w8/
                                     (1 + exp
                                      (-(int8 + w18*D13 + w28*M + w38*W + w48*C 
                                         + w58*x + w68*X + w78*S + w88*A + w98*L
                                         + w108*G))))+
                                  (w9/
                                     (1 + exp
                                      (-(int9 + w19*D13 + w29*M + w39*W + w49*C 
                                         + w59*x + w69*X + w79*S + w89*A + w99*L
                                         + w109*G))))+
                                  (w10/
                                     (1 + exp
                                      (-(int10 + w110*D13 + w210*M + w310*W + w410*C 
                                         + w510*x + w610*X + w710*S + w810*A + w910*L
                                         + w1010*G))))))
                   pred_14<-(10^(int 
                                +(w1/
                                    (1 + exp
                                     (-(int1 + w11*D14 + w21*M + w31*W + w41*C 
                                        + w51*x + w61*X + w71*S + w81*A + w91*L
                                        + w101*G))))+
                                  (w2/
                                     (1 + exp
                                      (-(int2 + w12*D14 + w22*M + w32*W + w42*C
                                         + w52*x + w62*X + w72*S + w82*A + w92*L
                                         + w102*G))))+
                                  (w3/
                                     (1 + exp
                                      (-(int3 + w13*D14 + w23*M + w33*W + w43*C
                                         + w53*x + w63*X + w73*S + w83*A + w93*L
                                         + w103*G))))+
                                  (w4/
                                     (1 + exp
                                      (-(int4 + w14*D14 + w24*M + w34*W + w44*C 
                                         + w54*x + w64*X + w74*S + w84*A + w94*L
                                         + w104*G))))+
                                  (w5/
                                     (1 + exp
                                      (-(int5 + w15*D14 + w25*M + w35*W + w45*C 
                                         + w55*x + w65*X + w75*S + w85*A + w95*L
                                         + w105*G))))+
                                  (w6/
                                     (1 + exp
                                      (-(int6 + w16*D14 + w26*M + w36*W + w46*C 
                                         + w56*x + w66*X + w76*S + w86*A + w96*L
                                         + w106*G))))+
                                  (w7/
                                     (1 + exp
                                      (-(int7 + w17*D14 + w27*M + w37*W + w47*C
                                         + w57*x + w67*X + w77*S + w87*A + w97*L
                                         + w107*G))))+
                                  (w8/
                                     (1 + exp
                                      (-(int8 + w18*D14 + w28*M + w38*W + w48*C 
                                         + w58*x + w68*X + w78*S + w88*A + w98*L
                                         + w108*G))))+
                                  (w9/
                                     (1 + exp
                                      (-(int9 + w19*D14 + w29*M + w39*W + w49*C 
                                         + w59*x + w69*X + w79*S + w89*A + w99*L
                                         + w109*G))))+
                                  (w10/
                                     (1 + exp
                                      (-(int10 + w110*D14 + w210*M + w310*W + w410*C 
                                         + w510*x + w610*X + w710*S + w810*A + w910*L
                                         + w1010*G))))))
                   pred_15<-(10^(int 
                                +(w1/
                                    (1 + exp
                                     (-(int1 + w11*D15 + w21*M + w31*W + w41*C 
                                        + w51*x + w61*X + w71*S + w81*A + w91*L
                                        + w101*G))))+
                                  (w2/
                                     (1 + exp
                                      (-(int2 + w12*D15 + w22*M + w32*W + w42*C
                                         + w52*x + w62*X + w72*S + w82*A + w92*L
                                         + w102*G))))+
                                  (w3/
                                     (1 + exp
                                      (-(int3 + w13*D15 + w23*M + w33*W + w43*C
                                         + w53*x + w63*X + w73*S + w83*A + w93*L
                                         + w103*G))))+
                                  (w4/
                                     (1 + exp
                                      (-(int4 + w14*D15 + w24*M + w34*W + w44*C 
                                         + w54*x + w64*X + w74*S + w84*A + w94*L
                                         + w104*G))))+
                                  (w5/
                                     (1 + exp
                                      (-(int5 + w15*D15 + w25*M + w35*W + w45*C 
                                         + w55*x + w65*X + w75*S + w85*A + w95*L
                                         + w105*G))))+
                                  (w6/
                                     (1 + exp
                                      (-(int6 + w16*D15 + w26*M + w36*W + w46*C 
                                         + w56*x + w66*X + w76*S + w86*A + w96*L
                                         + w106*G))))+
                                  (w7/
                                     (1 + exp
                                      (-(int7 + w17*D15 + w27*M + w37*W + w47*C
                                         + w57*x + w67*X + w77*S + w87*A + w97*L
                                         + w107*G))))+
                                  (w8/
                                     (1 + exp
                                      (-(int8 + w18*D15 + w28*M + w38*W + w48*C 
                                         + w58*x + w68*X + w78*S + w88*A + w98*L
                                         + w108*G))))+
                                  (w9/
                                     (1 + exp
                                      (-(int9 + w19*D15 + w29*M + w39*W + w49*C 
                                         + w59*x + w69*X + w79*S + w89*A + w99*L
                                         + w109*G))))+
                                  (w10/
                                     (1 + exp
                                      (-(int10 + w110*D15+ w210*M + w310*W + w410*C 
                                         + w510*x + w610*X + w710*S + w810*A + w910*L
                                         + w1010*G))))))
                   pred_16<-(10^(int 
                                +(w1/
                                    (1 + exp
                                     (-(int1 + w11*D16 + w21*M + w31*W + w41*C 
                                        + w51*x + w61*X + w71*S + w81*A + w91*L
                                        + w101*G))))+
                                  (w2/
                                     (1 + exp
                                      (-(int2 + w12*D16 + w22*M + w32*W + w42*C
                                         + w52*x + w62*X + w72*S + w82*A + w92*L
                                         + w102*G))))+
                                  (w3/
                                     (1 + exp
                                      (-(int3 + w13*D16 + w23*M + w33*W + w43*C
                                         + w53*x + w63*X + w73*S + w83*A + w93*L
                                         + w103*G))))+
                                  (w4/
                                     (1 + exp
                                      (-(int4 + w14*D16 + w24*M + w34*W + w44*C 
                                         + w54*x + w64*X + w74*S + w84*A + w94*L
                                         + w104*G))))+
                                  (w5/
                                     (1 + exp
                                      (-(int5 + w15*D16 + w25*M + w35*W + w45*C 
                                         + w55*x + w65*X + w75*S + w85*A + w95*L
                                         + w105*G))))+
                                  (w6/
                                     (1 + exp
                                      (-(int6 + w16*D16 + w26*M + w36*W + w46*C 
                                         + w56*x + w66*X + w76*S + w86*A + w96*L
                                         + w106*G))))+
                                  (w7/
                                     (1 + exp
                                      (-(int7 + w17*D16 + w27*M + w37*W + w47*C
                                         + w57*x + w67*X + w77*S + w87*A + w97*L
                                         + w107*G))))+
                                  (w8/
                                     (1 + exp
                                      (-(int8 + w18*D16 + w28*M + w38*W + w48*C 
                                         + w58*x + w68*X + w78*S + w88*A + w98*L
                                         + w108*G))))+
                                  (w9/
                                     (1 + exp
                                      (-(int9 + w19*D16 + w29*M + w39*W + w49*C 
                                         + w59*x + w69*X + w79*S + w89*A + w99*L
                                         + w109*G))))+
                                  (w10/
                                     (1 + exp
                                      (-(int10 + w110*D16 + w210*M + w310*W + w410*C 
                                         + w510*x + w610*X + w710*S + w810*A + w910*L
                                         + w1010*G))))))
                   pred_17<-(10^(int 
                                +(w1/
                                    (1 + exp
                                     (-(int1 + w11*D17 + w21*M + w31*W + w41*C 
                                        + w51*x + w61*X + w71*S + w81*A + w91*L
                                        + w101*G))))+
                                  (w2/
                                     (1 + exp
                                      (-(int2 + w12*D17 + w22*M + w32*W + w42*C
                                         + w52*x + w62*X + w72*S + w82*A + w92*L
                                         + w102*G))))+
                                  (w3/
                                     (1 + exp
                                      (-(int3 + w13*D17 + w23*M + w33*W + w43*C
                                         + w53*x + w63*X + w73*S + w83*A + w93*L
                                         + w103*G))))+
                                  (w4/
                                     (1 + exp
                                      (-(int4 + w14*D17 + w24*M + w34*W + w44*C 
                                         + w54*x + w64*X + w74*S + w84*A + w94*L
                                         + w104*G))))+
                                  (w5/
                                     (1 + exp
                                      (-(int5 + w15*D17 + w25*M + w35*W + w45*C 
                                         + w55*x + w65*X + w75*S + w85*A + w95*L
                                         + w105*G))))+
                                  (w6/
                                     (1 + exp
                                      (-(int6 + w16*D17 + w26*M + w36*W + w46*C 
                                         + w56*x + w66*X + w76*S + w86*A + w96*L
                                         + w106*G))))+
                                  (w7/
                                     (1 + exp
                                      (-(int7 + w17*D17 + w27*M + w37*W + w47*C
                                         + w57*x + w67*X + w77*S + w87*A + w97*L
                                         + w107*G))))+
                                  (w8/
                                     (1 + exp
                                      (-(int8 + w18*D17 + w28*M + w38*W + w48*C 
                                         + w58*x + w68*X + w78*S + w88*A + w98*L
                                         + w108*G))))+
                                  (w9/
                                     (1 + exp
                                      (-(int9 + w19*D17 + w29*M + w39*W + w49*C 
                                         + w59*x + w69*X + w79*S + w89*A + w99*L
                                         + w109*G))))+
                                  (w10/
                                     (1 + exp
                                      (-(int10 + w110*D17 + w210*M + w310*W + w410*C 
                                         + w510*x + w610*X + w710*S + w810*A + w910*L
                                         + w1010*G))))))
                   mean(pred_1, pred_2, pred_3, pred_4, pred_5, pred_6, pred_7,
                        pred_8, pred_9, pred_10, pred_11, pred_12, pred_13,
                        pred_14, pred_15, pred_16, pred_17)}
              
##apply minmax(PHDI)
max1<- apply(PHDI_matrix, 1, function(i) {
  optimize(minmax, c(-.5, -.3),   D1 = i[["day.of.year_1_t"]], D2 = i[["day.of.year_2_t"]], D3 = i[["day.of.year_3_t"]],
           D4 = i[["day.of.year_4_t"]], D5 = i[["day.of.year_5_t"]], D6 = i[["day.of.year_6_t"]], D7 = i[["day.of.year_7_t"]],
           D8 = i[["day.of.year_8_t"]], D9 = i[["day.of.year_9_t"]], D10 = i[["day.of.year_10_t"]], D11 = i[["day.of.year_11_t"]],
           D12 = i[["day.of.year_12_t"]], D13 = i[["day.of.year_13_t"]], D14 = i[["day.of.year_14_t"]], D15 = i[["day.of.year_15_t"]], 
           D16 = i[["day.of.year_16_t"]], D17 = i[["day.of.year_17_t"]], M=i[["MNA_t"]], W=i[["wt_t"]], C=i[["CDD_t"]], X=i[["sex"]],
           S = i[["subadult"]], A=i[["adult"]], G=i[["preg"]], L=i[["lac"]],maximum=T)$objective})

max2<- apply(PHDI_matrix, 1, function(i) {
  optimize(minmax, c(-.3, -.1),   D1 = i[["day.of.year_1_t"]], D2 = i[["day.of.year_2_t"]], D3 = i[["day.of.year_3_t"]],
           D4 = i[["day.of.year_4_t"]], D5 = i[["day.of.year_5_t"]], D6 = i[["day.of.year_6_t"]], D7 = i[["day.of.year_7_t"]],
           D8 = i[["day.of.year_8_t"]], D9 = i[["day.of.year_9_t"]], D10 = i[["day.of.year_10_t"]], D11 = i[["day.of.year_11_t"]],
           D12 = i[["day.of.year_12_t"]], D13 = i[["day.of.year_13_t"]], D14 = i[["day.of.year_14_t"]], D15 = i[["day.of.year_15_t"]], 
           D16 = i[["day.of.year_16_t"]], D17 = i[["day.of.year_17_t"]], M=i[["MNA_t"]], W=i[["wt_t"]], C=i[["CDD_t"]], X=i[["sex"]],
           S = i[["subadult"]], A=i[["adult"]], G=i[["preg"]], L=i[["lac"]],maximum=T)$objective})

max3<- apply(PHDI_matrix, 1, function(i) {
  optimize(minmax, c(-.1, .1),   D1 = i[["day.of.year_1_t"]], D2 = i[["day.of.year_2_t"]], D3 = i[["day.of.year_3_t"]],
           D4 = i[["day.of.year_4_t"]], D5 = i[["day.of.year_5_t"]], D6 = i[["day.of.year_6_t"]], D7 = i[["day.of.year_7_t"]],
           D8 = i[["day.of.year_8_t"]], D9 = i[["day.of.year_9_t"]], D10 = i[["day.of.year_10_t"]], D11 = i[["day.of.year_11_t"]],
           D12 = i[["day.of.year_12_t"]], D13 = i[["day.of.year_13_t"]], D14 = i[["day.of.year_14_t"]], D15 = i[["day.of.year_15_t"]], 
           D16 = i[["day.of.year_16_t"]], D17 = i[["day.of.year_17_t"]], M=i[["MNA_t"]], W=i[["wt_t"]], C=i[["CDD_t"]], X=i[["sex"]],
           S = i[["subadult"]], A=i[["adult"]], G=i[["preg"]], L=i[["lac"]],maximum=T)$objective})

max4<- apply(PHDI_matrix, 1, function(i) {
  optimize(minmax, c(.1, .3),   D1 = i[["day.of.year_1_t"]], D2 = i[["day.of.year_2_t"]], D3 = i[["day.of.year_3_t"]],
           D4 = i[["day.of.year_4_t"]], D5 = i[["day.of.year_5_t"]], D6 = i[["day.of.year_6_t"]], D7 = i[["day.of.year_7_t"]],
           D8 = i[["day.of.year_8_t"]], D9 = i[["day.of.year_9_t"]], D10 = i[["day.of.year_10_t"]], D11 = i[["day.of.year_11_t"]],
           D12 = i[["day.of.year_12_t"]], D13 = i[["day.of.year_13_t"]], D14 = i[["day.of.year_14_t"]], D15 = i[["day.of.year_15_t"]], 
           D16 = i[["day.of.year_16_t"]], D17 = i[["day.of.year_17_t"]], M=i[["MNA_t"]], W=i[["wt_t"]], C=i[["CDD_t"]], X=i[["sex"]],
           S = i[["subadult"]], A=i[["adult"]], G=i[["preg"]], L=i[["lac"]],maximum=T)$objective})

max5<- apply(PHDI_matrix, 1, function(i) {
  optimize(minmax, c(.3, .5),  D1 = i[["day.of.year_1_t"]], D2 = i[["day.of.year_2_t"]], D3 = i[["day.of.year_3_t"]],
           D4 = i[["day.of.year_4_t"]], D5 = i[["day.of.year_5_t"]], D6 = i[["day.of.year_6_t"]], D7 = i[["day.of.year_7_t"]],
           D8 = i[["day.of.year_8_t"]], D9 = i[["day.of.year_9_t"]], D10 = i[["day.of.year_10_t"]], D11 = i[["day.of.year_11_t"]],
           D12 = i[["day.of.year_12_t"]], D13 = i[["day.of.year_13_t"]], D14 = i[["day.of.year_14_t"]], D15 = i[["day.of.year_15_t"]], 
           D16 = i[["day.of.year_16_t"]], D17 = i[["day.of.year_17_t"]], M=i[["MNA_t"]], W=i[["wt_t"]], C=i[["CDD_t"]], X=i[["sex"]],
           S = i[["subadult"]], A=i[["adult"]], G=i[["preg"]], L=i[["lac"]],maximum=T)$objective})

min1<- apply(PHDI_matrix, 1, function(i) {
  optimize(minmax, c(-.5, -.3),  D1 = i[["day.of.year_1_t"]], D2 = i[["day.of.year_2_t"]], D3 = i[["day.of.year_3_t"]],
           D4 = i[["day.of.year_4_t"]], D5 = i[["day.of.year_5_t"]], D6 = i[["day.of.year_6_t"]], D7 = i[["day.of.year_7_t"]],
           D8 = i[["day.of.year_8_t"]], D9 = i[["day.of.year_9_t"]], D10 = i[["day.of.year_10_t"]], D11 = i[["day.of.year_11_t"]],
           D12 = i[["day.of.year_12_t"]], D13 = i[["day.of.year_13_t"]], D14 = i[["day.of.year_14_t"]], D15 = i[["day.of.year_15_t"]], 
           D16 = i[["day.of.year_16_t"]], D17 = i[["day.of.year_17_t"]], M=i[["MNA_t"]], W=i[["wt_t"]], C=i[["CDD_t"]], X=i[["sex"]],
           S = i[["subadult"]], A=i[["adult"]], G=i[["preg"]], L=i[["lac"]],maximum=F)$objective})

min2<- apply(PHDI_matrix, 1, function(i) {
  optimize(minmax, c(-.3,-.1),   D1 = i[["day.of.year_1_t"]], D2 = i[["day.of.year_2_t"]], D3 = i[["day.of.year_3_t"]],
           D4 = i[["day.of.year_4_t"]], D5 = i[["day.of.year_5_t"]], D6 = i[["day.of.year_6_t"]], D7 = i[["day.of.year_7_t"]],
           D8 = i[["day.of.year_8_t"]], D9 = i[["day.of.year_9_t"]], D10 = i[["day.of.year_10_t"]], D11 = i[["day.of.year_11_t"]],
           D12 = i[["day.of.year_12_t"]], D13 = i[["day.of.year_13_t"]], D14 = i[["day.of.year_14_t"]], D15 = i[["day.of.year_15_t"]], 
           D16 = i[["day.of.year_16_t"]], D17 = i[["day.of.year_17_t"]], M=i[["MNA_t"]], W=i[["wt_t"]], C=i[["CDD_t"]], X=i[["sex"]],
           S = i[["subadult"]], A=i[["adult"]], G=i[["preg"]], L=i[["lac"]],maximum=F)$objective})
min3<- apply(PHDI_matrix, 1, function(i) {
  optimize(minmax, c(-.1, .1),  D1 = i[["day.of.year_1_t"]], D2 = i[["day.of.year_2_t"]], D3 = i[["day.of.year_3_t"]],
           D4 = i[["day.of.year_4_t"]], D5 = i[["day.of.year_5_t"]], D6 = i[["day.of.year_6_t"]], D7 = i[["day.of.year_7_t"]],
           D8 = i[["day.of.year_8_t"]], D9 = i[["day.of.year_9_t"]], D10 = i[["day.of.year_10_t"]], D11 = i[["day.of.year_11_t"]],
           D12 = i[["day.of.year_12_t"]], D13 = i[["day.of.year_13_t"]], D14 = i[["day.of.year_14_t"]], D15 = i[["day.of.year_15_t"]], 
           D16 = i[["day.of.year_16_t"]], D17 = i[["day.of.year_17_t"]], M=i[["MNA_t"]], W=i[["wt_t"]], C=i[["CDD_t"]], X=i[["sex"]],
           S = i[["subadult"]], A=i[["adult"]], G=i[["preg"]], L=i[["lac"]],maximum=F)$objective})

min4<- apply(PHDI_matrix, 1, function(i) {
  optimize(minmax, c(.1, .3),   D1 = i[["day.of.year_1_t"]], D2 = i[["day.of.year_2_t"]], D3 = i[["day.of.year_3_t"]],
           D4 = i[["day.of.year_4_t"]], D5 = i[["day.of.year_5_t"]], D6 = i[["day.of.year_6_t"]], D7 = i[["day.of.year_7_t"]],
           D8 = i[["day.of.year_8_t"]], D9 = i[["day.of.year_9_t"]], D10 = i[["day.of.year_10_t"]], D11 = i[["day.of.year_11_t"]],
           D12 = i[["day.of.year_12_t"]], D13 = i[["day.of.year_13_t"]], D14 = i[["day.of.year_14_t"]], D15 = i[["day.of.year_15_t"]], 
           D16 = i[["day.of.year_16_t"]], D17 = i[["day.of.year_17_t"]], M=i[["MNA_t"]], W=i[["wt_t"]], C=i[["CDD_t"]], X=i[["sex"]],
           S = i[["subadult"]], A=i[["adult"]], G=i[["preg"]], L=i[["lac"]],maximum=F)$objective})

min5<- apply(PHDI_matrix, 1, function(i) {
  optimize(minmax, c(.3, .5),   D1 = i[["day.of.year_1_t"]], D2 = i[["day.of.year_2_t"]], D3 = i[["day.of.year_3_t"]],
           D4 = i[["day.of.year_4_t"]], D5 = i[["day.of.year_5_t"]], D6 = i[["day.of.year_6_t"]], D7 = i[["day.of.year_7_t"]],
           D8 = i[["day.of.year_8_t"]], D9 = i[["day.of.year_9_t"]], D10 = i[["day.of.year_10_t"]], D11 = i[["day.of.year_11_t"]],
           D12 = i[["day.of.year_12_t"]], D13 = i[["day.of.year_13_t"]], D14 = i[["day.of.year_14_t"]], D15 = i[["day.of.year_15_t"]], 
           D16 = i[["day.of.year_16_t"]], D17 = i[["day.of.year_17_t"]], M=i[["MNA_t"]], W=i[["wt_t"]], C=i[["CDD_t"]], X=i[["sex"]],
           S = i[["subadult"]], A=i[["adult"]], G=i[["preg"]], L=i[["lac"]],maximum=F)$objective})


PHDI_matrix_2 <- cbind(PHDI_matrix, max1)
PHDI_matrix_3<-cbind(PHDI_matrix_2,max2)
PHDI_matrix_4 <- cbind(PHDI_matrix_3, max3)
PHDI_matrix_5<-cbind(PHDI_matrix_4,max4)
PHDI_matrix_6<-cbind(PHDI_matrix_5,max5)
PHDI_matrix_7<-cbind(PHDI_matrix_6, min1)
PHDI_matrix_8<-cbind(PHDI_matrix_7,min2)
PHDI_matrix_9<-cbind(PHDI_matrix_8,min3)
PHDI_matrix_10<-cbind(PHDI_matrix_9,min4)
PHDI_matrix_11<-cbind(PHDI_matrix_10,min5)


absmax<-apply(PHDI_matrix_11[,26:30],1,max)
absmin<-apply(PHDI_matrix_11[,31:35],1,min)
PHDI_matrix_12<-cbind(PHDI_matrix,absmax)
PHDI_matrix_13<-cbind(PHDI_matrix_12, absmin)
diff<-absmax-absmin
PHDI_matrix_14<-cbind(PHDI_matrix_13,diff )

##save PHDI effect 
PHDI_peak_effect<-mean(diff)

##export csv
write.csv(PHDI_matrix_14,"PHDI_effect_new.csv" )

## graph PHDI effect over all conditions 
PHDI_effect<-as.data.frame(PHDI_matrix_14)
PHDI_effect<-read.csv("PHDI_effect.csv")

ggplot(PHDI_effect, aes(CDD, wt_t)) + geom_tile(aes(fill =  diff), 
            colour = "white") + scale_fill_distiller( palette = "Spectral",
              limits = c(min(0),max(CDD_effect$diff)))+facet_grid(.~MNA)+xlab("Lg: MNA, Sm: CDD")+ylab("weight") + theme_grey(base_size = 20)
               

## MNA effect
day.of.year_1_t<-rep(0.059471366,3630)
day.of.year_2_t<-rep(0.063876652,3630)
day.of.year_3_t<-rep(0.068281938,3630)
day.of.year_4_t<-rep(0.072687225,3630)
day.of.year_5_t<-rep(0.077092511,3630)
day.of.year_6_t<-rep(0.081497797,3630)
day.of.year_7_t<-rep(0.085903084,3630)
day.of.year_8_t<-rep(0.09030837,3630)
day.of.year_9_t<-rep(0.094713656,3630)
day.of.year_10_t<-rep(0.099118943,3630)
day.of.year_11_t<-rep(0.103524229,3630)
day.of.year_12_t<-rep(0.107929515,3630)
day.of.year_13_t<-rep(0.112334802,3630)
day.of.year_14_t<-rep(0.116740088,3630)
day.of.year_15_t<-rep(0.121145374,3630)
day.of.year_16_t<-rep(0.125550661,3630)
day.of.year_17_t<-rep(0.129955947,3630)
wt_t<-c(rep(-.5,726),rep(-.25,726),rep(0,726), rep(.25,726), rep(.5,726))

CDD_t<-rep(c(rep(-.5,66), rep(-.4,66),rep(-.3,66),rep(-.2,66),rep(-.1,66),rep(0,66),
             rep(.1,66),rep(.2,66),rep(.3,66),rep(.4,66),rep(.5,66)) ,5)

PHDI_t<-rep(c(rep(-.5,6), rep(-.4,6),rep(-.3,6),rep(-.2,6),rep(-.1,6),rep(0,6),
             rep(.1,6),rep(.2,6),rep(.3,6),rep(.4,6),rep(.5,6)) ,55)

sex<-rep(c(rep(0,3), rep(1,3)) ,605)
subadult<-rep(c(0,1,1) ,1210)
adult<-rep(c(0,0,1) ,1210)
preg<-rep(0,3630)
lac<-rep(0,3630)

MNA_matrix<-as.matrix(data.frame(day.of.year_1_t,day.of.year_2_t,day.of.year_3_t,day.of.year_4_t,
                                 day.of.year_5_t,day.of.year_6_t,day.of.year_7_t,day.of.year_8_t,
                                 day.of.year_9_t,day.of.year_10_t,day.of.year_11_t,day.of.year_12_t,
                                 day.of.year_13_t,day.of.year_14_t,day.of.year_15_t,day.of.year_16_t,
                                 day.of.year_17_t, PHDI_t, wt_t, delta.CDD_t, sex, subadult, adult, preg, lac))

## create minmax(MNA)
minmax <- function(P,D1,D2,D3,D4,D5,
                   D6,D7,D8,D9,D10,D11,
                   D12,D13,D14,D15,D16,
                   D17,x,W,C,X,S,A,G,L) {pred_1<-(10^(int 
                                              +(w1/
                                                  (1 + exp
                                                   (-(int1 + w11*D1 + w21*x + w31*W + w41*C 
                                                      + w51*P + w61*X + w71*S + w81*A + w91*L
                                                      + w101*G))))+
                                                (w2/
                                                   (1 + exp
                                                    (-(int2 + w12*D1 + w22*x + w32*W + w42*C
                                                       + w52*P + w62*X + w72*S + w82*A + w92*L
                                                       + w102*G))))+
                                                (w3/
                                                   (1 + exp
                                                    (-(int3 + w13*D1 + w23*x + w33*W + w43*C
                                                       + w53*P + w63*X + w73*S + w83*A + w93*L
                                                       + w103*G))))+
                                                (w4/
                                                   (1 + exp
                                                    (-(int4 + w14*D1 + w24*x + w34*W + w44*C 
                                                       + w54*P + w64*X + w74*S + w84*A + w94*L
                                                       + w104*G))))+
                                                (w5/
                                                   (1 + exp
                                                    (-(int5 + w15*D1 + w25*x + w35*W + w45*C 
                                                       + w55*P + w65*X + w75*S + w85*A + w95*L
                                                       + w105*G))))+
                                                (w6/
                                                   (1 + exp
                                                    (-(int6 + w16*D1 + w26*x + w36*W + w46*C 
                                                       + w56*P + w66*X + w76*S + w86*A + w96*L
                                                       + w106*G))))+
                                                (w7/
                                                   (1 + exp
                                                    (-(int7 + w17*D1 + w27*x + w37*W + w47*C
                                                       + w57*P + w67*X + w77*S + w87*A + w97*L
                                                       + w107*G))))+
                                                (w8/
                                                   (1 + exp
                                                    (-(int8 + w18*D1 + w28*x + w38*W + w48*C 
                                                       + w58*P + w68*X + w78*S + w88*A + w98*L
                                                       + w108*G))))+
                                                (w9/
                                                   (1 + exp
                                                    (-(int9 + w19*D1 + w29*x + w39*W + w49*C 
                                                       + w59*P + w69*X + w79*S + w89*A + w99*L
                                                       + w109*G))))+
                                                (w10/
                                                   (1 + exp
                                                    (-(int10 + w110*D1 + w210*x + w310*W + w410*C 
                                                       + w510*P + w610*X + w710*S + w810*A + w910*L
                                                       + w1010*G))))))
                   pred_2<-(10^(int 
                                +(w1/
                                    (1 + exp
                                     (-(int1 + w11*D2 + w21*x + w31*W + w41*C 
                                        + w51*P + w61*X + w71*S + w81*A + w91*L
                                        + w101*G))))+
                                  (w2/
                                     (1 + exp
                                      (-(int2 + w12*D2 + w22*x + w32*W + w42*C
                                         + w52*P + w62*X + w72*S + w82*A + w92*L
                                         + w102*G))))+
                                  (w3/
                                     (1 + exp
                                      (-(int3 + w13*D2 + w23*x + w33*W + w43*C
                                         + w53*P + w63*X + w73*S + w83*A + w93*L
                                         + w103*G))))+
                                  (w4/
                                     (1 + exp
                                      (-(int4 + w14*D2 + w24*x + w34*W + w44*C 
                                         + w54*P + w64*X + w74*S + w84*A + w94*L
                                         + w104*G))))+
                                  (w5/
                                     (1 + exp
                                      (-(int5 + w15*D2 + w25*x + w35*W + w45*C 
                                         + w55*P + w65*X + w75*S + w85*A + w95*L
                                         + w105*G))))+
                                  (w6/
                                     (1 + exp
                                      (-(int6 + w16*D2 + w26*x + w36*W + w46*C 
                                         + w56*P + w66*X + w76*S + w86*A + w96*L
                                         + w106*G))))+
                                  (w7/
                                     (1 + exp
                                      (-(int7 + w17*D2 + w27*x + w37*W + w47*C
                                         + w57*P + w67*X + w77*S + w87*A + w97*L
                                         + w107*G))))+
                                  (w8/
                                     (1 + exp
                                      (-(int8 + w18*D2 + w28*x + w38*W + w48*C 
                                         + w58*P + w68*X + w78*S + w88*A + w98*L
                                         + w108*G))))+
                                  (w9/
                                     (1 + exp
                                      (-(int9 + w19*D2 + w29*x + w39*W + w49*C 
                                         + w59*P + w69*X + w79*S + w89*A + w99*L
                                         + w109*G))))+
                                  (w10/
                                     (1 + exp
                                      (-(int10 + w110*D2 + w210*x + w310*W + w410*C 
                                         + w510*P + w610*X + w710*S + w810*A + w910*L
                                         + w1010*G))))))
                   pred_3<-(10^(int 
                                +(w1/
                                    (1 + exp
                                     (-(int1 + w11*D3 + w21*x + w31*W + w41*C 
                                        + w51*P + w61*X + w71*S + w81*A + w91*L
                                        + w101*G))))+
                                  (w2/
                                     (1 + exp
                                      (-(int2 + w12*D3 + w22*x + w32*W + w42*C
                                         + w52*P + w62*X + w72*S + w82*A + w92*L
                                         + w102*G))))+
                                  (w3/
                                     (1 + exp
                                      (-(int3 + w13*D3 + w23*x + w33*W + w43*C
                                         + w53*P + w63*X + w73*S + w83*A + w93*L
                                         + w103*G))))+
                                  (w4/
                                     (1 + exp
                                      (-(int4 + w14*D3 + w24*x + w34*W + w44*C 
                                         + w54*P + w64*X + w74*S + w84*A + w94*L
                                         + w104*G))))+
                                  (w5/
                                     (1 + exp
                                      (-(int5 + w15*D3 + w25*x + w35*W + w45*C 
                                         + w55*P + w65*X + w75*S + w85*A + w95*L
                                         + w105*G))))+
                                  (w6/
                                     (1 + exp
                                      (-(int6 + w16*D3 + w26*x + w36*W + w46*C 
                                         + w56*P + w66*X + w76*S + w86*A + w96*L
                                         + w106*G))))+
                                  (w7/
                                     (1 + exp
                                      (-(int7 + w17*D3 + w27*x + w37*W + w47*C
                                         + w57*P + w67*X + w77*S + w87*A + w97*L
                                         + w107*G))))+
                                  (w8/
                                     (1 + exp
                                      (-(int8 + w18*D3 + w28*x + w38*W + w48*C 
                                         + w58*P + w68*X + w78*S + w88*A + w98*L
                                         + w108*G))))+
                                  (w9/
                                     (1 + exp
                                      (-(int9 + w19*D3 + w29*x + w39*W + w49*C 
                                         + w59*P + w69*X + w79*S + w89*A + w99*L
                                         + w109*G))))+
                                  (w10/
                                     (1 + exp
                                      (-(int10 + w110*D3 + w210*x + w310*W + w410*C 
                                         + w510*P + w610*X + w710*S + w810*A + w910*L
                                         + w1010*G))))))
                   pred_4<-(10^(int 
                                +(w1/
                                    (1 + exp
                                     (-(int1 + w11*D4 + w21*x + w31*W + w41*C 
                                        + w51*P + w61*X + w71*S + w81*A + w91*L
                                        + w101*G))))+
                                  (w2/
                                     (1 + exp
                                      (-(int2 + w12*D4 + w22*x + w32*W + w42*C
                                         + w52*P + w62*X + w72*S + w82*A + w92*L
                                         + w102*G))))+
                                  (w3/
                                     (1 + exp
                                      (-(int3 + w13*D4 + w23*x + w33*W + w43*C
                                         + w53*P + w63*X + w73*S + w83*A + w93*L
                                         + w103*G))))+
                                  (w4/
                                     (1 + exp
                                      (-(int4 + w14*D4 + w24*x + w34*W + w44*C 
                                         + w54*P + w64*X + w74*S + w84*A + w94*L
                                         + w104*G))))+
                                  (w5/
                                     (1 + exp
                                      (-(int5 + w15*D4 + w25*x + w35*W + w45*C 
                                         + w55*P + w65*X + w75*S + w85*A + w95*L
                                         + w105*G))))+
                                  (w6/
                                     (1 + exp
                                      (-(int6 + w16*D4 + w26*x + w36*W + w46*C 
                                         + w56*P + w66*X + w76*S + w86*A + w96*L
                                         + w106*G))))+
                                  (w7/
                                     (1 + exp
                                      (-(int7 + w17*D4 + w27*x + w37*W + w47*C
                                         + w57*P + w67*X + w77*S + w87*A + w97*L
                                         + w107*G))))+
                                  (w8/
                                     (1 + exp
                                      (-(int8 + w18*D4 + w28*x + w38*W + w48*C 
                                         + w58*P + w68*X + w78*S + w88*A + w98*L
                                         + w108*G))))+
                                  (w9/
                                     (1 + exp
                                      (-(int9 + w19*D4 + w29*x + w39*W + w49*C 
                                         + w59*P + w69*X + w79*S + w89*A + w99*L
                                         + w109*G))))+
                                  (w10/
                                     (1 + exp
                                      (-(int10 + w110*D4 + w210*x + w310*W + w410*C 
                                         + w510*P + w610*X + w710*S + w810*A + w910*L
                                         + w1010*G))))))
                   pred_5<-(10^(int 
                                +(w1/
                                    (1 + exp
                                     (-(int1 + w11*D5 + w21*x + w31*W + w41*C 
                                        + w51*P + w61*X + w71*S + w81*A + w91*L
                                        + w101*G))))+
                                  (w2/
                                     (1 + exp
                                      (-(int2 + w12*D5 + w22*x + w32*W + w42*C
                                         + w52*P + w62*X + w72*S + w82*A + w92*L
                                         + w102*G))))+
                                  (w3/
                                     (1 + exp
                                      (-(int3 + w13*D5 + w23*x + w33*W + w43*C
                                         + w53*P + w63*X + w73*S + w83*A + w93*L
                                         + w103*G))))+
                                  (w4/
                                     (1 + exp
                                      (-(int4 + w14*D5 + w24*x + w34*W + w44*C 
                                         + w54*P + w64*X + w74*S + w84*A + w94*L
                                         + w104*G))))+
                                  (w5/
                                     (1 + exp
                                      (-(int5 + w15*D5 + w25*x + w35*W + w45*C 
                                         + w55*P + w65*X + w75*S + w85*A + w95*L
                                         + w105*G))))+
                                  (w6/
                                     (1 + exp
                                      (-(int6 + w16*D5 + w26*x + w36*W + w46*C 
                                         + w56*P + w66*X + w76*S + w86*A + w96*L
                                         + w106*G))))+
                                  (w7/
                                     (1 + exp
                                      (-(int7 + w17*D5 + w27*x + w37*W + w47*C
                                         + w57*P + w67*X + w77*S + w87*A + w97*L
                                         + w107*G))))+
                                  (w8/
                                     (1 + exp
                                      (-(int8 + w18*D5 + w28*x + w38*W + w48*C 
                                         + w58*P + w68*X + w78*S + w88*A + w98*L
                                         + w108*G))))+
                                  (w9/
                                     (1 + exp
                                      (-(int9 + w19*D5 + w29*x + w39*W + w49*C 
                                         + w59*P + w69*X + w79*S + w89*A + w99*L
                                         + w109*G))))+
                                  (w10/
                                     (1 + exp
                                      (-(int10 + w110*D5 + w210*x + w310*W + w410*C 
                                         + w510*P + w610*X + w710*S + w810*A + w910*L
                                         + w1010*G))))))
                   pred_6<-(10^(int 
                                +(w1/
                                    (1 + exp
                                     (-(int1 + w11*D6 + w21*x + w31*W + w41*C 
                                        + w51*P + w61*X + w71*S + w81*A + w91*L
                                        + w101*G))))+
                                  (w2/
                                     (1 + exp
                                      (-(int2 + w12*D6 + w22*x + w32*W + w42*C
                                         + w52*P + w62*X + w72*S + w82*A + w92*L
                                         + w102*G))))+
                                  (w3/
                                     (1 + exp
                                      (-(int3 + w13*D6 + w23*x + w33*W + w43*C
                                         + w53*P + w63*X + w73*S + w83*A + w93*L
                                         + w103*G))))+
                                  (w4/
                                     (1 + exp
                                      (-(int4 + w14*D6 + w24*x + w34*W + w44*C 
                                         + w54*P + w64*X + w74*S + w84*A + w94*L
                                         + w104*G))))+
                                  (w5/
                                     (1 + exp
                                      (-(int5 + w15*D6 + w25*x + w35*W + w45*C 
                                         + w55*P + w65*X + w75*S + w85*A + w95*L
                                         + w105*G))))+
                                  (w6/
                                     (1 + exp
                                      (-(int6 + w16*D6 + w26*x + w36*W + w46*C 
                                         + w56*P + w66*X + w76*S + w86*A + w96*L
                                         + w106*G))))+
                                  (w7/
                                     (1 + exp
                                      (-(int7 + w17*D6 + w27*x + w37*W + w47*C
                                         + w57*P + w67*X + w77*S + w87*A + w97*L
                                         + w107*G))))+
                                  (w8/
                                     (1 + exp
                                      (-(int8 + w18*D6 + w28*x + w38*W + w48*C 
                                         + w58*P + w68*X + w78*S + w88*A + w98*L
                                         + w108*G))))+
                                  (w9/
                                     (1 + exp
                                      (-(int9 + w19*D6 + w29*x + w39*W + w49*C 
                                         + w59*P + w69*X + w79*S + w89*A + w99*L
                                         + w109*G))))+
                                  (w10/
                                     (1 + exp
                                      (-(int10 + w110*D6 + w210*x + w310*W + w410*C 
                                         + w510*P + w610*X + w710*S + w810*A + w910*L
                                         + w1010*G))))))
                   pred_7<-(10^(int 
                                +(w1/
                                    (1 + exp
                                     (-(int1 + w11*D7 + w21*x + w31*W + w41*C 
                                        + w51*P + w61*X + w71*S + w81*A + w91*L
                                        + w101*G))))+
                                  (w2/
                                     (1 + exp
                                      (-(int2 + w12*D7 + w22*x + w32*W + w42*C
                                         + w52*P + w62*X + w72*S + w82*A + w92*L
                                         + w102*G))))+
                                  (w3/
                                     (1 + exp
                                      (-(int3 + w13*D7 + w23*x + w33*W + w43*C
                                         + w53*P + w63*X + w73*S + w83*A + w93*L
                                         + w103*G))))+
                                  (w4/
                                     (1 + exp
                                      (-(int4 + w14*D7 + w24*x + w34*W + w44*C 
                                         + w54*P + w64*X + w74*S + w84*A + w94*L
                                         + w104*G))))+
                                  (w5/
                                     (1 + exp
                                      (-(int5 + w15*D7 + w25*x + w35*W + w45*C 
                                         + w55*P + w65*X + w75*S + w85*A + w95*L
                                         + w105*G))))+
                                  (w6/
                                     (1 + exp
                                      (-(int6 + w16*D7 + w26*x + w36*W + w46*C 
                                         + w56*P + w66*X + w76*S + w86*A + w96*L
                                         + w106*G))))+
                                  (w7/
                                     (1 + exp
                                      (-(int7 + w17*D7 + w27*x + w37*W + w47*C
                                         + w57*P + w67*X + w77*S + w87*A + w97*L
                                         + w107*G))))+
                                  (w8/
                                     (1 + exp
                                      (-(int8 + w18*D7 + w28*x + w38*W + w48*C 
                                         + w58*P + w68*X + w78*S + w88*A + w98*L
                                         + w108*G))))+
                                  (w9/
                                     (1 + exp
                                      (-(int9 + w19*D7 + w29*x + w39*W + w49*C 
                                         + w59*P + w69*X + w79*S + w89*A + w99*L
                                         + w109*G))))+
                                  (w10/
                                     (1 + exp
                                      (-(int10 + w110*D7 + w210*x + w310*W + w410*C 
                                         + w510*P + w610*X + w710*S + w810*A + w910*L
                                         + w1010*G))))))
                   pred_8<-(10^(int 
                                +(w1/
                                    (1 + exp
                                     (-(int1 + w11*D8 + w21*x + w31*W + w41*C 
                                        + w51*P + w61*X + w71*S + w81*A + w91*L
                                        + w101*G))))+
                                  (w2/
                                     (1 + exp
                                      (-(int2 + w12*D8 + w22*x + w32*W + w42*C
                                         + w52*P + w62*X + w72*S + w82*A + w92*L
                                         + w102*G))))+
                                  (w3/
                                     (1 + exp
                                      (-(int3 + w13*D8 + w23*x + w33*W + w43*C
                                         + w53*P + w63*X + w73*S + w83*A + w93*L
                                         + w103*G))))+
                                  (w4/
                                     (1 + exp
                                      (-(int4 + w14*D8 + w24*x + w34*W + w44*C 
                                         + w54*P + w64*X + w74*S + w84*A + w94*L
                                         + w104*G))))+
                                  (w5/
                                     (1 + exp
                                      (-(int5 + w15*D8 + w25*x + w35*W + w45*C 
                                         + w55*P + w65*X + w75*S + w85*A + w95*L
                                         + w105*G))))+
                                  (w6/
                                     (1 + exp
                                      (-(int6 + w16*D8 + w26*x + w36*W + w46*C 
                                         + w56*P + w66*X + w76*S + w86*A + w96*L
                                         + w106*G))))+
                                  (w7/
                                     (1 + exp
                                      (-(int7 + w17*D8 + w27*x + w37*W + w47*C
                                         + w57*P + w67*X + w77*S + w87*A + w97*L
                                         + w107*G))))+
                                  (w8/
                                     (1 + exp
                                      (-(int8 + w18*D8 + w28*x + w38*W + w48*C 
                                         + w58*P + w68*X + w78*S + w88*A + w98*L
                                         + w108*G))))+
                                  (w9/
                                     (1 + exp
                                      (-(int9 + w19*D8 + w29*x + w39*W + w49*C 
                                         + w59*P + w69*X + w79*S + w89*A + w99*L
                                         + w109*G))))+
                                  (w10/
                                     (1 + exp
                                      (-(int10 + w110*D8+ w210*x + w310*W + w410*C 
                                         + w510*P + w610*X + w710*S + w810*A + w910*L
                                         + w1010*G))))))
                   pred_9<-(10^(int 
                                +(w1/
                                    (1 + exp
                                     (-(int1 + w11*D9 + w21*x + w31*W + w41*C 
                                        + w51*P + w61*X + w71*S + w81*A + w91*L
                                        + w101*G))))+
                                  (w2/
                                     (1 + exp
                                      (-(int2 + w12*D9 + w22*x + w32*W + w42*C
                                         + w52*P + w62*X + w72*S + w82*A + w92*L
                                         + w102*G))))+
                                  (w3/
                                     (1 + exp
                                      (-(int3 + w13*D9 + w23*x + w33*W + w43*C
                                         + w53*P + w63*X + w73*S + w83*A + w93*L
                                         + w103*G))))+
                                  (w4/
                                     (1 + exp
                                      (-(int4 + w14*D9 + w24*x + w34*W + w44*C 
                                         + w54*P + w64*X + w74*S + w84*A + w94*L
                                         + w104*G))))+
                                  (w5/
                                     (1 + exp
                                      (-(int5 + w15*D9 + w25*x + w35*W + w45*C 
                                         + w55*P + w65*X + w75*S + w85*A + w95*L
                                         + w105*G))))+
                                  (w6/
                                     (1 + exp
                                      (-(int6 + w16*D9 + w26*x + w36*W + w46*C 
                                         + w56*P + w66*X + w76*S + w86*A + w96*L
                                         + w106*G))))+
                                  (w7/
                                     (1 + exp
                                      (-(int7 + w17*D9 + w27*x + w37*W + w47*C
                                         + w57*P + w67*X + w77*S + w87*A + w97*L
                                         + w107*G))))+
                                  (w8/
                                     (1 + exp
                                      (-(int8 + w18*D9 + w28*x + w38*W + w48*C 
                                         + w58*P + w68*X + w78*S + w88*A + w98*L
                                         + w108*G))))+
                                  (w9/
                                     (1 + exp
                                      (-(int9 + w19*D9 + w29*x + w39*W + w49*C 
                                         + w59*P + w69*X + w79*S + w89*A + w99*L
                                         + w109*G))))+
                                  (w10/
                                     (1 + exp
                                      (-(int10 + w110*D9 + w210*x + w310*W + w410*C 
                                         + w510*P + w610*X + w710*S + w810*A + w910*L
                                         + w1010*G))))))
                   pred_10<-(10^(int 
                                +(w1/
                                    (1 + exp
                                     (-(int1 + w11*D10 + w21*x + w31*W + w41*C 
                                        + w51*P + w61*X + w71*S + w81*A + w91*L
                                        + w101*G))))+
                                  (w2/
                                     (1 + exp
                                      (-(int2 + w12*D10 + w22*x + w32*W + w42*C
                                         + w52*P + w62*X + w72*S + w82*A + w92*L
                                         + w102*G))))+
                                  (w3/
                                     (1 + exp
                                      (-(int3 + w13*D10 + w23*x + w33*W + w43*C
                                         + w53*P + w63*X + w73*S + w83*A + w93*L
                                         + w103*G))))+
                                  (w4/
                                     (1 + exp
                                      (-(int4 + w14*D10 + w24*x + w34*W + w44*C 
                                         + w54*P + w64*X + w74*S + w84*A + w94*L
                                         + w104*G))))+
                                  (w5/
                                     (1 + exp
                                      (-(int5 + w15*D10 + w25*x + w35*W + w45*C 
                                         + w55*P + w65*X + w75*S + w85*A + w95*L
                                         + w105*G))))+
                                  (w6/
                                     (1 + exp
                                      (-(int6 + w16*D10 + w26*x + w36*W + w46*C 
                                         + w56*P + w66*X + w76*S + w86*A + w96*L
                                         + w106*G))))+
                                  (w7/
                                     (1 + exp
                                      (-(int7 + w17*D10 + w27*x + w37*W + w47*C
                                         + w57*P + w67*X + w77*S + w87*A + w97*L
                                         + w107*G))))+
                                  (w8/
                                     (1 + exp
                                      (-(int8 + w18*D10 + w28*x + w38*W + w48*C 
                                         + w58*P + w68*X + w78*S + w88*A + w98*L
                                         + w108*G))))+
                                  (w9/
                                     (1 + exp
                                      (-(int9 + w19*D10 + w29*x + w39*W + w49*C 
                                         + w59*P + w69*X + w79*S + w89*A + w99*L
                                         + w109*G))))+
                                  (w10/
                                     (1 + exp
                                      (-(int10 + w110*D10 + w210*x + w310*W + w410*C 
                                         + w510*P + w610*X + w710*S + w810*A + w910*L
                                         + w1010*G))))))
                   pred_11<-(10^(int 
                                +(w1/
                                    (1 + exp
                                     (-(int1 + w11*D11 + w21*x + w31*W + w41*C 
                                        + w51*P + w61*X + w71*S + w81*A + w91*L
                                        + w101*G))))+
                                  (w2/
                                     (1 + exp
                                      (-(int2 + w12*D11 + w22*x + w32*W + w42*C
                                         + w52*P + w62*X + w72*S + w82*A + w92*L
                                         + w102*G))))+
                                  (w3/
                                     (1 + exp
                                      (-(int3 + w13*D11 + w23*x + w33*W + w43*C
                                         + w53*P + w63*X + w73*S + w83*A + w93*L
                                         + w103*G))))+
                                  (w4/
                                     (1 + exp
                                      (-(int4 + w14*D11 + w24*x + w34*W + w44*C 
                                         + w54*P + w64*X + w74*S + w84*A + w94*L
                                         + w104*G))))+
                                  (w5/
                                     (1 + exp
                                      (-(int5 + w15*D11 + w25*x + w35*W + w45*C 
                                         + w55*P + w65*X + w75*S + w85*A + w95*L
                                         + w105*G))))+
                                  (w6/
                                     (1 + exp
                                      (-(int6 + w16*D11 + w26*x + w36*W + w46*C 
                                         + w56*P + w66*X + w76*S + w86*A + w96*L
                                         + w106*G))))+
                                  (w7/
                                     (1 + exp
                                      (-(int7 + w17*D11 + w27*x + w37*W + w47*C
                                         + w57*P + w67*X + w77*S + w87*A + w97*L
                                         + w107*G))))+
                                  (w8/
                                     (1 + exp
                                      (-(int8 + w18*D11 + w28*x + w38*W + w48*C 
                                         + w58*P + w68*X + w78*S + w88*A + w98*L
                                         + w108*G))))+
                                  (w9/
                                     (1 + exp
                                      (-(int9 + w19*D11 + w29*x + w39*W + w49*C 
                                         + w59*P + w69*X + w79*S + w89*A + w99*L
                                         + w109*G))))+
                                  (w10/
                                     (1 + exp
                                      (-(int10 + w110*D11 + w210*x + w310*W + w410*C 
                                         + w510*P + w610*X + w710*S + w810*A + w910*L
                                         + w1010*G))))))
                   pred_12<-(10^(int 
                                +(w1/
                                    (1 + exp
                                     (-(int1 + w11*D12 + w21*x + w31*W + w41*C 
                                        + w51*P + w61*X + w71*S + w81*A + w91*L
                                        + w101*G))))+
                                  (w2/
                                     (1 + exp
                                      (-(int2 + w12*D12 + w22*x + w32*W + w42*C
                                         + w52*P + w62*X + w72*S + w82*A + w92*L
                                         + w102*G))))+
                                  (w3/
                                     (1 + exp
                                      (-(int3 + w13*D12 + w23*x + w33*W + w43*C
                                         + w53*P + w63*X + w73*S + w83*A + w93*L
                                         + w103*G))))+
                                  (w4/
                                     (1 + exp
                                      (-(int4 + w14*D12 + w24*x + w34*W + w44*C 
                                         + w54*P + w64*X + w74*S + w84*A + w94*L
                                         + w104*G))))+
                                  (w5/
                                     (1 + exp
                                      (-(int5 + w15*D12 + w25*x + w35*W + w45*C 
                                         + w55*P + w65*X + w75*S + w85*A + w95*L
                                         + w105*G))))+
                                  (w6/
                                     (1 + exp
                                      (-(int6 + w16*D12 + w26*x + w36*W + w46*C 
                                         + w56*P + w66*X + w76*S + w86*A + w96*L
                                         + w106*G))))+
                                  (w7/
                                     (1 + exp
                                      (-(int7 + w17*D12 + w27*x + w37*W + w47*C
                                         + w57*P + w67*X + w77*S + w87*A + w97*L
                                         + w107*G))))+
                                  (w8/
                                     (1 + exp
                                      (-(int8 + w18*D12 + w28*x + w38*W + w48*C 
                                         + w58*P + w68*X + w78*S + w88*A + w98*L
                                         + w108*G))))+
                                  (w9/
                                     (1 + exp
                                      (-(int9 + w19*D12 + w29*x + w39*W + w49*C 
                                         + w59*P + w69*X + w79*S + w89*A + w99*L
                                         + w109*G))))+
                                  (w10/
                                     (1 + exp
                                      (-(int10 + w110*D12 + w210*x + w310*W + w410*C 
                                         + w510*P + w610*X + w710*S + w810*A + w910*L
                                         + w1010*G))))))
                   pred_13<-(10^(int 
                                +(w1/
                                    (1 + exp
                                     (-(int1 + w11*D13 + w21*x + w31*W + w41*C 
                                        + w51*P + w61*X + w71*S + w81*A + w91*L
                                        + w101*G))))+
                                  (w2/
                                     (1 + exp
                                      (-(int2 + w12*D13 + w22*x + w32*W + w42*C
                                         + w52*P + w62*X + w72*S + w82*A + w92*L
                                         + w102*G))))+
                                  (w3/
                                     (1 + exp
                                      (-(int3 + w13*D13 + w23*x + w33*W + w43*C
                                         + w53*P + w63*X + w73*S + w83*A + w93*L
                                         + w103*G))))+
                                  (w4/
                                     (1 + exp
                                      (-(int4 + w14*D13 + w24*x + w34*W + w44*C 
                                         + w54*P + w64*X + w74*S + w84*A + w94*L
                                         + w104*G))))+
                                  (w5/
                                     (1 + exp
                                      (-(int5 + w15*D13 + w25*x + w35*W + w45*C 
                                         + w55*P + w65*X + w75*S + w85*A + w95*L
                                         + w105*G))))+
                                  (w6/
                                     (1 + exp
                                      (-(int6 + w16*D13 + w26*x + w36*W + w46*C 
                                         + w56*P + w66*X + w76*S + w86*A + w96*L
                                         + w106*G))))+
                                  (w7/
                                     (1 + exp
                                      (-(int7 + w17*D13 + w27*x + w37*W + w47*C
                                         + w57*P + w67*X + w77*S + w87*A + w97*L
                                         + w107*G))))+
                                  (w8/
                                     (1 + exp
                                      (-(int8 + w18*D13 + w28*x + w38*W + w48*C 
                                         + w58*P + w68*X + w78*S + w88*A + w98*L
                                         + w108*G))))+
                                  (w9/
                                     (1 + exp
                                      (-(int9 + w19*D13 + w29*x + w39*W + w49*C 
                                         + w59*P + w69*X + w79*S + w89*A + w99*L
                                         + w109*G))))+
                                  (w10/
                                     (1 + exp
                                      (-(int10 + w110*D13 + w210*x + w310*W + w410*C 
                                         + w510*P + w610*X + w710*S + w810*A + w910*L
                                         + w1010*G))))))
                   pred_14<-(10^(int 
                                +(w1/
                                    (1 + exp
                                     (-(int1 + w11*D14 + w21*x + w31*W + w41*C 
                                        + w51*P + w61*X + w71*S + w81*A + w91*L
                                        + w101*G))))+
                                  (w2/
                                     (1 + exp
                                      (-(int2 + w12*D14 + w22*x + w32*W + w42*C
                                         + w52*P + w62*X + w72*S + w82*A + w92*L
                                         + w102*G))))+
                                  (w3/
                                     (1 + exp
                                      (-(int3 + w13*D14 + w23*x + w33*W + w43*C
                                         + w53*P + w63*X + w73*S + w83*A + w93*L
                                         + w103*G))))+
                                  (w4/
                                     (1 + exp
                                      (-(int4 + w14*D14 + w24*x + w34*W + w44*C 
                                         + w54*P + w64*X + w74*S + w84*A + w94*L
                                         + w104*G))))+
                                  (w5/
                                     (1 + exp
                                      (-(int5 + w15*D14 + w25*x + w35*W + w45*C 
                                         + w55*P + w65*X + w75*S + w85*A + w95*L
                                         + w105*G))))+
                                  (w6/
                                     (1 + exp
                                      (-(int6 + w16*D14 + w26*x + w36*W + w46*C 
                                         + w56*P + w66*X + w76*S + w86*A + w96*L
                                         + w106*G))))+
                                  (w7/
                                     (1 + exp
                                      (-(int7 + w17*D14 + w27*x + w37*W + w47*C
                                         + w57*P + w67*X + w77*S + w87*A + w97*L
                                         + w107*G))))+
                                  (w8/
                                     (1 + exp
                                      (-(int8 + w18*D14 + w28*x + w38*W + w48*C 
                                         + w58*P + w68*X + w78*S + w88*A + w98*L
                                         + w108*G))))+
                                  (w9/
                                     (1 + exp
                                      (-(int9 + w19*D14 + w29*x + w39*W + w49*C 
                                         + w59*P + w69*X + w79*S + w89*A + w99*L
                                         + w109*G))))+
                                  (w10/
                                     (1 + exp
                                      (-(int10 + w110*D14 + w210*x + w310*W + w410*C 
                                         + w510*P + w610*X + w710*S + w810*A + w910*L
                                         + w1010*G))))))
                   pred_15<-(10^(int 
                                +(w1/
                                    (1 + exp
                                     (-(int1 + w11*D15 + w21*x + w31*W + w41*C 
                                        + w51*P + w61*X + w71*S + w81*A + w91*L
                                        + w101*G))))+
                                  (w2/
                                     (1 + exp
                                      (-(int2 + w12*D15 + w22*x + w32*W + w42*C
                                         + w52*P + w62*X + w72*S + w82*A + w92*L
                                         + w102*G))))+
                                  (w3/
                                     (1 + exp
                                      (-(int3 + w13*D15 + w23*x + w33*W + w43*C
                                         + w53*P + w63*X + w73*S + w83*A + w93*L
                                         + w103*G))))+
                                  (w4/
                                     (1 + exp
                                      (-(int4 + w14*D15 + w24*x + w34*W + w44*C 
                                         + w54*P + w64*X + w74*S + w84*A + w94*L
                                         + w104*G))))+
                                  (w5/
                                     (1 + exp
                                      (-(int5 + w15*D15 + w25*x + w35*W + w45*C 
                                         + w55*P + w65*X + w75*S + w85*A + w95*L
                                         + w105*G))))+
                                  (w6/
                                     (1 + exp
                                      (-(int6 + w16*D15 + w26*x + w36*W + w46*C 
                                         + w56*P + w66*X + w76*S + w86*A + w96*L
                                         + w106*G))))+
                                  (w7/
                                     (1 + exp
                                      (-(int7 + w17*D15 + w27*x + w37*W + w47*C
                                         + w57*P + w67*X + w77*S + w87*A + w97*L
                                         + w107*G))))+
                                  (w8/
                                     (1 + exp
                                      (-(int8 + w18*D15 + w28*x + w38*W + w48*C 
                                         + w58*P + w68*X + w78*S + w88*A + w98*L
                                         + w108*G))))+
                                  (w9/
                                     (1 + exp
                                      (-(int9 + w19*D15 + w29*x + w39*W + w49*C 
                                         + w59*P + w69*X + w79*S + w89*A + w99*L
                                         + w109*G))))+
                                  (w10/
                                     (1 + exp
                                      (-(int10 + w110*D15 + w210*x + w310*W + w410*C 
                                         + w510*P + w610*X + w710*S + w810*A + w910*L
                                         + w1010*G))))))
                   pred_16<-(10^(int 
                                +(w1/
                                    (1 + exp
                                     (-(int1 + w11*D16 + w21*x + w31*W + w41*C 
                                        + w51*P + w61*X + w71*S + w81*A + w91*L
                                        + w101*G))))+
                                  (w2/
                                     (1 + exp
                                      (-(int2 + w12*D16 + w22*x + w32*W + w42*C
                                         + w52*P + w62*X + w72*S + w82*A + w92*L
                                         + w102*G))))+
                                  (w3/
                                     (1 + exp
                                      (-(int3 + w13*D16 + w23*x + w33*W + w43*C
                                         + w53*P + w63*X + w73*S + w83*A + w93*L
                                         + w103*G))))+
                                  (w4/
                                     (1 + exp
                                      (-(int4 + w14*D16 + w24*x + w34*W + w44*C 
                                         + w54*P + w64*X + w74*S + w84*A + w94*L
                                         + w104*G))))+
                                  (w5/
                                     (1 + exp
                                      (-(int5 + w15*D16 + w25*x + w35*W + w45*C 
                                         + w55*P + w65*X + w75*S + w85*A + w95*L
                                         + w105*G))))+
                                  (w6/
                                     (1 + exp
                                      (-(int6 + w16*D16 + w26*x + w36*W + w46*C 
                                         + w56*P + w66*X + w76*S + w86*A + w96*L
                                         + w106*G))))+
                                  (w7/
                                     (1 + exp
                                      (-(int7 + w17*D16 + w27*x + w37*W + w47*C
                                         + w57*P + w67*X + w77*S + w87*A + w97*L
                                         + w107*G))))+
                                  (w8/
                                     (1 + exp
                                      (-(int8 + w18*D16 + w28*x + w38*W + w48*C 
                                         + w58*P + w68*X + w78*S + w88*A + w98*L
                                         + w108*G))))+
                                  (w9/
                                     (1 + exp
                                      (-(int9 + w19*D16 + w29*x + w39*W + w49*C 
                                         + w59*P + w69*X + w79*S + w89*A + w99*L
                                         + w109*G))))+
                                  (w10/
                                     (1 + exp
                                      (-(int10 + w110*D16 + w210*x + w310*W + w410*C 
                                         + w510*P + w610*X + w710*S + w810*A + w910*L
                                         + w1010*G))))))
                   pred_17<-(10^(int 
                                +(w1/
                                    (1 + exp
                                     (-(int1 + w11*D17 + w21*x + w31*W + w41*C 
                                        + w51*P + w61*X + w71*S + w81*A + w91*L
                                        + w101*G))))+
                                  (w2/
                                     (1 + exp
                                      (-(int2 + w12*D17 + w22*x + w32*W + w42*C
                                         + w52*P + w62*X + w72*S + w82*A + w92*L
                                         + w102*G))))+
                                  (w3/
                                     (1 + exp
                                      (-(int3 + w13*D17 + w23*x + w33*W + w43*C
                                         + w53*P + w63*X + w73*S + w83*A + w93*L
                                         + w103*G))))+
                                  (w4/
                                     (1 + exp
                                      (-(int4 + w14*D17 + w24*x + w34*W + w44*C 
                                         + w54*P + w64*X + w74*S + w84*A + w94*L
                                         + w104*G))))+
                                  (w5/
                                     (1 + exp
                                      (-(int5 + w15*D17 + w25*x + w35*W + w45*C 
                                         + w55*P + w65*X + w75*S + w85*A + w95*L
                                         + w105*G))))+
                                  (w6/
                                     (1 + exp
                                      (-(int6 + w16*D17 + w26*x + w36*W + w46*C 
                                         + w56*P + w66*X + w76*S + w86*A + w96*L
                                         + w106*G))))+
                                  (w7/
                                     (1 + exp
                                      (-(int7 + w17*D17 + w27*x + w37*W + w47*C
                                         + w57*P + w67*X + w77*S + w87*A + w97*L
                                         + w107*G))))+
                                  (w8/
                                     (1 + exp
                                      (-(int8 + w18*D17 + w28*x + w38*W + w48*C 
                                         + w58*P + w68*X + w78*S + w88*A + w98*L
                                         + w108*G))))+
                                  (w9/
                                     (1 + exp
                                      (-(int9 + w19*D17 + w29*x + w39*W + w49*C 
                                         + w59*P + w69*X + w79*S + w89*A + w99*L
                                         + w109*G))))+
                                  (w10/
                                     (1 + exp
                                      (-(int10 + w110*D17 + w210*x + w310*W + w410*C 
                                         + w510*P + w610*X + w710*S + w810*A + w910*L
                                         + w1010*G)))))) 
                                mean(pred_1, pred_2, pred_3, pred_4, pred_5, pred_6, pred_7,
                                pred_8, pred_9, pred_10, pred_11, pred_12, pred_13,
                                pred_14, pred_15, pred_16, pred_17)}
              

##apply minmax(MNA)
max1<- apply(MNA_matrix, 1, function(i) {
  optimize(minmax, c(-.5, -.3),  D1 = i[["day.of.year_1_t"]], D2 = i[["day.of.year_2_t"]], D3 = i[["day.of.year_3_t"]],
           D4 = i[["day.of.year_4_t"]], D5 = i[["day.of.year_5_t"]], D6 = i[["day.of.year_6_t"]], D7 = i[["day.of.year_7_t"]],
           D8 = i[["day.of.year_8_t"]], D9 = i[["day.of.year_9_t"]], D10 = i[["day.of.year_10_t"]], D11 = i[["day.of.year_11_t"]],
           D12 = i[["day.of.year_12_t"]], D13 = i[["day.of.year_13_t"]], D14 = i[["day.of.year_14_t"]], D15 = i[["day.of.year_15_t"]], 
           D16 = i[["day.of.year_16_t"]], D17 = i[["day.of.year_17_t"]], P=i[["PHDI_t"]], W=i[["wt_t"]], C=i[["CDD_t"]], X=i[["sex"]],
           S = i[["subadult"]], A=i[["adult"]], G=i[["preg"]], L=i[["lac"]],maximum=T)$objective})

max2<- apply(MNA_matrix, 1, function(i) {
  optimize(minmax, c(-.3, -.1),  D1 = i[["day.of.year_1_t"]], D2 = i[["day.of.year_2_t"]], D3 = i[["day.of.year_3_t"]],
           D4 = i[["day.of.year_4_t"]], D5 = i[["day.of.year_5_t"]], D6 = i[["day.of.year_6_t"]], D7 = i[["day.of.year_7_t"]],
           D8 = i[["day.of.year_8_t"]], D9 = i[["day.of.year_9_t"]], D10 = i[["day.of.year_10_t"]], D11 = i[["day.of.year_11_t"]],
           D12 = i[["day.of.year_12_t"]], D13 = i[["day.of.year_13_t"]], D14 = i[["day.of.year_14_t"]], D15 = i[["day.of.year_15_t"]], 
           D16 = i[["day.of.year_16_t"]], D17 = i[["day.of.year_17_t"]], P=i[["PHDI_t"]], W=i[["wt_t"]], C=i[["CDD_t"]], X=i[["sex"]],
           S = i[["subadult"]], A=i[["adult"]], G=i[["preg"]], L=i[["lac"]],maximum=T)$objective})


max3<- apply(MNA_matrix, 1, function(i) {
  optimize(minmax, c(-.1, .1),  D1 = i[["day.of.year_1_t"]], D2 = i[["day.of.year_2_t"]], D3 = i[["day.of.year_3_t"]],
           D4 = i[["day.of.year_4_t"]], D5 = i[["day.of.year_5_t"]], D6 = i[["day.of.year_6_t"]], D7 = i[["day.of.year_7_t"]],
           D8 = i[["day.of.year_8_t"]], D9 = i[["day.of.year_9_t"]], D10 = i[["day.of.year_10_t"]], D11 = i[["day.of.year_11_t"]],
           D12 = i[["day.of.year_12_t"]], D13 = i[["day.of.year_13_t"]], D14 = i[["day.of.year_14_t"]], D15 = i[["day.of.year_15_t"]], 
           D16 = i[["day.of.year_16_t"]], D17 = i[["day.of.year_17_t"]], P=i[["PHDI_t"]], W=i[["wt_t"]], C=i[["CDD_t"]], X=i[["sex"]],
           S = i[["subadult"]], A=i[["adult"]], G=i[["preg"]], L=i[["lac"]],maximum=T)$objective})


max4<- apply(MNA_matrix, 1, function(i) {
  optimize(minmax, c(.1, .3), D1 = i[["day.of.year_1_t"]], D2 = i[["day.of.year_2_t"]], D3 = i[["day.of.year_3_t"]],
           D4 = i[["day.of.year_4_t"]], D5 = i[["day.of.year_5_t"]], D6 = i[["day.of.year_6_t"]], D7 = i[["day.of.year_7_t"]],
           D8 = i[["day.of.year_8_t"]], D9 = i[["day.of.year_9_t"]], D10 = i[["day.of.year_10_t"]], D11 = i[["day.of.year_11_t"]],
           D12 = i[["day.of.year_12_t"]], D13 = i[["day.of.year_13_t"]], D14 = i[["day.of.year_14_t"]], D15 = i[["day.of.year_15_t"]], 
           D16 = i[["day.of.year_16_t"]], D17 = i[["day.of.year_17_t"]], P=i[["PHDI_t"]], W=i[["wt_t"]], C=i[["CDD_t"]], X=i[["sex"]],
           S = i[["subadult"]], A=i[["adult"]], G=i[["preg"]], L=i[["lac"]],maximum=T)$objective})

max5<- apply(MNA_matrix, 1, function(i) {
  optimize(minmax, c(.3, .5),  D1 = i[["day.of.year_1_t"]], D2 = i[["day.of.year_2_t"]], D3 = i[["day.of.year_3_t"]],
           D4 = i[["day.of.year_4_t"]], D5 = i[["day.of.year_5_t"]], D6 = i[["day.of.year_6_t"]], D7 = i[["day.of.year_7_t"]],
           D8 = i[["day.of.year_8_t"]], D9 = i[["day.of.year_9_t"]], D10 = i[["day.of.year_10_t"]], D11 = i[["day.of.year_11_t"]],
           D12 = i[["day.of.year_12_t"]], D13 = i[["day.of.year_13_t"]], D14 = i[["day.of.year_14_t"]], D15 = i[["day.of.year_15_t"]], 
           D16 = i[["day.of.year_16_t"]], D17 = i[["day.of.year_17_t"]], P=i[["PHDI_t"]], W=i[["wt_t"]], C=i[["CDD_t"]], X=i[["sex"]],
           S = i[["subadult"]], A=i[["adult"]], G=i[["preg"]], L=i[["lac"]],maximum=T)$objective})


min1<- apply(MNA_matrix, 1, function(i) {
  optimize(minmax, c(-.5, -.3), D1 = i[["day.of.year_1_t"]], D2 = i[["day.of.year_2_t"]], D3 = i[["day.of.year_3_t"]],
           D4 = i[["day.of.year_4_t"]], D5 = i[["day.of.year_5_t"]], D6 = i[["day.of.year_6_t"]], D7 = i[["day.of.year_7_t"]],
           D8 = i[["day.of.year_8_t"]], D9 = i[["day.of.year_9_t"]], D10 = i[["day.of.year_10_t"]], D11 = i[["day.of.year_11_t"]],
           D12 = i[["day.of.year_12_t"]], D13 = i[["day.of.year_13_t"]], D14 = i[["day.of.year_14_t"]], D15 = i[["day.of.year_15_t"]], 
           D16 = i[["day.of.year_16_t"]], D17 = i[["day.of.year_17_t"]], P=i[["PHDI_t"]], W=i[["wt_t"]], C=i[["CDD_t"]], X=i[["sex"]],
           S = i[["subadult"]], A=i[["adult"]], G=i[["preg"]], L=i[["lac"]],maximum=F)$objective})

min2<- apply(MNA_matrix, 1, function(i) {
  optimize(minmax, c(-.3,-.1),  D1 = i[["day.of.year_1_t"]], D2 = i[["day.of.year_2_t"]], D3 = i[["day.of.year_3_t"]],
           D4 = i[["day.of.year_4_t"]], D5 = i[["day.of.year_5_t"]], D6 = i[["day.of.year_6_t"]], D7 = i[["day.of.year_7_t"]],
           D8 = i[["day.of.year_8_t"]], D9 = i[["day.of.year_9_t"]], D10 = i[["day.of.year_10_t"]], D11 = i[["day.of.year_11_t"]],
           D12 = i[["day.of.year_12_t"]], D13 = i[["day.of.year_13_t"]], D14 = i[["day.of.year_14_t"]], D15 = i[["day.of.year_15_t"]], 
           D16 = i[["day.of.year_16_t"]], D17 = i[["day.of.year_17_t"]], P=i[["PHDI_t"]], W=i[["wt_t"]], C=i[["CDD_t"]], X=i[["sex"]],
           S = i[["subadult"]], A=i[["adult"]], G=i[["preg"]], L=i[["lac"]],maximum=F)$objective})
min3<- apply(MNA_matrix, 1, function(i) {
  optimize(minmax, c(-.1, .1),  D1 = i[["day.of.year_1_t"]], D2 = i[["day.of.year_2_t"]], D3 = i[["day.of.year_3_t"]],
           D4 = i[["day.of.year_4_t"]], D5 = i[["day.of.year_5_t"]], D6 = i[["day.of.year_6_t"]], D7 = i[["day.of.year_7_t"]],
           D8 = i[["day.of.year_8_t"]], D9 = i[["day.of.year_9_t"]], D10 = i[["day.of.year_10_t"]], D11 = i[["day.of.year_11_t"]],
           D12 = i[["day.of.year_12_t"]], D13 = i[["day.of.year_13_t"]], D14 = i[["day.of.year_14_t"]], D15 = i[["day.of.year_15_t"]], 
           D16 = i[["day.of.year_16_t"]], D17 = i[["day.of.year_17_t"]], P=i[["PHDI_t"]], W=i[["wt_t"]], C=i[["CDD_t"]], X=i[["sex"]],
           S = i[["subadult"]], A=i[["adult"]], G=i[["preg"]], L=i[["lac"]],maximum=F)$objective})

min4<- apply(MNA_matrix, 1, function(i) {
  optimize(minmax, c(.1, .3),  D1 = i[["day.of.year_1_t"]], D2 = i[["day.of.year_2_t"]], D3 = i[["day.of.year_3_t"]],
           D4 = i[["day.of.year_4_t"]], D5 = i[["day.of.year_5_t"]], D6 = i[["day.of.year_6_t"]], D7 = i[["day.of.year_7_t"]],
           D8 = i[["day.of.year_8_t"]], D9 = i[["day.of.year_9_t"]], D10 = i[["day.of.year_10_t"]], D11 = i[["day.of.year_11_t"]],
           D12 = i[["day.of.year_12_t"]], D13 = i[["day.of.year_13_t"]], D14 = i[["day.of.year_14_t"]], D15 = i[["day.of.year_15_t"]], 
           D16 = i[["day.of.year_16_t"]], D17 = i[["day.of.year_17_t"]], P=i[["PHDI_t"]], W=i[["wt_t"]], C=i[["CDD_t"]], X=i[["sex"]],
           S = i[["subadult"]], A=i[["adult"]], G=i[["preg"]], L=i[["lac"]],maximum=F)$objective})


min5<- apply(MNA_matrix, 1, function(i) {
  optimize(minmax, c(.3, .5),  D1 = i[["day.of.year_1_t"]], D2 = i[["day.of.year_2_t"]], D3 = i[["day.of.year_3_t"]],
           D4 = i[["day.of.year_4_t"]], D5 = i[["day.of.year_5_t"]], D6 = i[["day.of.year_6_t"]], D7 = i[["day.of.year_7_t"]],
           D8 = i[["day.of.year_8_t"]], D9 = i[["day.of.year_9_t"]], D10 = i[["day.of.year_10_t"]], D11 = i[["day.of.year_11_t"]],
           D12 = i[["day.of.year_12_t"]], D13 = i[["day.of.year_13_t"]], D14 = i[["day.of.year_14_t"]], D15 = i[["day.of.year_15_t"]], 
           D16 = i[["day.of.year_16_t"]], D17 = i[["day.of.year_17_t"]], P=i[["PHDI_t"]], W=i[["wt_t"]], C=i[["CDD_t"]], X=i[["sex"]],
           S = i[["subadult"]], A=i[["adult"]], G=i[["preg"]], L=i[["lac"]],maximum=F)$objective})


MNA_matrix_2 <- cbind(MNA_matrix, max1)
MNA_matrix_3<-cbind(MNA_matrix_2,max2)
MNA_matrix_4 <- cbind(MNA_matrix_3, max3)
MNA_matrix_5<-cbind(MNA_matrix_4,max4)
MNA_matrix_6<-cbind(MNA_matrix_5,max5)
MNA_matrix_7<-cbind(MNA_matrix_6, min1)
MNA_matrix_8<-cbind(MNA_matrix_7,min2)
MNA_matrix_9<-cbind(MNA_matrix_8,min3)
MNA_matrix_10<-cbind(MNA_matrix_9,min4)
MNA_matrix_11<-cbind(MNA_matrix_10,min5)


absmax<-apply(MNA_matrix_11[,26:30],1,max)
absmin<-apply(MNA_matrix_11[,31:35],1,min)
MNA_matrix_12<-cbind(MNA_matrix,absmax)
MNA_matrix_13<-cbind(MNA_matrix_12, absmin)
diff<-absmax-absmin
MNA_matrix_14<-cbind(MNA_matrix_13,diff )

#save MNA effect 
MNA_peak_effect<-mean(diff)

##export csv
write.csv(MNA_matrix_14,"MNA_effect.csv" )

## graph MNA effect over all conditions 
MNA_effect<-as.data.frame(MNA_matrix_14)

MNA_effect<-read.csv("MNA_effect.csv")
ggplot(MNA_effect, aes(PHDI, wt_t)) + geom_tile(aes(fill =  diff), ## choose effect you want to graph 
            colour = "white") + scale_fill_distiller( palette = "Spectral",
                limits = c(min(0),max(CDD_effect$diff)))+facet_grid(.~CDD)+xlab("Lg: CDD, Sm: PHDI")+ylab("weight"
                ) + theme_grey(base_size = 20)


## wt effect
day.of.year_1_t<-rep(0.059471366,3630)
day.of.year_2_t<-rep(0.063876652,3630)
day.of.year_3_t<-rep(0.068281938,3630)
day.of.year_4_t<-rep(0.072687225,3630)
day.of.year_5_t<-rep(0.077092511,3630)
day.of.year_6_t<-rep(0.081497797,3630)
day.of.year_7_t<-rep(0.085903084,3630)
day.of.year_8_t<-rep(0.09030837,3630)
day.of.year_9_t<-rep(0.094713656,3630)
day.of.year_10_t<-rep(0.099118943,3630)
day.of.year_11_t<-rep(0.103524229,3630)
day.of.year_12_t<-rep(0.107929515,3630)
day.of.year_13_t<-rep(0.112334802,3630)
day.of.year_14_t<-rep(0.116740088,3630)
day.of.year_15_t<-rep(0.121145374,3630)
day.of.year_16_t<-rep(0.125550661,3630)
day.of.year_17_t<-rep(0.129955947,3630)
MNA_t<-c(rep(-.5,726),rep(-.25,726),rep(0,726), rep(.25,726), rep(.5,726))

CDD_t<-rep(c(rep(-.5,66), rep(-.4,66),rep(-.3,66),rep(-.2,66),rep(-.1,66),rep(0,66),
             rep(.1,66),rep(.2,66),rep(.3,66),rep(.4,66),rep(.5,66)) ,5)

PHDI_t<-rep(c(rep(-.5,6), rep(-.4,6),rep(-.3,6),rep(-.2,6),rep(-.1,6),rep(0,6),
              rep(.1,6),rep(.2,6),rep(.3,6),rep(.4,6),rep(.5,6)) ,55)

sex<-rep(c(rep(0,3), rep(1,3)) ,605)
subadult<-rep(c(0,1,1) ,1210)
adult<-rep(c(0,0,1) ,1210)
preg<-rep(0,3630)
lac<-rep(0,3630)

wt_matrix<-as.matrix(data.frame(day.of.year_1_t,day.of.year_2_t,day.of.year_3_t,day.of.year_4_t,
                                day.of.year_5_t,day.of.year_6_t,day.of.year_7_t,day.of.year_8_t,
                                day.of.year_9_t,day.of.year_10_t,day.of.year_11_t,day.of.year_12_t,
                                day.of.year_13_t,day.of.year_14_t,day.of.year_15_t,day.of.year_16_t,
                                day.of.year_17_t, PHDI_t, MNA_t, CDD_t, sex, subadult, adult, preg, lac))



##create minmax(wt)
minmax <- function(P,D1,D2,D3,D4,D5,
                   D6,D7,D8,D9,D10,D11,
                   D12,D13,D14,D15,D16,D17,
                   M,x,C,X,S,A,G,L) {pred_1<-(10^(int 
                                             +(w1/
                                                 (1 + exp
                                                  (-(int1 + w11*D1 + w21*M + w31*x + w41*C 
                                                     + w51*P + w61*X + w71*S + w81*A + w91*L
                                                     + w101*G))))+
                                               (w2/
                                               (1 + exp
                                                (-(int2 + w12*D1 + w22*M + w32*x + w42*C
                                                   + w52*P + w62*X + w72*S + w82*A + w92*L
                                                   + w102*G))))+
                                               (w3/
                                                  (1 + exp
                                                   (-(int3 + w13*D1 + w23*M + w33*x + w43*C
                                                      + w53*P + w63*X + w73*S + w83*A + w93*L
                                                      + w103*G))))+
                                               (w4/
                                                  (1 + exp
                                                   (-(int4 + w14*D1 + w24*M + w34*x + w44*C 
                                                      + w54*P + w64*X + w74*S + w84*A + w94*L
                                                      + w104*G))))+
                                             (w5/
                                                 (1 + exp
                                                  (-(int5 + w15*D1 + w25*M + w35*x + w45*C 
                                                     + w55*P + w65*X + w75*S + w85*A + w95*L
                                                     + w105*G))))+
                                               (w6/
                                                  (1 + exp
                                                   (-(int6 + w16*D1 + w26*M + w36*x + w46*C 
                                                      + w56*P + w66*X + w76*S + w86*A + w96*L
                                                      + w106*G))))+
                                             (w7/
                                                 (1 + exp
                                                  (-(int7 + w17*D1 + w27*M + w37*x + w47*C
                                                     + w57*P + w67*X + w77*S + w87*A + w97*L
                                                     + w107*G))))+
                                               (w8/
                                                  (1 + exp
                                                   (-(int8 + w18*D1 + w28*M + w38*x + w48*C 
                                                      + w58*P + w68*X + w78*S + w88*A + w98*L
                                                      + w108*G))))+
                                               (w9/
                                                  (1 + exp
                                                   (-(int9 + w19*D1 + w29*M + w39*x + w49*C 
                                                      + w59*P + w69*X + w79*S + w89*A + w99*L
                                                      + w109*G))))+
                                               (w10/
                                                  (1 + exp
                                                   (-(int10 + w110*D1 + w210*M + w310*x + w410*C 
                                                      + w510*P + w610*X + w710*S + w810*A + w910*L
                                                      + w1010*G))))))
                   pred_2<-(10^(int 
                                +(w1/
                                    (1 + exp
                                     (-(int1 + w11*D2 + w21*M + w31*x + w41*C 
                                        + w51*P + w61*X + w71*S + w81*A + w91*L
                                        + w101*G))))+
                                  (w2/
                                     (1 + exp
                                      (-(int2 + w12*D2 + w22*M + w32*x + w42*C
                                         + w52*P + w62*X + w72*S + w82*A + w92*L
                                         + w102*G))))+
                                  (w3/
                                     (1 + exp
                                      (-(int3 + w13*D2 + w23*M + w33*x + w43*C
                                         + w53*P + w63*X + w73*S + w83*A + w93*L
                                         + w103*G))))+
                                  (w4/
                                     (1 + exp
                                      (-(int4 + w14*D2 + w24*M + w34*x + w44*C 
                                         + w54*P + w64*X + w74*S + w84*A + w94*L
                                         + w104*G))))+
                                  (w5/
                                     (1 + exp
                                      (-(int5 + w15*D2 + w25*M + w35*x + w45*C 
                                         + w55*P + w65*X + w75*S + w85*A + w95*L
                                         + w105*G))))+
                                  (w6/
                                     (1 + exp
                                      (-(int6 + w16*D2 + w26*M + w36*x + w46*C 
                                         + w56*P + w66*X + w76*S + w86*A + w96*L
                                         + w106*G))))+
                                  (w7/
                                     (1 + exp
                                      (-(int7 + w17*D2 + w27*M + w37*x + w47*C
                                         + w57*P + w67*X + w77*S + w87*A + w97*L
                                         + w107*G))))+
                                  (w8/
                                     (1 + exp
                                      (-(int8 + w18*D2 + w28*M + w38*x + w48*C 
                                         + w58*P + w68*X + w78*S + w88*A + w98*L
                                         + w108*G))))+
                                  (w9/
                                     (1 + exp
                                      (-(int9 + w19*D2 + w29*M + w39*x + w49*C 
                                         + w59*P + w69*X + w79*S + w89*A + w99*L
                                         + w109*G))))+
                                  (w10/
                                     (1 + exp
                                      (-(int10 + w110*D2 + w210*M + w310*x + w410*C 
                                         + w510*P + w610*X + w710*S + w810*A + w910*L
                                         + w1010*G))))))
                   pred_3<-(10^(int 
                                +(w1/
                                    (1 + exp
                                     (-(int1 + w11*D3 + w21*M + w31*x + w41*C 
                                        + w51*P + w61*X + w71*S + w81*A + w91*L
                                        + w101*G))))+
                                  (w2/
                                     (1 + exp
                                      (-(int2 + w12*D3 + w22*M + w32*x + w42*C
                                         + w52*P + w62*X + w72*S + w82*A + w92*L
                                         + w102*G))))+
                                  (w3/
                                     (1 + exp
                                      (-(int3 + w13*D3 + w23*M + w33*x + w43*C
                                         + w53*P + w63*X + w73*S + w83*A + w93*L
                                         + w103*G))))+
                                  (w4/
                                     (1 + exp
                                      (-(int4 + w14*D3 + w24*M + w34*x + w44*C 
                                         + w54*P + w64*X + w74*S + w84*A + w94*L
                                         + w104*G))))+
                                  (w5/
                                     (1 + exp
                                      (-(int5 + w15*D3 + w25*M + w35*x + w45*C 
                                         + w55*P + w65*X + w75*S + w85*A + w95*L
                                         + w105*G))))+
                                  (w6/
                                     (1 + exp
                                      (-(int6 + w16*D3 + w26*M + w36*x + w46*C 
                                         + w56*P + w66*X + w76*S + w86*A + w96*L
                                         + w106*G))))+
                                  (w7/
                                     (1 + exp
                                      (-(int7 + w17*D3 + w27*M + w37*x + w47*C
                                         + w57*P + w67*X + w77*S + w87*A + w97*L
                                         + w107*G))))+
                                  (w8/
                                     (1 + exp
                                      (-(int8 + w18*D3 + w28*M + w38*x + w48*C 
                                         + w58*P + w68*X + w78*S + w88*A + w98*L
                                         + w108*G))))+
                                  (w9/
                                     (1 + exp
                                      (-(int9 + w19*D3 + w29*M + w39*x + w49*C 
                                         + w59*P + w69*X + w79*S + w89*A + w99*L
                                         + w109*G))))+
                                  (w10/
                                     (1 + exp
                                      (-(int10 + w110*D3 + w210*M + w310*x + w410*C 
                                         + w510*P + w610*X + w710*S + w810*A + w910*L
                                         + w1010*G))))))
                   pred_4<-(10^(int 
                                +(w1/
                                    (1 + exp
                                     (-(int1 + w11*D4 + w21*M + w31*x + w41*C 
                                        + w51*P + w61*X + w71*S + w81*A + w91*L
                                        + w101*G))))+
                                  (w2/
                                     (1 + exp
                                      (-(int2 + w12*D4 + w22*M + w32*x + w42*C
                                         + w52*P + w62*X + w72*S + w82*A + w92*L
                                         + w102*G))))+
                                  (w3/
                                     (1 + exp
                                      (-(int3 + w13*D4 + w23*M + w33*x + w43*C
                                         + w53*P + w63*X + w73*S + w83*A + w93*L
                                         + w103*G))))+
                                  (w4/
                                     (1 + exp
                                      (-(int4 + w14*D4 + w24*M + w34*x + w44*C 
                                         + w54*P + w64*X + w74*S + w84*A + w94*L
                                         + w104*G))))+
                                  (w5/
                                     (1 + exp
                                      (-(int5 + w15*D4 + w25*M + w35*x + w45*C 
                                         + w55*P + w65*X + w75*S + w85*A + w95*L
                                         + w105*G))))+
                                  (w6/
                                     (1 + exp
                                      (-(int6 + w16*D4 + w26*M + w36*x + w46*C 
                                         + w56*P + w66*X + w76*S + w86*A + w96*L
                                         + w106*G))))+
                                  (w7/
                                     (1 + exp
                                      (-(int7 + w17*D4 + w27*M + w37*x + w47*C
                                         + w57*P + w67*X + w77*S + w87*A + w97*L
                                         + w107*G))))+
                                  (w8/
                                     (1 + exp
                                      (-(int8 + w18*D4 + w28*M + w38*x + w48*C 
                                         + w58*P + w68*X + w78*S + w88*A + w98*L
                                         + w108*G))))+
                                  (w9/
                                     (1 + exp
                                      (-(int9 + w19*D4 + w29*M + w39*x + w49*C 
                                         + w59*P + w69*X + w79*S + w89*A + w99*L
                                         + w109*G))))+
                                  (w10/
                                     (1 + exp
                                      (-(int10 + w110*D4 + w210*M + w310*x + w410*C 
                                         + w510*P + w610*X + w710*S + w810*A + w910*L
                                         + w1010*G))))))
                   pred_5<-(10^(int 
                                +(w1/
                                    (1 + exp
                                     (-(int1 + w11*D5 + w21*M + w31*x + w41*C 
                                        + w51*P + w61*X + w71*S + w81*A + w91*L
                                        + w101*G))))+
                                  (w2/
                                     (1 + exp
                                      (-(int2 + w12*D5 + w22*M + w32*x + w42*C
                                         + w52*P + w62*X + w72*S + w82*A + w92*L
                                         + w102*G))))+
                                  (w3/
                                     (1 + exp
                                      (-(int3 + w13*D5 + w23*M + w33*x + w43*C
                                         + w53*P + w63*X + w73*S + w83*A + w93*L
                                         + w103*G))))+
                                  (w4/
                                     (1 + exp
                                      (-(int4 + w14*D5 + w24*M + w34*x + w44*C 
                                         + w54*P + w64*X + w74*S + w84*A + w94*L
                                         + w104*G))))+
                                  (w5/
                                     (1 + exp
                                      (-(int5 + w15*D5 + w25*M + w35*x + w45*C 
                                         + w55*P + w65*X + w75*S + w85*A + w95*L
                                         + w105*G))))+
                                  (w6/
                                     (1 + exp
                                      (-(int6 + w16*D5 + w26*M + w36*x + w46*C 
                                         + w56*P + w66*X + w76*S + w86*A + w96*L
                                         + w106*G))))+
                                  (w7/
                                     (1 + exp
                                      (-(int7 + w17*D5 + w27*M + w37*x + w47*C
                                         + w57*P + w67*X + w77*S + w87*A + w97*L
                                         + w107*G))))+
                                  (w8/
                                     (1 + exp
                                      (-(int8 + w18*D5 + w28*M + w38*x + w48*C 
                                         + w58*P + w68*X + w78*S + w88*A + w98*L
                                         + w108*G))))+
                                  (w9/
                                     (1 + exp
                                      (-(int9 + w19*D5 + w29*M + w39*x + w49*C 
                                         + w59*P + w69*X + w79*S + w89*A + w99*L
                                         + w109*G))))+
                                  (w10/
                                     (1 + exp
                                      (-(int10 + w110*D5 + w210*M + w310*x + w410*C 
                                         + w510*P + w610*X + w710*S + w810*A + w910*L
                                         + w1010*G))))))
                   pred_6<-(10^(int 
                                +(w1/
                                    (1 + exp
                                     (-(int1 + w11*D6 + w21*M + w31*x + w41*C 
                                        + w51*P + w61*X + w71*S + w81*A + w91*L
                                        + w101*G))))+
                                  (w2/
                                     (1 + exp
                                      (-(int2 + w12*D6 + w22*M + w32*x + w42*C
                                         + w52*P + w62*X + w72*S + w82*A + w92*L
                                         + w102*G))))+
                                  (w3/
                                     (1 + exp
                                      (-(int3 + w13*D6 + w23*M + w33*x + w43*C
                                         + w53*P + w63*X + w73*S + w83*A + w93*L
                                         + w103*G))))+
                                  (w4/
                                     (1 + exp
                                      (-(int4 + w14*D6 + w24*M + w34*x + w44*C 
                                         + w54*P + w64*X + w74*S + w84*A + w94*L
                                         + w104*G))))+
                                  (w5/
                                     (1 + exp
                                      (-(int5 + w15*D6 + w25*M + w35*x + w45*C 
                                         + w55*P + w65*X + w75*S + w85*A + w95*L
                                         + w105*G))))+
                                  (w6/
                                     (1 + exp
                                      (-(int6 + w16*D6 + w26*M + w36*x + w46*C 
                                         + w56*P + w66*X + w76*S + w86*A + w96*L
                                         + w106*G))))+
                                  (w7/
                                     (1 + exp
                                      (-(int7 + w17*D6 + w27*M + w37*x + w47*C
                                         + w57*P + w67*X + w77*S + w87*A + w97*L
                                         + w107*G))))+
                                  (w8/
                                     (1 + exp
                                      (-(int8 + w18*D6 + w28*M + w38*x + w48*C 
                                         + w58*P + w68*X + w78*S + w88*A + w98*L
                                         + w108*G))))+
                                  (w9/
                                     (1 + exp
                                      (-(int9 + w19*D6 + w29*M + w39*x + w49*C 
                                         + w59*P + w69*X + w79*S + w89*A + w99*L
                                         + w109*G))))+
                                  (w10/
                                     (1 + exp
                                      (-(int10 + w110*D6 + w210*M + w310*x + w410*C 
                                         + w510*P + w610*X + w710*S + w810*A + w910*L
                                         + w1010*G))))))
                   pred_7<-(10^(int 
                                +(w1/
                                    (1 + exp
                                     (-(int1 + w11*D7 + w21*M + w31*x + w41*C 
                                        + w51*P + w61*X + w71*S + w81*A + w91*L
                                        + w101*G))))+
                                  (w2/
                                     (1 + exp
                                      (-(int2 + w12*D7 + w22*M + w32*x + w42*C
                                         + w52*P + w62*X + w72*S + w82*A + w92*L
                                         + w102*G))))+
                                  (w3/
                                     (1 + exp
                                      (-(int3 + w13*D7 + w23*M + w33*x + w43*C
                                         + w53*P + w63*X + w73*S + w83*A + w93*L
                                         + w103*G))))+
                                  (w4/
                                     (1 + exp
                                      (-(int4 + w14*D7 + w24*M + w34*x + w44*C 
                                         + w54*P + w64*X + w74*S + w84*A + w94*L
                                         + w104*G))))+
                                  (w5/
                                     (1 + exp
                                      (-(int5 + w15*D7 + w25*M + w35*x + w45*C 
                                         + w55*P + w65*X + w75*S + w85*A + w95*L
                                         + w105*G))))+
                                  (w6/
                                     (1 + exp
                                      (-(int6 + w16*D7 + w26*M + w36*x + w46*C 
                                         + w56*P + w66*X + w76*S + w86*A + w96*L
                                         + w106*G))))+
                                  (w7/
                                     (1 + exp
                                      (-(int7 + w17*D7 + w27*M + w37*x + w47*C
                                         + w57*P + w67*X + w77*S + w87*A + w97*L
                                         + w107*G))))+
                                  (w8/
                                     (1 + exp
                                      (-(int8 + w18*D7 + w28*M + w38*x + w48*C 
                                         + w58*P + w68*X + w78*S + w88*A + w98*L
                                         + w108*G))))+
                                  (w9/
                                     (1 + exp
                                      (-(int9 + w19*D7 + w29*M + w39*x + w49*C 
                                         + w59*P + w69*X + w79*S + w89*A + w99*L
                                         + w109*G))))+
                                  (w10/
                                     (1 + exp
                                      (-(int10 + w110*D7 + w210*M + w310*x + w410*C 
                                         + w510*P + w610*X + w710*S + w810*A + w910*L
                                         + w1010*G))))))
                   pred_8<-(10^(int 
                                +(w1/
                                    (1 + exp
                                     (-(int1 + w11*D8 + w21*M + w31*x + w41*C 
                                        + w51*P + w61*X + w71*S + w81*A + w91*L
                                        + w101*G))))+
                                  (w2/
                                     (1 + exp
                                      (-(int2 + w12*D8 + w22*M + w32*x + w42*C
                                         + w52*P + w62*X + w72*S + w82*A + w92*L
                                         + w102*G))))+
                                  (w3/
                                     (1 + exp
                                      (-(int3 + w13*D8 + w23*M + w33*x + w43*C
                                         + w53*P + w63*X + w73*S + w83*A + w93*L
                                         + w103*G))))+
                                  (w4/
                                     (1 + exp
                                      (-(int4 + w14*D8 + w24*M + w34*x + w44*C 
                                         + w54*P + w64*X + w74*S + w84*A + w94*L
                                         + w104*G))))+
                                  (w5/
                                     (1 + exp
                                      (-(int5 + w15*D8 + w25*M + w35*x + w45*C 
                                         + w55*P + w65*X + w75*S + w85*A + w95*L
                                         + w105*G))))+
                                  (w6/
                                     (1 + exp
                                      (-(int6 + w16*D8 + w26*M + w36*x + w46*C 
                                         + w56*P + w66*X + w76*S + w86*A + w96*L
                                         + w106*G))))+
                                  (w7/
                                     (1 + exp
                                      (-(int7 + w17*D8 + w27*M + w37*x + w47*C
                                         + w57*P + w67*X + w77*S + w87*A + w97*L
                                         + w107*G))))+
                                  (w8/
                                     (1 + exp
                                      (-(int8 + w18*D8 + w28*M + w38*x + w48*C 
                                         + w58*P + w68*X + w78*S + w88*A + w98*L
                                         + w108*G))))+
                                  (w9/
                                     (1 + exp
                                      (-(int9 + w19*D8 + w29*M + w39*x + w49*C 
                                         + w59*P + w69*X + w79*S + w89*A + w99*L
                                         + w109*G))))+
                                  (w10/
                                     (1 + exp
                                      (-(int10 + w110*D8 + w210*M + w310*x + w410*C 
                                         + w510*P + w610*X + w710*S + w810*A + w910*L
                                         + w1010*G))))))
                   pred_9<-(10^(int 
                                +(w1/
                                    (1 + exp
                                     (-(int1 + w11*D9 + w21*M + w31*x + w41*C 
                                        + w51*P + w61*X + w71*S + w81*A + w91*L
                                        + w101*G))))+
                                  (w2/
                                     (1 + exp
                                      (-(int2 + w12*D9 + w22*M + w32*x + w42*C
                                         + w52*P + w62*X + w72*S + w82*A + w92*L
                                         + w102*G))))+
                                  (w3/
                                     (1 + exp
                                      (-(int3 + w13*D9 + w23*M + w33*x + w43*C
                                         + w53*P + w63*X + w73*S + w83*A + w93*L
                                         + w103*G))))+
                                  (w4/
                                     (1 + exp
                                      (-(int4 + w14*D9 + w24*M + w34*x + w44*C 
                                         + w54*P + w64*X + w74*S + w84*A + w94*L
                                         + w104*G))))+
                                  (w5/
                                     (1 + exp
                                      (-(int5 + w15*D9 + w25*M + w35*x + w45*C 
                                         + w55*P + w65*X + w75*S + w85*A + w95*L
                                         + w105*G))))+
                                  (w6/
                                     (1 + exp
                                      (-(int6 + w16*D9 + w26*M + w36*x + w46*C 
                                         + w56*P + w66*X + w76*S + w86*A + w96*L
                                         + w106*G))))+
                                  (w7/
                                     (1 + exp
                                      (-(int7 + w17*D9 + w27*M + w37*x + w47*C
                                         + w57*P + w67*X + w77*S + w87*A + w97*L
                                         + w107*G))))+
                                  (w8/
                                     (1 + exp
                                      (-(int8 + w18*D9 + w28*M + w38*x + w48*C 
                                         + w58*P + w68*X + w78*S + w88*A + w98*L
                                         + w108*G))))+
                                  (w9/
                                     (1 + exp
                                      (-(int9 + w19*D9 + w29*M + w39*x + w49*C 
                                         + w59*P + w69*X + w79*S + w89*A + w99*L
                                         + w109*G))))+
                                  (w10/
                                     (1 + exp
                                      (-(int10 + w110*D9 + w210*M + w310*x + w410*C 
                                         + w510*P + w610*X + w710*S + w810*A + w910*L
                                         + w1010*G))))))
                   pred_10<-(10^(int 
                                +(w1/
                                    (1 + exp
                                     (-(int1 + w11*D10 + w21*M + w31*x + w41*C 
                                        + w51*P + w61*X + w71*S + w81*A + w91*L
                                        + w101*G))))+
                                  (w2/
                                     (1 + exp
                                      (-(int2 + w12*D10 + w22*M + w32*x + w42*C
                                         + w52*P + w62*X + w72*S + w82*A + w92*L
                                         + w102*G))))+
                                  (w3/
                                     (1 + exp
                                      (-(int3 + w13*D10 + w23*M + w33*x + w43*C
                                         + w53*P + w63*X + w73*S + w83*A + w93*L
                                         + w103*G))))+
                                  (w4/
                                     (1 + exp
                                      (-(int4 + w14*D10 + w24*M + w34*x + w44*C 
                                         + w54*P + w64*X + w74*S + w84*A + w94*L
                                         + w104*G))))+
                                  (w5/
                                     (1 + exp
                                      (-(int5 + w15*D10 + w25*M + w35*x + w45*C 
                                         + w55*P + w65*X + w75*S + w85*A + w95*L
                                         + w105*G))))+
                                  (w6/
                                     (1 + exp
                                      (-(int6 + w16*D10 + w26*M + w36*x + w46*C 
                                         + w56*P + w66*X + w76*S + w86*A + w96*L
                                         + w106*G))))+
                                  (w7/
                                     (1 + exp
                                      (-(int7 + w17*D10 + w27*M + w37*x + w47*C
                                         + w57*P + w67*X + w77*S + w87*A + w97*L
                                         + w107*G))))+
                                  (w8/
                                     (1 + exp
                                      (-(int8 + w18*D10 + w28*M + w38*x + w48*C 
                                         + w58*P + w68*X + w78*S + w88*A + w98*L
                                         + w108*G))))+
                                  (w9/
                                     (1 + exp
                                      (-(int9 + w19*D10 + w29*M + w39*x + w49*C 
                                         + w59*P + w69*X + w79*S + w89*A + w99*L
                                         + w109*G))))+
                                  (w10/
                                     (1 + exp
                                      (-(int10 + w110*D10 + w210*M + w310*x + w410*C 
                                         + w510*P + w610*X + w710*S + w810*A + w910*L
                                         + w1010*G))))))
                   pred_11<-(10^(int 
                                +(w1/
                                    (1 + exp
                                     (-(int1 + w11*D11 + w21*M + w31*x + w41*C 
                                        + w51*P + w61*X + w71*S + w81*A + w91*L
                                        + w101*G))))+
                                  (w2/
                                     (1 + exp
                                      (-(int2 + w12*D11 + w22*M + w32*x + w42*C
                                         + w52*P + w62*X + w72*S + w82*A + w92*L
                                         + w102*G))))+
                                  (w3/
                                     (1 + exp
                                      (-(int3 + w13*D11 + w23*M + w33*x + w43*C
                                         + w53*P + w63*X + w73*S + w83*A + w93*L
                                         + w103*G))))+
                                  (w4/
                                     (1 + exp
                                      (-(int4 + w14*D11 + w24*M + w34*x + w44*C 
                                         + w54*P + w64*X + w74*S + w84*A + w94*L
                                         + w104*G))))+
                                  (w5/
                                     (1 + exp
                                      (-(int5 + w15*D11 + w25*M + w35*x + w45*C 
                                         + w55*P + w65*X + w75*S + w85*A + w95*L
                                         + w105*G))))+
                                  (w6/
                                     (1 + exp
                                      (-(int6 + w16*D11 + w26*M + w36*x + w46*C 
                                         + w56*P + w66*X + w76*S + w86*A + w96*L
                                         + w106*G))))+
                                  (w7/
                                     (1 + exp
                                      (-(int7 + w17*D11 + w27*M + w37*x + w47*C
                                         + w57*P + w67*X + w77*S + w87*A + w97*L
                                         + w107*G))))+
                                  (w8/
                                     (1 + exp
                                      (-(int8 + w18*D11 + w28*M + w38*x + w48*C 
                                         + w58*P + w68*X + w78*S + w88*A + w98*L
                                         + w108*G))))+
                                  (w9/
                                     (1 + exp
                                      (-(int9 + w19*D11 + w29*M + w39*x + w49*C 
                                         + w59*P + w69*X + w79*S + w89*A + w99*L
                                         + w109*G))))+
                                  (w10/
                                     (1 + exp
                                      (-(int10 + w110*D11 + w210*M + w310*x + w410*C 
                                         + w510*P + w610*X + w710*S + w810*A + w910*L
                                         + w1010*G))))))
                   pred_12<-(10^(int 
                                +(w1/
                                    (1 + exp
                                     (-(int1 + w11*D12 + w21*M + w31*x + w41*C 
                                        + w51*P + w61*X + w71*S + w81*A + w91*L
                                        + w101*G))))+
                                  (w2/
                                     (1 + exp
                                      (-(int2 + w12*D12 + w22*M + w32*x + w42*C
                                         + w52*P + w62*X + w72*S + w82*A + w92*L
                                         + w102*G))))+
                                  (w3/
                                     (1 + exp
                                      (-(int3 + w13*D12 + w23*M + w33*x + w43*C
                                         + w53*P + w63*X + w73*S + w83*A + w93*L
                                         + w103*G))))+
                                  (w4/
                                     (1 + exp
                                      (-(int4 + w14*D12 + w24*M + w34*x + w44*C 
                                         + w54*P + w64*X + w74*S + w84*A + w94*L
                                         + w104*G))))+
                                  (w5/
                                     (1 + exp
                                      (-(int5 + w15*D12 + w25*M + w35*x + w45*C 
                                         + w55*P + w65*X + w75*S + w85*A + w95*L
                                         + w105*G))))+
                                  (w6/
                                     (1 + exp
                                      (-(int6 + w16*D12 + w26*M + w36*x + w46*C 
                                         + w56*P + w66*X + w76*S + w86*A + w96*L
                                         + w106*G))))+
                                  (w7/
                                     (1 + exp
                                      (-(int7 + w17*D12 + w27*M + w37*x + w47*C
                                         + w57*P + w67*X + w77*S + w87*A + w97*L
                                         + w107*G))))+
                                  (w8/
                                     (1 + exp
                                      (-(int8 + w18*D12 + w28*M + w38*x + w48*C 
                                         + w58*P + w68*X + w78*S + w88*A + w98*L
                                         + w108*G))))+
                                  (w9/
                                     (1 + exp
                                      (-(int9 + w19*D12 + w29*M + w39*x + w49*C 
                                         + w59*P + w69*X + w79*S + w89*A + w99*L
                                         + w109*G))))+
                                  (w10/
                                     (1 + exp
                                      (-(int10 + w110*D12 + w210*M + w310*x + w410*C 
                                         + w510*P + w610*X + w710*S + w810*A + w910*L
                                         + w1010*G))))))
                   pred_13<-(10^(int 
                                +(w1/
                                    (1 + exp
                                     (-(int1 + w11*D13 + w21*M + w31*x + w41*C 
                                        + w51*P + w61*X + w71*S + w81*A + w91*L
                                        + w101*G))))+
                                  (w2/
                                     (1 + exp
                                      (-(int2 + w12*D13 + w22*M + w32*x + w42*C
                                         + w52*P + w62*X + w72*S + w82*A + w92*L
                                         + w102*G))))+
                                  (w3/
                                     (1 + exp
                                      (-(int3 + w13*D13 + w23*M + w33*x + w43*C
                                         + w53*P + w63*X + w73*S + w83*A + w93*L
                                         + w103*G))))+
                                  (w4/
                                     (1 + exp
                                      (-(int4 + w14*D13 + w24*M + w34*x + w44*C 
                                         + w54*P + w64*X + w74*S + w84*A + w94*L
                                         + w104*G))))+
                                  (w5/
                                     (1 + exp
                                      (-(int5 + w15*D13 + w25*M + w35*x + w45*C 
                                         + w55*P + w65*X + w75*S + w85*A + w95*L
                                         + w105*G))))+
                                  (w6/
                                     (1 + exp
                                      (-(int6 + w16*D13 + w26*M + w36*x + w46*C 
                                         + w56*P + w66*X + w76*S + w86*A + w96*L
                                         + w106*G))))+
                                  (w7/
                                     (1 + exp
                                      (-(int7 + w17*D13 + w27*M + w37*x + w47*C
                                         + w57*P + w67*X + w77*S + w87*A + w97*L
                                         + w107*G))))+
                                  (w8/
                                     (1 + exp
                                      (-(int8 + w18*D13 + w28*M + w38*x + w48*C 
                                         + w58*P + w68*X + w78*S + w88*A + w98*L
                                         + w108*G))))+
                                  (w9/
                                     (1 + exp
                                      (-(int9 + w19*D13 + w29*M + w39*x + w49*C 
                                         + w59*P + w69*X + w79*S + w89*A + w99*L
                                         + w109*G))))+
                                  (w10/
                                     (1 + exp
                                      (-(int10 + w110*D13 + w210*M + w310*x + w410*C 
                                         + w510*P + w610*X + w710*S + w810*A + w910*L
                                         + w1010*G))))))
                   pred_14<-(10^(int 
                                +(w1/
                                    (1 + exp
                                     (-(int1 + w11*D14 + w21*M + w31*x + w41*C 
                                        + w51*P + w61*X + w71*S + w81*A + w91*L
                                        + w101*G))))+
                                  (w2/
                                     (1 + exp
                                      (-(int2 + w12*D14 + w22*M + w32*x + w42*C
                                         + w52*P + w62*X + w72*S + w82*A + w92*L
                                         + w102*G))))+
                                  (w3/
                                     (1 + exp
                                      (-(int3 + w13*D14 + w23*M + w33*x + w43*C
                                         + w53*P + w63*X + w73*S + w83*A + w93*L
                                         + w103*G))))+
                                  (w4/
                                     (1 + exp
                                      (-(int4 + w14*D14 + w24*M + w34*x + w44*C 
                                         + w54*P + w64*X + w74*S + w84*A + w94*L
                                         + w104*G))))+
                                  (w5/
                                     (1 + exp
                                      (-(int5 + w15*D14 + w25*M + w35*x + w45*C 
                                         + w55*P + w65*X + w75*S + w85*A + w95*L
                                         + w105*G))))+
                                  (w6/
                                     (1 + exp
                                      (-(int6 + w16*D14 + w26*M + w36*x + w46*C 
                                         + w56*P + w66*X + w76*S + w86*A + w96*L
                                         + w106*G))))+
                                  (w7/
                                     (1 + exp
                                      (-(int7 + w17*D14 + w27*M + w37*x + w47*C
                                         + w57*P + w67*X + w77*S + w87*A + w97*L
                                         + w107*G))))+
                                  (w8/
                                     (1 + exp
                                      (-(int8 + w18*D14 + w28*M + w38*x + w48*C 
                                         + w58*P + w68*X + w78*S + w88*A + w98*L
                                         + w108*G))))+
                                  (w9/
                                     (1 + exp
                                      (-(int9 + w19*D14 + w29*M + w39*x + w49*C 
                                         + w59*P + w69*X + w79*S + w89*A + w99*L
                                         + w109*G))))+
                                  (w10/
                                     (1 + exp
                                      (-(int10 + w110*D14 + w210*M + w310*x + w410*C 
                                         + w510*P + w610*X + w710*S + w810*A + w910*L
                                         + w1010*G))))))
                   pred_15<-(10^(int 
                                +(w1/
                                    (1 + exp
                                     (-(int1 + w11*D15 + w21*M + w31*x + w41*C 
                                        + w51*P + w61*X + w71*S + w81*A + w91*L
                                        + w101*G))))+
                                  (w2/
                                     (1 + exp
                                      (-(int2 + w12*D15 + w22*M + w32*x + w42*C
                                         + w52*P + w62*X + w72*S + w82*A + w92*L
                                         + w102*G))))+
                                  (w3/
                                     (1 + exp
                                      (-(int3 + w13*D15 + w23*M + w33*x + w43*C
                                         + w53*P + w63*X + w73*S + w83*A + w93*L
                                         + w103*G))))+
                                  (w4/
                                     (1 + exp
                                      (-(int4 + w14*D15 + w24*M + w34*x + w44*C 
                                         + w54*P + w64*X + w74*S + w84*A + w94*L
                                         + w104*G))))+
                                  (w5/
                                     (1 + exp
                                      (-(int5 + w15*D15 + w25*M + w35*x + w45*C 
                                         + w55*P + w65*X + w75*S + w85*A + w95*L
                                         + w105*G))))+
                                  (w6/
                                     (1 + exp
                                      (-(int6 + w16*D15 + w26*M + w36*x + w46*C 
                                         + w56*P + w66*X + w76*S + w86*A + w96*L
                                         + w106*G))))+
                                  (w7/
                                     (1 + exp
                                      (-(int7 + w17*D15 + w27*M + w37*x + w47*C
                                         + w57*P + w67*X + w77*S + w87*A + w97*L
                                         + w107*G))))+
                                  (w8/
                                     (1 + exp
                                      (-(int8 + w18*D15 + w28*M + w38*x + w48*C 
                                         + w58*P + w68*X + w78*S + w88*A + w98*L
                                         + w108*G))))+
                                  (w9/
                                     (1 + exp
                                      (-(int9 + w19*D15 + w29*M + w39*x + w49*C 
                                         + w59*P + w69*X + w79*S + w89*A + w99*L
                                         + w109*G))))+
                                  (w10/
                                     (1 + exp
                                      (-(int10 + w110*D15 + w210*M + w310*x + w410*C 
                                         + w510*P + w610*X + w710*S + w810*A + w910*L
                                         + w1010*G))))))
                   pred_16<-(10^(int 
                                +(w1/
                                    (1 + exp
                                     (-(int1 + w11*D16 + w21*M + w31*x + w41*C 
                                        + w51*P + w61*X + w71*S + w81*A + w91*L
                                        + w101*G))))+
                                  (w2/
                                     (1 + exp
                                      (-(int2 + w12*D16 + w22*M + w32*x + w42*C
                                         + w52*P + w62*X + w72*S + w82*A + w92*L
                                         + w102*G))))+
                                  (w3/
                                     (1 + exp
                                      (-(int3 + w13*D16 + w23*M + w33*x + w43*C
                                         + w53*P + w63*X + w73*S + w83*A + w93*L
                                         + w103*G))))+
                                  (w4/
                                     (1 + exp
                                      (-(int4 + w14*D16 + w24*M + w34*x + w44*C 
                                         + w54*P + w64*X + w74*S + w84*A + w94*L
                                         + w104*G))))+
                                  (w5/
                                     (1 + exp
                                      (-(int5 + w15*D16 + w25*M + w35*x + w45*C 
                                         + w55*P + w65*X + w75*S + w85*A + w95*L
                                         + w105*G))))+
                                  (w6/
                                     (1 + exp
                                      (-(int6 + w16*D16 + w26*M + w36*x + w46*C 
                                         + w56*P + w66*X + w76*S + w86*A + w96*L
                                         + w106*G))))+
                                  (w7/
                                     (1 + exp
                                      (-(int7 + w17*D16 + w27*M + w37*x + w47*C
                                         + w57*P + w67*X + w77*S + w87*A + w97*L
                                         + w107*G))))+
                                  (w8/
                                     (1 + exp
                                      (-(int8 + w18*D16 + w28*M + w38*x + w48*C 
                                         + w58*P + w68*X + w78*S + w88*A + w98*L
                                         + w108*G))))+
                                  (w9/
                                     (1 + exp
                                      (-(int9 + w19*D16 + w29*M + w39*x + w49*C 
                                         + w59*P + w69*X + w79*S + w89*A + w99*L
                                         + w109*G))))+
                                  (w10/
                                     (1 + exp
                                      (-(int10 + w110*D16 + w210*M + w310*x + w410*C 
                                         + w510*P + w610*X + w710*S + w810*A + w910*L
                                         + w1010*G))))))
                   pred_17<-(10^(int 
                                +(w1/
                                    (1 + exp
                                     (-(int1 + w11*D17 + w21*M + w31*x + w41*C 
                                        + w51*P + w61*X + w71*S + w81*A + w91*L
                                        + w101*G))))+
                                  (w2/
                                     (1 + exp
                                      (-(int2 + w12*D17 + w22*M + w32*x + w42*C
                                         + w52*P + w62*X + w72*S + w82*A + w92*L
                                         + w102*G))))+
                                  (w3/
                                     (1 + exp
                                      (-(int3 + w13*D17 + w23*M + w33*x + w43*C
                                         + w53*P + w63*X + w73*S + w83*A + w93*L
                                         + w103*G))))+
                                  (w4/
                                     (1 + exp
                                      (-(int4 + w14*D17 + w24*M + w34*x + w44*C 
                                         + w54*P + w64*X + w74*S + w84*A + w94*L
                                         + w104*G))))+
                                  (w5/
                                     (1 + exp
                                      (-(int5 + w15*D17 + w25*M + w35*x + w45*C 
                                         + w55*P + w65*X + w75*S + w85*A + w95*L
                                         + w105*G))))+
                                  (w6/
                                     (1 + exp
                                      (-(int6 + w16*D17 + w26*M + w36*x + w46*C 
                                         + w56*P + w66*X + w76*S + w86*A + w96*L
                                         + w106*G))))+
                                  (w7/
                                     (1 + exp
                                      (-(int7 + w17*D17 + w27*M + w37*x + w47*C
                                         + w57*P + w67*X + w77*S + w87*A + w97*L
                                         + w107*G))))+
                                  (w8/
                                     (1 + exp
                                      (-(int8 + w18*D17 + w28*M + w38*x + w48*C 
                                         + w58*P + w68*X + w78*S + w88*A + w98*L
                                         + w108*G))))+
                                  (w9/
                                     (1 + exp
                                      (-(int9 + w19*D17 + w29*M + w39*x + w49*C 
                                         + w59*P + w69*X + w79*S + w89*A + w99*L
                                         + w109*G))))+
                                  (w10/
                                     (1 + exp
                                      (-(int10 + w110*D17 + w210*M + w310*x + w410*C 
                                         + w510*P + w610*X + w710*S + w810*A + w910*L
                                         + w1010*G))))))
                   mean(pred_1, pred_2, pred_3, pred_4, pred_5, pred_6, pred_7,
                        pred_8, pred_9, pred_10, pred_11, pred_12, pred_13,
                        pred_14, pred_15, pred_16, pred_17)}
                                      

##apply minmax(wt)
max1<- apply(wt_matrix, 1, function(i) {
  optimize(minmax, c(-.5, -.3),   D1 = i[["day.of.year_1_t"]], D2 = i[["day.of.year_2_t"]], D3 = i[["day.of.year_3_t"]],
           D4 = i[["day.of.year_4_t"]], D5 = i[["day.of.year_5_t"]], D6 = i[["day.of.year_6_t"]], D7 = i[["day.of.year_7_t"]],
           D8 = i[["day.of.year_8_t"]], D9 = i[["day.of.year_9_t"]], D10 = i[["day.of.year_10_t"]], D11 = i[["day.of.year_11_t"]],
           D12 = i[["day.of.year_12_t"]], D13 = i[["day.of.year_13_t"]], D14 = i[["day.of.year_14_t"]], D15 = i[["day.of.year_15_t"]], 
           D16 = i[["day.of.year_16_t"]], D17 = i[["day.of.year_17_t"]], M=i[["MNA_t"]], P=i[["PHDI_t"]], C=i[["CDD_t"]], X=i[["sex"]],
                                S = i[["subadult"]], A=i[["adult"]], G=i[["preg"]], L=i[["lac"]],maximum=T)$objective})

  max2<- apply(wt_matrix, 1, function(i) {
    optimize(minmax, c(-.3, -.1),  D1 = i[["day.of.year_1_t"]], D2 = i[["day.of.year_2_t"]], D3 = i[["day.of.year_3_t"]],
             D4 = i[["day.of.year_4_t"]], D5 = i[["day.of.year_5_t"]], D6 = i[["day.of.year_6_t"]], D7 = i[["day.of.year_7_t"]],
             D8 = i[["day.of.year_8_t"]], D9 = i[["day.of.year_9_t"]], D10 = i[["day.of.year_10_t"]], D11 = i[["day.of.year_11_t"]],
             D12 = i[["day.of.year_12_t"]], D13 = i[["day.of.year_13_t"]], D14 = i[["day.of.year_14_t"]], D15 = i[["day.of.year_15_t"]], 
             D16 = i[["day.of.year_16_t"]], D17 = i[["day.of.year_17_t"]], M=i[["MNA_t"]], P=i[["PHDI_t"]], C=i[["CDD_t"]], X=i[["sex"]],
             S = i[["subadult"]], A=i[["adult"]], G=i[["preg"]], L=i[["lac"]],maximum=T)$objective})
  
  
  max3<- apply(wt_matrix, 1, function(i) {
    optimize(minmax, c(-.1, .1),  D1 = i[["day.of.year_1_t"]], D2 = i[["day.of.year_2_t"]], D3 = i[["day.of.year_3_t"]],
             D4 = i[["day.of.year_4_t"]], D5 = i[["day.of.year_5_t"]], D6 = i[["day.of.year_6_t"]], D7 = i[["day.of.year_7_t"]],
             D8 = i[["day.of.year_8_t"]], D9 = i[["day.of.year_9_t"]], D10 = i[["day.of.year_10_t"]], D11 = i[["day.of.year_11_t"]],
             D12 = i[["day.of.year_12_t"]], D13 = i[["day.of.year_13_t"]], D14 = i[["day.of.year_14_t"]], D15 = i[["day.of.year_15_t"]], 
             D16 = i[["day.of.year_16_t"]], D17 = i[["day.of.year_17_t"]], M=i[["MNA_t"]], P=i[["PHDI_t"]], C=i[["CDD_t"]], X=i[["sex"]],
             S = i[["subadult"]], A=i[["adult"]], G=i[["preg"]], L=i[["lac"]],maximum=T)$objective})
  

  max4<- apply(wt_matrix, 1, function(i) {
    optimize(minmax, c(.1, .3),   D1 = i[["day.of.year_1_t"]], D2 = i[["day.of.year_2_t"]], D3 = i[["day.of.year_3_t"]],
             D4 = i[["day.of.year_4_t"]], D5 = i[["day.of.year_5_t"]], D6 = i[["day.of.year_6_t"]], D7 = i[["day.of.year_7_t"]],
             D8 = i[["day.of.year_8_t"]], D9 = i[["day.of.year_9_t"]], D10 = i[["day.of.year_10_t"]], D11 = i[["day.of.year_11_t"]],
             D12 = i[["day.of.year_12_t"]], D13 = i[["day.of.year_13_t"]], D14 = i[["day.of.year_14_t"]], D15 = i[["day.of.year_15_t"]], 
             D16 = i[["day.of.year_16_t"]], D17 = i[["day.of.year_17_t"]], M=i[["MNA_t"]], P=i[["PHDI_t"]], C=i[["CDD_t"]], X=i[["sex"]],
             S = i[["subadult"]], A=i[["adult"]], G=i[["preg"]], L=i[["lac"]],maximum=T)$objective})
  
  max5<- apply(wt_matrix, 1, function(i) {
    optimize(minmax, c(.3, .5),   D1 = i[["day.of.year_1_t"]], D2 = i[["day.of.year_2_t"]], D3 = i[["day.of.year_3_t"]],
             D4 = i[["day.of.year_4_t"]], D5 = i[["day.of.year_5_t"]], D6 = i[["day.of.year_6_t"]], D7 = i[["day.of.year_7_t"]],
             D8 = i[["day.of.year_8_t"]], D9 = i[["day.of.year_9_t"]], D10 = i[["day.of.year_10_t"]], D11 = i[["day.of.year_11_t"]],
             D12 = i[["day.of.year_12_t"]], D13 = i[["day.of.year_13_t"]], D14 = i[["day.of.year_14_t"]], D15 = i[["day.of.year_15_t"]], 
             D16 = i[["day.of.year_16_t"]], D17 = i[["day.of.year_17_t"]], M=i[["MNA_t"]], P=i[["PHDI_t"]], C=i[["CDD_t"]], X=i[["sex"]],
             S = i[["subadult"]], A=i[["adult"]], G=i[["preg"]], L=i[["lac"]],maximum=T)$objective})
  
 
  min1<- apply(wt_matrix, 1, function(i) {
    optimize(minmax, c(-.5, -.3),  D1 = i[["day.of.year_1_t"]], D2 = i[["day.of.year_2_t"]], D3 = i[["day.of.year_3_t"]],
             D4 = i[["day.of.year_4_t"]], D5 = i[["day.of.year_5_t"]], D6 = i[["day.of.year_6_t"]], D7 = i[["day.of.year_7_t"]],
             D8 = i[["day.of.year_8_t"]], D9 = i[["day.of.year_9_t"]], D10 = i[["day.of.year_10_t"]], D11 = i[["day.of.year_11_t"]],
             D12 = i[["day.of.year_12_t"]], D13 = i[["day.of.year_13_t"]], D14 = i[["day.of.year_14_t"]], D15 = i[["day.of.year_15_t"]], 
             D16 = i[["day.of.year_16_t"]], D17 = i[["day.of.year_17_t"]], M=i[["MNA_t"]], P=i[["PHDI_t"]], C=i[["CDD_t"]], X=i[["sex"]],
             S = i[["subadult"]], A=i[["adult"]], G=i[["preg"]], L=i[["lac"]],maximum=F)$objective})
  
  min2<- apply(wt_matrix, 1, function(i) {
    optimize(minmax, c(-.3,-.1),   D1 = i[["day.of.year_1_t"]], D2 = i[["day.of.year_2_t"]], D3 = i[["day.of.year_3_t"]],
             D4 = i[["day.of.year_4_t"]], D5 = i[["day.of.year_5_t"]], D6 = i[["day.of.year_6_t"]], D7 = i[["day.of.year_7_t"]],
             D8 = i[["day.of.year_8_t"]], D9 = i[["day.of.year_9_t"]], D10 = i[["day.of.year_10_t"]], D11 = i[["day.of.year_11_t"]],
             D12 = i[["day.of.year_12_t"]], D13 = i[["day.of.year_13_t"]], D14 = i[["day.of.year_14_t"]], D15 = i[["day.of.year_15_t"]], 
             D16 = i[["day.of.year_16_t"]], D17 = i[["day.of.year_17_t"]], M=i[["MNA_t"]], P=i[["PHDI_t"]], C=i[["CDD_t"]], X=i[["sex"]],
             S = i[["subadult"]], A=i[["adult"]], G=i[["preg"]], L=i[["lac"]],maximum=F)$objective})
  
  min3<- apply(wt_matrix, 1, function(i) {
    optimize(minmax, c(-.1, .1),   D1 = i[["day.of.year_1_t"]], D2 = i[["day.of.year_2_t"]], D3 = i[["day.of.year_3_t"]],
             D4 = i[["day.of.year_4_t"]], D5 = i[["day.of.year_5_t"]], D6 = i[["day.of.year_6_t"]], D7 = i[["day.of.year_7_t"]],
             D8 = i[["day.of.year_8_t"]], D9 = i[["day.of.year_9_t"]], D10 = i[["day.of.year_10_t"]], D11 = i[["day.of.year_11_t"]],
             D12 = i[["day.of.year_12_t"]], D13 = i[["day.of.year_13_t"]], D14 = i[["day.of.year_14_t"]], D15 = i[["day.of.year_15_t"]], 
             D16 = i[["day.of.year_16_t"]], D17 = i[["day.of.year_17_t"]], M=i[["MNA_t"]], P=i[["PHDI_t"]], C=i[["CDD_t"]], X=i[["sex"]],
             S = i[["subadult"]], A=i[["adult"]], G=i[["preg"]], L=i[["lac"]],maximum=F)$objective})
  
  min4<- apply(wt_matrix, 1, function(i) {
    optimize(minmax, c(.1, .3),   D1 = i[["day.of.year_1_t"]], D2 = i[["day.of.year_2_t"]], D3 = i[["day.of.year_3_t"]],
             D4 = i[["day.of.year_4_t"]], D5 = i[["day.of.year_5_t"]], D6 = i[["day.of.year_6_t"]], D7 = i[["day.of.year_7_t"]],
             D8 = i[["day.of.year_8_t"]], D9 = i[["day.of.year_9_t"]], D10 = i[["day.of.year_10_t"]], D11 = i[["day.of.year_11_t"]],
             D12 = i[["day.of.year_12_t"]], D13 = i[["day.of.year_13_t"]], D14 = i[["day.of.year_14_t"]], D15 = i[["day.of.year_15_t"]], 
             D16 = i[["day.of.year_16_t"]], D17 = i[["day.of.year_17_t"]], M=i[["MNA_t"]], P=i[["PHDI_t"]], C=i[["CDD_t"]], X=i[["sex"]],
             S = i[["subadult"]], A=i[["adult"]], G=i[["preg"]], L=i[["lac"]],maximum=F)$objective})

  
  min5<- apply(wt_matrix, 1, function(i) {
    optimize(minmax, c(.3, .5),   D1 = i[["day.of.year_1_t"]], D2 = i[["day.of.year_2_t"]], D3 = i[["day.of.year_3_t"]],
             D4 = i[["day.of.year_4_t"]], D5 = i[["day.of.year_5_t"]], D6 = i[["day.of.year_6_t"]], D7 = i[["day.of.year_7_t"]],
             D8 = i[["day.of.year_8_t"]], D9 = i[["day.of.year_9_t"]], D10 = i[["day.of.year_10_t"]], D11 = i[["day.of.year_11_t"]],
             D12 = i[["day.of.year_12_t"]], D13 = i[["day.of.year_13_t"]], D14 = i[["day.of.year_14_t"]], D15 = i[["day.of.year_15_t"]], 
             D16 = i[["day.of.year_16_t"]], D17 = i[["day.of.year_17_t"]], M=i[["MNA_t"]], P=i[["PHDI_t"]], C=i[["CDD_t"]], X=i[["sex"]],
             S = i[["subadult"]], A=i[["adult"]], G=i[["preg"]], L=i[["lac"]],maximum=F)$objective})
   

wt_matrix_2 <- cbind(wt_matrix, max1)
wt_matrix_3<-cbind(wt_matrix_2,max2)
wt_matrix_4 <- cbind(wt_matrix_3, max3)
wt_matrix_5<-cbind(wt_matrix_4,max4)
wt_matrix_6<-cbind(wt_matrix_5,max5)
wt_matrix_7<-cbind(wt_matrix_6, min1)
wt_matrix_8<-cbind(wt_matrix_7,min2)
wt_matrix_9<-cbind(wt_matrix_8,min3)
wt_matrix_10<-cbind(wt_matrix_9,min4)
wt_matrix_11<-cbind(wt_matrix_10,min5)


absmax<-apply(wt_matrix_11[,26:30],1,max)
absmin<-apply(wt_matrix_11[,31:35],1,min)
wt_matrix_12<-cbind(wt_matrix,absmax)
wt_matrix_13<-cbind(wt_matrix_12, absmin)
diff<-absmax-absmin
wt_matrix_14<-cbind(wt_matrix_13,diff )

## save average weight effect 
wt_peak_effect<-mean(diff)

##export csv

write.csv(wt_matrix_14,"wt_effect.csv" )

## graph weight effect over all conditions 
wt_effect<-as.data.frame(wt_matrix_14)


ggplot(wt_effect, aes(PHDI_t, MNA_t)) + geom_tile(aes(fill =  diff), 
            colour = "white") + scale_fill_distiller( palette = "Spectral",
                limits = c(min(0),max(CDD_effect$diff)))+facet_grid(.~CDD_t)+xlab("Lg: CDD, Sm: PHDI")+ylab("MNA"
                ) + ggtitle("Weight effect (peak)") 



#####OUTPUT GRAPH - VARIABLE EFFECT#####
## graph overall average variable effect

barplot(c( CDD_peak_effect,MNA_peak_effect,
           PHDI_peak_effect, wt_peak_effect,sex_effect,
           subadult_effect,
          pregnancy_effect,  adult_effect,lactation_effect),main = "Average variable effect",ylab = "Larvae",
          names.arg = c( "CDD", "MNA","PHDI", "weight","sex", "subadult",   "pregnancy","adult","lactation" ))




#####TO TROUBLESHOOT#####

minmaxplot <- function(x) {10^(-0.664428192
                                              +(0.870364964/
                                                  (1 + exp
                                                   (-(-0.708097264 + -16.53039393*0.143171806

 + -3.629048592*-.5	 + -0.124623751*-.5 + -1.242250616*-.5 
                                                      + -1.575384798*x	))))+
                                                (-0.704240926/
                                                   (1 + exp
                                                    (-(-6.258907107 + 12.13374444*0.143171806

 + -6.245711558*-.5	 + -3.028058501*-.5 	 + 30.38328154*-.5
                                                       + 16.40790673*x	 ))))+
                                                (1.270485349/
                                                   (1 + exp
                                                    (-(-0.627959659 + 60.34624947*0.143171806

 + 1.506221866*-.5	 + 0.036174052*-.5 	 + 5.320812179*-.5 
                                                       + -0.852795917*x	 ))))+
                                                (-0.915798035/
                                                   (1 + exp
                                                    (-(-8.69410586 + 25.14109863*0.143171806

 + 0.201666802*-.5	 + -2.537451399*-.5 	 + 1.488115678*-.5
                                                       + 0.350731592*x	))))+
                                              (-0.64721446/
                                                  (1 + exp
                                                   (-(8.467978803 + -13.0917691*0.143171806

 + 16.08725548*-.5	 + 2.798523314*-.5 	 + -33.70127003*-.5
                                                      + -16.89093077*x	))))+
                                                (1.064030704/
                                                   (1 + exp
                                                    (-(4.908102697 + 13.76779268*0.143171806

 + 1.305639801*-.5	 + 1.197959107*-.5  + -0.059610064*-.5
                                                       + -1.645669707*x	 ))))+
                                                (-0.40665191/
                                                   (1 + exp
                                                    (-(2.571361663 + 1.456251587*0.143171806

 + 5.77649957*-.5	 + -0.444320348*-.5 	 + 7.769444466*-.5 
                                                       + -7.781232052*x	 ))))+
                                                (0.317760395/
                                                   (1 + exp
                                                    (-(-0.578429661 + 94.12658292*0.143171806

 + 122.190706*-.5	 + 5.78569473*-.5 	 + 367.8618824*-.5 
                                                       + 45.13273908*x	))))+
                                                (0.387690279/
                                                   (1 + exp
                                                    (-(-7.329696226 + -31.48858137*0.143171806

 + -5.573322449*-.5	 + 2.954855363*-.5 	 + 0.389683935*-.5 
                                                       + -1.235474158*x ))))+
                                                (-0.125516527/
                                                   (1 + exp
                                                    (-(-21.79965796 + -39.24347721*0.143171806

 + -42.21588637*-.5	 + 156.4967844*-.5 	 + 5.133559209*-.5 
                                                       + 27.5136322*x	)))))}
  

curve(minmaxplot, from=-.5, to=.5)
optimize(minmaxplot,c(0,.1), maximum = T)


