##################################################################################################################################
#  Scénario 5 de l'article de Franklin en utilisant le DAG de l'article
# de Matthew Sperrin dans le cas de deux temps de mesure. 

#  J'ai utilisé une taille d'échantillon de n=5000 et choisi des valeurs 
# pour les paramères de sorte que la prévalence globale de l'exposition 
# à chaque temps de mesure est d'environ 50% et le taux de résultat global 
# d'environ 20%.
#
# Dans ce scénario, on spécifie que la variable m est instrumentale 
# en fixant les coefficients à zéro sur tous les termes du modèle générateur 
#  de résultats impliquant m. 
#                                                                     
###################################################################################################################################

library(confoundr); 
library(magrittr);
library(tidyr);
library(dplyr);
library(scales);
library(gridExtra);
library(ROCR);
library(cem);
library(Hmisc)
library(spatstat)
library(cli)

setwd("/Users/david/Documents/R code for Paper 2")
source("Metriques.R")
source("Data.long.R")

# Calcul des vraies valeurs des paramètres par simulation

n = 100000

# Simulation basée sur le scénario 1 de l'article de Franklin

l_0 = as.numeric(rnorm(n,0,1)); # Le cholestérol(rapport HDL)
m_0 = as.numeric(rlnorm(n,0,0.5)); # par exemple le BMI
n_0 = as.numeric(rnorm(n,0,10)) ; # par exemple age
o_0 = as.numeric(rbinom(n,1,plogis(-0.05+2*l_0))); # Hypertension
p_0 = as.numeric(rbinom(n,1,0.2)); # Diabète de type 2
q_0 = as.numeric(sample(c(1,2,3,4,5),n,replace=TRUE,
                        c(0.5,0.3,0.1,0.05,0.05))); # Indice de défavorisation

r_0 =sin(l_0)
u_0 =m_0^2
v_0 =n_0*o_0
t_0 =o_0*p_0

# Hypertension, le diabète, Le cholestérol,age, BMI et lndice de défavorisation
# sont des déterminants de la prescription de statines

a_0 = as.numeric(rbinom(n,1,plogis(-0.4+0.2*l_0+0.03*m_0+0.02*n_0+0.5*o_0+
                                     0.25*p_0+0.01*q_0+0.01*r_0+0.02*u_0+0.01*v_0+0.1*t_0)));

l_1 = as.numeric(rnorm(n,-a_0,1));
m_1 = as.numeric(rlnorm(n,-0.5*a_0,0.5));
n_1 = as.numeric(rnorm(n,-0.25*a_0,10));
o_1 = as.numeric(rbinom(n,1,plogis(1.21+2*l_1-0.5*a_0)));
p_1 = as.numeric(rbinom(n,1,plogis(-1.08-0.75*a_0)));
q_1 = as.numeric(sample(c(1,2,3,4,5),n,replace=TRUE,c(0.4,0.3,0.2,0.05,0.05)));

r_1 =sin(l_1)
u_1 =m_1^2
v_1 =n_1*o_1
t_1 =o_1*p_1

a_1 = as.numeric(rbinom(n,size=1,plogis(-0.6+0.69*a_0+0.2*l_1+0.03*m_1+
                                          0.02*n_1+0.5*o_1+0.25*p_1+0.01*q_1
                                        +0.01*r_1+0.02*u_1+0.01*v_1+0.1*t_1)));

Y = as.numeric(rbinom(n,size=1,plogis(-3-0.69*a_0+0.4*l_0
                                      +0.03*n_0+0.75*o_0+0.75*p_0+0.2*q_0
                                      -0.69*a_1+0.4*l_1+0.03*n_1
                                      +0.75*o_1+0.75*p_1+0.2*q_1
                                      +0.4*r_0+0.04*v_0+0.5*t_0+
                                        0.4*r_1+0.04*v_1+0.5*t_1)));

l_1.0=as.numeric(rnorm(n,0,1));
l_1.1=as.numeric(rnorm(n,-1,1));

m_1.0=as.numeric(rlnorm(n,-0.5*0,0.5));
m_1.1=as.numeric(rlnorm(n,-0.5*1,0.5));

n_1.0= as.numeric(rnorm(n,-0.25*0,10));
n_1.1= as.numeric(rnorm(n,-0.25*1,10));

o_1.0=as.numeric(rbinom(n,1,plogis(1.21+2*l_1.0-0.5*0)));
o_1.1=as.numeric(rbinom(n,1,plogis(1.21+2*l_1.1-0.5*1)));

p_1.0=as.numeric(rbinom(n,1,plogis(-1.08-0.75*0)));
p_1.1=as.numeric(rbinom(n,1,plogis(-1.08-0.75*1)));


r_1.0 =sin(l_1.0)
r_1.1 =sin(l_1.1)

u_1.0 =m_1.0^2
u_1.1 =m_1.1^2

v_1.0 =n_1.0*o_1.0
v_1.1 =n_1.1*o_1.1

t_1.0 =o_1.0*p_1.0
t_1.1 =o_1.1*p_1.1


Y.00 = as.numeric(rbinom(n,size=1,plogis(-3-0.69*0+0.4*l_0
                                         +0.03*n_0+0.75*o_0+0.75*p_0+0.2*q_0
                                         -0.69*0+0.4*l_1.0+0.03*n_1.0
                                         +0.75*o_1.0+0.75*p_1.0+0.2*q_1
                                         +0.4*r_0+0.04*v_0+0.5*t_0+
                                           0.4*r_1.0+0.04*v_1.0+0.5*t_1.0)));


Y.01 = as.numeric(rbinom(n,size=1,plogis(-3-0.69*0+0.4*l_0
                                         +0.03*n_0+0.75*o_0+0.75*p_0+0.2*q_0
                                         -0.69*1+0.4*l_1.0+0.03*n_1.0
                                         +0.75*o_1.0+0.75*p_1.0+0.2*q_1
                                         +0.4*r_0+0.04*v_0+0.5*t_0+
                                           0.4*r_1.0+0.04*v_1.0+0.5*t_1.0)));

Y.10 = as.numeric(rbinom(n,size=1,plogis(-3-0.69*1+0.4*l_0
                                         +0.03*n_0+0.75*o_0+0.75*p_0+0.2*q_0
                                         -0.69*0+0.4*l_1.1+0.03*n_1.1
                                         +0.75*o_1.1+0.75*p_1.1+0.2*q_1
                                         +0.4*r_0+0.04*v_0+0.5*t_0+
                                           0.4*r_1.1+0.04*v_1.1+0.5*t_1.1)));

Y.11 = as.numeric(rbinom(n,size=1,plogis(-3-0.69*1+0.4*l_0
                                         +0.03*n_0+0.75*o_0+0.75*p_0+0.2*q_0
                                         -0.69*1+0.4*l_1.1+0.03*n_1.1
                                         +0.75*o_1.1+0.75*p_1.1+0.2*q_1
                                         +0.4*r_0+0.04*v_0+0.5*t_0+
                                           0.4*r_1.1+0.04*v_1.1+0.5*t_1.1)));


beta_1=log(odds(mean(Y.10))/odds(mean(Y.00)))
beta_2=log(odds(mean(Y.01))/odds(mean(Y.00)))



set.seed(317931);
B = 10;
n = 10000;
Biais0 = rep(NA,B);
Biais1 = rep(NA,B);
Biais2 = rep(NA,B);
Biais3 = rep(NA,B);
Biais4 = rep(NA,B);
Biais5 = rep(NA,B);
Biais6 = rep(NA,B);
Biais7 = rep(NA,B);
Biais8 = rep(NA,B);
Biais9 = rep(NA,B);
Biais10 = rep(NA,B);
Biais11 = rep(NA,B);
Biais12 = rep(NA,B);
Biais13 = rep(NA,B);
Biais14 = rep(NA,B);
Biais15 = rep(NA,B);
Biais16 = rep(NA,B);
Biais17 = rep(NA,B);
mean.D = rep(NA,B) ;
mean.D1 = rep(NA,B) ;
mean.D2 = rep(NA,B) ;
mean.D3 = rep(NA,B) ;
mean.D4 = rep(NA,B) ;
mean.D5 = rep(NA,B) ;
mean.D6 = rep(NA,B) ;
mean.D7 = rep(NA,B) ;
mean.D8 = rep(NA,B) ;
mean.D9 = rep(NA,B) ;
mean.D10 = rep(NA,B) ;
mean.D11 = rep(NA,B) ;
mean.D12 = rep(NA,B) ;
mean.D13 = rep(NA,B) ;
mean.D14 = rep(NA,B) ;
mean.D15 = rep(NA,B) ;
mean.D16 = rep(NA,B) ;
mean.D17 = rep(NA,B) ;
mean.SMD = rep(NA,B) ;
mean.SMD1 = rep(NA,B) ;
mean.SMD2 = rep(NA,B) ;
mean.SMD3 = rep(NA,B) ;
mean.SMD4 = rep(NA,B) ;
mean.SMD5 = rep(NA,B) ;
mean.SMD6 = rep(NA,B) ;
mean.SMD7 = rep(NA,B) ;
mean.SMD8 = rep(NA,B) ;
mean.SMD9 = rep(NA,B) ;
mean.SMD10 =rep(NA,B) ;
mean.SMD11 =rep(NA,B) ;
mean.SMD12 = rep(NA,B) ;
mean.SMD13 = rep(NA,B) ;
mean.SMD14 = rep(NA,B) ;
mean.SMD15 = rep(NA,B) ;
mean.SMD16 =rep(NA,B) ;
mean.SMD17 =rep(NA,B) ;
mean.OVL = rep(NA,B);
mean.OVL1 = rep(NA,B) ;
mean.OVL2 = rep(NA,B) ;
mean.OVL3 = rep(NA,B) ;
mean.OVL4 = rep(NA,B) ;
mean.OVL5 = rep(NA,B) ;
mean.OVL6 = rep(NA,B) ;
mean.OVL7 = rep(NA,B) ;
mean.OVL8 = rep(NA,B) ;
mean.OVL9 = rep(NA,B) ;
mean.OVL10 = rep(NA,B) ;
mean.OVL11 = rep(NA,B) ;
mean.OVL12 = rep(NA,B) ;
mean.OVL13 = rep(NA,B) ;
mean.OVL14 = rep(NA,B) ;
mean.OVL15 = rep(NA,B) ;
mean.OVL16 = rep(NA,B) ;
mean.OVL17 = rep(NA,B) ;
mean.LD = rep(NA,B);
mean.LD1 = rep(NA,B) ;
mean.LD2 = rep(NA,B) ;
mean.LD3 = rep(NA,B) ;
mean.LD4 = rep(NA,B) ;
mean.LD5 = rep(NA,B) ;
mean.LD6 = rep(NA,B) ;
mean.LD7 = rep(NA,B) ;
mean.LD8 = rep(NA,B) ;
mean.LD9 = rep(NA,B) ;
mean.LD10 = rep(NA,B) ;
mean.LD11 =rep(NA,B) ;
mean.LD12 = rep(NA,B) ;
mean.LD13 = rep(NA,B) ;
mean.LD14 = rep(NA,B) ;
mean.LD15 = rep(NA,B) ;
mean.LD16 = rep(NA,B) ;
mean.LD17 =rep(NA,B) ;
mean.KS = rep(NA,B);
mean.KS1 = rep(NA,B) ;
mean.KS2 = rep(NA,B) ;
mean.KS3 = rep(NA,B) ;
mean.KS4 = rep(NA,B) ;
mean.KS5 = rep(NA,B) ;
mean.KS6 = rep(NA,B) ;
mean.KS7 = rep(NA,B) ;
mean.KS8 = rep(NA,B) ;
mean.KS9 = rep(NA,B) ;
mean.KS10 = rep(NA,B) ;
mean.KS11 = rep(NA,B) ;
mean.KS12 = rep(NA,B) ;
mean.KS13 = rep(NA,B) ;
mean.KS14 = rep(NA,B) ;
mean.KS15 = rep(NA,B) ;
mean.KS16 = rep(NA,B) ;
mean.KS17 = rep(NA,B) ;
mean.Stat.C = rep(NA,B);
mean.Stat.C1 = rep(NA,B) ;
mean.Stat.C2 = rep(NA,B) ;
mean.Stat.C3 = rep(NA,B) ;
mean.Stat.C4 = rep(NA,B) ;
mean.Stat.C5 = rep(NA,B) ;
mean.Stat.C6 = rep(NA,B) ;
mean.Stat.C7 = rep(NA,B) ;
mean.Stat.C8 = rep(NA,B) ;
mean.Stat.C9 = rep(NA,B) ;
mean.Stat.C10 = rep(NA,B) ;
mean.Stat.C11 =rep(NA,B) ;
mean.Stat.C12 = rep(NA,B) ;
mean.Stat.C13 = rep(NA,B) ;
mean.Stat.C14 = rep(NA,B) ;
mean.Stat.C15 = rep(NA,B) ;
mean.Stat.C16 = rep(NA,B) ;
mean.Stat.C17 =rep(NA,B) ;
mean.GWD = rep(NA,B);
mean.GWD1 = rep(NA,B) ;
mean.GWD2 = rep(NA,B) ;
mean.GWD3 = rep(NA,B) ;
mean.GWD4 = rep(NA,B) ;
mean.GWD5 = rep(NA,B) ;
mean.GWD6 = rep(NA,B) ;
mean.GWD7 = rep(NA,B) ;
mean.GWD8 = rep(NA,B) ;
mean.GWD9 = rep(NA,B) ;
mean.GWD10 = rep(NA,B) ;
mean.GWD11 = rep(NA,B) ;
mean.GWD12 = rep(NA,B) ;
mean.GWD13 = rep(NA,B) ;
mean.GWD14 = rep(NA,B) ;
mean.GWD15 = rep(NA,B) ;
mean.GWD16 = rep(NA,B) ;
mean.GWD17 = rep(NA,B) ;
mean.MHB = rep(NA,B);
mean.MHB1 = rep(NA,B) ;
mean.MHB2 = rep(NA,B) ;
mean.MHB3 = rep(NA,B) ;
mean.MHB4 = rep(NA,B) ;
mean.MHB5 = rep(NA,B) ;
mean.MHB6 = rep(NA,B) ;
mean.MHB7 = rep(NA,B) ;
mean.MHB8 = rep(NA,B) ;
mean.MHB9 = rep(NA,B) ;
mean.MHB10 = rep(NA,B) ;
mean.MHB11 = rep(NA,B) ;
mean.MHB12 = rep(NA,B) ;
mean.MHB13 = rep(NA,B) ;
mean.MHB14 = rep(NA,B) ;
mean.MHB15 = rep(NA,B) ;
mean.MHB16 = rep(NA,B) ;
mean.MHB17 = rep(NA,B) ;


for(i in 1:B){
  
  # Simulation basée sur le scénario 1 de l'article de Franklin
  
  l_0 = as.numeric(rnorm(n,0,1)); # Le cholestérol(rapport HDL)
  m_0 = as.numeric(rlnorm(n,0,0.5)); # par exemple le BMI
  n_0 = as.numeric(rnorm(n,0,10)) ; # par exemple age
  o_0 = as.numeric(rbinom(n,1,plogis(-0.05+2*l_0))); # Hypertension
  p_0 = as.numeric(rbinom(n,1,0.2)); # Diabète de type 2
  q_0 = as.numeric(sample(c(1,2,3,4,5),n,replace=TRUE,
                          c(0.5,0.3,0.1,0.05,0.05))); # Indice de défavorisation
  
  r_0 =sin(l_0)
  u_0 =m_0^2
  v_0 =n_0*o_0
  t_0 =o_0*p_0
  
  # Hypertension, le diabète, Le cholestérol,age, BMI et lndice de défavorisation
  # sont des déterminants de la prescription de statines
  
  a_0 = as.numeric(rbinom(n,1,plogis(-0.4+0.2*l_0+0.03*m_0+0.02*n_0+0.5*o_0+
                                       0.25*p_0+0.01*q_0+0.01*r_0+0.02*u_0+0.01*v_0+0.1*t_0)));
  
  l_1 = as.numeric(rnorm(n,-a_0,1));
  m_1 = as.numeric(rlnorm(n,-0.5*a_0,0.5));
  n_1 = as.numeric(rnorm(n,-0.25*a_0,10));
  o_1 = as.numeric(rbinom(n,1,plogis(1.21+2*l_1-0.5*a_0)));
  p_1 = as.numeric(rbinom(n,1,plogis(-1.08-0.75*a_0)));
  q_1 = as.numeric(sample(c(1,2,3,4,5),n,replace=TRUE,c(0.4,0.3,0.2,0.05,0.05)));
  
  r_1 =sin(l_1)
  u_1 =m_1^2
  v_1 =n_1*o_1
  t_1 =o_1*p_1
  
  a_1 = as.numeric(rbinom(n,size=1,plogis(-0.6+0.69*a_0+0.2*l_1+0.03*m_1+
                                            0.02*n_1+0.5*o_1+0.25*p_1+0.01*q_1
                                          +0.01*r_1+0.02*u_1+0.01*v_1+0.1*t_1)));
  
  Y = as.numeric(rbinom(n,size=1,plogis(-3-0.69*a_0+0.4*l_0
                                        +0.03*n_0+0.75*o_0+0.75*p_0+0.2*q_0
                                        -0.69*a_1+0.4*l_1+0.03*n_1
                                        +0.75*o_1+0.75*p_1+0.2*q_1
                                        +0.4*r_0+0.04*v_0+0.5*t_0+
                                          0.4*r_1+0.04*v_1+0.5*t_1)));

  ### Calcul des termes pour les poids dans le cas non censuré
  
  #  Dénominateur du poids
  
  mod.dnom1 = glm(a_0~l_0+m_0+n_0+o_0+p_0+q_0+r_0+u_0+v_0+t_0,family=binomial(link = "logit"));
  dnom1 =ifelse(a_0== 0, 1-predict(mod.dnom1, type = "response"),predict(mod.dnom1, type = "response")) 
  mod.dnom2 = glm(a_1~l_0+m_0+n_0+o_0+p_0+q_0+l_1+m_1+n_1+o_1+p_1+q_1+a_0+r_0+u_0+v_0+t_0+r_1+u_1+v_1+t_1,family=binomial(link = "logit"));
  dnom2=ifelse(a_1== 0, 1-predict(mod.dnom2, type = "response"),predict(mod.dnom2, type = "response"))
  
  
  #  Numérateur du poids
  
  mod.num1 = glm(a_0~1,family=binomial(link="logit"));
  num1 = ifelse(a_0== 0, 1-predict(mod.num1, type = "response"),predict(mod.num1, type = "response"))
  mod.num2=glm(a_1~a_0,family=binomial(link="logit"));
  num2 = ifelse(a_1== 0, 1-predict(mod.num2, type = "response"),predict(mod.num2, type = "response"))
  mod.num3=glm(a_1~1,family=binomial(link="logit"));
  num3 = ifelse(a_1== 0, 1-predict(mod.num3, type = "response"),predict(mod.num3, type = "response"))
  
  # Poids standard 
  
  wtx_0=(num1/dnom1)*(num3/dnom2);
  wtx_1=wtx_0;
  
  # Poids standard tronqué à 99.5e percentile
  
  wtx1_0=1*(num3/dnom2);
  wtx1_1=1*(num3/dnom2);
  
  # Poids standard tronqué à 99.5e percentile
  
  wtx2_0=(num1/dnom1)*1;
  wtx2_1=(num1/dnom1)*1;
  
  # Poids stabilisé marginal
  
  wtx3_0=pmin(num1/dnom1, quantile(num1/dnom1, 0.90))*(num3/dnom2);
  wtx3_1=wtx3_0;
  
  # Poids stabilisé marginal
  
  swax_0=(num1/dnom1)*pmin(num3/dnom2, quantile(num3/dnom2, 0.90));
  swax_1=swax_0;
  
  # Base de données finale 
  
  id = as.numeric(c(1:n));
  W=as.numeric(rep(1,n))
  
  data.a= data.frame(id,a_0,a_1,l_0,l_1,m_0,m_1,n_0,n_1,o_0,o_1,p_0,
                     p_1,q_0,q_1,r_0,r_1,u_0,u_1,v_0,v_1,t_0,t_1,wtx_0,wtx_1,
                     wtx1_0,wtx1_1,wtx2_0,wtx2_1,wtx3_0,wtx3_1,swax_0,swax_1,W,Y)
  
  ###########################################################################
  ##  Calcul des métriques 
  ## évaluant léquilibre de manière unique et global dans le cas non pondéré
  ###########################################################################
  
  ## Données non pondérées sous format long 
  
  data.b=my.lengthen(data.a,id="id",
                     times.exposure=c(0,1),
                     diagnostic=1,censoring="no",
                     times.covariate=c(0,1),
                     exposure="a", 
                     temporal.covariate=c("l","n","o","p","q","r","v","t"))
  
  
  ############################
  ##  Pour D et SMD         ##
  ############################ 
  
  mytable=my.balance(
    input=data.b,
    diagnostic=1,
    censoring="no",
    approach="none",
    scope="all",
    times.exposure=c(0,1),
    times.covariate=c(0,1),
    exposure="a",
    loop="yes",
    metric="SMD",
    sort.order= c("l","n","o","p","q","r","v","t")
  )
  
  
  table.0=mytable[mytable$time.covariate==0&mytable$time.exposure==0,];
  table.1=mytable[mytable$time.covariate==0&mytable$time.exposure==1,];
  table.2=mytable[mytable$time.covariate==1&mytable$time.exposure==1,];
  
  # Pour rendre les métriques comparables, on fait la moyenne des 
  # métriques sur toutes les covaraibles pour chaque temps de mesure
  
  mean.D[i]= mean(table.0$D);
  mean.D1[i]=mean(table.1$D);
  mean.D2[i]=mean(table.2$D);
  
  mean.SMD[i]=mean(table.0$SMD);
  mean.SMD1[i]=mean(table.1$SMD);
  mean.SMD2[i]=mean(table.2$SMD);
  
  #######################
  ##  Pour le KS       ##
  #######################
  
  mytable=my.balance(
    input=data.b,
    diagnostic=1,
    censoring="no",
    approach="none",
    scope="all",
    times.exposure=c(0,1),
    times.covariate=c(0,1),
    exposure="a",
    loop="yes",
    metric="KS",
    sort.order= c("l","n","o","p","q","r","v","t")
  )
  
  table.0=mytable[mytable$time.covariate==0&mytable$time.exposure==0,];
  table.1=mytable[mytable$time.covariate==0&mytable$time.exposure==1,];
  table.2=mytable[mytable$time.covariate==1&mytable$time.exposure==1,];
  
  # Pour rendre les métriques comparables, on fait la moyenne des 
  # métriques sur toutes les covaraibles pour chaque temps de mesure
  
  mean.KS[i]= mean(table.0$KS);
  mean.KS1[i]=mean(table.1$KS);
  mean.KS2[i]=mean(table.2$KS);
  
  ############################################
  ##  Overlapping coefficient (OVL)         ##
  ############################################
  
  mytable=my.balance(
    input=data.b,
    diagnostic=1,
    censoring="no",
    approach="none",
    scope="all",
    times.exposure=c(0,1),
    times.covariate=c(0,1),
    exposure="a",
    loop="yes",
    metric="OVL",
    sort.order= c("l","n","o","p","q","r","v","t")
  )
  
  table.0=mytable[mytable$time.covariate==0&mytable$time.exposure==0,];
  table.1=mytable[mytable$time.covariate==0&mytable$time.exposure==1,];
  table.2=mytable[mytable$time.covariate==1&mytable$time.exposure==1,];
  
  # Pour rendre les métriques comparables, on fait la moyenne des 
  # métriques sur toutes les covaraibles pour chaque temps de mesure
  
  mean.OVL[i]=mean(table.0$OVL);
  mean.OVL1[i]=mean(table.1$OVL);
  mean.OVL2[i]=mean(table.2$OVL);
  
  ############################################
  ##  Distance de Lévy LD                   ##
  ############################################
  
  data.c=mutate(data.b,W=1)
  
  data.l=filter(data.c,name.cov=="l",time.covariate==0,time.exposure==0)
  data1.l=filter(data.c,name.cov=="l",time.covariate==0,time.exposure==1)
  data2.l=filter(data.c,name.cov=="l",time.covariate==1,time.exposure==1)
  
  data.n=filter(data.c,name.cov=="n",time.covariate==0,time.exposure==0)
  data1.n=filter(data.c,name.cov=="n",time.covariate==0,time.exposure==1)
  data2.n=filter(data.c,name.cov=="n",time.covariate==1,time.exposure==1)
  
  data.o=filter(data.c,name.cov=="o",time.covariate==0,time.exposure==0)
  data1.o=filter(data.c,name.cov=="o",time.covariate==0,time.exposure==1)
  data2.o=filter(data.c,name.cov=="o",time.covariate==1,time.exposure==1)
  
  data.p=filter(data.c,name.cov=="p",time.covariate==0,time.exposure==0)
  data1.p=filter(data.c,name.cov=="p",time.covariate==0,time.exposure==1)
  data2.p=filter(data.c,name.cov=="p",time.covariate==1,time.exposure==1)
  
  data.q=filter(data.c,name.cov=="q",time.covariate==0,time.exposure==0)
  data1.q=filter(data.c,name.cov=="q",time.covariate==0,time.exposure==1)
  data2.q=filter(data.c,name.cov=="q",time.covariate==1,time.exposure==1)
  
  data.r=filter(data.c,name.cov=="r",time.covariate==0,time.exposure==0)
  data1.r=filter(data.c,name.cov=="r",time.covariate==0,time.exposure==1)
  data2.r=filter(data.c,name.cov=="r",time.covariate==1,time.exposure==1)
  
  data.v=filter(data.c,name.cov=="v",time.covariate==0,time.exposure==0)
  data1.v=filter(data.c,name.cov=="v",time.covariate==0,time.exposure==1)
  data2.v=filter(data.c,name.cov=="v",time.covariate==1,time.exposure==1)
  
  data.t=filter(data.c,name.cov=="t",time.covariate==0,time.exposure==0)
  data1.t=filter(data.c,name.cov=="t",time.covariate==0,time.exposure==1)
  data2.t=filter(data.c,name.cov=="t",time.covariate==1,time.exposure==1)
  
  
  # Distance de Lévy
  
  LD=c(ld(data.l,6,7,5),ld(data.n,6,7,5),ld(data.o,6,7,5),
       ld(data.p,6,7,5),ld(data.q,6,7,5),ld(data.r,6,7,5),
       ld(data.v,6,7,5),ld(data.t,6,7,5),ld(data1.l,6,7,5),
       ld(data2.l,6,7,5),ld(data1.n,6,7,5),ld(data2.n,6,7,5),
       ld(data1.o,6,7,5),ld(data2.o,6,7,5),ld(data1.p,6,7,5),
       ld(data2.p,6,7,5),ld(data1.q,6,7,5), ld(data2.q,6,7,5),
       ld(data1.r,6,7,5),ld(data2.r,6,7,5),ld(data1.v,6,7,5), 
       ld(data2.v,6,7,5),ld(data1.t,6,7,5), ld(data2.t,6,7,5));
  
  # Tableau récapitutlatif
  
  time.exposure=as.numeric(c(rep(0,8),rep(1,16)));
  time.covariate=as.numeric(c(rep(0,8),rep(c(0,1),8)));
  output=data.frame(time.exposure,time.covariate,LD);
  
  table.0=output[output$time.covariate==0&output$time.exposure==0,];
  table.1=output[output$time.covariate==0&output$time.exposure==1,];
  table.2=output[output$time.covariate==1&output$time.exposure==1,];
  
  # Pour rendre les métriques comparables, on fait la moyenne des 
  # métriques sur toutes les covaraibles pour chaque temps de mesure
  
  mean.LD[i]=mean(table.0$LD);
  mean.LD1[i]=mean(table.1$LD);
  mean.LD2[i]=mean(table.2$LD);
  
  ## Equilibre dans le cas global 
  
  ## Distance de Mahalanobis 
  
  # Pour t=0
  
  # A0 indépendant de X0
  
  covs=select(data.a,l_0,n_0,o_0,p_0,q_0,r_0,v_0,t_0)
  trt=data.a$a_0
  weight=data.a$W
  mean.MHB[i]=mhb(covs,trt,weight)
  
  ## Pour t=1
  
  ## A1 indépendant de X0, X1
  
  # Pour les covariables X0
  
  covs=select(data.a,l_0,n_0,o_0,p_0,q_0,r_0,v_0,t_0)
  trt=data.a$a_1
  weight=data.a$W
  mean.MHB1[i]=mhb(covs,trt,weight);
  
  # Pour les covariables X1
  
  covs=select(data.a,l_1,n_1,o_1,p_1,q_1,r_1,v_1,t_1)
  trt=data.a$a_1
  weight=data.a$W
  mean.MHB2[i]=mhb(covs,trt,weight);
  
  ## Différence pondérée générale
  
  # Pour t=0
  
  # A0 indépendant de X0
  
  weight=data.a$W
  dat=select(data.a,l_0,n_0,o_0,p_0,q_0,r_0,v_0,t_0,a_0)
  trt=dat$a_0
  mean.GWD[i]=gwd(dat,8, trt, weight)
  
  ## Pour t=1
  
  ## A1 indépendant de X0, X1
  
  # Pour les covariables X0
  
  weight=data.a$W
  dat=select(data.a,l_0,n_0,o_0,p_0,q_0,r_0,v_0,t_0,a_1)
  trt=dat$a_1
  mean.GWD1[i]=gwd(dat,8, trt, weight);
  
  # Pour les covariables X1
  
  weight=data.a$W
  dat=select(data.a,l_1,n_1,o_1,p_1,q_1,r_1,v_1,t_1,a_1)
  trt=dat$a_1
  mean.GWD2[i]=gwd(dat,8, trt, weight);  
  
  ## Statistique C
  
  # Calcul du score de propension dans les données non pondérées
  
  mod.dnom1=glm(a_0~l_0+m_0+n_0+o_0+p_0+q_0+r_0+u_0+v_0+t_0,data = data.a,family=binomial(link="logit"))
  mod.dnom2=glm(a_1~l_0+m_0+n_0+o_0+p_0+q_0+r_0+u_0+v_0+t_0+l_1+m_1+n_1+o_1+p_1+q_1+r_1+u_1+v_1+t_1,data = data.a,family=binomial(link="logit"))
  
  psx_0=mod.dnom1$fit
  psx_1=mod.dnom2$fit
  
  data1=data.frame(id,a_0,psx_0,a_1,psx_1);
  
  prd2=prediction(data1$psx_0,data1$a_0)
  mean.Stat.C[i]=2*(unlist(performance(prd2, "auc")@y.values)-0.5)
  prd3=prediction(data1$psx_1,data1$a_1)
  mean.Stat.C1[i]=2*(unlist(performance(prd3, "auc")@y.values)-0.5)
  mean.Stat.C2[i]=mean.Stat.C1[i]
  
  ##  Calcul du biais pour comparer les métriques  
  
  # Fit unweighted model
  lmfit=glm(as.factor(Y)~as.factor(a_0)+as.factor(a_1),
            data=data.a,family=binomial(link="logit"))
  
  s0=exp(lmfit$coef[2]+lmfit$coef[3])
  s1=exp(lmfit$coef[2])
  s2=exp(lmfit$coef[3])
  
  Biais0[i]=s0-exp(beta_1+beta_2)
  Biais1[i]=s1-exp(beta_1)
  Biais2[i]=s2-exp(beta_2)
  
  ###########################################################################
  ##  Calcul des métriques évaluant
  ## léquilibre de manière unique et global dans le cas pondéré
  ###########################################################################
  
  ## Conversion des données sous format long
  
  # Poids stabilisé tronqué à 95th
  
  mydata.a=my.lengthen(input=data.a,id="id",
                       diagnostic=3,censoring="no",times.exposure=c(0,1),
                       times.covariate=c(0,1),exposure="a",
                       temporal.covariate=c("l","n","o","p","q","r","v","t"),
                       weight.exposure="wtx");
  
  # # Poids stabilisé tronqué à 97.5th
  
  mydata.b=my.lengthen(input=data.a,id="id",
                       diagnostic=3,censoring="no",times.exposure=c(0,1),
                       times.covariate=c(0,1),exposure="a",
                       temporal.covariate=c("l","n","o","p","q","r","v","t"),
                       weight.exposure="wtx1");
  
  # # Poids stabilisé tronqué à 99th
  
  mydata.c=my.lengthen(input=data.a,id="id",
                       diagnostic=3,censoring="no",times.exposure=c(0,1),
                       times.covariate=c(0,1),exposure="a",
                       temporal.covariate=c("l","n","o","p","q","r","v","t"),
                       weight.exposure="wtx2");
  
  # Poids stabilisé tronqué à 99.5th
  
  mydata.d=my.lengthen(input=data.a,id="id",
                       diagnostic=3,censoring="no",times.exposure=c(0,1),
                       times.covariate=c(0,1),exposure="a",
                       temporal.covariate=c("l","n","o","p","q","r","v","t"),
                       weight.exposure="wtx3");
  
  # Poids stabilisé 
  
  mydata.e=my.lengthen(input=data.a,id="id",
                       diagnostic=3,censoring="no",times.exposure=c(0,1),
                       times.covariate=c(0,1),exposure="a",
                       temporal.covariate=c("l","n","o","p","q","r","v","t"),
                       weight.exposure="swax");
  
  ##  Calcul des métriques pour le poids stabilisé tronqué à 95th
  
  
  ############################
  ##  Pour D et SMD         ##
  ############################ 
  
  mytable=my.balance(
    input=mydata.a,
    diagnostic=3,
    censoring="no",
    approach="weight",
    scope="all",
    times.exposure=c(0,1),
    times.covariate=c(0,1),
    exposure="a",
    loop="yes",
    weight.exposure="wtx",
    metric="SMD",
    sort.order= c("l","n","o","p","q","r","v","t")
  )
  
  table.0=mytable[mytable$time.covariate==0&mytable$time.exposure==0,];
  table.1=mytable[mytable$time.covariate==0&mytable$time.exposure==1,];
  table.2=mytable[mytable$time.covariate==1&mytable$time.exposure==1,];
  
  mean.D3[i]=mean(table.0$D);
  mean.D4[i]=mean(table.1$D);
  mean.D5[i]=mean(table.2$D);
  
  mean.SMD3[i]=mean(table.0$SMD);
  mean.SMD4[i]=mean(table.1$SMD);
  mean.SMD5[i]=mean(table.2$SMD);
  
  #######################
  ##  Pour le KS       ##
  #######################
  
  mytable=my.balance(
    input=mydata.a,
    diagnostic=3,
    censoring="no",
    approach="weight",
    scope="all",
    times.exposure=c(0,1),
    times.covariate=c(0,1),
    exposure="a",
    loop="yes",
    weight.exposure="wtx",
    metric="KS",
    sort.order= c("l","n","o","p","q","r","v","t")
  )
  
  table.0=mytable[mytable$time.covariate==0&mytable$time.exposure==0,];
  table.1=mytable[mytable$time.covariate==0&mytable$time.exposure==1,];
  table.2=mytable[mytable$time.covariate==1&mytable$time.exposure==1,];
  
  mean.KS3[i]=mean(table.0$KS);
  mean.KS4[i]=mean(table.1$KS);
  mean.KS5[i]=mean(table.2$KS);
  
  ############################################
  ##  Overlapping coefficient (OVL)         ##
  ############################################
  
  mytable=my.balance(
    input=mydata.a,
    diagnostic=3,
    censoring="no",
    approach="weight",
    scope="all",
    times.exposure=c(0,1),
    times.covariate=c(0,1),
    exposure="a",
    loop="yes",
    weight.exposure="wtx",
    metric="OVL",
    sort.order= c("l","n","o","p","q","r","v","t")
  )
  
  table.0=mytable[mytable$time.covariate==0&mytable$time.exposure==0,];
  table.1=mytable[mytable$time.covariate==0&mytable$time.exposure==1,];
  table.2=mytable[mytable$time.covariate==1&mytable$time.exposure==1,];
  
  mean.OVL3[i]=mean(table.0$OVL);
  mean.OVL4[i]=mean(table.1$OVL);
  mean.OVL5[i]=mean(table.2$OVL);
  
  ############################################
  ##  Distance de Lévy LD                   ##
  ############################################
  
  data.l=filter(mydata.a,name.cov=="l",time.covariate==0,time.exposure==0)
  data1.l=filter(mydata.a,name.cov=="l",time.covariate==0,time.exposure==1)
  data2.l=filter(mydata.a,name.cov=="l",time.covariate==1,time.exposure==1)
  
  data.n=filter(mydata.a,name.cov=="n",time.covariate==0,time.exposure==0)
  data1.n=filter(mydata.a,name.cov=="n",time.covariate==0,time.exposure==1)
  data2.n=filter(mydata.a,name.cov=="n",time.covariate==1,time.exposure==1)
  
  data.o=filter(mydata.a,name.cov=="o",time.covariate==0,time.exposure==0)
  data1.o=filter(mydata.a,name.cov=="o",time.covariate==0,time.exposure==1)
  data2.o=filter(mydata.a,name.cov=="o",time.covariate==1,time.exposure==1)
  
  data.p=filter(mydata.a,name.cov=="p",time.covariate==0,time.exposure==0)
  data1.p=filter(mydata.a,name.cov=="p",time.covariate==0,time.exposure==1)
  data2.p=filter(mydata.a,name.cov=="p",time.covariate==1,time.exposure==1)
  
  data.q=filter(mydata.a,name.cov=="q",time.covariate==0,time.exposure==0)
  data1.q=filter(mydata.a,name.cov=="q",time.covariate==0,time.exposure==1)
  data2.q=filter(mydata.a,name.cov=="q",time.covariate==1,time.exposure==1)
  
  data.r=filter(mydata.a,name.cov=="r",time.covariate==0,time.exposure==0)
  data1.r=filter(mydata.a,name.cov=="r",time.covariate==0,time.exposure==1)
  data2.r=filter(mydata.a,name.cov=="r",time.covariate==1,time.exposure==1)
  
  data.v=filter(mydata.a,name.cov=="v",time.covariate==0,time.exposure==0)
  data1.v=filter(mydata.a,name.cov=="v",time.covariate==0,time.exposure==1)
  data2.v=filter(mydata.a,name.cov=="v",time.covariate==1,time.exposure==1)
  
  data.t=filter(mydata.a,name.cov=="t",time.covariate==0,time.exposure==0)
  data1.t=filter(mydata.a,name.cov=="t",time.covariate==0,time.exposure==1)
  data2.t=filter(mydata.a,name.cov=="t",time.covariate==1,time.exposure==1)
  
  
  # Distance de Lévy
  
  LD=c(ld(data.l,6,7,5),ld(data.n,6,7,5),ld(data.o,6,7,5),
       ld(data.p,6,7,5),ld(data.q,6,7,5),ld(data.r,6,7,5),
       ld(data.v,6,7,5),ld(data.t,6,7,5),ld(data1.l,6,7,5),
       ld(data2.l,6,7,5),ld(data1.n,6,7,5),ld(data2.n,6,7,5),
       ld(data1.o,6,7,5),ld(data2.o,6,7,5),ld(data1.p,6,7,5),
       ld(data2.p,6,7,5),ld(data1.q,6,7,5), ld(data2.q,6,7,5),
       ld(data1.r,6,7,5),ld(data2.r,6,7,5),ld(data1.v,6,7,5), 
       ld(data2.v,6,7,5),ld(data1.t,6,7,5), ld(data2.t,6,7,5));
  
  # Tableau récapitutlatif
  
  time.exposure=as.numeric(c(rep(0,8),rep(1,16)));
  time.covariate=as.numeric(c(rep(0,8),rep(c(0,1),8)));
  output=data.frame(time.exposure,time.covariate,LD);
  
  table.0=output[output$time.covariate==0&output$time.exposure==0,];
  table.1=output[output$time.covariate==0&output$time.exposure==1,];
  table.2=output[output$time.covariate==1&output$time.exposure==1,];
  
  mean.LD3[i]=mean(table.0$LD);
  mean.LD4[i]=mean(table.1$LD);
  mean.LD5[i]=mean(table.2$LD);
  
  ## Equilibre dans le cas global 
  
  ## Distance de Mahalanobis 
  
  # Pour t=0
  
  # A0 indépendant de X0
  
  covs=select(data.a,l_0,n_0,o_0,p_0,q_0,r_0,v_0,t_0)
  trt=data.a$a_0
  weight=data.a$wtx_0
  mean.MHB3[i]=mhb(covs,trt,weight)
  
  # Pour t=1
  
  # A1 indépendant de X0,X1
  
  # Pour les covariables X0
  
  covs=select(data.a,l_0,n_0,o_0,p_0,q_0,r_0,v_0,t_0)
  trt=data.a$a_1
  weight=data.a$wtx_1
  mean.MHB4[i]=mhb(covs,trt,weight);
  
  # Pour les covariables X1
  
  covs=select(data.a,l_1,n_1,o_1,p_1,q_1,r_1,v_1,t_1)
  trt=data.a$a_1
  weight=data.a$wtx_1
  mean.MHB5[i]=mhb(covs,trt,weight);
  
  # Différence pondérée générale
  
  weight=data.a$wtx_0
  dat=select(data.a,l_0,n_0,o_0,p_0,q_0,r_0,v_0,t_0,a_0)
  trt=dat$a_0
  mean.GWD3[i]=gwd(dat,8, trt, weight)
  
  weight=data.a$wtx_1
  dat=select(data.a,l_0,n_0,o_0,p_0,q_0,r_0,v_0,t_0,a_1)
  trt=dat$a_1
  mean.GWD4[i]=gwd(dat,8, trt, weight);
  
  weight=data.a$wtx_1
  dat=select(data.a,l_1,n_1,o_1,p_1,q_1,r_1,v_1,t_1,a_1)
  trt=dat$a_1
  mean.GWD5[i]=gwd(dat,8, trt, weight);
  
  ## Statistique C
  
  # Calcul du score de propension dans les données pondérées
  
  mod.dnom1=glm(a_0~l_0+m_0+n_0+o_0+p_0+q_0+r_0+u_0+v_0+t_0,data = data.a,
                weights=wtx_1,family=binomial(link="logit"))
  mod.dnom2=glm(a_1~l_0+m_0+n_0+o_0+p_0+q_0+r_0+u_0+v_0+t_0+l_1+m_1+n_1+o_1+p_1+q_1
                +r_1+u_1+v_1+t_1,data = data.a,weights=wtx_1,family=binomial(link="logit"))
  
  psx_0=mod.dnom1$fit
  psx_1=mod.dnom2$fit
  
  data1=data.frame(id,a_0,psx_0,a_1,psx_1);
  
  prd2=prediction(data1$psx_0,data1$a_0)
  mean.Stat.C3[i]=2*(unlist(performance(prd2, "auc")@y.values)-0.5)
  prd3=prediction(data1$psx_1,data1$a_1)
  mean.Stat.C4[i]=2*(unlist(performance(prd3, "auc")@y.values)-0.5)
  mean.Stat.C5[i]=mean.Stat.C4[i]
  
  ##  Calcul du biais pour comparer les métriques  
  
  # Fit weighted model
  lmfit=glm(as.factor(Y)~as.factor(a_0)+as.factor(a_1),weights=wtx_1,
            data=data.a,family=binomial(link="logit"))
  
  s3=exp(lmfit$coef[2]+lmfit$coef[3])
  s4=exp(lmfit$coef[2])
  s5=exp(lmfit$coef[3])
  
  Biais3[i]=s3-exp(beta_1+beta_2)
  Biais4[i]=s4-exp(beta_1)
  Biais5[i]=s5-exp(beta_2)
  
  ##  Calcul des métriques pour le poids stabilisé tronqué à 97.5th
  
  ############################
  ##  Pour D et SMD         ##
  ############################ 
  
  mytable=my.balance(
    input=mydata.b,
    diagnostic=3,
    censoring="no",
    approach="weight",
    scope="all",
    times.exposure=c(0,1),
    times.covariate=c(0,1),
    exposure="a",
    loop="yes",
    weight.exposure="wtx1",
    metric="SMD",
    sort.order= c("l","n","o","p","q","r","v","t")
  )
  
  table.0=mytable[mytable$time.covariate==0&mytable$time.exposure==0,];
  table.1=mytable[mytable$time.covariate==0&mytable$time.exposure==1,];
  table.2=mytable[mytable$time.covariate==1&mytable$time.exposure==1,];
  
  mean.D6[i]=mean(table.0$D);
  mean.D7[i]=mean(table.1$D);
  mean.D8[i]=mean(table.2$D);
  
  mean.SMD6[i]=mean(table.0$SMD);
  mean.SMD7[i]=mean(table.1$SMD);
  mean.SMD8[i]=mean(table.2$SMD);
  
  #######################
  ##  Pour le KS       ##
  #######################
  
  mytable=my.balance(
    input=mydata.b,
    diagnostic=3,
    censoring="no",
    approach="weight",
    scope="all",
    times.exposure=c(0,1),
    times.covariate=c(0,1),
    exposure="a",
    loop="yes",
    weight.exposure="wtx1",
    metric="KS",
    sort.order= c("l","n","o","p","q","r","v","t")
  )
  
  table.0=mytable[mytable$time.covariate==0&mytable$time.exposure==0,];
  table.1=mytable[mytable$time.covariate==0&mytable$time.exposure==1,];
  table.2=mytable[mytable$time.covariate==1&mytable$time.exposure==1,];
  
  mean.KS6[i]=mean(table.0$KS);
  mean.KS7[i]=mean(table.1$KS);
  mean.KS8[i]=mean(table.2$KS);
  
  ############################################
  ##  Overlapping coefficient (OVL)         ##
  ############################################
  
  mytable=my.balance(
    input=mydata.b,
    diagnostic=3,
    censoring="no",
    approach="weight",
    scope="all",
    times.exposure=c(0,1),
    times.covariate=c(0,1),
    exposure="a",
    loop="yes",
    weight.exposure="wtx1",
    metric="OVL",
    sort.order= c("l","n","o","p","q","r","v","t")
  )
  
  table.0=mytable[mytable$time.covariate==0&mytable$time.exposure==0,];
  table.1=mytable[mytable$time.covariate==0&mytable$time.exposure==1,];
  table.2=mytable[mytable$time.covariate==1&mytable$time.exposure==1,];
  
  mean.OVL6[i]=mean(table.0$OVL);
  mean.OVL7[i]=mean(table.1$OVL);
  mean.OVL8[i]=mean(table.2$OVL);
  
  ############################################
  ##  Distance de Lévy LD                   ##
  ############################################
  
  data.l=filter(mydata.b,name.cov=="l",time.covariate==0,time.exposure==0)
  data1.l=filter(mydata.b,name.cov=="l",time.covariate==0,time.exposure==1)
  data2.l=filter(mydata.b,name.cov=="l",time.covariate==1,time.exposure==1)
  
  data.n=filter(mydata.b,name.cov=="n",time.covariate==0,time.exposure==0)
  data1.n=filter(mydata.b,name.cov=="n",time.covariate==0,time.exposure==1)
  data2.n=filter(mydata.b,name.cov=="n",time.covariate==1,time.exposure==1)
  
  data.o=filter(mydata.b,name.cov=="o",time.covariate==0,time.exposure==0)
  data1.o=filter(mydata.b,name.cov=="o",time.covariate==0,time.exposure==1)
  data2.o=filter(mydata.b,name.cov=="o",time.covariate==1,time.exposure==1)
  
  data.p=filter(mydata.b,name.cov=="p",time.covariate==0,time.exposure==0)
  data1.p=filter(mydata.b,name.cov=="p",time.covariate==0,time.exposure==1)
  data2.p=filter(mydata.b,name.cov=="p",time.covariate==1,time.exposure==1)
  
  data.q=filter(mydata.b,name.cov=="q",time.covariate==0,time.exposure==0)
  data1.q=filter(mydata.b,name.cov=="q",time.covariate==0,time.exposure==1)
  data2.q=filter(mydata.b,name.cov=="q",time.covariate==1,time.exposure==1)
  
  data.r=filter(mydata.b,name.cov=="r",time.covariate==0,time.exposure==0)
  data1.r=filter(mydata.b,name.cov=="r",time.covariate==0,time.exposure==1)
  data2.r=filter(mydata.b,name.cov=="r",time.covariate==1,time.exposure==1)
  
  data.v=filter(mydata.b,name.cov=="v",time.covariate==0,time.exposure==0)
  data1.v=filter(mydata.b,name.cov=="v",time.covariate==0,time.exposure==1)
  data2.v=filter(mydata.b,name.cov=="v",time.covariate==1,time.exposure==1)
  
  data.t=filter(mydata.b,name.cov=="t",time.covariate==0,time.exposure==0)
  data1.t=filter(mydata.b,name.cov=="t",time.covariate==0,time.exposure==1)
  data2.t=filter(mydata.b,name.cov=="t",time.covariate==1,time.exposure==1)
  
  
  # Distance de Lévy
  
  LD=c(ld(data.l,6,7,5),ld(data.n,6,7,5),ld(data.o,6,7,5),
       ld(data.p,6,7,5),ld(data.q,6,7,5),ld(data.r,6,7,5),
       ld(data.v,6,7,5),ld(data.t,6,7,5),ld(data1.l,6,7,5),
       ld(data2.l,6,7,5),ld(data1.n,6,7,5),ld(data2.n,6,7,5),
       ld(data1.o,6,7,5),ld(data2.o,6,7,5),ld(data1.p,6,7,5),
       ld(data2.p,6,7,5),ld(data1.q,6,7,5), ld(data2.q,6,7,5),
       ld(data1.r,6,7,5),ld(data2.r,6,7,5),ld(data1.v,6,7,5), 
       ld(data2.v,6,7,5),ld(data1.t,6,7,5), ld(data2.t,6,7,5));
  
  # Tableau récapitutlatif
  
  time.exposure=as.numeric(c(rep(0,8),rep(1,16)));
  time.covariate=as.numeric(c(rep(0,8),rep(c(0,1),8)));
  output=data.frame(time.exposure,time.covariate,LD);
  
  table.0=output[output$time.covariate==0&output$time.exposure==0,];
  table.1=output[output$time.covariate==0&output$time.exposure==1,];
  table.2=output[output$time.covariate==1&output$time.exposure==1,];
  
  mean.LD6[i]=mean(table.0$LD);
  mean.LD7[i]=mean(table.1$LD);
  mean.LD8[i]=mean(table.2$LD);
  
  ## Equilibre dans le cas global 
  
  # Distance de Mahalanobis 
  
  # Pour t=0
  
  # A0 indépendant de X0
  
  covs=select(data.a,l_0,n_0,o_0,p_0,q_0,r_0,v_0,t_0)
  trt=data.a$a_0
  weight=data.a$wtx1_0
  mean.MHB6[i]=mhb(covs,trt,weight)
  
  # Pour t=1
  
  # A1 indépendant de X0, X1
  
  # Pour les covariables X0
  
  covs=select(data.a,l_0,n_0,o_0,p_0,q_0,r_0,v_0,t_0)
  trt=data.a$a_1
  weight=data.a$wtx1_1
  mean.MHB7[i]=mhb(covs,trt,weight);
  
  # Pour les covariables X1
  
  covs=select(data.a,l_1,n_1,o_1,p_1,q_1,r_1,v_1,t_1)
  trt=data.a$a_1
  weight=data.a$wtx1_1
  mean.MHB8[i]=mhb(covs,trt,weight);
  
  # Différence pondérée générale
  
  weight=data.a$wtx1_0
  dat=select(data.a,l_0,n_0,o_0,p_0,q_0,r_0,v_0,t_0,a_0)
  trt=dat$a_0
  mean.GWD6[i]=gwd(dat,8, trt, weight)
  
  weight=data.a$wtx1_1
  dat=select(data.a,l_0,n_0,o_0,p_0,q_0,r_0,v_0,t_0,a_1)
  trt=dat$a_1
  mean.GWD7[i]=gwd(dat,8, trt, weight);
  
  weight=data.a$wtx1_1
  dat=select(data.a,l_1,n_1,o_1,p_1,q_1,r_1,v_1,t_1,a_1)
  trt=dat$a_1
  mean.GWD8[i]=gwd(dat,8, trt, weight);
  
  # Statistique C
  
  # Calcul du score de propension dans les données pondérées
  
  mod.dnom1=glm(a_0~l_0+m_0+n_0+o_0+p_0+q_0+r_0+u_0+v_0+t_0,data = data.a,
                weights=wtx1_1,family=binomial(link="logit"))
  mod.dnom2=glm(a_1~l_0+m_0+n_0+o_0+p_0+q_0+r_0+u_0+v_0+t_0+l_1+m_1+n_1+o_1+p_1+q_1
                +r_1+u_1+v_1+t_1,data = data.a,weights=wtx1_1,family=binomial(link="logit"))
  
  psx_0=mod.dnom1$fit
  psx_1=mod.dnom2$fit
  
  data1=data.frame(id,a_0,psx_0,a_1,psx_1);
  
  prd2=prediction(data1$psx_0,data1$a_0)
  mean.Stat.C6[i]=2*(unlist(performance(prd2, "auc")@y.values)-0.5)
  prd3=prediction(data1$psx_1,data1$a_1)
  mean.Stat.C7[i]=2*(unlist(performance(prd3, "auc")@y.values)-0.5)
  mean.Stat.C8[i]=mean.Stat.C7[i]
  
  ##  Calcul du biais pour comparer les métriques   
  
  # Fit weighted model
  lmfit=glm(as.factor(Y)~as.factor(a_0)+as.factor(a_1),weights=wtx1_1,
            data=data.a,family=binomial(link="logit"))
  
  s6=exp(lmfit$coef[2]+lmfit$coef[3])
  s7=exp(lmfit$coef[2])
  s8=exp(lmfit$coef[3])
  
  Biais6[i]=s6-exp(beta_1+beta_2)
  Biais7[i]=s7-exp(beta_1)
  Biais8[i]=s8-exp(beta_2)
  
  ##  Calcul des métriques pour le poids stabilisé tronqué à 99th
  
  
  ############################
  ##  Pour D et SMD         ##
  ############################ 
  
  mytable=my.balance(
    input=mydata.c,
    diagnostic=3,
    censoring="no",
    approach="weight",
    scope="all",
    times.exposure=c(0,1),
    times.covariate=c(0,1),
    exposure="a",
    loop="yes",
    weight.exposure="wtx2",
    metric="SMD",
    sort.order= c("l","n","o","p","q","r","v","t")
  )
  
  table.0=mytable[mytable$time.covariate==0&mytable$time.exposure==0,];
  table.1=mytable[mytable$time.covariate==0&mytable$time.exposure==1,];
  table.2=mytable[mytable$time.covariate==1&mytable$time.exposure==1,];
  
  mean.D9[i]=mean(table.0$D);
  mean.D10[i]=mean(table.1$D);
  mean.D11[i]=mean(table.2$D);
  
  mean.SMD9[i]=mean(table.0$SMD);
  mean.SMD10[i]=mean(table.1$SMD);
  mean.SMD11[i]=mean(table.2$SMD);
  
  #######################
  ##  Pour le KS       ##
  #######################
  
  mytable=my.balance(
    input=mydata.c,
    diagnostic=3,
    censoring="no",
    approach="weight",
    scope="all",
    times.exposure=c(0,1),
    times.covariate=c(0,1),
    exposure="a",
    loop="yes",
    weight.exposure="wtx2",
    metric="KS",
    sort.order= c("l","n","o","p","q","r","v","t")
  )
  
  table.0=mytable[mytable$time.covariate==0&mytable$time.exposure==0,];
  table.1=mytable[mytable$time.covariate==0&mytable$time.exposure==1,];
  table.2=mytable[mytable$time.covariate==1&mytable$time.exposure==1,];
  
  mean.KS9[i]=mean(table.0$KS);
  mean.KS10[i]=mean(table.1$KS);
  mean.KS11[i]=mean(table.2$KS);
  
  ############################################
  ##  Overlapping coefficient (OVL)         ##
  ############################################
  
  mytable=my.balance(
    input=mydata.c,
    diagnostic=3,
    censoring="no",
    approach="weight",
    scope="all",
    times.exposure=c(0,1),
    times.covariate=c(0,1),
    exposure="a",
    loop="yes",
    weight.exposure="wtx2",
    metric="OVL",
    sort.order= c("l","n","o","p","q","r","v","t")
  )
  
  table.0=mytable[mytable$time.covariate==0&mytable$time.exposure==0,];
  table.1=mytable[mytable$time.covariate==0&mytable$time.exposure==1,];
  table.2=mytable[mytable$time.covariate==1&mytable$time.exposure==1,];
  
  mean.OVL9[i]=mean(table.0$OVL);
  mean.OVL10[i]=mean(table.1$OVL);
  mean.OVL11[i]=mean(table.2$OVL);
  
  ############################################
  ##  Distance de Lévy LD                   ##
  ############################################
  
  data.l=filter(mydata.c,name.cov=="l",time.covariate==0,time.exposure==0)
  data1.l=filter(mydata.c,name.cov=="l",time.covariate==0,time.exposure==1)
  data2.l=filter(mydata.c,name.cov=="l",time.covariate==1,time.exposure==1)
  
  data.n=filter(mydata.c,name.cov=="n",time.covariate==0,time.exposure==0)
  data1.n=filter(mydata.c,name.cov=="n",time.covariate==0,time.exposure==1)
  data2.n=filter(mydata.c,name.cov=="n",time.covariate==1,time.exposure==1)
  
  data.o=filter(mydata.c,name.cov=="o",time.covariate==0,time.exposure==0)
  data1.o=filter(mydata.c,name.cov=="o",time.covariate==0,time.exposure==1)
  data2.o=filter(mydata.c,name.cov=="o",time.covariate==1,time.exposure==1)
  
  data.p=filter(mydata.c,name.cov=="p",time.covariate==0,time.exposure==0)
  data1.p=filter(mydata.c,name.cov=="p",time.covariate==0,time.exposure==1)
  data2.p=filter(mydata.c,name.cov=="p",time.covariate==1,time.exposure==1)
  
  data.q=filter(mydata.c,name.cov=="q",time.covariate==0,time.exposure==0)
  data1.q=filter(mydata.c,name.cov=="q",time.covariate==0,time.exposure==1)
  data2.q=filter(mydata.c,name.cov=="q",time.covariate==1,time.exposure==1)
  
  data.r=filter(mydata.c,name.cov=="r",time.covariate==0,time.exposure==0)
  data1.r=filter(mydata.c,name.cov=="r",time.covariate==0,time.exposure==1)
  data2.r=filter(mydata.c,name.cov=="r",time.covariate==1,time.exposure==1)
  
  data.v=filter(mydata.c,name.cov=="v",time.covariate==0,time.exposure==0)
  data1.v=filter(mydata.c,name.cov=="v",time.covariate==0,time.exposure==1)
  data2.v=filter(mydata.c,name.cov=="v",time.covariate==1,time.exposure==1)
  
  data.t=filter(mydata.c,name.cov=="t",time.covariate==0,time.exposure==0)
  data1.t=filter(mydata.c,name.cov=="t",time.covariate==0,time.exposure==1)
  data2.t=filter(mydata.c,name.cov=="t",time.covariate==1,time.exposure==1)
  
  
  # Distance de Lévy
  
  LD=c(ld(data.l,6,7,5),ld(data.n,6,7,5),ld(data.o,6,7,5),
       ld(data.p,6,7,5),ld(data.q,6,7,5),ld(data.r,6,7,5),
       ld(data.v,6,7,5),ld(data.t,6,7,5),ld(data1.l,6,7,5),
       ld(data2.l,6,7,5),ld(data1.n,6,7,5),ld(data2.n,6,7,5),
       ld(data1.o,6,7,5),ld(data2.o,6,7,5),ld(data1.p,6,7,5),
       ld(data2.p,6,7,5),ld(data1.q,6,7,5), ld(data2.q,6,7,5),
       ld(data1.r,6,7,5),ld(data2.r,6,7,5),ld(data1.v,6,7,5), 
       ld(data2.v,6,7,5),ld(data1.t,6,7,5), ld(data2.t,6,7,5));
  
  # Tableau récapitutlatif
  
  time.exposure=as.numeric(c(rep(0,8),rep(1,16)));
  time.covariate=as.numeric(c(rep(0,8),rep(c(0,1),8)));
  output=data.frame(time.exposure,time.covariate,LD);
  
  table.0=output[output$time.covariate==0&output$time.exposure==0,];
  table.1=output[output$time.covariate==0&output$time.exposure==1,];
  table.2=output[output$time.covariate==1&output$time.exposure==1,];
  
  mean.LD9[i]=mean(table.0$LD);
  mean.LD10[i]=mean(table.1$LD);
  mean.LD11[i]=mean(table.2$LD);
  
  ## Equilibre dans le cas global 
  
  # Distance de Mahalanobis 
  
  # Pour t=0
  
  # A0 indépendant de X0
  
  covs=select(data.a,l_0,n_0,o_0,p_0,q_0,r_0,v_0,t_0)
  trt=data.a$a_0
  weight=data.a$wtx2_0
  mean.MHB9[i]=mhb(covs,trt,weight)
  
  # Pour t=1
  
  # A1 indépendant de X0, A0, X1
  
  # Pour les covariables X0
  
  covs=select(data.a,l_0,n_0,o_0,p_0,q_0,r_0,v_0,t_0)
  trt=data.a$a_1
  weight=data.a$wtx2_1
  mean.MHB10[i]=mhb(covs,trt,weight);
  
  # Pour les covariables X1
  
  covs=select(data.a,l_1,n_1,o_1,p_1,q_1,r_1,v_1,t_1)
  trt=data.a$a_1
  weight=data.a$wtx2_1
  mean.MHB11[i]=mhb(covs,trt,weight);
  
  # Différence pondérée générale
  
  weight=data.a$wtx2_0
  dat=select(data.a,l_0,n_0,o_0,p_0,q_0,r_0,v_0,t_0,a_0)
  trt=dat$a_0
  mean.GWD9[i]=gwd(dat,8, trt, weight)
  
  weight=data.a$wtx2_1
  dat=select(data.a,l_0,n_0,o_0,p_0,q_0,r_0,v_0,t_0,a_1)
  trt=dat$a_1
  mean.GWD10[i]=gwd(dat,8, trt, weight);
  
  weight=data.a$wtx2_1
  dat=select(data.a,l_1,n_1,o_1,p_1,q_1,r_1,v_1,t_1,a_1)
  trt=dat$a_1
  mean.GWD11[i]=gwd(dat,8, trt, weight);
  
  # Statistique C
  
  # Calcul du score de propension dans les données pondérées
  
  mod.dnom1=glm(a_0~l_0+m_0+n_0+o_0+p_0+q_0+r_0+u_0+v_0+t_0,data = data.a,
                weights=wtx2_1,family=binomial(link="logit"))
  mod.dnom2=glm(a_1~l_0+m_0+n_0+o_0+p_0+q_0+r_0+u_0+v_0+t_0+l_1+m_1+n_1+o_1+p_1+q_1
                +r_1+u_1+v_1+t_1,data = data.a,weights=wtx2_1,family=binomial(link="logit"))
  
  psx_0=mod.dnom1$fit
  psx_1=mod.dnom2$fit
  
  data1=data.frame(id,a_0,psx_0,a_1,psx_1);
  
  prd2=prediction(data1$psx_0,data1$a_0)
  mean.Stat.C9[i]=2*(unlist(performance(prd2, "auc")@y.values)-0.5)
  prd3=prediction(data1$psx_1,data1$a_1)
  mean.Stat.C10[i]=2*(unlist(performance(prd3, "auc")@y.values)-0.5)
  mean.Stat.C11[i]=mean.Stat.C10[i]
  
  ##  Calcul du biais pour comparer les métriques  ### 
  
  # Fit weighted model
  lmfit=glm(as.factor(Y)~as.factor(a_0)+as.factor(a_1),weights=wtx2_1,
            data=data.a,family=binomial(link="logit"))
  
  s9=exp(lmfit$coef[2]+lmfit$coef[3])
  s10=exp(lmfit$coef[2])
  s11=exp(lmfit$coef[3])
  
  Biais9[i]=s9-exp(beta_1+beta_2)
  Biais10[i]=s10-exp(beta_1)
  Biais11[i]=s11-exp(beta_2)
  
  ##  Calcul des métriques pour le poids stabilisé tronqué à 99.5th
  
  
  ############################
  ##  Pour D et SMD         ##
  ############################ 
  
  mytable=my.balance(
    input=mydata.d,
    diagnostic=3,
    censoring="no",
    approach="weight",
    scope="all",
    times.exposure=c(0,1),
    times.covariate=c(0,1),
    exposure="a",
    loop="yes",
    weight.exposure="wtx3",
    metric="SMD",
    sort.order= c("l","n","o","p","q","r","v","t")
  )
  
  table.0=mytable[mytable$time.covariate==0&mytable$time.exposure==0,];
  table.1=mytable[mytable$time.covariate==0&mytable$time.exposure==1,];
  table.2=mytable[mytable$time.covariate==1&mytable$time.exposure==1,];
  
  mean.D12[i]=mean(table.0$D);
  mean.D13[i]=mean(table.1$D);
  mean.D14[i]=mean(table.2$D);
  
  mean.SMD12[i]=mean(table.0$SMD);
  mean.SMD13[i]=mean(table.1$SMD);
  mean.SMD14[i]=mean(table.2$SMD);
  
  #######################
  ##  Pour le KS       ##
  #######################
  
  mytable=my.balance(
    input=mydata.d,
    diagnostic=3,
    censoring="no",
    approach="weight",
    scope="all",
    times.exposure=c(0,1),
    times.covariate=c(0,1),
    exposure="a",
    loop="yes",
    weight.exposure="wtx3",
    metric="KS",
    sort.order= c("l","n","o","p","q","r","v","t")
  )
  
  table.0=mytable[mytable$time.covariate==0&mytable$time.exposure==0,];
  table.1=mytable[mytable$time.covariate==0&mytable$time.exposure==1,];
  table.2=mytable[mytable$time.covariate==1&mytable$time.exposure==1,];
  
  mean.KS12[i]=mean(table.0$KS);
  mean.KS13[i]=mean(table.1$KS);
  mean.KS14[i]=mean(table.2$KS);
  
  ############################################
  ##  Overlapping coefficient (OVL)         ##
  ############################################
  
  mytable=my.balance(
    input=mydata.d,
    diagnostic=3,
    censoring="no",
    approach="weight",
    scope="all",
    times.exposure=c(0,1),
    times.covariate=c(0,1),
    exposure="a",
    loop="yes",
    weight.exposure="wtx3",
    metric="OVL",
    sort.order= c("l","n","o","p","q","r","v","t")
  )
  
  table.0=mytable[mytable$time.covariate==0&mytable$time.exposure==0,];
  table.1=mytable[mytable$time.covariate==0&mytable$time.exposure==1,];
  table.2=mytable[mytable$time.covariate==1&mytable$time.exposure==1,];
  
  mean.OVL12[i]=mean(table.0$OVL);
  mean.OVL13[i]=mean(table.1$OVL);
  mean.OVL14[i]=mean(table.2$OVL);
  
  ############################################
  ##  Distance de Lévy LD                   ##
  ############################################
  
  data.l=filter(mydata.d,name.cov=="l",time.covariate==0,time.exposure==0)
  data1.l=filter(mydata.d,name.cov=="l",time.covariate==0,time.exposure==1)
  data2.l=filter(mydata.d,name.cov=="l",time.covariate==1,time.exposure==1)
  
  data.n=filter(mydata.d,name.cov=="n",time.covariate==0,time.exposure==0)
  data1.n=filter(mydata.d,name.cov=="n",time.covariate==0,time.exposure==1)
  data2.n=filter(mydata.d,name.cov=="n",time.covariate==1,time.exposure==1)
  
  data.o=filter(mydata.d,name.cov=="o",time.covariate==0,time.exposure==0)
  data1.o=filter(mydata.d,name.cov=="o",time.covariate==0,time.exposure==1)
  data2.o=filter(mydata.d,name.cov=="o",time.covariate==1,time.exposure==1)
  
  data.p=filter(mydata.d,name.cov=="p",time.covariate==0,time.exposure==0)
  data1.p=filter(mydata.d,name.cov=="p",time.covariate==0,time.exposure==1)
  data2.p=filter(mydata.d,name.cov=="p",time.covariate==1,time.exposure==1)
  
  data.q=filter(mydata.d,name.cov=="q",time.covariate==0,time.exposure==0)
  data1.q=filter(mydata.d,name.cov=="q",time.covariate==0,time.exposure==1)
  data2.q=filter(mydata.d,name.cov=="q",time.covariate==1,time.exposure==1)
  
  data.r=filter(mydata.d,name.cov=="r",time.covariate==0,time.exposure==0)
  data1.r=filter(mydata.d,name.cov=="r",time.covariate==0,time.exposure==1)
  data2.r=filter(mydata.d,name.cov=="r",time.covariate==1,time.exposure==1)
  
  data.v=filter(mydata.d,name.cov=="v",time.covariate==0,time.exposure==0)
  data1.v=filter(mydata.d,name.cov=="v",time.covariate==0,time.exposure==1)
  data2.v=filter(mydata.d,name.cov=="v",time.covariate==1,time.exposure==1)
  
  data.t=filter(mydata.d,name.cov=="t",time.covariate==0,time.exposure==0)
  data1.t=filter(mydata.d,name.cov=="t",time.covariate==0,time.exposure==1)
  data2.t=filter(mydata.d,name.cov=="t",time.covariate==1,time.exposure==1)
  
  # Distance de Lévy
  
  LD=c(ld(data.l,6,7,5),ld(data.n,6,7,5),ld(data.o,6,7,5),
       ld(data.p,6,7,5),ld(data.q,6,7,5),ld(data.r,6,7,5),
       ld(data.v,6,7,5),ld(data.t,6,7,5),ld(data1.l,6,7,5),
       ld(data2.l,6,7,5),ld(data1.n,6,7,5),ld(data2.n,6,7,5),
       ld(data1.o,6,7,5),ld(data2.o,6,7,5),ld(data1.p,6,7,5),
       ld(data2.p,6,7,5),ld(data1.q,6,7,5), ld(data2.q,6,7,5),
       ld(data1.r,6,7,5),ld(data2.r,6,7,5),ld(data1.v,6,7,5), 
       ld(data2.v,6,7,5),ld(data1.t,6,7,5), ld(data2.t,6,7,5));
  
  # Tableau récapitutlatif
  
  time.exposure=as.numeric(c(rep(0,8),rep(1,16)));
  time.covariate=as.numeric(c(rep(0,8),rep(c(0,1),8)));
  output=data.frame(time.exposure,time.covariate,LD);
  
  table.0=output[output$time.covariate==0&output$time.exposure==0,];
  table.1=output[output$time.covariate==0&output$time.exposure==1,];
  table.2=output[output$time.covariate==1&output$time.exposure==1,];
  
  mean.LD12[i]=mean(table.0$LD);
  mean.LD13[i]=mean(table.1$LD);
  mean.LD14[i]=mean(table.2$LD);
  
  ## Equilibre dans le cas global 
  
  # Distance de Mahalanobis 
  
  # Pour t=0
  
  # A0 indépendant de X0
  
  covs=select(data.a,l_0,n_0,o_0,p_0,q_0,r_0,v_0,t_0)
  trt=data.a$a_0
  weight=data.a$wtx3_0
  mean.MHB12[i]=mhb(covs,trt,weight)
  
  # Pour t=1
  
  # A1 indépendant de X0, A0, X1
  
  # Pour les covariables X0
  
  covs=select(data.a,l_0,n_0,o_0,p_0,q_0,r_0,v_0,t_0)
  trt=data.a$a_1
  weight=data.a$wtx3_1
  mean.MHB13[i]=mhb(covs,trt,weight);
  
  # Pour les covariables X1
  
  covs=select(data.a,l_1,n_1,o_1,p_1,q_1,r_1,v_1,t_1)
  trt=data.a$a_1
  weight=data.a$wtx3_1
  mean.MHB14[i]=mhb(covs,trt,weight);
  
  # Différence pondérée générale
  
  weight=data.a$wtx3_0
  dat=select(data.a,l_0,n_0,o_0,p_0,q_0,r_0,v_0,t_0,a_0)
  trt=dat$a_0
  mean.GWD12[i]=gwd(dat,8, trt, weight)
  
  weight=data.a$wtx3_1
  dat=select(data.a,l_0,n_0,o_0,p_0,q_0,r_0,v_0,t_0,a_1)
  trt=dat$a_1
  mean.GWD13[i]=gwd(dat,8, trt, weight);
  
  weight=data.a$wtx3_1
  dat=select(data.a,l_1,n_1,o_1,p_1,q_1,r_1,v_1,t_1,a_1)
  trt=dat$a_1
  mean.GWD14[i]=gwd(dat,8, trt, weight);
  
  # Statistique C
  
  # Calcul du score de propension dans les données pondérées
  
  mod.dnom1=glm(a_0~l_0+m_0+n_0+o_0+p_0+q_0+r_0+u_0+v_0+t_0,data = data.a,
                weights=wtx3_1,family=binomial(link="logit"))
  mod.dnom2=glm(a_1~l_0+m_0+n_0+o_0+p_0+q_0+r_0+u_0+v_0+t_0+l_1+m_1+n_1+o_1+p_1+q_1
                +r_1+u_1+v_1+t_1,data = data.a,weights=wtx3_1,family=binomial(link="logit"))
  
  psx_0=mod.dnom1$fit
  psx_1=mod.dnom2$fit
  
  data1=data.frame(id,a_0,psx_0,a_1,psx_1);
  
  prd2=prediction(data1$psx_0,data1$a_0)
  mean.Stat.C12[i]=2*(unlist(performance(prd2, "auc")@y.values)-0.5)
  prd3=prediction(data1$psx_1,data1$a_1)
  mean.Stat.C13[i]=2*(unlist(performance(prd3, "auc")@y.values)-0.5)
  mean.Stat.C14[i]=mean.Stat.C13[i]
  
  ##  Calcul du biais pour comparer les métriques  ### 
  
  # Fit weighted model
  lmfit=glm(as.factor(Y)~as.factor(a_0)+as.factor(a_1),weights=wtx3_1,
            data=data.a,family=binomial(link="logit"))
  
  s12=exp(lmfit$coef[2]+lmfit$coef[3])
  s13=exp(lmfit$coef[2])
  s14=exp(lmfit$coef[3])
  
  Biais12[i]=s12-exp(beta_1+beta_2)
  Biais13[i]=s13-exp(beta_1)
  Biais14[i]=s14-exp(beta_2)
  
  
  ##  Calcul des métriques pour le poids stabilisé non tronqué 
  
  ############################
  ##  Pour D et SMD         ##
  ############################ 
  
  mytable=my.balance(
    input=mydata.e,
    diagnostic=3,
    censoring="no",
    approach="weight",
    scope="all",
    times.exposure=c(0,1),
    times.covariate=c(0,1),
    exposure="a",
    loop="yes",
    weight.exposure="swax",
    metric="SMD",
    sort.order= c("l","n","o","p","q","r","v","t")
  )
  
  table.0=mytable[mytable$time.covariate==0&mytable$time.exposure==0,];
  table.1=mytable[mytable$time.covariate==0&mytable$time.exposure==1,];
  table.2=mytable[mytable$time.covariate==1&mytable$time.exposure==1,];
  
  mean.D15[i]=mean(table.0$D);
  mean.D16[i]=mean(table.1$D);
  mean.D17[i]=mean(table.2$D);
  
  mean.SMD15[i]=mean(table.0$SMD);
  mean.SMD16[i]=mean(table.1$SMD);
  mean.SMD17[i]=mean(table.2$SMD);
  
  #######################
  ##  Pour le KS       ##
  #######################
  
  mytable=my.balance(
    input=mydata.e,
    diagnostic=3,
    censoring="no",
    approach="weight",
    scope="all",
    times.exposure=c(0,1),
    times.covariate=c(0,1),
    exposure="a",
    loop="yes",
    weight.exposure="swax",
    metric="KS",
    sort.order= c("l","n","o","p","q","r","v","t")
  )
  
  table.0=mytable[mytable$time.covariate==0&mytable$time.exposure==0,];
  table.1=mytable[mytable$time.covariate==0&mytable$time.exposure==1,];
  table.2=mytable[mytable$time.covariate==1&mytable$time.exposure==1,];
  
  mean.KS15[i]=mean(table.0$KS);
  mean.KS16[i]=mean(table.1$KS);
  mean.KS17[i]=mean(table.2$KS);
  
  ############################################
  ##  Overlapping coefficient (OVL)         ##
  ############################################
  
  mytable=my.balance(
    input=mydata.e,
    diagnostic=3,
    censoring="no",
    approach="weight",
    scope="all",
    times.exposure=c(0,1),
    times.covariate=c(0,1),
    exposure="a",
    loop="yes",
    weight.exposure="swax",
    metric="OVL",
    sort.order= c("l","n","o","p","q","r","v","t")
  )
  
  table.0=mytable[mytable$time.covariate==0&mytable$time.exposure==0,];
  table.1=mytable[mytable$time.covariate==0&mytable$time.exposure==1,];
  table.2=mytable[mytable$time.covariate==1&mytable$time.exposure==1,];
  
  mean.OVL15[i]=mean(table.0$OVL);
  mean.OVL16[i]=mean(table.1$OVL);
  mean.OVL17[i]=mean(table.2$OVL);
  
  ############################################
  ##  Distance de Lévy LD                   ##
  ############################################
  
  data.l=filter(mydata.e,name.cov=="l",time.covariate==0,time.exposure==0)
  data1.l=filter(mydata.e,name.cov=="l",time.covariate==0,time.exposure==1)
  data2.l=filter(mydata.e,name.cov=="l",time.covariate==1,time.exposure==1)
  
  data.n=filter(mydata.e,name.cov=="n",time.covariate==0,time.exposure==0)
  data1.n=filter(mydata.e,name.cov=="n",time.covariate==0,time.exposure==1)
  data2.n=filter(mydata.e,name.cov=="n",time.covariate==1,time.exposure==1)
  
  data.o=filter(mydata.e,name.cov=="o",time.covariate==0,time.exposure==0)
  data1.o=filter(mydata.e,name.cov=="o",time.covariate==0,time.exposure==1)
  data2.o=filter(mydata.e,name.cov=="o",time.covariate==1,time.exposure==1)
  
  data.p=filter(mydata.e,name.cov=="p",time.covariate==0,time.exposure==0)
  data1.p=filter(mydata.e,name.cov=="p",time.covariate==0,time.exposure==1)
  data2.p=filter(mydata.e,name.cov=="p",time.covariate==1,time.exposure==1)
  
  data.q=filter(mydata.e,name.cov=="q",time.covariate==0,time.exposure==0)
  data1.q=filter(mydata.e,name.cov=="q",time.covariate==0,time.exposure==1)
  data2.q=filter(mydata.e,name.cov=="q",time.covariate==1,time.exposure==1)
  
  data.r=filter(mydata.e,name.cov=="r",time.covariate==0,time.exposure==0)
  data1.r=filter(mydata.e,name.cov=="r",time.covariate==0,time.exposure==1)
  data2.r=filter(mydata.e,name.cov=="r",time.covariate==1,time.exposure==1)
  
  data.v=filter(mydata.e,name.cov=="v",time.covariate==0,time.exposure==0)
  data1.v=filter(mydata.e,name.cov=="v",time.covariate==0,time.exposure==1)
  data2.v=filter(mydata.e,name.cov=="v",time.covariate==1,time.exposure==1)
  
  data.t=filter(mydata.e,name.cov=="t",time.covariate==0,time.exposure==0)
  data1.t=filter(mydata.e,name.cov=="t",time.covariate==0,time.exposure==1)
  data2.t=filter(mydata.e,name.cov=="t",time.covariate==1,time.exposure==1)
  
  # Distance de Lévy
  
  LD=c(ld(data.l,6,7,5),ld(data.n,6,7,5),ld(data.o,6,7,5),
       ld(data.p,6,7,5),ld(data.q,6,7,5),ld(data.r,6,7,5),
       ld(data.v,6,7,5),ld(data.t,6,7,5),ld(data1.l,6,7,5),
       ld(data2.l,6,7,5),ld(data1.n,6,7,5),ld(data2.n,6,7,5),
       ld(data1.o,6,7,5),ld(data2.o,6,7,5),ld(data1.p,6,7,5),
       ld(data2.p,6,7,5),ld(data1.q,6,7,5), ld(data2.q,6,7,5),
       ld(data1.r,6,7,5),ld(data2.r,6,7,5),ld(data1.v,6,7,5), 
       ld(data2.v,6,7,5),ld(data1.t,6,7,5), ld(data2.t,6,7,5));
  
  # Tableau récapitutlatif
  
  time.exposure=as.numeric(c(rep(0,8),rep(1,16)));
  time.covariate=as.numeric(c(rep(0,8),rep(c(0,1),8)));
  output=data.frame(time.exposure,time.covariate,LD);
  
  table.0=output[output$time.covariate==0&output$time.exposure==0,];
  table.1=output[output$time.covariate==0&output$time.exposure==1,];
  table.2=output[output$time.covariate==1&output$time.exposure==1,];
  
  mean.LD15[i]=mean(table.0$LD);
  mean.LD16[i]=mean(table.1$LD);
  mean.LD17[i]=mean(table.2$LD);
  
  ## Equilibre dans le cas global 
  
  # Distance de Mahalanobis 
  
  # Pour t=0
  
  # A0 indépendant de X0
  
  covs=select(data.a,l_0,n_0,o_0,p_0,q_0,r_0,v_0,t_0)
  trt=data.a$a_0
  weight=data.a$swax_0
  mean.MHB15[i]=mhb(covs,trt,weight)
  
  # Pour t=1
  
  # A1 indépendant de X0, A0, X1
  
  # Pour les covariables X0
  
  covs=select(data.a,l_0,n_0,o_0,p_0,q_0,r_0,v_0,t_0)
  trt=data.a$a_1
  weight=data.a$swax_1
  mean.MHB16[i]=mhb(covs,trt,weight);
  
  # Pour les covariables X1
  
  covs=select(data.a,l_1,n_1,o_1,p_1,q_1,r_1,v_1,t_1)
  trt=data.a$a_1
  weight=data.a$swax_1
  mean.MHB17[i]=mhb(covs,trt,weight);
  
  # Différence pondérée générale
  
  weight=data.a$swax_0
  dat=select(data.a,l_0,n_0,o_0,p_0,q_0,r_0,v_0,t_0,a_0)
  trt=dat$a_0
  mean.GWD15[i]=gwd(dat,8, trt, weight)
  
  weight=data.a$swax_1
  dat=select(data.a,l_0,n_0,o_0,p_0,q_0,r_0,v_0,t_0,a_1)
  trt=dat$a_1
  mean.GWD16[i]=gwd(dat,8, trt, weight);
  
  weight=data.a$swax_1
  dat=select(data.a,l_1,n_1,o_1,p_1,q_1,r_1,v_1,t_1,a_1)
  trt=dat$a_1
  mean.GWD17[i]=gwd(dat,8, trt, weight);
  
  # Statistique C
  
  # Calcul du score de propension dans les données pondérées
  
  mod.dnom1=glm(a_0~l_0+m_0+n_0+o_0+p_0+q_0+r_0+u_0+v_0+t_0,data = data.a,weights=swax_1,family=binomial(link="logit"))
  mod.dnom2=glm(a_1~l_0+m_0+n_0+o_0+p_0+q_0+r_0+u_0+v_0+t_0+l_1+m_1+n_1+o_1+p_1+q_1+r_1+u_1+v_1+t_1,data = data.a,weights=swax_1,family=binomial(link="logit"))
  
  psx_0=mod.dnom1$fit
  psx_1=mod.dnom2$fit
  
  data1=data.frame(id,a_0,psx_0,a_1,psx_1);
  
  prd2=prediction(data1$psx_0,data1$a_0)
  mean.Stat.C15[i]=2*(unlist(performance(prd2, "auc")@y.values)-0.5)
  prd3=prediction(data1$psx_1,data1$a_1)
  mean.Stat.C16[i]=2*(unlist(performance(prd3, "auc")@y.values)-0.5)
  mean.Stat.C17[i]=mean.Stat.C16[i]
  
  ##  Calcul du biais pour comparer les métriques  ### 
  
  # Fit weighted model
  lmfit=glm(as.factor(Y)~as.factor(a_0)+as.factor(a_1),weights=swax_1,
            data=data.a,family=binomial(link="logit"))
  
  s15=exp(lmfit$coef[2]+lmfit$coef[3])
  s16=exp(lmfit$coef[2])
  s17=exp(lmfit$coef[3])
  
  Biais15[i]=s15-exp(beta_1+beta_2)
  Biais16[i]=s16-exp(beta_1)
  Biais17[i]=s17-exp(beta_2)
  
  print(i);
}


# Biais moyen 

mean.BiaisT=c(mean(Biais0),mean(Biais3),mean(Biais6),mean(Biais9),mean(Biais12),mean(Biais15))
mean.BiaisT1=c(mean(Biais1),mean(Biais4),mean(Biais7),mean(Biais10),mean(Biais13),mean(Biais16))
mean.BiaisT2=c(mean(Biais2),mean(Biais5),mean(Biais8),mean(Biais11),mean(Biais14),mean(Biais17))

# Équilibre moyenne

meanDT=c(mean(mean.D),mean(mean.D3),mean(mean.D6),mean(mean.D9),mean(mean.D12),mean(mean.D15)) 
meanDT1=c(mean(mean.D1),mean(mean.D4),mean(mean.D7),mean(mean.D10),mean(mean.D13),mean(mean.D16)) 
meanDT2=c(mean(mean.D2),mean(mean.D5),mean(mean.D8),mean(mean.D11),mean(mean.D14),mean(mean.D17))

meanSMDT=c(mean(mean.SMD),mean(mean.SMD3),mean(mean.SMD6),mean(mean.SMD9),mean(mean.SMD12),mean(mean.SMD15)) 
meanSMDT1=c(mean(mean.SMD1),mean(mean.SMD4),mean(mean.SMD7),mean(mean.SMD10),mean(mean.SMD13),mean(mean.SMD16)) 
meanSMDT2=c(mean(mean.SMD2),mean(mean.SMD5),mean(mean.SMD8),mean(mean.SMD11),mean(mean.SMD14),mean(mean.SMD17)) 

meanOVLT=c(mean(mean.OVL),mean(mean.OVL3),mean(mean.OVL6),mean(mean.OVL9),mean(mean.OVL12),mean(mean.OVL15)) 
meanOVLT1=c(mean(mean.OVL1),mean(mean.OVL4),mean(mean.OVL7),mean(mean.OVL10),mean(mean.OVL13),mean(mean.OVL16)) 
meanOVLT2=c(mean(mean.OVL2),mean(mean.OVL5),mean(mean.OVL8),mean(mean.OVL11),mean(mean.OVL14),mean(mean.OVL17)) 

meanKST=c(mean(mean.KS),mean(mean.KS3),mean(mean.KS6),mean(mean.KS9),mean(mean.KS12),mean(mean.KS15)) 
meanKST1=c(mean(mean.KS1),mean(mean.KS4),mean(mean.KS7),mean(mean.KS10),mean(mean.KS13),mean(mean.KS16)) 
meanKST2=c(mean(mean.KS2),mean(mean.KS5),mean(mean.KS8),mean(mean.KS11),mean(mean.KS14),mean(mean.KS17)) 

meanLDT=c(mean(mean.LD),mean(mean.LD3),mean(mean.LD6),mean(mean.LD9),mean(mean.LD12),mean(mean.LD15)) 
meanLDT1=c(mean(mean.LD1),mean(mean.LD4),mean(mean.LD7),mean(mean.LD10),mean(mean.LD13),mean(mean.LD16)) 
meanLDT2=c(mean(mean.LD2),mean(mean.LD5),mean(mean.LD8),mean(mean.LD11),mean(mean.LD14),mean(mean.LD17)) 

meanStat.CT=c(mean(mean.Stat.C),mean(mean.Stat.C3),mean(mean.Stat.C6),mean(mean.Stat.C9),mean(mean.Stat.C12),mean(mean.Stat.C15)) 
meanStat.CT1=c(mean(mean.Stat.C1),mean(mean.Stat.C4),mean(mean.Stat.C7),mean(mean.Stat.C10),mean(mean.Stat.C13),mean(mean.Stat.C16)) 
meanStat.CT2=c(mean(mean.Stat.C2),mean(mean.Stat.C5),mean(mean.Stat.C8),mean(mean.Stat.C11),mean(mean.Stat.C14),mean(mean.Stat.C17)) 

meanMHBT=c(mean(mean.MHB),mean(mean.MHB3),mean(mean.MHB6),mean(mean.MHB9),mean(mean.MHB12),mean(mean.MHB15)) 
meanMHBT1=c(mean(mean.MHB1),mean(mean.MHB4),mean(mean.MHB7),mean(mean.MHB10),mean(mean.MHB13),mean(mean.MHB16)) 
meanMHBT2=c(mean(mean.MHB2),mean(mean.MHB5),mean(mean.MHB8),mean(mean.MHB11),mean(mean.MHB14),mean(mean.MHB17)) 

meanGWDT=c(mean(mean.GWD),mean(mean.GWD3),mean(mean.GWD6),mean(mean.GWD9),mean(mean.GWD12),mean(mean.GWD15)) 
meanGWDT1=c(mean(mean.GWD1),mean(mean.GWD4),mean(mean.GWD7),mean(mean.GWD10),mean(mean.GWD13),mean(mean.GWD16)) 
meanGWDT2=c(mean(mean.GWD2),mean(mean.GWD5),mean(mean.GWD8),mean(mean.GWD11),mean(mean.GWD14),mean(mean.GWD17))

# Métriques sur les B bases de données selon le temps de mesure

BiaisT=c(Biais0,Biais3,Biais6,Biais9,Biais12,Biais15)
BiaisT1=c(Biais1,Biais4,Biais7,Biais10,Biais13,Biais16)
BiaisT2=c(Biais2,Biais5,Biais8,Biais11,Biais14,Biais17)

mean.DT=c(mean.D,mean.D3,mean.D6,mean.D9,mean.D12,mean.D15) 
mean.DT1=c(mean.D1,mean.D4,mean.D7,mean.D10,mean.D13,mean.D16) 
mean.DT2=c(mean.D2,mean.D5,mean.D8,mean.D11,mean.D14,mean.D17)

mean.SMDT=c(mean.SMD,mean.SMD3,mean.SMD6,mean.SMD9,mean.SMD12,mean.SMD15) 
mean.SMDT1=c(mean.SMD1,mean.SMD4,mean.SMD7,mean.SMD10,mean.SMD13,mean.SMD16) 
mean.SMDT2=c(mean.SMD2,mean.SMD5,mean.SMD8,mean.SMD11,mean.SMD14,mean.SMD17) 

mean.OVLT=c(mean.OVL,mean.OVL3,mean.OVL6,mean.OVL9,mean.OVL12,mean.OVL15) 
mean.OVLT1=c(mean.OVL1,mean.OVL4,mean.OVL7,mean.OVL10,mean.OVL13,mean.OVL16) 
mean.OVLT2=c(mean.OVL2,mean.OVL5,mean.OVL8,mean.OVL11,mean.OVL14,mean.OVL17) 

mean.KST=c(mean.KS,mean.KS3,mean.KS6,mean.KS9,mean.KS12,mean.KS15) 
mean.KST1=c(mean.KS1,mean.KS4,mean.KS7,mean.KS10,mean.KS13,mean.KS16) 
mean.KST2=c(mean.KS2,mean.KS5,mean.KS8,mean.KS11,mean.KS14,mean.KS17) 

mean.LDT=c(mean.LD,mean.LD3,mean.LD6,mean.LD9,mean.LD12,mean.LD15) 
mean.LDT1=c(mean.LD1,mean.LD4,mean.LD7,mean.LD10,mean.LD13,mean.LD16) 
mean.LDT2=c(mean.LD2,mean.LD5,mean.LD8,mean.LD11,mean.LD14,mean.LD17) 

mean.Stat.CT=c(mean.Stat.C,mean.Stat.C3,mean.Stat.C6,mean.Stat.C9,
               mean.Stat.C12,mean.Stat.C15) 
mean.Stat.CT1=c(mean.Stat.C1,mean.Stat.C4,mean.Stat.C7,mean.Stat.C10,
                mean.Stat.C13,mean.Stat.C16) 
mean.Stat.CT2=c(mean.Stat.C2,mean.Stat.C5,mean.Stat.C8,mean.Stat.C11,
                mean.Stat.C14,mean.Stat.C17) 

mean.MHBT=c(mean.MHB,mean.MHB3,mean.MHB6,mean.MHB9,mean.MHB12,mean.MHB15) 
mean.MHBT1=c(mean.MHB1,mean.MHB4,mean.MHB7,mean.MHB10,mean.MHB13,mean.MHB16) 
mean.MHBT2=c(mean.MHB2,mean.MHB5,mean.MHB8,mean.MHB11,mean.MHB14,mean.MHB17) 

mean.GWDT=c(mean.GWD,mean.GWD3,mean.GWD6,mean.GWD9,mean.GWD12,mean.GWD15) 
mean.GWDT1=c(mean.GWD1,mean.GWD4,mean.GWD7,mean.GWD10,mean.GWD13,mean.GWD16) 
mean.GWDT2=c(mean.GWD2,mean.GWD5,mean.GWD8,mean.GWD11,mean.GWD14,mean.GWD17)

