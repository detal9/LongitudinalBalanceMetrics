odds = function(p){return((p/(1-p)))};

my.lengthen=function (input,diagnostic,censoring,id,times.exposure,times.covariate,
                      exposure,temporal.covariate,static.covariate=NULL,
                      weight.exposure=NULL,censor=NULL,weight.censor=NULL) {
  
  input=ungroup(input)
  
  #rename key vars
  s_id=sym(id)
  input=input %>%rename(ID=!!s_id)
  list.exposure=paste(exposure,times.exposure,sep="_")
  
  if (!is.null(weight.exposure))  {
    list.weight.exposure=paste(weight.exposure,times.exposure,sep="_")
  } else {list.weight.exposure=NULL
  }
  if (!is.null(censor) & diagnostic!=2)  {
    list.censor=paste(censor,times.exposure,sep="_")
  } else if (!is.null(censor) & diagnostic==2)  {
    list.censor=paste(censor,times.covariate,sep="_")
  } else {list.censor=NULL
  }
  
  if (!is.null(weight.censor) & diagnostic!=2)  {
    list.weight.censor=paste(weight.censor,times.exposure,sep="_")
  } else if (!is.null(weight.censor) & diagnostic==2)  {
    list.weight.censor=paste(weight.censor,times.covariate,sep="_")
  } else {list.weight.censor=NULL
  }
  
  if (censoring=="no") {
    censor.unique=NULL
  } else if (censoring=="yes") {
    censor.unique=censor
  }
  
  covariate.unique=c(static.covariate,temporal.covariate)
  
  if (is.null(static.covariate)) {
    list.static.covariate=NULL
  } else if (!is.null(static.covariate)) {
    list.static.covariate=sort(as.vector(sapply(static.covariate,
                                                paste,min(times.covariate,times.exposure),sep="_")))
  }
  
  if (is.null(temporal.covariate)) {
    list.temporal.covariate=NULL
  } else if (!is.null(temporal.covariate)) {
    list.temporal.covariate=sort(as.vector(sapply(temporal.covariate,
                                                  paste,times.covariate,sep="_")))
  }
  
  list.covariate=c(list.static.covariate,list.temporal.covariate)
  list.all.covariate=names(input)
  list.covariate=intersect(all_of(list.covariate),list.all.covariate)  #remove missing covariate measurements from list
  
  #issue a warning if the exposure and/or covariates contain missing data
  
  
  list.exposure.check=paste(all_of(list.exposure), collapse="|")
  list.covariate.check=paste(all_of(list.covariate), collapse="|")
  
  expCheck=input[,grep(list.exposure.check, names(input), value=TRUE)]
  covCheck=input[,grep(list.covariate.check, names(input), value=TRUE)]
  
  #issue an error and abort the program if any exposure or covariate is not numeric
  
  expCovList=c(all_of(list.exposure), all_of(list.covariate))
  covFormat_check=input[expCovList]
  
  step1=input[,c("ID",all_of(list.exposure),all_of(list.covariate),
                 all_of(list.weight.exposure),all_of(list.weight.censor),
                 all_of(list.censor))]
  
  if (censoring=="no" | (censoring=="yes" & diagnostic!=2)) {
    step2=step1 %>% gather(key="wide.name.exp",value="value.exp",c(all_of(list.exposure),all_of(list.weight.exposure),all_of(list.weight.censor),all_of(list.censor)))
  } else if (censoring=="yes" & diagnostic==2) {
    step2=step1 %>% gather(key="wide.name.exp",value="value.exp",c(all_of(list.exposure),all_of(list.weight.exposure)))
  }
  
  step3=step2 %>% separate(.data$wide.name.exp,c("name.exp","time.exposure"),sep="_") %>%
    spread(key="name.exp",value="value.exp")
  
  if (censoring=="no" | (censoring=="yes" & diagnostic!=2)) {
    step4=step3 %>% gather(key="wide.name.cov",value="value.cov",all_of(list.covariate)) %>%
      separate(.data$wide.name.cov,c("name.cov","time.covariate"),sep="_")
  } else if (censoring=="yes" & diagnostic==2) {
    step4=step3 %>% gather(key="wide.name.cov",value="value.cov",c(all_of(list.covariate),all_of(list.censor),all_of(list.weight.censor))) %>%
      separate(.data$wide.name.cov,c("name.cov","time.covariate"),sep="_") %>%
      spread(key="name.cov",value="value.cov") %>%
      gather(key="name.cov",value="value.cov",covariate.unique)
  }
  
  #format data
  if (is.null(censor)) {
    censor.column=NULL
    
    if (censoring=="no") {
      
      step5=step4
      
    }
    
  } else if (!is.null(censor)){
    censor.column="censor"
    
    if (censoring=="yes") {
      
      step5 =step4 %>% rename(censor=censor.unique) %>%
        filter(censor==0) #this step drops censored exposure times (for D1/D3), or censored covariate times (for D2)
      
    }
    
  }
  
  VarsToFormat=c(exposure,weight.exposure,censor.column,weight.censor)
  
  step6=step5 %>%mutate(time.exposure=as.numeric(.data$time.exposure),
                        time.covariate=as.numeric(.data$time.covariate)) %>%
    arrange(.data$name.cov,.data$ID,.data$time.exposure,.data$time.covariate) %>%
    mutate_at(VarsToFormat,as.numeric) %>%mutate(!! s_id := as.character(.data$ID),
                                                 name.cov=as.character(.data$name.cov)) %>%select(-.data$ID)
  
  #restrict to appropriate times
  if (diagnostic!=2) {
    
    step7=step6 %>% filter(.data$time.exposure>=.data$time.covariate) #this step drops censored covariate times
    
  } else if (diagnostic==2) {
    
    step7=step6 %>% filter(.data$time.covariate>.data$time.exposure) #this step drops censored exposure times
  }
  
  #remove missing data
  output=step7 %>%na.omit() %>%data.frame()
  output=output[,c(id,"name.cov","time.exposure","time.covariate",exposure,"value.cov",censor.column,weight.exposure,weight.censor)]
  
  return(output)
  
}


my.balance=function (input,
                     diagnostic,
                     approach="none",
                     censoring,
                     scope,
                     times.exposure,
                     times.covariate,
                     exposure,
                     weight.exposure=NULL,
                     weight.censor=NULL,
                     recency=NULL,
                     average.over=NULL,
                     periods=NULL,
                     list.distance=NULL,
                     sort.order="alphabetical",
                     loop="no",
                     ignore.missing.metric="no",
                     metric="SMD",
                     sd.ref="no") {
  
  input <- ungroup(input)
  
  if (!is.null(periods)) {
    
    vector.periods <- c(unlist(periods))
  }
  
  if (is.null(weight.exposure)) {
    input$weight.exposure.none <- 1
    weight.exposure <- "weight.exposure.none"
  }
  
  if (is.null(weight.censor)) {
    input$weight.censor.none <- 1
    weight.censor <- "weight.censor.none"
  }
  
  t.exp.data <- unique(input$time.exposure)
  t.cov.data <- unique(input$time.covariate)
  t.exp.spec <- unique(times.exposure)
  t.cov.spec <- unique(times.covariate)
  
  if ((all(t.exp.data %in% t.exp.spec)) & (all(t.cov.data %in% t.cov.spec))) {
  } else {
    input <- input %>% filter((.data$time.exposure %in% t.exp.spec) & (.data$time.covariate %in% t.cov.spec))
  }
  
  s_exposure <- sym(exposure)
  s_weight.exposure <- sym(weight.exposure)
  s_weight.censor <- sym(weight.censor)
  
  input <- rename(input,
                  E =  !! s_exposure,
                  W_a= !! s_weight.exposure,
                  W_s= !! s_weight.censor
  )
  
  if (metric=="SMD") {
    
    if (approach=="weight" | approach=="none") {
      
      if (censoring=="yes") {
        input <- mutate(input,W=as.numeric(.data$W_a)*as.numeric(.data$W_s))
      } else if (censoring=="no") {
        input <- mutate(input,W=as.numeric(.data$W_a))
      }
      
      if (diagnostic==1) {
        
        input <- mutate(input,W=1)
        
        temp.table <-
          data.frame(input %>%
                       select(.data$E,.data$W,.data$time.exposure,.data$time.covariate,.data$name.cov,.data$value.cov) %>%
                       group_by(.data$E,.data$name.cov,.data$time.exposure,.data$time.covariate) %>%
                       summarise(mean.cov_b=weighted.mean(x=.data$value.cov,w=.data$W,na.rm=TRUE),
                                 sd.cov_b=sd(x=.data$value.cov,na.rm=TRUE),
                                 n.cov_b=sum(.data$W)))
        
        
        temp.table <- temp.table %>%
          group_by(.data$name.cov,.data$time.exposure,.data$time.covariate) %>%
          mutate(mean.cov_a=first(.data$mean.cov_b),
                 sd.cov_a=first(.data$sd.cov_b),
                 n.cov_a=first(.data$n.cov_b)) %>%
          filter(.data$E!=first(.data$E))
        
        if (sd.ref=="no"){
          full.table <- temp.table %>%
            mutate(D=abs(.data$mean.cov_b-.data$mean.cov_a),
                   SMD=ifelse(.data$D==0,0,
                              ifelse(.data$sd.cov_a==0 & .data$sd.cov_b==0,NA_real_,
                                     ifelse(.data$sd.cov_a==0 | .data$sd.cov_b==0,(abs(.data$mean.cov_b-.data$mean.cov_a))/(isTRUE(.data$sd.cov_a!=0)*.data$sd.cov_a + isTRUE(.data$sd.cov_b!=0)*.data$sd.cov_b),
                                            (abs(.data$mean.cov_b-.data$mean.cov_a))/sqrt((.data$sd.cov_a^2*(.data$n.cov_a-1)+.data$sd.cov_b^2*(.data$n.cov_b-1))/(.data$n.cov_a+.data$n.cov_b-2))
                                     )
                              )
                   ),
                   N=.data$n.cov_a+.data$n.cov_b,
                   Nexp=.data$n.cov_b)
          
        } else if (sd.ref=="yes"){
          
          full.table <- temp.table %>%
            mutate(D=abs(.data$mean.cov_b-.data$mean.cov_a),
                   SMD=ifelse(.data$D==0,0,
                              ifelse(.data$sd.cov_a==0,NA_real_,
                                     (abs(.data$mean.cov_b-.data$mean.cov_a))/.data$sd.cov_a)),
                   N=.data$n.cov_a+.data$n.cov_b,
                   Nexp=.data$n.cov_b)
        }
        
        
        sub.table  <- full.table %>%
          select(.data$E,.data$name.cov,.data$time.exposure,.data$time.covariate,.data$D,.data$SMD,.data$N,.data$Nexp) %>%
          filter (!is.na(.data$D)) %>%
          filter(.data$time.exposure>=.data$time.covariate) %>%
          arrange(.data$name.cov,.data$time.exposure,.data$time.covariate)
        
      } else if (diagnostic==3) {
        
        temp.table <-
          data.frame(input %>%
                       select(.data$E,.data$W,.data$time.exposure,.data$time.covariate,.data$name.cov,.data$value.cov) %>%
                       group_by(.data$E,.data$name.cov,.data$time.exposure,.data$time.covariate) %>%
                       summarise(mean.cov_b=weighted.mean(x=.data$value.cov,w=.data$W,na.rm=TRUE),
                                 sd.cov_b=sd(x=.data$value.cov,na.rm=TRUE),
                                 n.cov_b=sum(.data$W)))
        
        
        temp.table <- temp.table %>%
          group_by(.data$name.cov,.data$time.exposure,.data$time.covariate) %>%
          mutate(mean.cov_a=first(.data$mean.cov_b),
                 sd.cov_a=first(.data$sd.cov_b),
                 n.cov_a=first(.data$n.cov_b)) %>%
          filter(.data$E!=first(.data$E))
        
        if (sd.ref=="no"){
          full.table <- temp.table %>%
            mutate(D=abs(.data$mean.cov_b-.data$mean.cov_a),
                   SMD=ifelse(.data$D==0,0,
                              ifelse(.data$sd.cov_a==0 & .data$sd.cov_b==0,NA_real_,
                                     ifelse(.data$sd.cov_a==0 | .data$sd.cov_b==0,(abs(.data$mean.cov_b-.data$mean.cov_a))/(isTRUE(.data$sd.cov_a!=0)*.data$sd.cov_a + isTRUE(.data$sd.cov_b!=0)*.data$sd.cov_b),
                                            (abs(.data$mean.cov_b-.data$mean.cov_a))/sqrt((.data$sd.cov_a^2*(.data$n.cov_a-1)+.data$sd.cov_b^2*(.data$n.cov_b-1))/(.data$n.cov_a+.data$n.cov_b-2))
                                     )
                              )
                   ),
                   N=.data$n.cov_a+.data$n.cov_b,
                   Nexp=.data$n.cov_b)
          
        } else if (sd.ref=="yes"){
          
          full.table <- temp.table %>%
            mutate(D=abs(.data$mean.cov_b-.data$mean.cov_a),
                   SMD=ifelse(.data$D==0,0,
                              ifelse(.data$sd.cov_a==0,NA_real_,
                                     (abs(.data$mean.cov_b-.data$mean.cov_a))/.data$sd.cov_a)),
                   N=.data$n.cov_a+.data$n.cov_b,
                   Nexp=.data$n.cov_b)
        }
        
        
        full.table <- full.table %>% select(.data$E,.data$name.cov,.data$time.exposure,.data$time.covariate,.data$D,.data$SMD,.data$N,.data$Nexp)
        
        if (diagnostic==3) {
          
          sub.table <- full.table %>% filter (!is.na(.data$D)) %>% filter(.data$time.exposure>=.data$time.covariate)
        }
      }
      
    } 
    
  } else if (metric=="KS") {
    
    if (approach=="weight" | approach=="none") {
      
      if (censoring=="yes") {
        input <- mutate(input,W=as.numeric(.data$W_a)*as.numeric(.data$W_s))
      } else if (censoring=="no") {
        input <- mutate(input,W=as.numeric(.data$W_a))
      }
      
      if (diagnostic==1) {
        
        input <- mutate(input,W=1)
        
        temp.table=data.frame(input %>%
                                select(.data$E,.data$W,.data$time.exposure,.data$time.covariate,.data$name.cov,.data$value.cov) %>%
                                group_by(.data$name.cov,.data$time.exposure,.data$time.covariate) %>%
                                summarise(KS=max(abs(ewcdf(.data$value.cov[.data$E==1],w = .data$W[.data$E == 1])(.data$value.cov)
                                                     -ewcdf(.data$value.cov[.data$E==0],w = .data$W[.data$E == 0])(.data$value.cov))),
                                          N=sum(.data$W[.data$E == 0])+sum(.data$W[.data$E == 1]),Nexp=sum(.data$W[.data$E == 1])))
        
        temp.table=data.frame(E=rep(1,nrow(temp.table)),temp.table)
        full.table=temp.table %>%mutate(KS=KS,N=N,Nexp=Nexp)
        
        
        sub.table  <- full.table %>%
          select(.data$E,.data$name.cov,.data$time.exposure,.data$time.covariate,.data$KS,.data$N,.data$Nexp) %>%
          filter (!is.na(.data$KS)) %>%
          filter(.data$time.exposure>=.data$time.covariate) %>%
          arrange(.data$name.cov,.data$time.exposure,.data$time.covariate)
        
      } else if (diagnostic==3) {
        
        temp.table <-
          data.frame(input %>%
                       select(.data$E,.data$W,.data$time.exposure,.data$time.covariate,.data$name.cov,.data$value.cov) %>%
                       group_by(.data$name.cov,.data$time.exposure,.data$time.covariate) %>%
                       summarise(KS=max(abs(ewcdf(.data$value.cov[.data$E==1],w = .data$W[.data$E == 1])(.data$value.cov)
                                            -ewcdf(.data$value.cov[.data$E==0],w = .data$W[.data$E == 0])(.data$value.cov))),
                                 N=sum(.data$W[.data$E == 0])+sum(.data$W[.data$E == 1]),Nexp=sum(.data$W[.data$E == 1]))) 
        
        temp.table=data.frame(E=rep(1,nrow(temp.table)),temp.table)
        full.table=temp.table %>%mutate(KS=KS,N=N,Nexp=Nexp)
        
        
        full.table=full.table %>% select(.data$E,.data$name.cov,.data$time.exposure,.data$time.covariate,.data$KS,.data$N,.data$Nexp)
        
        if (diagnostic==3) {
          
          sub.table <- full.table %>% filter (!is.na(.data$KS)) %>% filter(.data$time.exposure>=.data$time.covariate)
        }
      }
      
    } 
    
  } else if (metric=="OVL") {
    
    if (approach=="weight" | approach=="none") {
      
      if (censoring=="yes") {
        input <- mutate(input,W=as.numeric(.data$W_a)*as.numeric(.data$W_s))
      } else if (censoring=="no") {
        input <- mutate(input,W=as.numeric(.data$W_a))
      }
      
      if (diagnostic==1) {
        
        input <- mutate(input,W=1)
        
        temp.table=data.frame(input%>%select(.data$E,.data$W,.data$time.exposure,.data$time.covariate,.data$name.cov,.data$value.cov) %>%
                                group_by(.data$name.cov,.data$time.exposure,.data$time.covariate) %>%
                                summarise(OVL=tryCatch(1-integrate(function(x)pmin(approxfun(density(.data$value.cov[.data$E==1],weights = .data$W[.data$E == 1]/sum(.data$W[.data$E == 1]),
                                                                                                     from=weighted.quantile(.data$value.cov,.data$W,0.05),to=weighted.quantile(.data$value.cov,.data$W,0.95),bw="nrd0"))(x), 
                                                                                   approxfun(density(.data$value.cov[.data$E==0], weights = .data$W[.data$E == 0]/sum(.data$W[.data$E == 0]),
                                                                                                     from=weighted.quantile(.data$value.cov,.data$W,0.05),to=weighted.quantile(.data$value.cov,.data$W,0.95),bw="nrd0"))(x)), 
                                                                   lower=weighted.quantile(.data$value.cov,.data$W, 0.05),upper=weighted.quantile(.data$value.cov,.data$W,0.95),
                                                                   subdivisions = 500)$value,error = function(e){s = NA}),N=sum(.data$W[.data$E == 0])+sum(.data$W[.data$E == 1]),Nexp=sum(.data$W[.data$E == 1])))      
        
        temp.table=data.frame(E=rep(1,nrow(temp.table)),temp.table)
        full.table=temp.table %>%mutate(OVL=OVL,N=N,Nexp=Nexp)
        
        
        sub.table  <- full.table %>%
          select(.data$E,.data$name.cov,.data$time.exposure,.data$time.covariate,.data$OVL,.data$N,.data$Nexp) %>%
          filter (!is.na(.data$OVL)) %>%
          filter(.data$time.exposure>=.data$time.covariate) %>%
          arrange(.data$name.cov,.data$time.exposure,.data$time.covariate)
        
      } else if (diagnostic==3) {
        
        temp.table=data.frame(input%>%select(.data$E,.data$W,.data$time.exposure,.data$time.covariate,.data$name.cov,.data$value.cov) %>%
                                group_by(.data$name.cov,.data$time.exposure,.data$time.covariate) %>%
                                summarise(OVL=tryCatch(1-integrate(function(x)pmin(approxfun(density(.data$value.cov[.data$E==1],weights = .data$W[.data$E == 1]/sum(.data$W[.data$E == 1]),
                                                                                                     from=weighted.quantile(.data$value.cov,.data$W,0.05),to=weighted.quantile(.data$value.cov,.data$W,0.95),bw="nrd0"))(x), 
                                                                                   approxfun(density(.data$value.cov[.data$E==0], weights = .data$W[.data$E == 0]/sum(.data$W[.data$E == 0]),
                                                                                                     from=weighted.quantile(.data$value.cov,.data$W,0.05),to=weighted.quantile(.data$value.cov,.data$W,0.95),bw="nrd0"))(x)), 
                                                                   lower=weighted.quantile(.data$value.cov,.data$W, 0.05),upper=weighted.quantile(.data$value.cov,.data$W,0.95),
                                                                   subdivisions = 500)$value,error = function(e){s = NA}),N=sum(.data$W[.data$E == 0])+sum(.data$W[.data$E == 1]),Nexp=sum(.data$W[.data$E == 1])))      
        
        temp.table=data.frame(E=rep(1,nrow(temp.table)),temp.table)
        full.table=temp.table %>%mutate(OVL=OVL,N=N,Nexp=Nexp)
        
        
        full.table=full.table %>% select(.data$E,.data$name.cov,.data$time.exposure,.data$time.covariate,.data$OVL,.data$N,.data$Nexp)
        
        if (diagnostic==3) {
          
          sub.table <- full.table %>% filter (!is.na(.data$OVL)) %>% filter(.data$time.exposure>=.data$time.covariate)
        }
      }
      
    }  
    
    if (loop=="no") {
      
      output <- apply.scope(input=sub.table,
                            diagnostic=diagnostic,
                            approach=approach,
                            scope=scope,
                            average.over=average.over,
                            periods=periods,
                            list.distance=list.distance,
                            recency=recency,
                            sort.order=sort.order,
                            ignore.missing.metric=ignore.missing.metric,
                            metric=metric) %>%data.frame()
      
    } else if (loop=="yes") {
      
      output <- sub.table %>%data.frame()
      
    }
    
    return(output)
  }
} 




balance.table=function (input,
                        diagnostic,
                        approach="none",
                        censoring,
                        scope,
                        times.exposure,
                        times.covariate,
                        exposure,
                        history=NULL,
                        weight.exposure=NULL,
                        weight.censor=NULL,
                        strata=NULL,
                        recency=NULL,
                        average.over=NULL,
                        periods=NULL,
                        list.distance=NULL,
                        sort.order="alphabetical",
                        loop="no",
                        ignore.missing.metric="no",
                        metric="SMD",
                        sd.ref="no") {
  
  input <- ungroup(input)
  
  if(is.null(input)) {
    stop ("ERROR: 'input' is missing. Please specify the dataframe created by the lengthen() function")
  }
  if(is.null(diagnostic) | !diagnostic %in% c(1,2,3)) {
    stop ("ERROR: 'diagnostic' is missing or misspecified. Please specify as 1, 2 or 3")
  }
  if(is.null(approach) | !approach %in% c("none","weight","stratify")) {
    stop ("ERROR: 'approach' is missing or misspecified. Please specify as none, weight, or stratify")
  }
  
  if (diagnostic==3 & approach=="none") stop ("'diagnostic' has been specified as 2 or 3, and to implement the choice properly, approach needs to be specified as weight or stratify.")
  
  if(is.null(censoring) | !censoring %in% c("no","yes")) {
    stop ("ERROR: 'censoring' is missing. Please specify it as yes or no")
  }
  if (is.null(scope) | !scope %in% c("all","recent","average")) {
    stop ("ERROR: 'scope' is missing. Please specify either all, recent, or average")
  }
  if(is.null(exposure) | !exposure %in% names(input)) {
    stop ("ERROR: Either 'exposure' has not been specified OR the exposure root name is not present in the input dataframe")
  }
  if(diagnostic!=2 && !is.null(history) && !history %in% names(input)){
    stop("ERROR: the specified root name for 'history' is not present in the input dataframe")
  }
  if(!is.null(strata) && !strata %in% names(input)){
    stop("ERROR: the specified root name for 'strata' is not present in the input dataframe")
  }
  if(!is.null(weight.exposure) && !weight.exposure %in% names(input)){
    stop("ERROR: the specified root name for 'weight.exposure' is not present in the input dataframe")
  }
  if(!is.null(weight.censor) && !weight.censor %in% names(input)){
    stop("ERROR: the specified root name for 'weight.censor' is not present in the input dataframe")
  }
  if(is.null(times.exposure)) {
    stop ("ERROR: 'times.exposure' is missing. Please specify an integer for times.exposure or a numeric vector of times")
  }
  if(is.null(times.covariate)) {
    stop ("ERROR: 'times.covariate' is missing. Please specify an integer for times.covariate or a numeric vector of times")
  }
  if (scope=="recent" & is.null(recency)) {
    stop ("ERROR: 'recency' is missing. Please specify an integer between 0 and t for diagnostics 1 and 3, or 0 and t-1 for diagnostic 2")
  }
  if (scope=="average" & (is.null(average.over))) {
    stop ("ERROR: 'average.over' is missing. Please specify one of the following: values, strata, history, time, distance.")
  }
  
  if (!is.null(periods)) {
    
    vector.periods <- c(unlist(periods))
    
    if (!is.list(periods)) {
      stop ("ERROR: When specifying 'periods', it must be specified as a list e.g. list(0,2:3,5:9)")
    } else if ((!class(vector.periods) %in% "numeric") & (!class(vector.periods) %in% "integer")) {
      stop ("ERROR: When specifying 'periods', the list must only contain numeric or integer values e.g. list(0,2:3,5:9)")
    } else if (!all(!vector.periods %in% vector.periods[duplicated(vector.periods)])) {
      stop ("ERROR: When specifying 'periods', the values should be unique e.g. list(0,2:3,5:9")
    } else if (!any(vector.periods==sort(vector.periods))) {
      stop ("ERROR: When specifying 'periods', the values must be in sorted order e.g. list(0,2:3,5:9)")
    }
  }
  
  if (diagnostic==1 & is.null(history)) {
    stop("ERROR: For diagnostic 1, please specify the root names for exposure history")
  } else if (diagnostic==2 & approach=="weight" & (is.null(history) | is.null(weight.exposure))) {
    stop("ERROR: For diagnostic 2 under weighting, please specify the root names for exposure history and exposure weights")
  } else if (diagnostic==2 & approach=="stratify" & is.null(strata)) {
    stop("ERROR: For diagnostic 2 under stratification, please specify the root names for strata")
  } else if (diagnostic==3 & approach=="weight" & (is.null(history) | is.null(weight.exposure))) {
    stop("ERROR: For diagnostic 3 under weighting, please specify the root names for exposure history and exposure weights")
  } else if (diagnostic==3 & approach=="stratify" & (is.null(history) | is.null(strata))) {
    stop("ERROR: For diagnostic 3 under stratification, please specify the root names for exposure history and strata")
  }
  
  if ((length(sort.order)==1) | is.null(sort.order)) {
    if (sort.order!="alphabetical") {
      stop ("ERROR: either specify sort.order as 'alphabetical' or as a character vector of covariates")
    }
  } else if (length(sort.order)>1 & !all(unique(input$name.cov) %in% unique(sort.order))) {
    stop ("ERROR: when specifying the character vector for sort.order, include all covariate names in the input dataframe, and also ensure that their spelling match those specified in sort.order. Provided these criteria are met, the software will still run even if sort.order includes extraneous covariate names that are NOT present in the input dataframe")
  }
  
  if (is.null(history)) {
    input$history.none <- "H"
    history <- "history.none"
  }
  
  if (is.null(strata)) {
    input$strata.none <- 1
    strata <- "strata.none"
  }
  
  if (is.null(weight.exposure)) {
    input$weight.exposure.none <- 1
    weight.exposure <- "weight.exposure.none"
  }
  
  if (is.null(weight.censor)) {
    input$weight.censor.none <- 1
    weight.censor <- "weight.censor.none"
  }
  
  t.exp.data <- unique(input$time.exposure)
  t.cov.data <- unique(input$time.covariate)
  t.exp.spec <- unique(times.exposure)
  t.cov.spec <- unique(times.covariate)
  
  if ((all(t.exp.data %in% t.exp.spec)) & (all(t.cov.data %in% t.cov.spec))) {
  } else {
    input <- input %>% filter((.data$time.exposure %in% t.exp.spec) & (.data$time.covariate %in% t.cov.spec))
  }
  
  s_exposure <- sym(exposure)
  s_history <- sym(history)
  s_strata <- sym(strata)
  s_weight.exposure <- sym(weight.exposure)
  s_weight.censor <- sym(weight.censor)
  
  input <- rename(input,
                  E =  !! s_exposure,
                  H=   !! s_history,
                  S=   !! s_strata,
                  W_a= !! s_weight.exposure,
                  W_s= !! s_weight.censor
  )
  
  if (metric=="SMD") {
    
    if (approach=="weight" | approach=="none") {
      
      if (censoring=="yes") {
        input <- mutate(input,W=as.numeric(.data$W_a)*as.numeric(.data$W_s))
      } else if (censoring=="no") {
        input <- mutate(input,W=as.numeric(.data$W_a))
      }
      
      if (diagnostic==1) {
        
        input <- mutate(input,W=1)
        
        temp.table <-
          data.frame(input %>%
                       select(.data$E,.data$H,.data$W,.data$time.exposure,.data$time.covariate,.data$name.cov,.data$value.cov) %>%
                       group_by(.data$E,.data$H,.data$name.cov,.data$time.exposure,.data$time.covariate) %>%
                       summarise(mean.cov_b=weighted.mean(x=.data$value.cov,w=.data$W,na.rm=TRUE),
                                 sd.cov_b=sd(x=.data$value.cov,na.rm=TRUE),
                                 n.cov_b=sum(.data$W)))
        
        check.table <- temp.table %>%
          group_by(.data$H,.data$time.exposure) %>%
          summarise(nexpval=n_distinct(.data$E)) %>%
          ungroup()
        
        if (all(check.table$nexpval==1)) stop("ERROR: None of the exposure times have exposure variation within levels of exposure history. The program has terminated because the resulting balance table is empty")
        if (any(check.table$nexpval==1)) warning("Some exposure times have no exposure variation within levels of exposure history. Estimates for these times will not appear in the results")
        
        temp.table <- temp.table %>%
          group_by(.data$H,.data$name.cov,.data$time.exposure,.data$time.covariate) %>%
          mutate(mean.cov_a=first(.data$mean.cov_b),
                 sd.cov_a=first(.data$sd.cov_b),
                 n.cov_a=first(.data$n.cov_b)) %>%
          filter(.data$E!=first(.data$E))
        
        if (sd.ref=="no"){
          full.table <- temp.table %>%
            mutate(D=abs(.data$mean.cov_b-.data$mean.cov_a),
                   SMD=ifelse(.data$D==0,0,
                              ifelse(.data$sd.cov_a==0 & .data$sd.cov_b==0,NA_real_,
                                     ifelse(.data$sd.cov_a==0 | .data$sd.cov_b==0,(abs(.data$mean.cov_b-.data$mean.cov_a))/(isTRUE(.data$sd.cov_a!=0)*.data$sd.cov_a + isTRUE(.data$sd.cov_b!=0)*.data$sd.cov_b),
                                            (abs(.data$mean.cov_b-.data$mean.cov_a))/sqrt((.data$sd.cov_a^2*(.data$n.cov_a-1)+.data$sd.cov_b^2*(.data$n.cov_b-1))/(.data$n.cov_a+.data$n.cov_b-2))
                                     )
                              )
                   ),
                   N=.data$n.cov_a+.data$n.cov_b,
                   Nexp=.data$n.cov_b)
          
        } else if (sd.ref=="yes"){
          
          full.table <- temp.table %>%
            mutate(D=abs(.data$mean.cov_b-.data$mean.cov_a),
                   SMD=ifelse(.data$D==0,0,
                              ifelse(.data$sd.cov_a==0,NA_real_,
                                     (abs(.data$mean.cov_b-.data$mean.cov_a))/.data$sd.cov_a)),
                   N=.data$n.cov_a+.data$n.cov_b,
                   Nexp=.data$n.cov_b)
        }
        
        if ( any(is.na(unique(full.table$SMD))) ) warning("SMD values have been set to missing where there is no covariate variation within some level of time-exposure, time-covariate, exposure history, and exposure value; in this case averages for SMD estimates will also appear as missing")
        if ( all(full.table$D==0) & all(full.table$SMD==0) ) warning("There may be no covariate variation within any level of time-exposure, time-covariate, exposure history and/or strata, and exposure value; please ensure that the temporal covariates are specified correctly.")
        
        sub.table  <- full.table %>%
          select(.data$E,.data$H,.data$name.cov,.data$time.exposure,.data$time.covariate,.data$D,.data$SMD,.data$N,.data$Nexp) %>%
          filter (!is.na(.data$D)) %>%
          filter(.data$time.exposure>=.data$time.covariate) %>%
          arrange(.data$name.cov,.data$time.exposure,.data$time.covariate,.data$H)
        
      } else if (diagnostic==2 | diagnostic==3) {
        
        temp.table <-
          data.frame(input %>%
                       select(.data$E,.data$H,.data$W,.data$time.exposure,.data$time.covariate,.data$name.cov,.data$value.cov) %>%
                       group_by(.data$E,.data$H,.data$name.cov,.data$time.exposure,.data$time.covariate) %>%
                       summarise(mean.cov_b=weighted.mean(x=.data$value.cov,w=.data$W,na.rm=TRUE),
                                 sd.cov_b=sd(x=.data$value.cov,na.rm=TRUE),
                                 n.cov_b=sum(.data$W)))
        
        check.table <- temp.table %>%
          group_by(.data$H,.data$time.exposure) %>%
          summarise(nexpval=n_distinct(.data$E)) %>%
          ungroup()
        
        if (all(check.table$nexpval==1)) stop("ERROR: None of the exposure times have exposure variation within levels of exposure history. The program has terminated because the resulting balance table is empty")
        if (any(check.table$nexpval==1)) warning("Some exposure times have no exposure variation within levels of exposure history. Estimates for these times will not appear in the results")
        
        temp.table <- temp.table %>%
          group_by(.data$H,.data$name.cov,.data$time.exposure,.data$time.covariate) %>%
          mutate(mean.cov_a=first(.data$mean.cov_b),
                 sd.cov_a=first(.data$sd.cov_b),
                 n.cov_a=first(.data$n.cov_b)) %>%
          filter(.data$E!=first(.data$E))
        
        if (sd.ref=="no"){
          full.table <- temp.table %>%
            mutate(D=abs(.data$mean.cov_b-.data$mean.cov_a),
                   SMD=ifelse(.data$D==0,0,
                              ifelse(.data$sd.cov_a==0 & .data$sd.cov_b==0,NA_real_,
                                     ifelse(.data$sd.cov_a==0 | .data$sd.cov_b==0,(abs(.data$mean.cov_b-.data$mean.cov_a))/(isTRUE(.data$sd.cov_a!=0)*.data$sd.cov_a + isTRUE(.data$sd.cov_b!=0)*.data$sd.cov_b),
                                            (abs(.data$mean.cov_b-.data$mean.cov_a))/sqrt((.data$sd.cov_a^2*(.data$n.cov_a-1)+.data$sd.cov_b^2*(.data$n.cov_b-1))/(.data$n.cov_a+.data$n.cov_b-2))
                                     )
                              )
                   ),
                   N=.data$n.cov_a+.data$n.cov_b,
                   Nexp=.data$n.cov_b)
          
        } else if (sd.ref=="yes"){
          
          full.table <- temp.table %>%
            mutate(D=abs(.data$mean.cov_b-.data$mean.cov_a),
                   SMD=ifelse(.data$D==0,0,
                              ifelse(.data$sd.cov_a==0,NA_real_,
                                     (abs(.data$mean.cov_b-.data$mean.cov_a))/.data$sd.cov_a)),
                   N=.data$n.cov_a+.data$n.cov_b,
                   Nexp=.data$n.cov_b)
        }
        
        
        if ( any(is.na(unique(full.table$SMD))) ) warning("SMD values have been set to missing where there is no covariate variation within  some level of time-exposure, time-covariate, exposure history, and exposure value; in this case averages for SMD estimates will also appear as missing")
        if ( all(full.table$D==0) & all(full.table$SMD==0) ) warning("There may be no covariate variation within any level of time-exposure, time-covariate, exposure history and/or strata, and exposure value; please ensure that the temporal covariates are specified correctly.")
        
        full.table <- full.table %>% select(.data$E,.data$H,.data$name.cov,.data$time.exposure,.data$time.covariate,.data$D,.data$SMD,.data$N,.data$Nexp)
        
        if (diagnostic==2) {
          
          sub.table <- full.table %>% filter (!is.na(.data$D)) %>% filter(.data$time.exposure<.data$time.covariate)
          
        } else if (diagnostic==3) {
          
          sub.table <- full.table %>% filter (!is.na(.data$D)) %>% filter(.data$time.exposure>=.data$time.covariate)
        }
      }
      
    } else if (approach=="stratify") {
      
      if (censoring=="yes") {
        
        input <- mutate(input,W=as.numeric(.data$W_s))
        
      } else if (censoring=="no") {
        
        input <- mutate(input,W=1)
        
      }
      
      if (diagnostic==1) {
        
        input <- mutate(input,W=1)
        
        temp.table <-
          data.frame(input %>% select(.data$E,.data$H,.data$W,.data$time.exposure,.data$time.covariate,.data$name.cov,.data$value.cov) %>%
                       group_by(.data$E,.data$H,.data$name.cov,.data$time.exposure,.data$time.covariate) %>%
                       summarise(mean.cov_b=weighted.mean(x=.data$value.cov,w=.data$W,na.rm=TRUE),
                                 sd.cov_b=sd(x=.data$value.cov,na.rm=TRUE),
                                 n.cov_b=sum(.data$W)))
        
        check.table <- temp.table %>%
          group_by(.data$H,.data$time.exposure) %>%
          summarise(nexpval=n_distinct(.data$E)) %>%
          ungroup()
        
        if (all(check.table$nexpval==1)) stop("ERROR: None of the exposure times have exposure variation within levels of exposure history. The program has terminated because the resulting balance table is empty")
        if (any(check.table$nexpval==1)) warning("Some exposure times have no exposure variation within levels of exposure history. Estimates for these times will not appear in the results")
        
        temp.table <- temp.table %>% group_by(.data$H,.data$name.cov,.data$time.exposure,.data$time.covariate) %>%
          mutate(mean.cov_a=first(.data$mean.cov_b),
                 sd.cov_a=first(.data$sd.cov_b),
                 n.cov_a=first(.data$n.cov_b)) %>%
          filter(.data$E!=first(.data$E))
        
        if (sd.ref=="no"){
          full.table <- temp.table %>%
            mutate(D=abs(.data$mean.cov_b-.data$mean.cov_a),
                   SMD=ifelse(.data$D==0,0,
                              ifelse(.data$sd.cov_a==0 & .data$sd.cov_b==0,NA_real_,
                                     ifelse(.data$sd.cov_a==0 | .data$sd.cov_b==0,(abs(.data$mean.cov_b-.data$mean.cov_a))/(isTRUE(.data$sd.cov_a!=0)*.data$sd.cov_a + isTRUE(.data$sd.cov_b!=0)*.data$sd.cov_b),
                                            (abs(.data$mean.cov_b-.data$mean.cov_a))/sqrt((.data$sd.cov_a^2*(.data$n.cov_a-1)+.data$sd.cov_b^2*(.data$n.cov_b-1))/(.data$n.cov_a+.data$n.cov_b-2))
                                     )
                              )
                   ),
                   N=.data$n.cov_a+.data$n.cov_b,
                   Nexp=.data$n.cov_b)
          
        } else if (sd.ref=="yes"){
          
          full.table <- temp.table %>%
            mutate(D=abs(.data$mean.cov_b-.data$mean.cov_a),
                   SMD=ifelse(.data$D==0,0,
                              ifelse(.data$sd.cov_a==0,NA_real_,
                                     (abs(.data$mean.cov_b-.data$mean.cov_a))/.data$sd.cov_a)),
                   N=.data$n.cov_a+.data$n.cov_b,
                   Nexp=.data$n.cov_b)
        }
        
        
        if ( any(is.na(unique(full.table$SMD))) ) warning("SMD values have been set to missing where there is no covariate variation within some level of time-exposure, time-covariate, exposure history, and exposure value; in this case averages for SMD estimates will also appear as missing")
        if ( all(full.table$D==0) & all(full.table$SMD==0) ) warning("There may be no covariate variation within any level of time-exposure, time-covariate, exposure history and/or strata, and exposure value; please ensure that the temporal covariates are specified correctly.")
        
        sub.table <- full.table %>%
          select(.data$E,.data$H,.data$name.cov,.data$time.exposure,.data$time.covariate,.data$D,.data$SMD,.data$N,.data$Nexp) %>%
          filter (!is.na(.data$D)) %>%
          filter(.data$time.exposure>=.data$time.covariate) %>%
          arrange(.data$name.cov,.data$time.exposure,.data$time.covariate,.data$H)
        
      } else if (diagnostic==2) {
        
        values.exposure <- sort(unique(input$E))
        temp.table <-
          data.frame(input %>%
                       select(.data$E,.data$S,.data$W,.data$time.exposure,.data$time.covariate,.data$name.cov,.data$value.cov) %>%
                       group_by(.data$E,.data$S,.data$name.cov,.data$time.exposure,.data$time.covariate) %>%
                       summarise(mean.cov_b=weighted.mean(x=.data$value.cov,w=.data$W,na.rm=TRUE),
                                 sd.cov_b=sd(x=.data$value.cov,na.rm=TRUE),
                                 n.cov_b=sum(.data$W)))
        
        check.table <- temp.table %>%
          group_by(.data$S,.data$time.exposure) %>%
          summarise(nexpval=n_distinct(.data$E)) %>%
          ungroup()
        
        if (all(check.table$nexpval==1)) stop("ERROR: None of the exposure times have exposure variation within levels of exposure history. The program has terminated because the resulting balance table is empty")
        if (any(check.table$nexpval==1)) warning("Some exposure times have no exposure variation within levels of strata. Estimates for these times will not appear in the results")
        
        temp.table <- temp.table %>%
          group_by(.data$S,.data$name.cov,.data$time.exposure,.data$time.covariate) %>%
          mutate(mean.cov_a=first(.data$mean.cov_b),
                 sd.cov_a=first(.data$sd.cov_b),
                 n.cov_a=first(.data$n.cov_b)) %>%
          filter(.data$E!=first(.data$E))
        
        if (sd.ref=="no"){
          full.table <- temp.table %>%
            mutate(D=abs(.data$mean.cov_b-.data$mean.cov_a),
                   SMD=ifelse(.data$D==0,0,
                              ifelse(.data$sd.cov_a==0 & .data$sd.cov_b==0,NA_real_,
                                     ifelse(.data$sd.cov_a==0 | .data$sd.cov_b==0,(abs(.data$mean.cov_b-.data$mean.cov_a))/(isTRUE(.data$sd.cov_a!=0)*.data$sd.cov_a + isTRUE(.data$sd.cov_b!=0)*.data$sd.cov_b),
                                            (abs(.data$mean.cov_b-.data$mean.cov_a))/sqrt((.data$sd.cov_a^2*(.data$n.cov_a-1)+.data$sd.cov_b^2*(.data$n.cov_b-1))/(.data$n.cov_a+.data$n.cov_b-2))
                                     )
                              )
                   ),
                   N=.data$n.cov_a+.data$n.cov_b,
                   Nexp=.data$n.cov_b)
          
        } else if (sd.ref=="yes"){
          
          full.table <- temp.table %>%
            mutate(D=abs(.data$mean.cov_b-.data$mean.cov_a),
                   SMD=ifelse(.data$D==0,0,
                              ifelse(.data$sd.cov_a==0,NA_real_,
                                     (abs(.data$mean.cov_b-.data$mean.cov_a))/.data$sd.cov_a)),
                   N=.data$n.cov_a+.data$n.cov_b,
                   Nexp=.data$n.cov_b)
        }
        
        if ( any(is.na(unique(full.table$SMD))) ) warning("SMD values have been set to missing where there is no covariate variation within some level of time-exposure, time-covariate, strata, and exposure value; in this case averages for SMD estimates will also appear as missing")
        if ( all(full.table$D==0) & all(full.table$SMD==0) ) warning("There may be no covariate variation within any level of time-exposure, time-covariate, exposure history and/or strata, and exposure value; please ensure that the temporal covariates are specified correctly.")
        
        
        sub.table <-  full.table %>%
          select(.data$E,.data$S,.data$name.cov,.data$time.exposure,.data$time.covariate,.data$D,.data$SMD,.data$N,.data$Nexp) %>%
          filter (!is.na(.data$D)) %>%
          filter(.data$time.exposure<.data$time.covariate) %>%
          arrange(.data$S,.data$name.cov,.data$time.exposure,.data$time.covariate)
        
      } else if (diagnostic==3) {
        
        temp.table <-
          data.frame(input %>%
                       select(.data$E,.data$S,.data$H,.data$W,.data$time.exposure,.data$time.covariate,.data$name.cov,.data$value.cov) %>%
                       group_by(.data$E,.data$S,.data$H,.data$time.exposure,.data$time.covariate,.data$name.cov) %>%
                       summarise(mean.cov_b=weighted.mean(x=.data$value.cov,w=.data$W,na.rm=TRUE),
                                 sd.cov_b=sd(x=.data$value.cov,na.rm=TRUE),
                                 n.cov_b=sum(.data$W)))
        
        check.table <- temp.table %>%
          group_by(.data$S,.data$H,.data$time.exposure) %>%
          summarise(nexpval=n_distinct(.data$E)) %>%
          ungroup()
        
        if (all(check.table$nexpval==1)) stop("ERROR: None of the exposure times have exposure variation within levels of exposure history. The program has terminated because the resulting balance table is empty")
        if (any(check.table$nexpval==1)) warning("Some exposure times have no exposure variation within levels of strata and exposure history. Estimates for these times will not appear in the results")
        
        temp.table <- temp.table %>%
          group_by(.data$S,.data$H,.data$name.cov,.data$time.exposure,.data$time.covariate) %>%
          mutate(mean.cov_a=first(.data$mean.cov_b),
                 sd.cov_a=first(.data$sd.cov_b),
                 n.cov_a=first(.data$n.cov_b)) %>%
          filter(.data$E!=first(.data$E))
        
        if (sd.ref=="no"){
          full.table <- temp.table %>%
            mutate(D=abs(.data$mean.cov_b-.data$mean.cov_a),
                   SMD=ifelse(.data$D==0,0,
                              ifelse(.data$sd.cov_a==0 & .data$sd.cov_b==0,NA_real_,
                                     ifelse(.data$sd.cov_a==0 | .data$sd.cov_b==0,(abs(.data$mean.cov_b-.data$mean.cov_a))/(isTRUE(.data$sd.cov_a!=0)*.data$sd.cov_a + isTRUE(.data$sd.cov_b!=0)*.data$sd.cov_b),
                                            (abs(.data$mean.cov_b-.data$mean.cov_a))/sqrt((.data$sd.cov_a^2*(.data$n.cov_a-1)+.data$sd.cov_b^2*(.data$n.cov_b-1))/(.data$n.cov_a+.data$n.cov_b-2))
                                     )
                              )
                   ),
                   N=.data$n.cov_a+.data$n.cov_b,
                   Nexp=.data$n.cov_b)
          
        } else if (sd.ref=="yes"){
          
          full.table <- temp.table %>%
            mutate(D=abs(.data$mean.cov_b-.data$mean.cov_a),
                   SMD=ifelse(.data$D==0,0,
                              ifelse(.data$sd.cov_a==0,NA_real_,
                                     (abs(.data$mean.cov_b-.data$mean.cov_a))/.data$sd.cov_a)),
                   N=.data$n.cov_a+.data$n.cov_b,
                   Nexp=.data$n.cov_b)
        }
        
        if ( any(is.na(unique(full.table$SMD))) ) warning("SMD values have been set to missing where there is no covariate variation within some level of time-exposure, time-covariate, strata, exposure history, and exposure value; in this case averages for SMD estimates will also appear as missing")
        if ( all(full.table$D==0) & all(full.table$SMD==0) ) warning("There may be no covariate variation within any level of time-exposure, time-covariate, exposure history and/or strata, and exposure value; please ensure that the temporal covariates are specified correctly.")
        
        sub.table <- full.table %>%
          select(.data$E,.data$S,.data$H,.data$name.cov,.data$time.exposure,.data$time.covariate,.data$D,.data$SMD,.data$N,.data$Nexp) %>%
          filter (!is.na(.data$D)) %>%
          filter(.data$time.exposure>=.data$time.covariate) %>%
          arrange(.data$S,.data$name.cov,.data$time.exposure,.data$time.covariate,.data$H)
        
      }
    }
    
  } else if (metric=="KS") {
    
    if (approach=="weight" | approach=="none") {
      
      if (censoring=="yes") {
        input <- mutate(input,W=as.numeric(.data$W_a)*as.numeric(.data$W_s))
      } else if (censoring=="no") {
        input <- mutate(input,W=as.numeric(.data$W_a))
      }
      
      if (diagnostic==1) {
        
        input <- mutate(input,W=1)
        
        temp.table=data.frame(input %>%
                                select(.data$E,.data$H,.data$W,.data$time.exposure,.data$time.covariate,.data$name.cov,.data$value.cov) %>%
                                group_by(.data$H,.data$name.cov,.data$time.exposure,.data$time.covariate) %>%
                                summarise(KS=max(abs(ewcdf(.data$value.cov[.data$E==1],w = .data$W[.data$E == 1])(.data$value.cov)
                                                     -ewcdf(.data$value.cov[.data$E==0],w = .data$W[.data$E == 0])(.data$value.cov))),N=sum(.data$W),Nexp=sum(.data$E==1)))
        
        temp.table=data.frame(E=rep(1,nrow(temp.table)),temp.table)
        full.table=temp.table %>%mutate(KS=KS,N=N,Nexp=Nexp)
        
        if ( any(is.na(unique(full.table$KS))) ) warning("KS values have been set to missing where there is no covariate variation within some level of time-exposure, time-covariate, exposure history, and exposure value; in this case averages for KS estimates will also appear as missing")
        if ( all(full.table$KS==0)) warning("There may be no covariate variation within any level of time-exposure, time-covariate, exposure history and/or strata, and exposure value; please ensure that the temporal covariates are specified correctly.")
        
        sub.table  <- full.table %>%
          select(.data$E,.data$H,.data$name.cov,.data$time.exposure,.data$time.covariate,.data$KS,.data$N,.data$Nexp) %>%
          filter (!is.na(.data$KS)) %>%
          filter(.data$time.exposure>=.data$time.covariate) %>%
          arrange(.data$name.cov,.data$time.exposure,.data$time.covariate,.data$H)
        
      } else if (diagnostic==2 | diagnostic==3) {
        
        temp.table <-
          data.frame(input %>%
                       select(.data$E,.data$H,.data$W,.data$time.exposure,.data$time.covariate,.data$name.cov,.data$value.cov) %>%
                       group_by(.data$H,.data$name.cov,.data$time.exposure,.data$time.covariate) %>%
                       summarise(KS=max(abs(ewcdf(.data$value.cov[.data$E==1],w = .data$W[.data$E == 1])(.data$value.cov)
                                            -ewcdf(.data$value.cov[.data$E==0],w = .data$W[.data$E == 0])(.data$value.cov))),N=sum(.data$W),Nexp=sum(.data$E==1))) 
        
        temp.table=data.frame(E=rep(1,nrow(temp.table)),temp.table)
        full.table=temp.table %>%mutate(KS=KS,N=N,Nexp=Nexp)
        
        if ( any(is.na(unique(full.table$KS))) ) warning("KS values have been set to missing where there is no covariate variation within some level of time-exposure, time-covariate, exposure history, and exposure value; in this case averages for KS estimates will also appear as missing")
        if ( all(full.table$KS==0)) warning("There may be no covariate variation within any level of time-exposure, time-covariate, exposure history and/or strata, and exposure value; please ensure that the temporal covariates are specified correctly.")
        
        
        full.table=full.table %>% select(.data$E,.data$H,.data$name.cov,.data$time.exposure,.data$time.covariate,.data$KS,.data$N,.data$Nexp)
        
        if (diagnostic==2) {
          
          sub.table <- full.table %>% filter (!is.na(.data$KS)) %>% filter(.data$time.exposure<.data$time.covariate)
          
        } else if (diagnostic==3) {
          
          sub.table <- full.table %>% filter (!is.na(.data$KS)) %>% filter(.data$time.exposure>=.data$time.covariate)
        }
      }
      
    } else if (approach=="stratify") {
      
      if (censoring=="yes") {
        
        input <- mutate(input,W=as.numeric(.data$W_s))
        
      } else if (censoring=="no") {
        
        input <- mutate(input,W=1)
        
      }
      
      if (diagnostic==1) {
        
        input <- mutate(input,W=1)
        
        temp.table <-
          data.frame(input %>%
                       select(.data$E,.data$H,.data$W,.data$time.exposure,.data$time.covariate,.data$name.cov,.data$value.cov) %>%
                       group_by(.data$H,.data$name.cov,.data$time.exposure,.data$time.covariate) %>%
                       summarise(KS=max(abs(ewcdf(.data$value.cov[.data$E==1],w = .data$W[.data$E == 1])(.data$value.cov)
                                            -ewcdf(.data$value.cov[.data$E==0],w = .data$W[.data$E == 0])(.data$value.cov))),N=sum(.data$W),Nexp=sum(.data$E==1)))    
        
        temp.table=data.frame(E=rep(1,nrow(temp.table)),temp.table)
        full.table=temp.table %>%mutate(KS=KS,N=N,Nexp=Nexp)
        
        if ( any(is.na(unique(full.table$KS))) ) warning("KS values have been set to missing where there is no covariate variation within some level of time-exposure, time-covariate, exposure history, and exposure value; in this case averages for KS estimates will also appear as missing")
        if ( all(full.table$KS==0)) warning("There may be no covariate variation within any level of time-exposure, time-covariate, exposure history and/or strata, and exposure value; please ensure that the temporal covariates are specified correctly.")
        
        sub.table  <- full.table %>%
          select(.data$E,.data$H,.data$name.cov,.data$time.exposure,.data$time.covariate,.data$KS,.data$N,.data$Nexp) %>%
          filter (!is.na(.data$KS)) %>%
          filter(.data$time.exposure>=.data$time.covariate) %>%
          arrange(.data$name.cov,.data$time.exposure,.data$time.covariate,.data$H)
        
      } else if (diagnostic==2) {
        
        values.exposure <- sort(unique(input$E))
        temp.table <-
          data.frame(input %>%
                       select(.data$E,.data$S,.data$H,.data$W,.data$time.exposure,.data$time.covariate,.data$name.cov,.data$value.cov) %>%
                       group_by(.data$S,.data$H,.data$name.cov,.data$time.exposure,.data$time.covariate) %>%
                       summarise(KS=max(abs(ewcdf(.data$value.cov[.data$E==1],w = .data$W[.data$E == 1])(.data$value.cov)
                                            -ewcdf(.data$value.cov[.data$E==0],w = .data$W[.data$E == 0])(.data$value.cov))),N=sum(.data$W),Nexp=sum(.data$E==1)))
        
        
        temp.table=data.frame(E=rep(1,nrow(temp.table)),temp.table)
        full.table=temp.table %>%mutate(KS=KS,N=N,Nexp=Nexp)
        
        if ( any(is.na(unique(full.table$KS))) ) warning("KS values have been set to missing where there is no covariate variation within some level of time-exposure, time-covariate, exposure history, and exposure value; in this case averages for KS estimates will also appear as missing")
        if ( all(full.table$KS==0)) warning("There may be no covariate variation within any level of time-exposure, time-covariate, exposure history and/or strata, and exposure value; please ensure that the temporal covariates are specified correctly.")
        
        sub.table  <- full.table %>%
          select(.data$E,.data$S,.data$H,.data$name.cov,.data$time.exposure,.data$time.covariate,.data$KS,.data$N,.data$Nexp) %>%
          filter (!is.na(.data$KS)) %>%
          filter(.data$time.exposure>=.data$time.covariate) %>%
          arrange(.data$S,.data$name.cov,.data$time.exposure,.data$time.covariate,.data$H)
        
        
      } else if (diagnostic==3) {
        
        temp.table <-
          data.frame(input %>%
                       select(.data$E,.data$S,.data$H,.data$W,.data$time.exposure,.data$time.covariate,.data$name.cov,.data$value.cov) %>%
                       group_by(.data$S,.data$H,.data$name.cov,.data$time.exposure,.data$time.covariate) %>%
                       summarise(KS=max(abs(ewcdf(.data$value.cov[.data$E==1],w = .data$W[.data$E == 1])(.data$value.cov)
                                            -ewcdf(.data$value.cov[.data$E==0],w = .data$W[.data$E == 0])(.data$value.cov))),N=sum(.data$W),Nexp=sum(.data$E==1)))
        
        
        temp.table=data.frame(E=rep(1,nrow(temp.table)),temp.table)
        full.table=temp.table %>%mutate(KS=KS,N=N,Nexp=Nexp)
        
        if ( any(is.na(unique(full.table$KS))) ) warning("KS values have been set to missing where there is no covariate variation within some level of time-exposure, time-covariate, exposure history, and exposure value; in this case averages for KS estimates will also appear as missing")
        if ( all(full.table$KS==0)) warning("There may be no covariate variation within any level of time-exposure, time-covariate, exposure history and/or strata, and exposure value; please ensure that the temporal covariates are specified correctly.")
        
        sub.table  <- full.table %>%
          select(.data$E,.data$S,.data$H,.data$name.cov,.data$time.exposure,.data$time.covariate,.data$KS,.data$N,.data$Nexp) %>%
          filter (!is.na(.data$KS)) %>%
          filter(.data$time.exposure>=.data$time.covariate) %>%
          arrange(.data$S,.data$name.cov,.data$time.exposure,.data$time.covariate,.data$H)  
        
      }
    }
  } else if (metric=="OVL") {
    
    if (approach=="weight" | approach=="none") {
      
      if (censoring=="yes") {
        input <- mutate(input,W=as.numeric(.data$W_a)*as.numeric(.data$W_s))
      } else if (censoring=="no") {
        input <- mutate(input,W=as.numeric(.data$W_a))
      }
      
      if (diagnostic==1) {
        
        input <- mutate(input,W=1)
        
        temp.table=data.frame(input%>%select(.data$H,.data$E,.data$W,.data$time.exposure,.data$time.covariate,.data$name.cov,.data$value.cov) %>%
                                group_by(.data$H,.data$name.cov,.data$time.exposure,.data$time.covariate) %>%
                                summarise(OVL=tryCatch(1-integrate(function(x)pmin(approxfun(density(.data$value.cov[.data$E==1],weights = .data$W[.data$E == 1]/sum(.data$W[.data$E == 1]),
                                                                                                     from=weighted.quantile(.data$value.cov,.data$W,0.05),to=weighted.quantile(.data$value.cov,.data$W,0.95),bw="nrd0"))(x), 
                                                                                   approxfun(density(.data$value.cov[.data$E==0], weights = .data$W[.data$E == 0]/sum(.data$W[.data$E == 0]),
                                                                                                     from=weighted.quantile(.data$value.cov,.data$W,0.05),to=weighted.quantile(.data$value.cov,.data$W,0.95),bw="nrd0"))(x)), 
                                                                   lower=weighted.quantile(.data$value.cov,.data$W, 0.05),upper=weighted.quantile(.data$value.cov,.data$W,0.95),
                                                                   subdivisions = 500)$value,error = function(e){s = NA}),N=sum(.data$W),Nexp=sum(.data$E==1)))      
        
        temp.table=data.frame(E=rep(1,nrow(temp.table)),temp.table)
        full.table=temp.table %>%mutate(OVL=OVL,N=N,Nexp=Nexp)
        
        if ( any(is.na(unique(full.table$OVL))) ) warning("OVL values have been set to missing where there is no covariate variation within some level of time-exposure, time-covariate, exposure history, and exposure value; in this case averages for OVL estimates will also appear as missing")
        if ( all(full.table$OVL==0)) warning("There may be no covariate variation within any level of time-exposure, time-covariate, exposure history and/or strata, and exposure value; please ensure that the temporal covariates are specified correctly.")
        
        sub.table  <- full.table %>%
          select(.data$E,.data$H,.data$name.cov,.data$time.exposure,.data$time.covariate,.data$OVL,.data$N,.data$Nexp) %>%
          filter (!is.na(.data$OVL)) %>%
          filter(.data$time.exposure>=.data$time.covariate) %>%
          arrange(.data$name.cov,.data$time.exposure,.data$time.covariate,.data$H)
        
      } else if (diagnostic==2 | diagnostic==3) {
        
        temp.table=data.frame(input%>%select(.data$H,.data$E,.data$W,.data$time.exposure,.data$time.covariate,.data$name.cov,.data$value.cov) %>%
                                group_by(.data$H,.data$name.cov,.data$time.exposure,.data$time.covariate) %>%
                                summarise(OVL=tryCatch(1-integrate(function(x)pmin(approxfun(density(.data$value.cov[.data$E==1],weights = .data$W[.data$E == 1]/sum(.data$W[.data$E == 1]),
                                                                                                     from=weighted.quantile(.data$value.cov,.data$W,0.05),to=weighted.quantile(.data$value.cov,.data$W,0.95),bw="nrd0"))(x), 
                                                                                   approxfun(density(.data$value.cov[.data$E==0], weights = .data$W[.data$E == 0]/sum(.data$W[.data$E == 0]),
                                                                                                     from=weighted.quantile(.data$value.cov,.data$W,0.05),to=weighted.quantile(.data$value.cov,.data$W,0.95),bw="nrd0"))(x)), 
                                                                   lower=weighted.quantile(.data$value.cov,.data$W, 0.05),upper=weighted.quantile(.data$value.cov,.data$W,0.95),
                                                                   subdivisions = 500)$value,error = function(e){s = NA}),N=sum(.data$W),Nexp=sum(.data$E==1)))      
        
        temp.table=data.frame(E=rep(1,nrow(temp.table)),temp.table)
        full.table=temp.table %>%mutate(OVL=OVL,N=N,Nexp=Nexp)
        
        if ( any(is.na(unique(full.table$OVL))) ) warning("OVL values have been set to missing where there is no covariate variation within some level of time-exposure, time-covariate, exposure history, and exposure value; in this case averages for OVL estimates will also appear as missing")
        if ( all(full.table$OVL==0)) warning("There may be no covariate variation within any level of time-exposure, time-covariate, exposure history and/or strata, and exposure value; please ensure that the temporal covariates are specified correctly.")  
        
        
        full.table=full.table %>% select(.data$E,.data$H,.data$name.cov,.data$time.exposure,.data$time.covariate,.data$OVL,.data$N,.data$Nexp)
        
        if (diagnostic==2) {
          
          sub.table <- full.table %>% filter (!is.na(.data$OVL)) %>% filter(.data$time.exposure<.data$time.covariate)
          
        } else if (diagnostic==3) {
          
          sub.table <- full.table %>% filter (!is.na(.data$OVL)) %>% filter(.data$time.exposure>=.data$time.covariate)
        }
      }
      
    } else if (approach=="stratify") {
      
      if (censoring=="yes") {
        
        input <- mutate(input,W=as.numeric(.data$W_s))
        
      } else if (censoring=="no") {
        
        input <- mutate(input,W=1)
        
      }
      
      if (diagnostic==1) {
        
        input <- mutate(input,W=1)
        
        temp.table=data.frame(input%>%select(.data$H,.data$E,.data$W,.data$time.exposure,.data$time.covariate,.data$name.cov,.data$value.cov) %>%
                                group_by(.data$H,.data$name.cov,.data$time.exposure,.data$time.covariate) %>%
                                summarise(OVL=tryCatch(1-integrate(function(x)pmin(approxfun(density(.data$value.cov[.data$E==1],weights = .data$W[.data$E == 1]/sum(.data$W[.data$E == 1]),
                                                                                                     from=weighted.quantile(.data$value.cov,.data$W,0.05),to=weighted.quantile(.data$value.cov,.data$W,0.95),bw="nrd0"))(x), 
                                                                                   approxfun(density(.data$value.cov[.data$E==0], weights = .data$W[.data$E == 0]/sum(.data$W[.data$E == 0]),
                                                                                                     from=weighted.quantile(.data$value.cov,.data$W,0.05),to=weighted.quantile(.data$value.cov,.data$W,0.95),bw="nrd0"))(x)), 
                                                                   lower=weighted.quantile(.data$value.cov,.data$W, 0.05),upper=weighted.quantile(.data$value.cov,.data$W,0.95),
                                                                   subdivisions = 500)$value,error = function(e){s = NA}),N=sum(.data$W),Nexp=sum(.data$E==1)))      
        
        temp.table=data.frame(E=rep(1,nrow(temp.table)),temp.table)
        full.table=temp.table %>%mutate(OVL=OVL,N=N,Nexp=Nexp)
        
        if ( any(is.na(unique(full.table$OVL))) ) warning("OVL values have been set to missing where there is no covariate variation within some level of time-exposure, time-covariate, exposure history, and exposure value; in this case averages for OVL estimates will also appear as missing")
        if ( all(full.table$OVL==0)) warning("There may be no covariate variation within any level of time-exposure, time-covariate, exposure history and/or strata, and exposure value; please ensure that the temporal covariates are specified correctly.")    
        
        
        sub.table  <- full.table %>%
          select(.data$E,.data$H,.data$name.cov,.data$time.exposure,.data$time.covariate,.data$OVL,.data$N,.data$Nexp) %>%
          filter (!is.na(.data$OVL)) %>%
          filter(.data$time.exposure>=.data$time.covariate) %>%
          arrange(.data$name.cov,.data$time.exposure,.data$time.covariate,.data$H)
        
      } else if (diagnostic==2) {
        
        values.exposure <- sort(unique(input$E))
        
        temp.table=data.frame(input%>%select(data$S,.data$H,.data$E,.data$W,.data$time.exposure,.data$time.covariate,.data$name.cov,.data$value.cov) %>%
                                group_by(data$S,.data$H,.data$name.cov,.data$time.exposure,.data$time.covariate) %>%
                                summarise(OVL=tryCatch(1-integrate(function(x)pmin(approxfun(density(.data$value.cov[.data$E==1],weights = .data$W[.data$E == 1]/sum(.data$W[.data$E == 1]),
                                                                                                     from=weighted.quantile(.data$value.cov,.data$W,0.05),to=weighted.quantile(.data$value.cov,.data$W,0.95),bw="nrd0"))(x), 
                                                                                   approxfun(density(.data$value.cov[.data$E==0], weights = .data$W[.data$E == 0]/sum(.data$W[.data$E == 0]),
                                                                                                     from=weighted.quantile(.data$value.cov,.data$W,0.05),to=weighted.quantile(.data$value.cov,.data$W,0.95),bw="nrd0"))(x)), 
                                                                   lower=weighted.quantile(.data$value.cov,.data$W, 0.05),upper=weighted.quantile(.data$value.cov,.data$W,0.95),
                                                                   subdivisions = 500)$value,error = function(e){s = NA}),N=sum(.data$W),Nexp=sum(.data$E==1)))      
        
        temp.table=data.frame(E=rep(1,nrow(temp.table)),temp.table)
        full.table=temp.table %>%mutate(OVL=OVL,N=N,Nexp=Nexp)
        
        if ( any(is.na(unique(full.table$OVL))) ) warning("OVL values have been set to missing where there is no covariate variation within some level of time-exposure, time-covariate, exposure history, and exposure value; in this case averages for OVL estimates will also appear as missing")
        if ( all(full.table$OVL==0)) warning("There may be no covariate variation within any level of time-exposure, time-covariate, exposure history and/or strata, and exposure value; please ensure that the temporal covariates are specified correctly.")          
        
        sub.table  <- full.table %>%
          select(.data$E,.data$S,.data$H,.data$name.cov,.data$time.exposure,.data$time.covariate,.data$OVL,.data$N,.data$Nexp) %>%
          filter (!is.na(.data$OVL)) %>%
          filter(.data$time.exposure>=.data$time.covariate) %>%
          arrange(.data$S,.data$name.cov,.data$time.exposure,.data$time.covariate,.data$H)
        
        
      } else if (diagnostic==3) {
        
        temp.table=data.frame(input%>%select(data$S,.data$H,.data$E,.data$W,.data$time.exposure,.data$time.covariate,.data$name.cov,.data$value.cov) %>%
                                group_by(data$S,.data$H,.data$name.cov,.data$time.exposure,.data$time.covariate) %>%
                                summarise(OVL=tryCatch(1-integrate(function(x)pmin(approxfun(density(.data$value.cov[.data$E==1],weights = .data$W[.data$E == 1]/sum(.data$W[.data$E == 1]),
                                                                                                     from=weighted.quantile(.data$value.cov,.data$W,0.05),to=weighted.quantile(.data$value.cov,.data$W,0.95),bw="nrd0"))(x), 
                                                                                   approxfun(density(.data$value.cov[.data$E==0], weights = .data$W[.data$E == 0]/sum(.data$W[.data$E == 0]),
                                                                                                     from=weighted.quantile(.data$value.cov,.data$W,0.05),to=weighted.quantile(.data$value.cov,.data$W,0.95),bw="nrd0"))(x)), 
                                                                   lower=weighted.quantile(.data$value.cov,.data$W, 0.05),upper=weighted.quantile(.data$value.cov,.data$W,0.95),
                                                                   subdivisions = 500)$value,error = function(e){s = NA}),N=sum(.data$W),Nexp=sum(.data$E==1)))      
        
        temp.table=data.frame(E=rep(1,nrow(temp.table)),temp.table)
        full.table=temp.table %>%mutate(OVL=OVL,N=N,Nexp=Nexp)
        
        if ( any(is.na(unique(full.table$OVL))) ) warning("OVL values have been set to missing where there is no covariate variation within some level of time-exposure, time-covariate, exposure history, and exposure value; in this case averages for OVL estimates will also appear as missing")
        if ( all(full.table$OVL==0)) warning("There may be no covariate variation within any level of time-exposure, time-covariate, exposure history and/or strata, and exposure value; please ensure that the temporal covariates are specified correctly.")          
        
        sub.table  <- full.table %>%
          select(.data$E,.data$S,.data$H,.data$name.cov,.data$time.exposure,.data$time.covariate,.data$OVL,.data$N,.data$Nexp) %>%
          filter (!is.na(.data$OVL)) %>%
          filter(.data$time.exposure>=.data$time.covariate) %>%
          arrange(.data$S,.data$name.cov,.data$time.exposure,.data$time.covariate,.data$H) 
      }
    }
    
  }  
  
  if (loop=="no") {
    
    output <- apply.scope(input=sub.table,
                          diagnostic=diagnostic,
                          approach=approach,
                          scope=scope,
                          average.over=average.over,
                          periods=periods,
                          list.distance=list.distance,
                          recency=recency,
                          sort.order=sort.order,
                          ignore.missing.metric=ignore.missing.metric,
                          metric=metric) %>%
      data.frame()
    
  } else if (loop=="yes") {
    
    output <- sub.table %>%
      data.frame()
    
  }
  
  return(output)
}
