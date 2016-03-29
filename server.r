library(shiny)
library(truncnorm)
library(parallel)
shinyServer(
  function(input, output, session){
    socks = reactive(
      {
        if(input$priorsock=="neg"){
          mean <- input$p_mu # for a total of 7*2=14 paired socks.
          sd <- input$p_sd
          prior_size_param <- -mean^2 / (mean - sd^2)
          socks1=rnbinom(input$sims, mu = mean, size = prior_size_param)
          return(socks1)
        }
        if(input$priorsock=='pois'){
          mean=input$lambda
          socks2=rpois(input$sims, lambda=mean)
          return(socks2)
        }
        if(input$priorsock=="bin"){
          s=input$nmu
          p=input$np
          socks3=rbinom(input$sims,size=s,prob=p)
          return (socks3)
        }
      }
    )
    
    proportion=reactive(
      {
        if (input$priorpair=="beta"){
          prop_pairs1 <- rbeta(input$sims, shape1 = input$prop_a, shape2 = input$prop_b)
          return (prop_pairs1)
        }
        if (input$priorpair=='uni'){
          prop_pairs2<-runif(input$sims, min=input$unif_range[1], max=input$unif_range[2])
          return(prop_pairs2)
        }
        if (input$priorpair=="tnorm"){
          prop_pairs3<-rtruncnorm(input$sims, a=0, b=1, mean=input$t_mu, sd=input$t_sd)
          return(prop_pairs3)
        }
      }
    )
    
    prioronpairs=reactive(
      {
        n_pairs <- round(floor(socks() / 2) * proportion())
        return(n_pairs)
      }
    )
    prioronodds=reactive(
      {
        n_odd <- socks() - prioronpairs() * 2
        return(n_odd)
      }
    )
    
    #GENERATIVE MODELS
    generativemodel=reactive(
      {
        picked=input$n_pairs*2+input$n_odds
        df=data.frame("n_socks"=socks(), "prop_pairs"=proportion(), "n_odd"=prioronodds(),"n_pairs"=prioronpairs())
        
        socksim=mclapply(seq_len(input$sims), function(i){socks <- rep(seq_len(df$n_pairs[i] + df$n_odd[i]), rep(c(2, 1), c(df$n_pairs[i], df$n_odd[i])))
        picked_socks <- sample(socks, size =  min(picked, df$n_socks[i]))
        sock_counts <- table(picked_socks)
        unique = sum(sock_counts == 1)
        pairs = sum(sock_counts == 2)
        c(unique = sum(sock_counts == 1), pairs = sum(sock_counts == 2),
          n_socks = df$n_socks[i], n_pairs = df$n_pairs[i], n_odd = df$n_odd[i], prop_pairs = df$prop_pairs[i])}, mc.cores=input$core)
        
        
        
        data=data.frame(matrix(unlist(socksim), ncol=6, byrow=T),stringsAsFactors=FALSE)
        data.sub=subset(data, data$X1==input$n_odds & data$X2==input$n_pairs)
        return(data.sub)
      }
    )
    
    ##OUTPUTS
    ##OUTPUTS
    ##OUTPUTS
    output$post=renderPlot(
      {
        ans=generativemodel()$X3
        plot(hist(ans), main="post on n socks", xlab='number of socks', yaxt='n')
        if (1 %in% input$sumpost){
          abline(v=mean(ans), col='red')
        }
        if (2 %in% input$sumpost){
          abline(v=median(ans), col='blue')
        }
        if (3 %in% input$sumpost){
          abline(v=quantile(ans, 0.025), col='green')
          abline(v=quantile(ans, 0.975), col='green')
        }
      }
    )
    output$postprop=renderPlot(
      {
        ans=generativemodel()$X6
        plot(hist(ans), main="post on prop pairs", xlab='number of socks', yaxt='n')
        if (1 %in% input$sumpost){
          abline(v=mean(ans), col='red')
        }
        if (2 %in% input$sumpost){
          abline(v=median(ans), col='blue')
        }
        if (3 %in% input$sumpost){
          abline(v=quantile(ans, 0.025), col='green')
          abline(v=quantile(ans, 0.975), col='green')
        }
      }
    )
    output$postpairs=renderPlot(
      {
        ans=generativemodel()$X4
        plot(hist(ans), main="post on n pairs", xlab='number of socks', yaxt='n')
        if (1 %in% input$sumpost){
          abline(v=mean(ans), col='red')
        }
        if (2 %in% input$sumpost){
          abline(v=median(ans), col='blue')
        }
        if (3 %in% input$sumpost){
          abline(v=quantile(ans, 0.025), col='green')
          abline(v=quantile(ans, 0.975), col='green')
        }
      }
    )
    output$postodds=renderPlot(
      {
        ans=generativemodel()$X5
        plot(hist(ans), main="post on n odds", xlab='number of socks', yaxt='n')
        if (1 %in% input$sumpost){
          abline(v=mean(ans), col='red')
        }
        if (2 %in% input$sumpost){
          abline(v=median(ans), col='blue')
        }
        if (3 %in% input$sumpost){
          abline(v=quantile(ans, 0.025), col='green')
          abline(v=quantile(ans, 0.975), col='green')
        }
      }
    )
    ##outputs
    output$prior=renderPlot(
      {
        plot(hist(socks()), main="prior on n socks", xlab='number of socks', yaxt='n')
        if (1 %in% input$sumprior){
          abline(v=mean(socks()), col='red')
        }
        if (2 %in% input$sumprior){
          abline(v=median(socks()), col='blue')
        }
        if (3 %in% input$sumprior){
          abline(v=quantile(socks(), 0.025), col='green')
          abline(v=quantile(socks(), 0.975), col='green')
        }
        
      }
    )
    output$prop=renderPlot(
      {
        plot(hist(proportion()), main="prior on prop_pairs", xlab='proportion of pairs of socks', yaxt='n')
        if (1 %in% input$sumprior){
          abline(v=mean(proportion()), col='red')
        }
        if (2 %in% input$sumprior){
          abline(v=median(proportion()), col='blue')
        }
        if (3 %in% input$sumprior){
          abline(v=quantile(proportion(), 0.025), col='green')
          abline(v=quantile(proportion(), 0.975), col='green')
        }
      }
    )
    output$pairs=renderPlot(
      
      {
        plot(hist(prioronpairs()), main="resulting prior on n pairs", xlab='number of sock pairs', yaxt='n')
        if (1 %in% input$sumprior){
          abline(v=mean(prioronpairs()), col='red')
        }
        if (2 %in% input$sumprior){
          abline(v=median(prioronpairs()), col='blue')
        }
        if (3 %in% input$sumprior){
          abline(v=quantile(prioronpairs(), 0.025), col='green')
          abline(v=quantile(prioronpairs(), 0.975), col='green')
        }
        
      }
      
    )
    output$odds=renderPlot(
      {
        plot(hist(prioronodds()), main="resulting prior on n odds", xlab='number of odd socks', yaxt='n')
        if (1 %in% input$sumprior){
          abline(v=mean(prioronodds()), col='red')
        }
        if (2 %in% input$sumprior){
          abline(v=median(prioronodds()), col='blue')
        }
        if (3 %in% input$sumprior){
          abline(v=quantile(prioronodds(), 0.025), col='green')
          abline(v=quantile(prioronodds(), 0.975), col='green')
        }
      }
    )
    output$priorsocksum=renderPrint(
      {
        j=socks()
        k=c(mean(j), sd(j), quantile(j, 0.25), median(j), quantile(j, 0.75),  quantile(j, 0.025),quantile(j, 0.975))
        names(k)=c("mean", "sd",  "1st quantile", "median", "3rd quantile","0.025 percentile ","0.975 percentile")
        return (k)
      }
    )
    output$priorpropsum=renderPrint(
      {
        
        j=proportion()
        k=c(mean(j), sd(j), quantile(j, 0.25), median(j), quantile(j, 0.75),  quantile(j, 0.025),quantile(j, 0.975))
        names(k)=c("mean", "sd",  "1st quantile", "median", "3rd quantile","0.025 percentile ","0.975 percentile")
        return (k)
        
      }
    )
    output$priorpriorpairsum=renderPrint(
      {
        
        j=prioronpairs()
        k=c(mean(j), sd(j), quantile(j, 0.25), median(j), quantile(j, 0.75),  quantile(j, 0.025),quantile(j, 0.975))
        names(k)=c("mean", "sd",  "1st quantile", "median", "3rd quantile","0.025 percentile ","0.975 percentile")
        return (k)
        
      }
    )
    output$priorprioroddsum=renderPrint(
      {
        j=prioronodds()
        k=c(mean(j), sd(j), quantile(j, 0.25), median(j), quantile(j, 0.75),  quantile(j, 0.025),quantile(j, 0.975))
        names(k)=c("mean", "sd",  "1st quantile", "median", "3rd quantile","0.025 percentile ","0.975 percentile")
        return (k)
        
      }
    )
    output$postsocksum=renderPrint(
      {
        j=generativemodel()$X3
        k=c(mean(j), sd(j), quantile(j, 0.25), median(j), quantile(j, 0.75),  quantile(j, 0.025),quantile(j, 0.975))
        names(k)=c("mean", "sd",  "1st quantile", "median", "3rd quantile","0.025 percentile ","0.975 percentile")
        return (k)
        
      }
    )
    output$postpropsum=renderPrint(
      {
        j=generativemodel()$X6
        k=c(mean(j), sd(j), quantile(j, 0.25), median(j), quantile(j, 0.75),  quantile(j, 0.025),quantile(j, 0.975))
        names(k)=c("mean", "sd",  "1st quantile", "median", "3rd quantile","0.025 percentile ","0.975 percentile")
        return (k)
        
      }
    )
    output$postpairsum=renderPrint(
      {
        j=generativemodel()$X4
        k=c(mean(j), sd(j), quantile(j, 0.25), median(j), quantile(j, 0.75),  quantile(j, 0.025),quantile(j, 0.975))
        names(k)=c("mean", "sd",  "1st quantile", "median", "3rd quantile","0.025 percentile ","0.975 percentile")
        return (k)
        
      }
    )
    output$postoddsum=renderPrint(
      {
        j=generativemodel()$X5
        k=c(mean(j), sd(j), quantile(j, 0.25), median(j), quantile(j, 0.75),  quantile(j, 0.025),quantile(j, 0.975))
        names(k)=c("mean", "sd",  "1st quantile", "median", "3rd quantile","0.025 percentile ","0.975 percentile")
        return (k)
        
      }
    )
    output$answer=renderImage(
      {
        if (input$displayans=="yes"){
          return(list(
            src = "karl_tweet_2.png",
            contentType = "image/png",
            alt="45 socks: 21 pairs 3 odds"
          ))
        }
        else{
          return(list(
            src = "",
            contentType = "image/png",
            alt=""
          ))
        }
      }, deleteFile=FALSE
    )
  }
)