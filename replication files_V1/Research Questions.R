get_pvalue <-function(m1, se1, m2, se2){
  M<-100000
  sim1 <- rnorm(M,mean=m1,sd=se1)
  sim2 <- rnorm(M,mean=m2,sd=se2)
  return (min(2*(1-sum(sim1 > sim2)/M), 2*(1-sum(sim1 < sim2)/M)))
}
get_abs_pvalue <-function(m1, se1, m2, se2){
  M<-100000
  sim1 <- rnorm(M,mean=m1,sd=se1)
  sim2 <- rnorm(M,mean=m2,sd=se2)
  return (min(2*(1-sum(abs(sim1) > abs(sim2))/M), 2*(1-sum(abs(sim1) < abs(sim2))/M)))
}
#results in main texts
#RQ1
#RQ1a outlets
#Forced response
get_abs_pvalue(-0.101486106,0.010123162,0.007448326,0.009925174)
#rated response
get_abs_pvalue(-0.4585347931,0.04074106,0.0005717667,0.03809274 )

#RQ1b messages
#Forced Response
get_abs_pvalue(0.030843098,0.010114340,-0.121567941,0.010125989)
#rated response
get_abs_pvalue(-0.4198352877,0.04041170, 0.2098008880,0.03767195)

#RQ2
#avoidance 
#forced response
get_pvalue(-0.101486106,0.010123162,-0.121567941,0.010125989)
#rated response
get_pvalue(-0.4585347931, 0.04074106,-0.4198352877, 0.04041170)
#selection
#forced response
get_pvalue(0.007448326, 0.009925174, 0.030843098, 0.010114340)
#rated response
get_pvalue(0.0005717667, 0.03809274, 0.2098008880, 0.03767195)

#Robustness 1: deduplicated responses
#RQ1
#RQ1a outlets
#Forced response
get_abs_pvalue(-0.101558418,0.011049198,0.010411799,0.010723900)
#rated response
get_abs_pvalue(-0.465415613,0.04402258,0.008843235,0.04143241)

#RQ1b messages
#Forced Response
get_abs_pvalue(-0.124664867,0.011016245,0.030021576 ,0.010942811)
#rated response
get_abs_pvalue(-0.425291109, 0.04423662, 0.207286183, 0.04081570 )

#RQ2
#avoidance 
#forced response
get_pvalue(-0.101558418, 0.011049198,-0.124664867, 0.011016245)
#rated response
get_pvalue(-0.465415613, 0.04402258, -0.425291109, 0.04423662)
#selection
#forced response
get_pvalue(0.010411799, 0.010723900,0.030021576, 0.010942811)
#rated response
get_pvalue(0.008843235, 0.04143241,0.207286183, 0.04081570)

#Robustness 2: valid responses
#RQ1
#RQ1a outlets
#Forced response
get_abs_pvalue(-0.1062600305, 0.010501391,0.0035434827,0.010284720)
#rated response
get_abs_pvalue(-0.49845237,0.04260843, 0.01014350 ,0.03972628)

#RQ1b messages
#Forced Response
get_abs_pvalue(-0.1267193190, 0.010575634,0.0268176018, 0.010570097)
#rated response
get_abs_pvalue(-0.46064177, 0.04249192, 0.22120302, 0.03957874 )

#RQ2
#avoidance 
#forced response
get_pvalue(-0.1062600305, 0.010501391,-0.1267193190, 0.010575634)
#rated response
get_pvalue(-0.49845237, 0.04260843, -0.46064177, 0.04249192)
#selection
#forced response
get_pvalue(0.0035434827, 0.010284720,0.0268176018, 0.010570097)
#rated response
get_pvalue(0.01014350, 0.03972628,0.22120302, 0.03957874)


#Robustness 3-1: Democrats responses
#RQ1
#RQ1a outlets
#Forced response
get_abs_pvalue(-0.148710268,0.01318629, 0.004332170, 0.01282034 )
#rated response
get_abs_pvalue(-0.66069708,0.05485603, -0.01303814, 0.04686911)

#RQ1b messages
#Forced Response
get_abs_pvalue(-0.125574007,0.01341820, 0.032327119, 0.01316751 )
#rated response
get_abs_pvalue(-0.48236660, 0.05331166, 0.20476581, 0.04801291)

#RQ2
#avoidance 
#forced response
get_pvalue(-0.148710268, 0.01318629, -0.125574007, 0.01341820)
#rated response
get_pvalue(-0.66069708 ,0.05485603, -0.48236660, 0.05331166)
#selection
#forced response
get_pvalue(0.032327119, 0.01316751,0.004332170, 0.01282034)
#rated response
get_pvalue(0.20476581, 0.04801291, -0.01303814, 0.04686911)


#Robustness 3-2: Republican responses
#RQ1
#RQ1a outlets
#Forced response
get_abs_pvalue(-0.031131442,0.01539395,0.013622371,0.01564039 )
#rated response
get_abs_pvalue(-0.15736091 ,0.05741900,0.01309339 ,0.06359435  )

#RQ1b messages
#Forced Response
get_abs_pvalue(-0.116018796, 0.01535352, 0.026895487, 0.01578662 )
#rated response
get_abs_pvalue(-0.32264281, 0.06005643,0.21592406, 0.05969405)

#RQ2
#avoidance 
#forced response
get_pvalue(-0.031131442, 0.01539395,-0.116018796, 0.01535352 )
#rated response
get_pvalue(0.026895487,0.01578662,0.013622371 ,0.01564039)
#selection
#forced response
get_pvalue(-0.15736091, 0.05741900,-0.32264281 ,0.06005643)
#rated response
get_pvalue(0.21592406, 0.05969405,0.01309339, 0.06359435)
