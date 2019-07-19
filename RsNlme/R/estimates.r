
#' @import shiny
#' @import ggplot2
#' @import graphics
NULL

#'
#'@export
#'
generateInitialEstimatesInputAscii <-function(fileName,thetas,variables,sweepStart,sweepLength,numSweepSteps){


  outFile=fileName
  cat(length(thetas),file=outFile,append=FALSE,sep="\n")

  tnames=names(thetas)
  for ( n in tnames ) {

    cat(n,file=outFile,append=TRUE,sep="\n")
    cat(thetas[n],file=outFile,append=TRUE,sep="\n")
  }
  cat(as.integer(length(variables)),file=outFile,append=TRUE,sep="\n")

  for ( v in variables ) {
    cat(v,file=outFile,append=TRUE,sep="\n")
  }
  num=numSweepSteps + 1

  cat(as.integer(num),file=outFile,append=TRUE,sep="\n")

  sweepStep = sweepLength / numSweepSteps

  for ( inx in 0:numSweepSteps ) {
    cat(as.numeric(sweepStart + inx * sweepStep) , file=outFile,append=TRUE,sep="\n")
    class((sweepStart + inx * sweepStep))
  }
  n=999999
  cat(as.integer(n),file=outFile,append=TRUE,sep="\n")

}

readInitialEstimatesParams <-function(fileName){
  inputFile = file(fileName,"rb")

  numThetas = readBin(inputFile,integer(),n=1)
  print(paste0("numThetas ",numThetas))
  for ( n in 1:numThetas ) {
    name=readBin(inputFile,character(),n=1)
    value=readBin(inputFile,numeric(),n=1)
    print(paste0(name," ",value))

  }
  numVars = readBin(inputFile,integer(),n=1)
  print(paste0("numVariabels ",numVars))
  if ( numVars > 0 )
    for ( n in 1:numVars ) {
      name=readBin(inputFile,character(),n=1)
      print(name)
    }
  numSteps = readBin(inputFile,integer(),n=1)
  print(numSteps)



  steps=readBin(inputFile,numeric(),n=numSteps)

  print(steps)
  end=readBin(inputFile,integer(),n=1)
  print(paste0("end marker", end))


}

#'
#'@export
#'
generateInitialEstimatesInput <-function(fileName,thetas,variables,sweepStart,sweepLength,numSweepSteps){


  outFile=file(paste0(getwd(),"/",fileName),"wb")
  writeBin(as.integer(length(thetas)),outFile)
  tnames=names(thetas)
  for ( n in tnames ) {
    l=nchar(n)
    writeBin(l,outFile,size=1)

    writeChar(n,outFile,nchars=l)

    #    writeBin(n,outFile)

    writeBin(as.numeric(thetas[n]), outFile)
  }
  writeBin(as.integer(length(variables)),outFile)

  for ( v in variables ) {
    #    writeBin(v,outFile)
    l=nchar(v)
    writeBin(l,outFile,size=1)
    writeChar(v,outFile,nchars=l)
  }
  num=numSweepSteps + 1

  writeBin(as.integer(num),outFile)
  print(paste0("numSweepSteps ",num))
  sweepStep = sweepLength / numSweepSteps

  for ( inx in 0:numSweepSteps ) {
    writeBin(as.numeric(sweepStart + inx * sweepStep) , outFile)
    class((sweepStart + inx * sweepStep))
  }
  n=999999
  writeBin(as.integer(n),outFile)
  close(outFile)
}

#'
#'@export
#'

generateGraph <-function(workingDir,inputFile,outputFile){


  cwd=getwd()
  tryCatch(
  {
  setwd(workingDir)
  args = paste0("/plotinascii ",  inputFile, " /plotout ", outputFile,
                " /m 3 ",
                " /n 10 ",
                " /e -1 ",
                " /o 6",
                " /csv ",
                " /sort ",
                " cols1.txt ",
                " data1.txt ",
                " out.txt")


  cmd=paste0(" NLME7.exe ",args)
  print(cmd)
print(getwd())
  shell(cmd, wait = TRUE)
print("-----")
  setwd(cwd)
print(getwd())
  },
  error = function(ex) {
    setwd(cwd)
  } )
}


#'
#'@export
#'
readModelData <-function(fileName) {



  out=list()
  inputFile=file(fileName,"rb")
  sig=readBin(inputFile,integer(),n=1)
  tBased=readBin(inputFile,"integer",n=1)
  nThetas=readBin(inputFile,"integer",n=1)
  out$tBased = tBased
  out$nThetas = nThetas
  out$thetas= list()
  for ( n in 1:nThetas ){
    l=readBin(inputFile,"raw",size=1)
    theta1=readChar(inputFile,nchars=as.integer(l))
    value=readBin(inputFile,"double")
    theta=c(name=theta1,value=value)
    out$thetas[[n]] = theta
  }
  nVars=readBin(inputFile,"integer",n=1)
  out$nVars = nVars
  out$vars=list()
  for ( n in 1:nVars ) {
    l=readBin(inputFile,"raw",size=1)
    var=readChar(inputFile,nchars=as.integer(l))
    flag=readBin(inputFile,"integer",n=1)
    out$vars=c(out$vars,list(name=var,flag=flag))

  }
  nSubjects=readBin(inputFile,"integer",n=1)
  out$nSubjects=nSubjects


  nVarsReturned=readBin(inputFile,"integer",n=1)
  out$nVarsReturned = nVarsReturned
  out$varsReturned = list()
  for ( n in 1:nVarsReturned ) {
    l=readBin(inputFile,"raw",size=1)
    varName=readChar(inputFile,nchars=as.integer(l))
    isAvail=readBin(inputFile,"integer",n=1)
    sweep=readBin(inputFile,"integer",n=1)
    out$varsReturned=c(out$varsReturned,list(varName=varName,isAvail=isAvail,sweep=sweep))

  }
  out$subjects=list()
  for ( n in 1:nSubjects ) {
    subject=list()

    for ( m in 1:5 ) {
      l=readBin(inputFile,"raw",size=1)
      if ( l > 0 ) {
        id=readChar(inputFile,nchars=as.integer(l))
        subject$id = id
      }
      else
        id=""
    }
    subject$observations = list()
    for ( r in 1:out$nVarsReturned ){
      obsValues = list()

      nObs = readBin(inputFile,"integer",n=1)
      obsValues$nObs = nObs
      obsValues$obsTimes = list()
      obsValues$obsValues= list()

      for ( o in 1:nObs){
        t=readBin(inputFile,"double",n=1)
        v=readBin(inputFile,"double",n=1)
        obsValues$obsTimes= c(obsValues$obsTimes,t)
        obsValues$obsValues= c(obsValues$obsValues,v)

      }


      nVal = readBin(inputFile,"integer",n=1)

      num = as.integer( 2 * nVal )
      vals=readBin(inputFile,"double",n= num )

      m=matrix(vals,ncol=2, byrow=TRUE)
      x=m[,1]
      y=m[,2]

      obsValues$nVal = nVal ;
      obsValues$x = x
      obsValues$y = y


      subject$observations[[r]] = obsValues

    }
    end=readBin(inputFile,"integer",n=1)
    stopifnot ( end == 9999 )

    out$subjects[[n]] = subject

  }
  close(inputFile)
  return( out )
}



#'
#'@export
#'
compileModel <-function(model,host){

  installDir=host@installationDirectory
  modelDir= model@modelInfo@workingDir

  script=paste0(installDir,"/","execNlmeCmd.bat")


  Sys.setenv("INSTALLDIR"=installDir)
  if ( attr(host,"hostType")== "Windows" )
      args=paste0(" COMPILE test.mdl " , gsub("/","\\",modelDir,fixed=TRUE)  , 
                  " MPINO NO 1" )
  else
      args=paste0(" COMPILE test.mdl " , modelDir , " MPINO NO 1" )
  shell(paste0(script," ",args))
}




#'
#'estimatesUI
#'
#'User Interface to examine the model and evaluate estimates for fixed effects
#'
#'@param model        PK/PD model
#'@param subjectIds   Subject IDs
#'@param host         Optional host parameter if model needs to be compiled
#'@param dataset      Optional dataset parameter
#'
#'@examples
#'    host = NlmeParallelHost(sharedDirectory=Sys.getenv("NLME_ROOT_DIRECTORY"),
#'                            parallelMethod=NlmeParallelMethod("LOCAL_MPI"),
#'                            hostName="MPI",
#'                            numCores=4)
#'    input=read.csv("D:/SDCard/NlmeInstall_04_30_18/Pml/pk01cov3.csv")
#'    model = pkmodel(numComp=1,
#'                    isPopulation=TRUE,
#'                    absorption = Intravenous,
#'                    parameterization = Clearance,
#'                    modelName="PK01Model",
#'                    isTlag = FALSE,
#'                    hasEliminationComp = FALSE,
#'                    isClosedForm = TRUE)
#'
#'    dataset=NlmeDataset(model@modelInfo@workingDir)
#'    initColMapping(model)=input
#'    modelColumnMapping(model)=c(ID="xid", CObs="conc",A1="dose")
#'    writeDefaultFiles(model,dataset)
#'    estimatesUI(model,unique(input$id),host)
#'
#'    estimates = getInitialEstimates(model)
#'    print(estimates)
#'    initFixedEffects(model) = estimates
#'
#'@export
#'

estimatesUI <-function(model,subjectIds,host=NULL,dataset=NULL) {

require(shiny)

  if (  is.null(dataset) )
      dataset=model@dataset
  if ( ! is.null(dataset) )
      writeDefaultFiles(model,dataset)

  if ( ! is.null(host ) ) {
    compileModel(model,host)
  }
#  thetas=initFixedEffects(model)
  thetas=getThetas(model)
  numThetas   = length(thetas)
  numSubjects = length(subjectIds)
  observationNames=c()
  if ( model@isTextual == FALSE ) {
    numObservations = length(model@errorModel@effectsList)

    for ( n in 1:numObservations )
      observationNames = c(observationNames,model@errorModel@effectsList[[n]]@effectName)
  } else {

    observationNames = observationNames(model)
    numObservations = length(observationNames)
  }
  plotVariables=observationNames

  modelVar = reactiveValues()

  modelVar = model

shinyApp(
ui = fluidPage(
  sidebarLayout(
    sidebarPanel(

      fluidRow(
        column(width = 6,
          uiOutput("subject")
        ),
        column(width = 6,
               uiOutput("observation")
        ),
        column(width = 6,
          checkboxInput("overlay","Overlay",value=FALSE)
      ),
      column( width = 6,
          checkboxInput("log","Log",value=FALSE)
      ),
      column( width = 12 ,
      sliderInput(inputId="starttime",label="Start Time",min=0,max=100,value=0),
      sliderInput(inputId="duration",label="Duration",min=1,max=100,value=25),

      #Place holders for dynamically created elements
      shiny::uiOutput("theta1"),
      shiny::uiOutput("theta2"),
      shiny::uiOutput("theta3"),
      shiny::uiOutput("theta4"),
      shiny::uiOutput("theta5"),
      shiny::uiOutput("theta6"),
      shiny::uiOutput("theta7"),
      shiny::uiOutput("theta8"),
      shiny::uiOutput("theta9")
    ))

    ),
    mainPanel(
      plotOutput("plot")
    )
  )
)
,

server = function(input, output,session) {


  sliderMaxes <- reactiveValues()


  modelData <- eventReactive(c(input$Observation,input$starttime,input$duration,input$theta1,input$theta2,input$theta3,input$theta4,input$theta5,input$theta6,input$theta7,input$theta8,input$theta9), {
    sweepStart=input$starttime
    sweepLength=input$duration
    numSweepSteps=100

    tnames=names(thetas)
    values=c()

    for ( t in 1:length(tnames ) ) {
      val = switch( t, input$theta1,input$theta2,input$theta3,input$theta4,input$theta5,input$theta6,input$theta7,input$theta8,input$theta9)
      max = 0
      if ( !is.null(sliderMaxes) )
        if ( length(sliderMaxes ) > 0 )
          max = isolate(sliderMaxes[[paste0("theta",t)]])


      if (  ( val >= (max - 0.1 )  ) ){

        updateSliderInput(session,paste0("theta",t),
                          max=(val * 1.5 ))
        sliderMaxes[[paste0("theta",t)]] = val * 1.5
      }
      values=c(values, val)
    }
    names(values)=tnames
    ts=values

    plotVariables = c(input$Observation)

    workingDir = modelVar@modelInfo@workingDir
print("-------------------")
print(workingDir)
    generateInitialEstimatesInputAscii(paste0(workingDir,"/params.txt"),ts,plotVariables,sweepStart,sweepLength,numSweepSteps)
    generateGraph(workingDir,"params.txt","output.bin")

    out=readModelData(paste0(workingDir,"/output.bin"))
    out
  } )

  output$plot <- renderPlot({

    out = modelData()
    lastSubject =input$Subject
    assign("lastSubject",lastSubject,envir=.GlobalEnv)
    sIndx=as.integer(input$Subject)


    obs=input$Observation
    oIndx = 1
    for ( indx in 1:length(observationNames)) {
      if ( obs == observationNames[[indx]])
        oIndx = indx
    }

    x=out$subjects[[sIndx]]$observations[[1]]$x
    y=out$subjects[[sIndx]]$observations[[1]]$y
    xs=list()
    ys=list()
    for ( indx in 1:out$nSubjects ) {
      xs[[indx]] = out$subjects[[indx]]$observations[[1]]$x
      ys[[indx]] = out$subjects[[indx]]$observations[[1]]$y

    }

    px=as.numeric(out$subjects[[sIndx]]$observations[[1]]$obsTimes)
    py=as.numeric(out$subjects[[sIndx]]$observations[[1]]$obsValues)
    pxs=c()
    pys=c()
    for ( indx in 1:out$nSubjects ) {
      xxx=as.numeric(out$subjects[[indx]]$observations[[1]]$obsTimes)

      yyy=as.numeric(out$subjects[[indx]]$observations[[1]]$obsValues)

      pxs=c(pxs,xxx)
      pys=c(pys,yyy)
    }

      xlim=c(max(min(c(x,as.numeric(pxs))),0.0001),max(c(x,as.numeric(pxs))))
      ylim=c(max(min(c(y,as.numeric(pys))),0.0001),max(c(y,as.numeric(pys))))


    if ( input$overlay == FALSE  ) {

      if ( input$log )
        plot(x,y, col="blue",type="l",log="y",xlim=xlim,ylim=ylim,ylab="",xlab="")
      else
        plot(x,y, col="blue",type="l",xlim=xlim,ylim=ylim,ylab="",xlab="")

    }
    else {

      for( indx in 1:out$nSubjects ) {
        if ( input$log )
          plot(xs[[indx]],ys[[indx]], col="blue",type="l",log = "y",xlim=xlim,ylim=ylim,ylab="",xlab="")
        else {
          if ( indx == 1 )
          plot(xs[[indx]],ys[[indx]], col="blue",type="l",xlim=xlim,ylim=ylim,ylab="",xlab="")
            else
        lines(xs[[indx]],ys[[indx]],col="blue")
      }
      }
    }

    if ( input$overlay == FALSE  ) {
      points(px,py,col="red")
      title(main = paste0("ID ",sIndx))
    }
    else{

      points(pxs,pys,col="red")
      title(main = paste0("All"))
    }


  })

  tnames=names(thetas)
  if ( length(thetas) > 0 ) {
    name1=tnames[1]
    val1=as.numeric(thetas[[1]])
    sliderMaxes$theta1 = max(as.numeric(val1) * 1.5,10.0)
    output$theta1 = shiny::renderUI({

      sliderInput(inputId="theta1",label=name1,min=0.01,max=max(as.numeric(val1) * 1.5,10.0),
                  value=val1,step=0.1,round=FALSE)
    })
  }

  if ( length(thetas) > 1 ) {
    name2=tnames[2]
    val2=as.numeric(thetas[[2]])
    sliderMaxes$theta2 = max(as.numeric(val2) * 1.5,10.0)
    output$theta2 = shiny::renderUI({
      sliderInput(inputId="theta2",label=name2,min=0.01,max=max(as.numeric(val2) * 1.5,10.0),
                  value=val2,step=0.1,round=FALSE)
    })
  }
  if ( length(thetas) > 2 ) {
    name3=tnames[3]
    val3=as.numeric(thetas[[3]])
    sliderMaxes$theta3 = max(as.numeric(val3) * 1.5,10.0)
    output$theta3 = shiny::renderUI({

      sliderInput(inputId="theta3",label=name3,min=0.01,max(as.numeric(val3) * 1.5,10.0),
                  value=val3,step=0.1,round=FALSE)
    })
  }
  if ( length(thetas) > 3 ) {

    name4=tnames[4]
    val4=as.numeric(thetas[[4]])
    sliderMaxes$theta4 = max(as.numeric(val4) * 1.5,10.0)
    output$theta4 = shiny::renderUI({

      sliderInput(inputId="theta4",label=name4,min=0.01,max(as.numeric(val4) * 1.5,10.0),
                  value=val4,step=0.1,round=FALSE)
    })
  }
  if ( length(thetas) > 4 ) {

    name5=tnames[5]
    val5=as.numeric(thetas[[5]])
    sliderMaxes$theta5 = max(as.numeric(val5) * 1.5,10.0)
    output$theta5 = shiny::renderUI({

      sliderInput(inputId="theta5",label=name5,min=0.01,max=max(as.numeric(val5) * 1.5,10.0),
                  value=val5,step=0.1,round=FALSE)
    })
  }
  if ( length(thetas) > 5 ) {
    name6=tnames[6]
    val6=as.numeric(thetas[[6]])
    sliderMaxes$theta6 = max(as.numeric(val6) * 1.5,10.0)
    output$theta6 = shiny::renderUI({

      sliderInput(inputId="theta6",label=name6,min=0.01,max=max(as.numeric(val6) * 1.5,10.0),
                  value=val6,step=0.1,round=FALSE)
    })
  }
  if ( length(thetas) > 6 ) {
    name7=tnames[7]
    val7=as.numeric(thetas[[7]])
    sliderMaxes$theta7 = max(as.numeric(val7) * 1.5,10.0)
    output$theta7 = shiny::renderUI({

      sliderInput(inputId="theta7",label=name7,min=0.01,max=max(as.numeric(val7) * 1.5,10.0),
                  value=val7,step=0.1,round=FALSE)
    })
  }
  if ( length(thetas) > 7 ) {
    name8=tnames[8]
    val8=as.numeric(thetas[[8]])
    sliderMaxes$theta8 = max(as.numeric(val8) * 1.5,10.0)
    output$theta8 = shiny::renderUI({

      sliderInput(inputId="theta8",label=name8,min=0.01,max=max(as.numeric(val8) * 1.5,10.0),
                  value=val8,step=0.1,round=FALSE)
    })
  }
  if ( length(thetas) > 8 ) {
    name9=tnames[9]
    val9=as.numeric(thetas[[9]])
    sliderMaxes$theta9 = max(as.numeric(val9) * 1.5,10.0)
    output$theta9 = shiny::renderUI({

      sliderInput(inputId="theta9",label=name9,min=0.01,max=max(as.numeric(val9) * 1.5,10.0),
                  value=val9,step=0.1,round=FALSE)
    })
  }

  output$subject = shiny::renderUI({
    #nSubjects = out$nSubjects
    nSubjects = numSubjects
    subjects = subjectIds

    selectInput("Subject", label ="Subject",
                choices = subjects,
                selected = 1)

  })

  output$observation = shiny::renderUI({

    nObservations = numObservations
    observations = observationNames

    selectInput("Observation", label ="Observation",
                choices = observations,
                selected = 1)

  })
}
)
}

#' getInitialEsimates
#'
#' Returns values from initial estimates shiny App.
#' Returned value can be used to set initial estimates in RsNlme
#'
#'@examples
#'      ...
#'      estimatesUI(model,unique(input$ID),host)
#'
#'      estimates = getInitialEstimates(model)
#'
#'      initFixedEffects(model) = estimates
#'
#'@export
#'
getInitialEstimates <-function(model){


  # Read latest parameters
  lines= readLines(paste0(model@modelInfo@workingDir,"/params.txt"))
  effects=list()
  numEffects=as.numeric(lines[[1]])
  for ( i in 1:numEffects ) {
    name =  trimws(lines[[ ( i -1 ) * 2 + 2]],"both")
    val = as.numeric(lines[[ ( i -1 ) * 2 + 3]])
    effects[[name]]=val
  }

  effects
}

assign("getInitialEstimates",getInitialEstimates,envir=.GlobalEnv)

