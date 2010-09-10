##-----------------------------------------------------------------##
##                  For class 'QtGraphicPars'
##-----------------------------------------------------------------##

setGeneric('setPar',function(obj,...) standardGeneric('setPar'))
setMethod('setPar','QtGraphicPars',
          function(obj,name,value){
            assign(name,value,obj@pars)
          })

setGeneric('getPar',function(obj,...) standardGeneric('getPar'))
setMethod('getPar','QtGraphicPars',
          function(obj,name){
            if(!exists(name,obj@pars))
              stop(paste('No graphic parameter named',name,'could be found!'))
            get(name,obj@pars)
          })


##-----------------------------------------------------------------##
##                 For class 'BiomartQuery'
##-----------------------------------------------------------------##
setGeneric('getBMQ',function(obj,...) standardGeneric('getBMQ'))
setMethod('getBMQ','BiomartQuery',function(obj,...){
  mart <- useMart(obj@mart,dataset=obj@dataset)
  getBM(obj@attributes,obj@filters,obj@values,mart=mart,...)
})







