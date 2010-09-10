##=================================================================##
##                 Define all the classes here
##=================================================================##


##-----------------------------------------------------------------##
##                Class Union Used
##-----------------------------------------------------------------##

setClassUnion('charOrNULL',c('character','NULL'))
setClassUnion('numOrNULL',c('numeric','NULL'))


##-----------------------------------------------------------------##
##                Universal Graphics Aesthetic
##-----------------------------------------------------------------##

setClass('QtGraphicPars',representation(pars='environment'),
         prototype(pars=new.env(parent=emptyenv())))

##-----------------------------------------------------------------##
##              classes used for biomart
##-----------------------------------------------------------------##

setClass('BiomartQuery',representation(mart='character',
                                    dataset='character',
                                    attributes='vector',
                                    filters='vector',
                                    values='list'
                                    ),
         prototype(mart='ensembl',
                   dataset='hsapiens_gene_ensembl',
                   attributes= c('chromosome_name',
                     'ensembl_gene_id',
                     'ensembl_transcript_id',
                     'ensembl_exon_id',
                     'exon_chrom_start',
                     'exon_chrom_end',
                     'rank',
                     'strand',
                     'transcript_start',
                     'transcript_end'),
                   filters=c('ensembl_gene_id'),
                   values=list('ENSG00000095203')
)
                   )


##-----------------------------------------------------------------##
##          Classes used in birdeye overview graphics                        
##-----------------------------------------------------------------##

setClass('ChrInfo',representation(species='character',
                                  cytobands='logical',
                                  chr='charOrNULL',
                                  start='numOrNULL',
                                  end='numOrNULL',
                                  stain='charOrNULL',
                                  arm='charOrNULL'),
         prototype(species='hsapiens_gene_ensembl',
                   cytobands=TRUE,
                   chr=NULL,
                   start=NULL,
                   end=NULL,
                   stain=NULL,
                   arm=NULL))

setClass('ChrInfoList',representation('list'))

##-----------------------------------------------------------------##
##          Classes used in Linear View                        
##-----------------------------------------------------------------##

setClass('BirdEyeObj',contain=c('QtGraphicPars','ChrInfo','BiomartQuery'))

##-----------------------------------------------------------------##
##          Classes used in Circular View                        
##-----------------------------------------------------------------##

setClass('BirdEyeObjCircle',contain=c('QtGraphicPars','ChrInfo'))

##-----------------------------------------------------------------##
##          Track object                        
##-----------------------------------------------------------------##



##-----------------------------------------------------------------##
##           Classes used in Zoomable Viewer
##-----------------------------------------------------------------##
setClass('ZoomableViewBM',representation(components='list',
                                      region_start='numOrNULL',
                                      region_end='numOrNULL'),
         prototype(components=list('transcripts','exons'),
                   region_start=NULL,
                   region_end=NULL),
         contain=c('QtGraphicPars','ChrInfo','BiomartQuery'))

setClass('ZoomableViewDb',representation(components='list',
                                      region_start='numOrNULL',
                                      region_end='numOrNULL'),
         prototype(components=list('transcripts','exons'),
                   region_start=NULL,
                   region_end=NULL),
         contain=c('QtGraphicPars','ChrInfo'))


##----------------------------------------------------------------##
##               "diagScatter"
##----------------------------------------------------------------##
setOldClass('mutaframe')
setClass('diagScatter',representation(mutaframe='mutaframe'),
         contain=c('QtGraphicPars')
         )





