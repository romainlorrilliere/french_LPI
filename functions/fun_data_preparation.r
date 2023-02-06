### functions to prepare data from several source to LPI assessment ###


fbbs2lpi <- function(d,file.write = "data/data_stoc_2019_LPI_pops.txt",output=FALSE,create_infile = TRUE,fromRaw= TRUE) {
    require(data.table)

    setDT(d)
    if(fromRaw) {
        d[,`:=`(binomial = gsub(" ","_",scientific_name),
                ID = paste0(carre,sprintf("%06d", taxref)),
                year = annee,
                popvalue = abondance)]
    }
    d[,nb_sample := .N,by= ID]
    d[,sum_ab := sum(popvalue),by= ID]
    dd <- d[nb_sample > 1 & sum_ab > 0,.(binomial,ID,year,popvalue)]

    if(!is.null(file.write)) write.table(dd,file.write,sep="\t",row.names=FALSE,quote=FALSE)
    if(create_infile) {

        d_infile <- data.frame(FileName = file.write, Group = 1, Weighting = 1)
        file_infile <- gsub("pops","infile",file.write)

        write.table(d_infile,file_infile,row.names=FALSE,sep="\t")
    }

    if(output) return(dd)



}
