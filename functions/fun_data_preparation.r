### functions to prepare data from several source to LPI assessment ###


fbbs2lpi <- function(d,file.write = "data/data_stoc_2019_LPI_pops.txt",output=FALSE) {
    require(data.table)

    setDT(d)

    d[,`:=`(binomial = gsub(" ","_",scientific_name),
            ID = as.numeric(paste0(carre,sprintf("%06d", taxref))),
            year = annee,
            popvalue = abondance)]
    dd <- d[!is.na(ID),.(binomial,ID,year,popvalue)]

    if(!is.null(file.write)) write.table(dd,file.write,sep="\t",row.names=FALSE,quote=FALSE)
    if(output) return(dd)



}
