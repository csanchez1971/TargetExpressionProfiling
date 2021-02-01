
library(shiny)

shinyServer(function(session, input, output) {

    observe({
        x <- input$ZScore_cutoff
        updateRadioButtons(session, "ZScore_cutoff2", selected = x)
    })
    
    observe({
        y <- input$ZScore_cutoff2
        updateRadioButtons(session, "ZScore_cutoff", selected = y)
    })
    
    observe({
        updateSelectizeInput(session = session, inputId = 'geneName', choices = c("Select Gene", sort(geneID$SYMBOL)), options = list(maxOptions = 100), server = TRUE)
    })

    

# -------------- Single DB creation -----------
    
    # -------- GTEx --------

    E_gene1 <- eventReactive(input$Submit, {
        
        genIDs <- gene_description_function(input_gene = input$geneName)
        
        ensembl <- geneID$ENSEMBL[geneID$SYMBOL %in% genIDs]
        
        GTEx_final <- GTEx_v8 %>%
            dplyr::filter(rownames(GTEx_v8) %in% ensembl)
        
        
        id <- which.max(rowMeans(GTEx_final))
        
        gene_name_selected <- geneID$SYMBOL[geneID$ENSEMBL == rownames(GTEx_final)[id]]   #Copy the gene name used to an external variable to be used on DB_Summary. Useful in the
        
        if (dim(GTEx_final)[1] == 0) {
            E_gene1 <- data.frame(tpm = numeric(0), tissue = character(0))
            
            
        } else {
            #Select the row with highest expression mean in case there are several register
            #case that the selected row belongs to an alias name instead the original (e.g. CCN2 and CTGF)
            
            GTEx_final <- as.data.frame(t(GTEx_final[id, ]))
            
            
            row.names(GTEx_final) <- gsub("\\.", "-", row.names(GTEx_final))
            GTEx_final$SAMPID <- row.names(GTEx_final)
            E_gene1 <- merge(GTEx_final, samp, by = 'SAMPID')
            E_gene1 <- E_gene1[, c(colnames(E_gene1)[2], "SMTSD")]
            colnames(E_gene1) <- c("tpm", "tissue")
            
            E_gene1 <- merge(E_gene1,
                             Atlas_Dictionary_final,
                             by.x = "tissue",
                             by.y = "GTEx.Carlos")
            
            
            E_gene1 <- dplyr::select(E_gene1, tpm, Master, Tissue_type, level1)
            
            if(nrow(E_gene1)>0){E_gene1$gene <- gene_name_selected}    
            
            E_gene1 <- E_gene(E_gene1, input$grouped_tissue)      #Replaced logic for selecting grouped or individual tissues (below) for a function used on every DB
            return(E_gene1)
        }
    })
    
    # -------- HPA --------
    
    
    
    E_gene2 <- eventReactive(input$Submit, {
        genIDs <- gene_description_function(input_gene = input$geneName)
        
        ensembl <- geneID$ENSEMBL[geneID$SYMBOL %in% genIDs]
        
        HPA_DB <- HPA_v19_3 %>%
            dplyr::filter(ensgid %in% ensembl) %>%
            dplyr::select(-enstid)
        HPA_DB <- HPA_DB %>% dplyr::group_by(ensgid) %>% summarise_all(list(sum))
        
        id <- which.max(apply(HPA_DB[, 2:ncol(HPA_DB)], 1, mean))
        gene_name_selected <- geneID$SYMBOL[geneID$ENSEMBL == HPA_DB$ensgid[id]]
        
        HPA_DB <- HPA_DB[id, -1]
        
        if (dim(HPA_DB)[1] == 0) {
            
            E_gene2 <- data.frame(tpm = numeric(0), tissue = character(0))
            
        } else {
            
            
            colnames(HPA_DB) <- gsub('(\\.V).*|(\\.a$)|(\\.b$)|(\\.c$)|(\\.d$)|(\\.e$)|(\\.f$)', "", colnames(HPA_DB))
            
            E_gene2 <- setNames(data.frame(t(HPA_DB), grp = colnames(HPA_DB)), c("tpm", "tissue"))
            rownames(E_gene2) <- NULL
            
            E_gene2 <- merge(E_gene2,
                             Atlas_Dictionary_final,
                             by.x = "tissue",
                             by.y = "HPA")
            
            
            E_gene2 <- dplyr::select(E_gene2, tpm, Master, Tissue_type, level1)
            
            if(nrow(E_gene2)>0){E_gene2$gene <- gene_name_selected}    
            E_gene(E_gene2, input$grouped_tissue) 
            
        }
    })
    
    
    
    # -------- BarkBase --------
    
    
    E_gene3 <- eventReactive(input$Submit, {
        
        genIDs <- gene_description_function(input_gene = input$geneName)
        
        BB_gene <- BarkBase %>%
            dplyr::filter(GeneSymbol %in% genIDs) %>%
            dplyr::select(-c(EnsemblGeneID))
        
        id <- which.max(rowMeans(BB_gene[-1]))
        gene_name_selected <- BB_gene$GeneSymbol[id]
        
        BB_gene <- BB_gene[id,-1]
        
        colnames(BB_gene) <- gsub(pattern = ".*_", replacement = "", colnames(BB_gene))
        
        if (dim(BB_gene)[1] == 0) {
            E_gene3 <- data.frame(CPM = numeric(0), tissue = character(0))
            
        } else {
            E_gene3 <- setNames(data.frame(t(BB_gene), grp = colnames(BB_gene)), c("CPM", "tissue"))
            
            rownames(E_gene3) <- NULL
            
            E_gene3 <- merge(E_gene3,
                             Atlas_Dictionary_final,
                             by.x = "tissue",
                             by.y = "BarkBase")
            
            E_gene3 <- dplyr::select(E_gene3, CPM, Master, Tissue_type, level1)
            if(nrow(E_gene3)>0){E_gene3$gene <- gene_name_selected}    
            E_gene(E_gene3, input$grouped_tissue) 
            
        }
        
    })
    
    
    
    # -------- NHPRTR cyno Chinese --------
    
    
    E_gene4 <- eventReactive(input$Submit, {
        genIDs <- gene_description_function(input_gene = input$geneName)
        
        NHPRTR_CMC2 <- NHPRTR_CMC %>%
            dplyr::filter(Gene %in% genIDs) 
        
        id <- which.max(rowMeans(NHPRTR_CMC2[-1]))
        
        gene_name_selected <- NHPRTR_CMC2$Gene[id]
        
        NHPRTR_CMC2 <- as.data.frame(NHPRTR_CMC2[id, -1])  #Remove gene column
        
        colnames(NHPRTR_CMC2) <- gsub(pattern = "(\\.).*", replacement = "", colnames(NHPRTR_CMC2))
        
        if (dim(NHPRTR_CMC2)[1] == 0) {
            E_gene4 <- data.frame(FPKM = numeric(0), tissue = character(0))
            
        } else {
            E_gene4 <- setNames(data.frame(t(NHPRTR_CMC2), grp = colnames(NHPRTR_CMC2)), c("FPKM", "tissue"))
            
            
            rownames(E_gene4) <- NULL
            E_gene4 <- merge(E_gene4,
                             Atlas_Dictionary_final,
                             by.x = "tissue",
                             by.y = "NHPRTR")
            
            E_gene4 <- dplyr::select(E_gene4, FPKM, Master, Tissue_type, level1)
            if(nrow(E_gene4)>0){E_gene4$gene <- gene_name_selected}    
            
            E_gene(E_gene4, input$grouped_tissue) 
            
        }
    })
    
    
    # -------- NHPRTR cyno Maurtian --------
    
    
    E_gene5 <- eventReactive(input$Submit, {
        genIDs <- gene_description_function(input_gene = input$geneName)
        
        NHPRTR_CMM2 <- NHPRTR_CMM %>%
            dplyr::filter(Gene %in% genIDs) 
        
        id <- which.max(rowMeans(NHPRTR_CMM2[-1]))
        gene_name_selected <- NHPRTR_CMM2$Gene[id]
        
        NHPRTR_CMM2 <- as.data.frame(NHPRTR_CMM2[id, -1])
        
        colnames(NHPRTR_CMM2) <- gsub(pattern = "(\\.).*", replacement = "", colnames(NHPRTR_CMM2))
        
        if (dim(NHPRTR_CMM2)[1] == 0) {
            E_gene5 <- data.frame(FPKM = numeric(0), tissue = character(0))
            
        } else {
            E_gene5 <- setNames(data.frame(t(NHPRTR_CMM2), grp = colnames(NHPRTR_CMM2)), c("FPKM", "tissue"))
            
            
            rownames(E_gene5) <- NULL
            E_gene5 <- merge(E_gene5,
                             Atlas_Dictionary_final,
                             by.x = "tissue",
                             by.y = "NHPRTR")
            
            E_gene5 <- dplyr::select(E_gene5, FPKM, Master, Tissue_type, level1)
            if(nrow(E_gene5)>0){E_gene5$gene <- gene_name_selected}    
            
            E_gene(E_gene5, input$grouped_tissue) 
            
        }
        
    })
    
    
    
    # -------- NHPRTR rhesus Indian --------
    
    
    E_gene6 <- eventReactive(input$Submit, {
        genIDs <- gene_description_function(input_gene = input$geneName)
        
        NHPRTR_RMI2 <- NHPRTR_RMI %>%
            dplyr::filter(Gene %in% genIDs) 
        
        id <- which.max(rowMeans(NHPRTR_RMI2[-1]))
        gene_name_selected <- NHPRTR_RMI2$Gene[id]
        
        NHPRTR_RMI2 <- as.data.frame(NHPRTR_RMI2[id, -1])
        
        colnames(NHPRTR_RMI2) <- gsub(pattern = "(\\.).*", replacement = "", colnames(NHPRTR_RMI2))
        
        if (dim(NHPRTR_RMI2)[1] == 0) {
            E_gene6 <- data.frame(FPKM = numeric(0), tissue = character(0))
            
        } else {
            E_gene6 <-setNames(data.frame(t(NHPRTR_RMI2), grp = colnames(NHPRTR_RMI2)), c("FPKM", "tissue"))
            
            
            rownames(E_gene6) <- NULL
            E_gene6 <- merge(E_gene6,
                             Atlas_Dictionary_final,
                             by.x = "tissue",
                             by.y = "NHPRTR")
            
            E_gene6 <- dplyr::select(E_gene6, FPKM, Master, Tissue_type, level1)
            if(nrow(E_gene6)>0){E_gene6$gene <- gene_name_selected}    
            
            E_gene(E_gene6, input$grouped_tissue) 
            
        }
        
    })
    
    
    

    # -------- BioGPS - MOUSE (GNF1M) --------
    
    
    BioGPS2M <- eventReactive(input$Submit, {
        
        genIDs <- gene_description_function(species = "mouse", input_gene = input$geneName)
        
        BioGPS2M  <- BioGPS_M %>%
            dplyr::filter(Symbol %in% genIDs) 
        
        BioGPS2M <- BioGPS2M[rowMeans(BioGPS2M[,5:length(BioGPS2M)]) > input$microarray_cutoff, ]
        
    })
    
    
    #Update the probes names on SelectInput
    
    observe({
        x <- BioGPS2M()$ProbesetID
        
        updateSelectInput(session, inputId = "probeBioGPS_Mouse", label = "Select Probe",
                          choices = x,
                          selected = BioGPS2M()$ProbesetID[which.max(apply(BioGPS2M()[, 5:length(BioGPS2M())], 1, mean))])
    })
    
    
    E_gene7 <- reactive({
        
        BioGPS2M_subset <- BioGPS2M()[BioGPS2M()$ProbesetID == input$probeBioGPS_Mouse, ]  #Remove the Gene name column now
        gene_name_selected <- as.character(BioGPS2M_subset[1, 2])   #as.character to avoid printing the level
        
        BioGPS2M_subset <- BioGPS2M_subset[, -c(2:4)]
        
        if (dim(BioGPS2M_subset)[1] == 0) {
            E_gene7 <- data.frame(Signal = numeric(0), tissue = character(0))
            
        } else {
            E_gene7 <- setNames(data.frame(t(BioGPS2M_subset[, 2:148]), grp = colnames(BioGPS2M_subset[, 2:148])), c("Signal", "tissue"))
            
            rownames(E_gene7) <- NULL
            E_gene7$tissue <- gsub(pattern = "(\\.1)", replacement = "", E_gene7$tissue)
            
            E_gene7 <- merge(E_gene7,
                             Atlas_Dictionary_final,
                             by.x = "tissue",
                             by.y = "BioGPS_Mouse")
            
            E_gene7 <- dplyr::select(E_gene7, Signal, Master, Tissue_type, level1)
            if(nrow(E_gene7)>0){E_gene7$gene <- gene_name_selected}    
            
            E_gene(E_gene7, input$grouped_tissue) 
            
        }
    })
    
    
    

    # -------- BioGPS - MOUSE2 (MOE403) --------
    
    #Filter the database based on selected gene
    
    BioGPS2M2 <- eventReactive(input$Submit, {
        genIDs <- gene_description_function(species = "mouse", input_gene = input$geneName)
        
        BioGPS2M2  <- BioGPS_M2 %>%
            dplyr::filter(Gene.Symbol %in% genIDs) 
        
        BioGPS2M2 <- BioGPS2M2[rowMeans(BioGPS2M2[,3:length(BioGPS2M2)]) > input$microarray_cutoff, ]
        
    })
    
    
    #Update the probes names on SelectInput
    
    observe({
        x <- BioGPS2M2()$Probe.Set.ID
        
        updateSelectInput(session, inputId = "probeBioGPS_Mouse2", label = "Select Probe",
                          choices = x,
                          selected = BioGPS2M2()$Probe.Set.ID[which.max(apply(BioGPS2M2()[, 3:length(BioGPS2M2())], 1, mean))]
        )
    })
    
    
    E_gene8 <- reactive({
        
        BioGPS2M2_subset <- BioGPS2M2()[BioGPS2M2()$Probe.Set.ID == input$probeBioGPS_Mouse2, ]   #Remove the Gene name column now
        gene_name_selected <- as.character(BioGPS2M2_subset[1, 2])   #as.character to avoid printing the level. Collect the gene name here to avoid losing this information once they change of Probe
        
        BioGPS2M2_subset <- BioGPS2M2_subset[, -2]
        
        
        if (dim(BioGPS2M2_subset)[1] == 0) {
            E_gene8 <- data.frame(Signal = numeric(0), tissue = character(0))
            
        } else {
            E_gene8 <- setNames(data.frame(t(BioGPS2M2_subset[, 2:190]), grp = colnames(BioGPS2M2_subset[, 2:190])), c("Signal", "tissue"))
            
            rownames(E_gene8) <- NULL
            E_gene8$tissue <- gsub(pattern = '(\\.1)|(\\.)|(\\..1).*', replacement = "", E_gene8$tissue)
            
            E_gene8 <- merge(E_gene8,
                             Atlas_Dictionary_final,
                             by.x = "tissue",
                             by.y = "BioGPS_Mouse2")
            
            E_gene8 <- dplyr::select(E_gene8, Signal, Master, Tissue_type, level1)
            if(nrow(E_gene8)>0){E_gene8$gene <- gene_name_selected}    
            
            E_gene(E_gene8, input$grouped_tissue) 
            
        }
    })
    
    

    # -------- BioGPS - Human --------
    
        
    #Filter the database based on selected gene
    
    BioGPS2H <- eventReactive(input$Submit, {
        genIDs <- gene_description_function(input_gene = input$geneName)
        
        BioGPS2H  <- BioGPS_H %>%
            dplyr::filter(toupper(Gene.Symbol) %in% genIDs) 
        
        BioGPS2H <- BioGPS2H[rowMeans(BioGPS2H[,3:length(BioGPS2H)]) > input$microarray_cutoff, ]
        
    })
    
    
    #Update the probes names on SelectInput
    
    observe({
        x <- BioGPS2H()$Probe.Set.ID
        
        updateSelectInput(session, inputId = "probeBioGPS_Human", label = "Select Probe",
                          choices = x,
                          selected = BioGPS2H()$Probe.Set.ID[which.max(apply(BioGPS2H()[, 3:length(BioGPS2H())], 1, mean))]
        )
    })
    
    
    E_gene9 <- reactive({
        
        BioGPS2H_subset <- BioGPS2H()[BioGPS2H()$Probe.Set.ID ==	input$probeBioGPS_Human, ]
        gene_name_selected <- as.character(BioGPS2H_subset[1, 2])   #as.character to avoid printing the level
        BioGPS2H_subset <- BioGPS2H_subset[, -2]
        
        if (dim(BioGPS2H_subset)[1] == 0) {
            E_gene9 <- data.frame(Signal = numeric(0), tissue = character(0))
            
        } else {
            E_gene9 <- setNames(data.frame(t(BioGPS2H_subset[, 2:175]), grp = colnames(BioGPS2H_subset[, 2:175])), c("Signal", "tissue"))
            
            rownames(E_gene9) <- NULL
            E_gene9$tissue <- gsub(pattern = "(\\.).*", replacement = "", E_gene9$tissue)
            E_gene9 <- merge(E_gene9,
                             Atlas_Dictionary_final,
                             by.x = "tissue",
                             by.y = "BioGPS_Human")
            
            E_gene9 <- dplyr::select(E_gene9, Signal, Master, Tissue_type, level1)
            if(nrow(E_gene9)>0){E_gene9$gene <- gene_name_selected}    
            
            E_gene(E_gene9, input$grouped_tissue) 
            
        }
        
    })
    

    
    
    # -------- Ratlas --------

        
    E_gene10 <- eventReactive(input$Submit, {
        genIDs <- gene_description_function(species = "rat", input_gene = input$geneName)
        
        Ratlas2 <- Ratlas %>%
            dplyr::filter(gene_name %in% genIDs) %>%
            dplyr::group_by(gene_name) %>%
            dplyr::mutate(D = mean(fpkm)) %>%
            ungroup() %>% 
            dplyr::filter(D==max(D)) %>%
            select(-D)
        
        
        gene_name_selected <- as.character(Ratlas2$gene_name[1])   #as.character to avoid printing the level
        
        E_gene10 <- Ratlas2 %>%
            dplyr::select(c(fpkm, organ))
        
        
        if (dim(Ratlas2)[1] == 0) {
            E_gene10 <- data.frame(FPKM = numeric(0), tissue = character(0))
            
        } else {
            colnames(E_gene10) <- c("FPKM", "tissue")
            
            E_gene10 <- merge(E_gene10,
                              Atlas_Dictionary_final,
                              by.x = "tissue",
                              by.y = "Ratlas")
            
            E_gene10 <- dplyr::select(E_gene10, FPKM, Master, Tissue_type, level1)
            if(nrow(E_gene10)>0){E_gene10$gene <- gene_name_selected}    
            
            E_gene(E_gene10, input$grouped_tissue) 
            
        }
    })
    
    

    
    # -------- Minipig Atlas --------
    
    
    E_gene11 <- eventReactive(input$Submit, {
        genIDs <- gene_description_function(input_gene = input$geneName)
        
        Minipig_Atlas2 <- Minipig_Atlas %>%
            dplyr::filter(toupper(gene_name) %in% genIDs) %>%
            dplyr::group_by(organ,transcript) %>%
            dplyr::mutate(D = mean(tpm)) %>%    #For tissues where several transcripts were available, those with highest mean on the transcript have been selected.
            dplyr::group_by(organ) %>%
            dplyr::filter(D==max(D)) %>%
            select(-D)
        
        Minipig_Atlas2$gene_name <- droplevels(Minipig_Atlas2$gene_name)
        
        gene_name_selected <- as.character(Minipig_Atlas2$gene_name[1])   #as.character to avoid printing the level
        
        E_gene11 <- Minipig_Atlas2 %>%
            dplyr::select(c(tpm, organ))
        
        
        if (dim(Minipig_Atlas)[1] == 0) {
            E_gene11 <- data.frame(tpm = numeric(0), tissue = character(0))
            
        } else {
            
            colnames(E_gene11) <- c("tpm", "tissue")
            E_gene11 <- merge(E_gene11,
                              Atlas_Dictionary_final,
                              by.x = "tissue",
                              by.y = "Minipig_Atlas")
            
            E_gene11 <- dplyr::select(E_gene11, tpm, Master, Tissue_type, level1)
            if(nrow(E_gene11)>0){E_gene11$gene <- gene_name_selected}    
            
            E_gene(E_gene11, input$grouped_tissue) 
            
        }
    })
    
    
    
    
    # -------- Mouse MTAB6081 --------
    
    
    E_gene12 <- eventReactive(input$Submit, {
        
        genIDs <- gene_description_function(species = "mouse", input_gene = input$geneName)
        
        ensembl <- geneID_mouse$ENSEMBL[geneID_mouse$SYMBOL %in% genIDs]
        
        E_gene12 <- Mouse_MTAB6081[row.names(Mouse_MTAB6081) %in% ensembl,]
        
        if (dim(E_gene12)[1] == 0) {
            
            E_gene12 <- data.frame(TPM = numeric(0), tissue = character(0))
            
        } else {
            
            id <- which.max(rowMeans(E_gene12))
            gene_name_selected <- geneID_mouse$SYMBOL[geneID_mouse$ENSEMBL == rownames(E_gene12)[id]]   #as.character to avoid printing the level
            
            E_gene12 <- as.data.frame(t(E_gene12[id, ]))
            
            E_gene12$tissue <- row.names(E_gene12)
            row.names(E_gene12) <- NULL
            
            E_gene12$tissue <- gsub('.*_', '', E_gene12$tissue)
            colnames(E_gene12) <- c("TPM", "tissue")
            
            E_gene12 <- merge(E_gene12,
                              Atlas_Dictionary_final,
                              by.x = "tissue",
                              by.y = "Mouse_MTAB6081")
            
            E_gene12 <- dplyr::select(E_gene12, TPM, Master, Tissue_type, level1)
            if(nrow(E_gene12)>0){E_gene12$gene <- gene_name_selected}    
            
            E_gene(E_gene12, input$grouped_tissue) 
            
        }
    })
    
    

    
    # -------- Rat MTAB6081 --------

        
    E_gene13 <- eventReactive(input$Submit, {
        
        genIDs <- gene_description_function(species = "rat", input_gene = input$geneName)
        
        ensembl <- geneID_rat$ENSEMBL[geneID_rat$SYMBOL %in% genIDs]
        
        E_gene13 <- Rat_MTAB6081[row.names(Rat_MTAB6081) %in% ensembl,]
        
        if (dim(E_gene13)[1] == 0) {
            
            E_gene13 <- data.frame(TPM = numeric(0), tissue = character(0))
            
        } else {
            
            id <- which.max(rowMeans(E_gene13))
            gene_name_selected <- geneID_rat$SYMBOL[geneID_rat$ENSEMBL == rownames(E_gene13)[id]]   #as.character to avoid printing the level
            
            E_gene13 <- as.data.frame(t(E_gene13[id, ]))
            
            E_gene13$tissue <- row.names(E_gene13)
            row.names(E_gene13) <- NULL
            
            
            E_gene13$tissue <- gsub('.*_', '', E_gene13$tissue)
            colnames(E_gene13) <- c("TPM", "tissue")
            
            
            E_gene13 <- merge(E_gene13,
                              Atlas_Dictionary_final,
                              by.x = "tissue",
                              by.y = "Rat_MTAB6081")
            
            E_gene13 <- dplyr::select(E_gene13, TPM, Master, Tissue_type, level1)
            if(nrow(E_gene13)>0){E_gene13$gene <- gene_name_selected}    
            
            E_gene(E_gene13, input$grouped_tissue) 
            
        }
    })  
    
    

    # -------- DIS Atlas Monkey --------
    
        
    DIS_Atlas_Monkey2 <- eventReactive(input$Submit, {
        
        genIDs <- gene_description_function(input_gene = input$geneName)
        
        DIS_Atlas_Monkey2  <- DIS_Atlas_Monkey[toupper(DIS_Atlas_Monkey$Gene.Symbol) %in% genIDs,]
        DIS_Atlas_Monkey2 <- DIS_Atlas_Monkey2[rowMeans(DIS_Atlas_Monkey2[,4:length(DIS_Atlas_Monkey2)]) > input$microarray_cutoff, ]
        
    })
    
    
    #Update the probes names on SelectInput
    
    observe({
        x <- as.character(DIS_Atlas_Monkey2()$Probe.Set.ID)
        
        updateSelectInput(session, inputId = "probeDIS_Monkey", label = "Select Probe",
                          choices = x,
                          selected = DIS_Atlas_Monkey2()$Probe.Set.ID[which.max(apply(DIS_Atlas_Monkey2()[, 4:length(DIS_Atlas_Monkey2())], 1, mean))]
        )
    })
    
    
    E_gene14 <- reactive({
        
        DIS_Atlas_Monkey2_subset <- DIS_Atlas_Monkey2()[DIS_Atlas_Monkey2()$Probe.Set.ID == input$probeDIS_Monkey, ] #First filter by Probe, then pick gene, then remove not necessary columns
        
        gene_name_selected <- as.character(DIS_Atlas_Monkey2_subset[1, 3])   #as.character to avoid printing the level
        
        DIS_Atlas_Monkey2_subset <- DIS_Atlas_Monkey2_subset[,-c(2:3)]
        
        if (dim(DIS_Atlas_Monkey2_subset)[1] == 0) {
            E_gene14 <- data.frame(Signal = numeric(0), tissue = character(0))
            
        } else {
            E_gene14 <-
                setNames(data.frame(t(DIS_Atlas_Monkey2_subset[, 2:length(DIS_Atlas_Monkey2_subset)]), grp = colnames(DIS_Atlas_Monkey2_subset[, 2:length(DIS_Atlas_Monkey2_subset)])), c("Signal", "tissue"))
            
            rownames(E_gene14) <- NULL
            E_gene14$tissue <- gsub(pattern = "(\\.\\d.*$)", replacement = "", E_gene14$tissue)
            
            E_gene14 <- merge(E_gene14,
                              Atlas_Dictionary_final,
                              by.x = "tissue",
                              by.y = "DIS_Atlas2")
            
            E_gene14 <- dplyr::select(E_gene14, Signal, Master, Tissue_type, level1)
            if(nrow(E_gene14)>0){E_gene14$gene <- gene_name_selected}    
            
            E_gene(E_gene14, input$grouped_tissue) 
            
        }
    })
    
    

    # -------- DIS Atlas Dog --------
    
    DIS_Atlas_Dog2 <- eventReactive(input$Submit, {
        genIDs <- gene_description_function(input_gene = input$geneName)
        
        DIS_Atlas_Dog2  <- DIS_Atlas_Dog[toupper(DIS_Atlas_Dog$Gene.Symbol) %in% genIDs,]
        DIS_Atlas_Dog2 <- DIS_Atlas_Dog2[rowMeans(DIS_Atlas_Dog2[,4:length(DIS_Atlas_Dog2)]) > input$microarray_cutoff, ]
        
    })
    
    observe({
        x <- as.character(DIS_Atlas_Dog2()$Probe.Set.ID)
        
        updateSelectInput(session, inputId = "probeDIS_Dog", label = "Select Probe",
                          choices = x,
                          selected = DIS_Atlas_Dog2()$Probe.Set.ID[which.max(apply(DIS_Atlas_Dog2()[, 4:length(DIS_Atlas_Dog2())], 1, mean))]
        )
    })
    
    
    E_gene15 <- reactive({
        
        DIS_Atlas_Dog2_subset <- DIS_Atlas_Dog2()[DIS_Atlas_Dog2()$Probe.Set.ID == input$probeDIS_Dog, ]
        gene_name_selected <- as.character(DIS_Atlas_Dog2_subset[1, 3])   #as.character to avoid printing the level
        
        DIS_Atlas_Dog2_subset <- DIS_Atlas_Dog2_subset[,-c(2:3)]
        
        if (dim(DIS_Atlas_Dog2_subset)[1] == 0) {
            E_gene15 <- data.frame(Signal = numeric(0), tissue = character(0))
            
        } else {
            E_gene15 <- setNames(data.frame(t(DIS_Atlas_Dog2_subset[, 2:length(DIS_Atlas_Dog2_subset)]), grp = colnames(DIS_Atlas_Dog2_subset[, 2:length(DIS_Atlas_Dog2_subset)])), c("Signal", "tissue"))
            
            rownames(E_gene15) <- NULL
            E_gene15$tissue <- gsub(pattern = "(\\.\\d.*$)", replacement = "", E_gene15$tissue)
            
            E_gene15 <- merge(E_gene15,
                              Atlas_Dictionary_final,
                              by.x = "tissue",
                              by.y = "DIS_Atlas2")
            
            E_gene15 <- dplyr::select(E_gene15, Signal, Master, Tissue_type, level1)
            if(nrow(E_gene15)>0){E_gene15$gene <- gene_name_selected}    
            
            E_gene(E_gene15, input$grouped_tissue) 
            
        }
    })
    

    
    # -------- DIS Atlas Rat --------
    
    DIS_Atlas_Rat2 <- eventReactive(input$Submit, {
        genIDs <- gene_description_function(species = "rat", input_gene = input$geneName)
        
        DIS_Atlas_Rat2  <- DIS_Atlas_Rat[DIS_Atlas_Rat$Gene.Symbol %in% genIDs,]
        DIS_Atlas_Rat2 <- DIS_Atlas_Rat2[rowMeans(DIS_Atlas_Rat2[,4:length(DIS_Atlas_Rat2)]) > input$microarray_cutoff, ]
        
        
    })
    
    
    observe({
        x <- as.character(DIS_Atlas_Rat2()$Probe.Set.ID)
        
        updateSelectInput(session, inputId = "probeDIS_Rat", label = "Select Probe",
                          choices = x,
                          selected = DIS_Atlas_Rat2()$Probe.Set.ID[which.max(apply(DIS_Atlas_Rat2()[, 4:length(DIS_Atlas_Rat2())], 1, mean))]
        )
    })
    
    
    E_gene16 <- reactive({
        
        DIS_Atlas_Rat2_subset <- DIS_Atlas_Rat2()[DIS_Atlas_Rat2()$Probe.Set.ID == input$probeDIS_Rat, ]
        gene_name_selected <- as.character(DIS_Atlas_Rat2_subset[1, 3])   #as.character to avoid printing the level
        
        DIS_Atlas_Rat2_subset <- DIS_Atlas_Rat2_subset[,-c(2:3)]
        
        if (dim(DIS_Atlas_Rat2_subset)[1] == 0) {
            E_gene16 <- data.frame(Signal = numeric(0), tissue = character(0))
            
        } else {
            E_gene16 <- setNames(data.frame(t(DIS_Atlas_Rat2_subset[, 2:length(DIS_Atlas_Rat2_subset)]), grp = colnames(DIS_Atlas_Rat2_subset[, 2:length(DIS_Atlas_Rat2_subset)])), c("Signal", "tissue"))
            
            rownames(E_gene16) <- NULL
            E_gene16$tissue <- gsub(pattern = "(\\.\\d.*$)", replacement = "", E_gene16$tissue)
            
            E_gene16 <- merge(E_gene16,
                              Atlas_Dictionary_final,
                              by.x = "tissue",
                              by.y = "DIS_Atlas2")
            
            E_gene16 <- dplyr::select(E_gene16, Signal, Master, Tissue_type, level1)
            if(nrow(E_gene16)>0){E_gene16$gene <- gene_name_selected}    
            
            E_gene(E_gene16, input$grouped_tissue) 
            
        }
    })
    
    

    # -------- DIS Atlas Mouse --------
    
    
    DIS_Atlas_Mouse2 <- eventReactive(input$Submit, {
        genIDs <- gene_description_function(species = "mouse", input_gene = input$geneName)
        
        
        DIS_Atlas_Mouse2  <- DIS_Atlas_Mouse[DIS_Atlas_Mouse$Gene.Symbol %in% genIDs,]
        DIS_Atlas_Mouse2 <- DIS_Atlas_Mouse2[rowMeans(DIS_Atlas_Mouse2[,4:length(DIS_Atlas_Mouse2)]) > input$microarray_cutoff, ]
        
    })
    
    
    observe({
        x <- as.character(DIS_Atlas_Mouse2()$Probe.Set.ID)
        
        updateSelectInput(session, inputId = "probeDIS_Mouse", label = "Select Probe",
                          choices = x,
                          selected = DIS_Atlas_Mouse2()$Probe.Set.ID[which.max(apply(DIS_Atlas_Mouse2()[, 4:length(DIS_Atlas_Mouse2())], 1, mean))]
        )
    })
    
    
    E_gene17 <- reactive({
        
        DIS_Atlas_Mouse2_subset <- DIS_Atlas_Mouse2()[DIS_Atlas_Mouse2()$Probe.Set.ID == input$probeDIS_Mouse, ]
        gene_name_selected <- as.character(DIS_Atlas_Mouse2_subset[1, 3])   #as.character to avoid printing the level
        
        DIS_Atlas_Mouse2_subset <- DIS_Atlas_Mouse2_subset[,-c(2:3)]
        
        if (dim(DIS_Atlas_Mouse2_subset)[1] == 0) {
            E_gene17 <- data.frame(Signal = numeric(0), tissue = character(0))
            
        } else {
            E_gene17 <- setNames(data.frame(t(DIS_Atlas_Mouse2_subset[, 2:length(DIS_Atlas_Mouse2_subset)]), grp = colnames(DIS_Atlas_Mouse2_subset[, 2:length(DIS_Atlas_Mouse2_subset)])), c("Signal", "tissue"))
            
            rownames(E_gene17) <- NULL
            E_gene17$tissue <- gsub(pattern = "(\\.\\d.*$)", replacement = "", E_gene17$tissue)
            
            E_gene17 <- merge(E_gene17,
                              Atlas_Dictionary_final,
                              by.x = "tissue",
                              by.y = "DIS_Atlas2")
            
            E_gene17 <- dplyr::select(E_gene17, Signal, Master, Tissue_type, level1)
            if(nrow(E_gene17)>0){E_gene17$gene <- gene_name_selected}    
            
            E_gene(E_gene17, input$grouped_tissue) 
            
        }
    })
    

    
    # -------- NHPRTR - All macaques --------
    
    
    E_gene18 <- eventReactive(input$Submit, {
        genIDs <- gene_description_function(input_gene = input$geneName)
        
        NHPRTR_Macaque2 <- NHPRTR_Macaque %>%
            dplyr::filter(Gene %in% genIDs) 
        
        id <- which.max(rowMeans(NHPRTR_Macaque2[-1]))
        gene_name_selected <- NHPRTR_Macaque2[id, "Gene"]
        
        NHPRTR_Macaque2 <- as.data.frame(NHPRTR_Macaque2[id, -1])
        
        colnames(NHPRTR_Macaque2) <- gsub(pattern = "(\\.).*", replacement = "", colnames(NHPRTR_Macaque2))
        
        if (dim(NHPRTR_Macaque2)[1] == 0) {
            E_gene18 <- data.frame(FPKM = numeric(0), tissue = character(0))
            
        } else {
            E_gene18 <- setNames(data.frame(t(NHPRTR_Macaque2), grp = colnames(NHPRTR_Macaque2)), c("FPKM", "tissue"))
            
            
            rownames(E_gene18) <- NULL
            E_gene18 <- merge(E_gene18,
                              Atlas_Dictionary_final,
                              by.x = "tissue",
                              by.y = "NHPRTR")
            
            E_gene18 <- dplyr::select(E_gene18, FPKM, Master, Tissue_type, level1)    ####CLASISF
            if(nrow(E_gene18)>0){E_gene18$gene <- gene_name_selected}    
            
            E_gene(E_gene18, input$grouped_tissue) 
            
        }
    })
    
    
    

    # -------- NHPRTR macaque Japanese --------
    
    E_gene19 <- eventReactive(input$Submit, {
        genIDs <- gene_description_function(input_gene = input$geneName)
        
        NHPRTR_JMI2 <- NHPRTR_JMI %>%
            dplyr::filter(Gene %in% genIDs) 
        
        id <- which.max(rowMeans(NHPRTR_JMI2[-1]))
        gene_name_selected <- NHPRTR_JMI2[id, "Gene"]
        
        NHPRTR_JMI2 <- as.data.frame(NHPRTR_JMI2[id, -1])
        
        colnames(NHPRTR_JMI2) <- gsub(pattern = "(\\.).*", replacement = "", colnames(NHPRTR_JMI2))
        
        if (dim(NHPRTR_JMI2)[1] == 0) {
            E_gene19 <- data.frame(FPKM = numeric(0), tissue = character(0))
            
        } else {
            E_gene19 <- setNames(data.frame(t(NHPRTR_JMI2), grp = colnames(NHPRTR_JMI2)), c("FPKM", "tissue"))
            
            
            rownames(E_gene19) <- NULL
            E_gene19 <- merge(E_gene19,
                              Atlas_Dictionary_final,
                              by.x = "tissue",
                              by.y = "NHPRTR")
            
            
            E_gene19 <- dplyr::select(E_gene19, FPKM, Master, Tissue_type, level1)
            if(nrow(E_gene19)>0){E_gene19$gene <- gene_name_selected}    
            
            E_gene(E_gene19, input$grouped_tissue) 
            
        }
        
    })
    
    

    
    # -------- DIS Atlas Human --------

        
    DIS_Atlas_Human2 <- eventReactive(input$Submit, {
        genIDs <- gene_description_function(input_gene = input$geneName)
        
        DIS_Atlas_Human2  <- DIS_Atlas_Human[toupper(DIS_Atlas_Human$Gene.Symbol) %in% genIDs,]
        DIS_Atlas_Human2 <- DIS_Atlas_Human2[rowMeans(DIS_Atlas_Human2[,4:length(DIS_Atlas_Human2)]) > input$microarray_cutoff, ]
        
    })
    
    
    #Update the probes names on SelectInput
    
    observe({
        x <- as.character(DIS_Atlas_Human2()$Probe.Set.ID)
        
        updateSelectInput(session, inputId = "probeDIS_Human", label = "Select Probe",
                          choices = x,
                          selected = DIS_Atlas_Human2()$Probe.Set.ID[which.max(apply(DIS_Atlas_Human2()[, 4:length(DIS_Atlas_Human2())], 1, mean))]
        )
    })
    
    
    E_gene20 <- reactive({
        
        DIS_Atlas_Human2_subset <- DIS_Atlas_Human2()[DIS_Atlas_Human2()$Probe.Set.ID == input$probeDIS_Human, ]
        gene_name_selected <-  as.character(DIS_Atlas_Human2_subset[1, 3])   #as.character to avoid printing the level
        
        DIS_Atlas_Human2_subset <- DIS_Atlas_Human2_subset[,-c(2:3)]
        
        if (dim(DIS_Atlas_Human2_subset)[1] == 0) {
            E_gene20 <- data.frame(Signal = numeric(0), tissue = character(0))
            
        } else {
            E_gene20 <- setNames(data.frame(t(DIS_Atlas_Human2_subset[, 2:length(DIS_Atlas_Human2_subset)]), grp = colnames(DIS_Atlas_Human2_subset[, 2:length(DIS_Atlas_Human2_subset)])), c("Signal", "tissue"))
            
            rownames(E_gene20) <- NULL
            E_gene20$tissue <- gsub(pattern = "(\\.\\d.*$)", replacement = "", E_gene20$tissue)
            
            E_gene20 <- merge(E_gene20, 
                              Atlas_Dictionary_final,
                              by.x = "tissue",
                              by.y = "DIS_Atlas2")
            
            
            E_gene20 <- dplyr::select(E_gene20, Signal, Master, Tissue_type, level1)
            if(nrow(E_gene20)>0){E_gene20$gene <- gene_name_selected}    
            E_gene(E_gene20, input$grouped_tissue) 
            
        }
    })
    

    
    
    scaleY <- reactive(scale_Y(input$scalePlot))
    

    
    #------------------------------------ T A B L E S -----------------------------------------------
  
    
    
    macaque_DB_summary <- function(){
        switch(input$NHPRTR_merge,
               NHPRTR_all_mac  = E_gene18(),
               NHPRTR_CMC = E_gene4(),
               NHPRTR_CMM = E_gene5(),
               NHPRTR_JMI = E_gene19(),
               NHPRTR_RMI = E_gene6())
    }
    
    DB_summary <- function() {
        #####Intermediate function created to be used on RMarkdown
        
        if (gene_description_function(input_gene = input$geneName) != "Select Gene" || gene_description_function(input_gene = input$geneName) != "" ) {

            if (nrow(E_gene1()) > 0 & length(E_gene1())>3) {
                
                GTEx_sum <- c("Homo sapiens", "GTEx", "RNASeq", " ", " ",  E_gene1()$gene[1], enrich_tissues(E_gene1(), input$ZScore_cutoff), round(mean(E_gene1()[,1]), 3),
                              round(mean_DB(E_gene1())$Tau[1], 3), 
                              round(max(mean_DB(E_gene1())[3]) / max(mean_DB(E_gene1())[3][mean_DB(E_gene1())[3] != max(mean_DB(E_gene1())[3])]), 3))
                
            } else {
                GTEx_sum <- c("Homo sapiens", "GTEx", "RNASeq", " ", " ", "", "", "", "", "")
            }
            
            
            
            if (nrow(E_gene2()) > 0) {
                HPA_sum <- c("Homo sapiens", "HPA", "RNASeq", " ", " ", E_gene2()$gene[1], enrich_tissues(E_gene2(), input$ZScore_cutoff), round(mean(E_gene2()[,1]), 3),
                             round(mean_DB(E_gene2())$Tau[1], 3), 
                             round(max(mean_DB(E_gene2())[3]) / max(mean_DB(E_gene2())[3][mean_DB(E_gene2())[3] != max(mean_DB(E_gene2())[3])]), 3))
                
            } else {
                HPA_sum <- c("Homo sapiens", "HPA", "RNASeq", " ", " ", "", "", "", "", "")
            }
            
            
            
            if (nrow(E_gene3()) > 0) {
                BarkBase_sum <- c("Canis familiaris", "BarkBase", "RNASeq", " ", " ", E_gene3()$gene[1], enrich_tissues(E_gene3(), input$ZScore_cutoff), round(mean(E_gene3()[,1]), 3),
                                  round(mean_DB(E_gene3())$Tau[1], 3),
                                  round(max(mean_DB(E_gene3())[3]) / max(mean_DB(E_gene3())[3][mean_DB(E_gene3())[3] != max(mean_DB(E_gene3())[3])]), 3)
                )
                
            } else {
                BarkBase_sum <- c("Canis familiaris", "BarkBase", "RNASeq", " ", " ", "", "", "", "", "")
            }
            
            
            if (nrow(E_gene7()) > 0) {
                BioGPS_M_sum <- c( "Mus musculus", "BioGPS_Mouse", "microarray", "Mouse GNF1M", input$probeBioGPS_Mouse, E_gene7()$gene[1], 
                                   enrich_tissues(E_gene7(), input$ZScore_cutoff), round(mean(E_gene7()[,1]), 3),
                                   round(mean_DB(E_gene7())$Tau[1], 3), round(max(mean_DB(E_gene7())[3]) / max(mean_DB(E_gene7())[3][mean_DB(E_gene7())[3] != max(mean_DB(E_gene7())[3])]), 3)
                )
                
            } else {
                BioGPS_M_sum <- c("Mus musculus", "BioGPS_Mouse", "microarray", " ", " ", "", "", "", "", "")
            }
            
            if (nrow(E_gene8()) > 0) {                                                                                      
                BioGPS_M2_sum <- c( "Mus musculus", "BioGPS_Mouse2", "microarray", "Mouse MOE430", input$probeBioGPS_Mouse2,  E_gene8()$gene[1], 
                                    enrich_tissues(E_gene8(), input$ZScore_cutoff), round(mean(E_gene8()[,1]), 3),
                                    round(mean_DB(E_gene8())$Tau[1], 3), round(max(mean_DB(E_gene8())[3]) / max(mean_DB(E_gene8())[3][mean_DB(E_gene8())[3] != max(mean_DB(E_gene8())[3])]), 3)
                )
                
            } else {
                BioGPS_M2_sum <- c("Mus musculus", "BioGPS_Mouse2", "microarray", " ", " ", "", "", "", "", "")
            }
            
            if (nrow(E_gene9()) > 0) {
                BioGPS_H_sum <- c("Homo sapiens", "BioGPS_Human", "microarray", ifelse(grepl("gnf", input$probeBioGPS_Human), "Human GNF1H", "Human U133A"), input$probeBioGPS_Human, E_gene9()$gene[1],
                                  enrich_tissues(E_gene9(), input$ZScore_cutoff), round(mean(E_gene9()[,1]), 3),
                                  round(mean_DB(E_gene9())$Tau[1], 3), round(max(mean_DB(E_gene9())[3]) / max(mean_DB(E_gene9())[3][mean_DB(E_gene9())[3] != max(mean_DB(E_gene9())[3])]), 3)
                )
                
            } else {
                BioGPS_H_sum <- c("Homo sapiens", "BioGPS_Human ", "microarray", " ", " ", "","", "", "", "")
            }
            
            if (nrow(E_gene10()) > 0) {
                Ratlas_sum <- c("Rattus norvegicus", "Ratlas", "RNASeq", " ", " ", E_gene10()$gene[1], 
                                enrich_tissues(E_gene10(), input$ZScore_cutoff), round(mean(E_gene10()[,1]), 3),  round(mean_DB(E_gene10())$Tau[1], 3),
                                round(max(mean_DB(E_gene10())[3]) / max(mean_DB(E_gene10())[3][mean_DB(E_gene10())[3] != max(mean_DB(E_gene10())[3])]), 3))
                
            } else {
                Ratlas_sum <- c("Rattus norvegicus", "Ratlas", "RNASeq", " ", " ", "", "", "", "", "")
            }
            
            if (nrow(E_gene11()) > 0) {
                Minipig_Atlas_sum <- c("Sus scrofa domesticus", "Minipig_Atlas", "RNASeq", " ", " ", E_gene11()$gene[1], enrich_tissues(E_gene11(), input$ZScore_cutoff), round(mean(E_gene11()[,1]), 3),
                                       round(mean_DB(E_gene11())$Tau[1], 3), round(max(mean_DB(E_gene11())[3]) / max(mean_DB(E_gene11())[3][mean_DB(E_gene11())[3] != max(mean_DB(E_gene11())[3])]), 3))
                
            } else {
                Minipig_Atlas_sum <- c("Sus scrofa domesticus", "Minipig_Atlas", "RNASeq", " ", " ", "", "", "", "", "")
            }
            
            if (nrow(E_gene12()) > 0) {
                Mouse_MTAB6081_sum <- c("Mus musculus", "Mouse_MTAB6081", "RNASeq", " ", " ", E_gene12()$gene[1], 
                                        enrich_tissues(E_gene12(), input$ZScore_cutoff), round(mean(E_gene12()[,1]), 3),
                                        round(mean_DB(E_gene12())$Tau[1], 3), round(max(mean_DB(E_gene12())[3]) / max(mean_DB(E_gene12())[3][mean_DB(E_gene12())[3] != max(mean_DB(E_gene12())[3])]), 3))
                
            } else {
                Mouse_MTAB6081_sum <- c("Mus musculus", "Mouse_MTAB6081", "RNASeq", " ", " ", "", "", "", "", "")
            }
            
            if (nrow(E_gene13()) > 0) {
                Rat_MTAB6081_sum <- c("Rattus norvegicus", "Rat_MTAB6081", "RNASeq", " ", " ",  E_gene13()$gene[1], 
                                      enrich_tissues(E_gene13(), input$ZScore_cutoff), round(mean(E_gene13()[,1]), 3),
                                      round(mean_DB(E_gene13())$Tau[1], 3), round(max(mean_DB(E_gene13())[3]) / max(mean_DB(E_gene13())[3][mean_DB(E_gene13())[3] != max(mean_DB(E_gene13())[3])]), 3))
                
            } else {
                Rat_MTAB6081_sum <- c("Rattus norvegicus", "Rat_MTAB6081", "RNASeq", " ", " ", "", "", "", "", "")
            }
            
            if (nrow(E_gene14()) > 0) {
                DIS_Monkey_sum <- c( "Nonhuman primate", "DIS_Atlas_Monkey", "microarray", "Human Genome U133 Plus 2.0", input$probeDIS_Monkey, E_gene14()$gene[1], enrich_tissues(E_gene14(), input$ZScore_cutoff), round(mean(E_gene14()[,1]), 3),
                                     round(mean_DB(E_gene14())$Tau[1], 3), round(max(mean_DB(E_gene14())[3]) / max(mean_DB(E_gene14())[3][mean_DB(E_gene14())[3] != max(mean_DB(E_gene14())[3])]), 3)
                )
                
            } else {
                DIS_Monkey_sum <- c("Nonhuman primate", "DIS_Atlas_Monkey", "microarray", " ", " ", "", "", "", "", "")
            }
            
            if (nrow(E_gene15()) > 0) {
                DIS_Dog_sum <- c( "Canis familiaris", "DIS_Atlas_Dog", "microarray", "Canine_2 Genome", input$probeDIS_Dog, E_gene15()$gene[1], enrich_tissues(E_gene15(), input$ZScore_cutoff), round(mean(E_gene15()[,1]), 3),
                                  round(mean_DB(E_gene15())$Tau[1], 3), round(max(mean_DB(E_gene15())[3]) / max(mean_DB(E_gene15())[3][mean_DB(E_gene15())[3] != max(mean_DB(E_gene15())[3])]), 3)
                )
                
            } else {
                DIS_Dog_sum <- c("Canis familiaris", "DIS_Atlas_Dog", "microarray", " ", " ", "", "", "", "", "")
            }
            
            if (nrow(E_gene16()) > 0) {
                DIS_Rat_sum <- c( "Rattus norvegicus", "DIS_Atlas_Rat", "microarray", "HT Rat Genome 230", input$probeDIS_Rat, E_gene16()$gene[1], enrich_tissues(E_gene16(), input$ZScore_cutoff), round(mean(E_gene16()[,1]), 3),
                                  round(mean_DB(E_gene16())$Tau[1], 3), round(max(mean_DB(E_gene16())[3]) / max(mean_DB(E_gene16())[3][mean_DB(E_gene16())[3] != max(mean_DB(E_gene16())[3])]), 3)
                )
                
            } else {
                DIS_Rat_sum <- c("Rattus norvegicus", "DIS_Atlas_Rat", "microarray", " ", " ", "", "", "", "", "")
            }
            
            if (nrow(E_gene17()) > 0) {
                DIS_Mouse_sum <- c( "Mus musculus", "DIS_Atlas_Mouse", "microarray", "HT Mouse Genome 230", input$probeDIS_Mouse, E_gene17()$gene[1], enrich_tissues(E_gene17(), input$ZScore_cutoff), round(mean(E_gene17()[,1]), 3),
                                    round(mean_DB(E_gene17())$Tau[1], 3), round(max(mean_DB(E_gene17())[3]) / max(mean_DB(E_gene17())[3][mean_DB(E_gene17())[3] != max(mean_DB(E_gene17())[3])]), 3)
                )
                
            } else {
                DIS_Mouse_sum <- c("Mus musculus", "DIS_Atlas_Mouse", "microarray", " ", " ", "", "", "", "", "")
            }
            
            
            if (nrow(macaque_DB_summary()) > 0) {
                NHPRTR_sum <- c( "Nonhuman primate", input$NHPRTR_merge, "RNASeq", " ", " ", macaque_DB_summary()$gene[1], enrich_tissues(macaque_DB_summary(), input$ZScore_cutoff), 
                                 round(mean(macaque_DB_summary()[,1]), 3), round(mean_DB(macaque_DB_summary())$Tau[1], 3),
                                 round(max(mean_DB(macaque_DB_summary())[3]) / max(mean_DB(macaque_DB_summary())[3][mean_DB(macaque_DB_summary())[3] != max(mean_DB(macaque_DB_summary())[3])]), 3)
                )
                
            } else {
                NHPRTR_sum <- c("Nonhuman primate", input$NHPRTR_merge, "RNASeq", " ", " ", "", "", "", "", "")
            }
            
            if (nrow(E_gene20()) > 0) {
                DIS_Human_sum <- c( "Homo sapiens", "DIS_Atlas_Human", "microarray", "Human Genome U133 Plus 2.0", input$probeDIS_Human, E_gene20()$gene[1], enrich_tissues(E_gene20(), input$ZScore_cutoff), round(mean(E_gene20()[,1]), 3),
                                    round(mean_DB(E_gene20())$Tau[1], 3), round(max(mean_DB(E_gene20())[3]) / max(mean_DB(E_gene20())[3][mean_DB(E_gene20())[3] != max(mean_DB(E_gene20())[3])]), 3)
                )
                
            } else {
                DIS_Human_sum <- c("Homo sapiens", "DIS_Atlas_Human", "microarray", " ", " ", "", "", "", "", "")
            }
            
            
            
            sum_table <- data.frame(rbind(
                
                DIS_Human_sum,
                GTEx_sum,
                HPA_sum,
                BioGPS_H_sum,
                DIS_Monkey_sum,
                NHPRTR_sum,
                DIS_Dog_sum,
                BarkBase_sum,
                Minipig_Atlas_sum,
                DIS_Rat_sum,
                Ratlas_sum,
                Rat_MTAB6081_sum,
                DIS_Mouse_sum,
                BioGPS_M_sum,
                BioGPS_M2_sum,
                Mouse_MTAB6081_sum
                
            )
            )
            
            colnames(sum_table) <- c("Species",
                                     "Databases",
                                     "Technology",
                                     "Chip",
                                     "Probes",
                                     "Gene",
                                     "Enrichment",
                                     "Mean Expression",
                                     "Tau",
                                     "Tau ratio"
            )
            
            
            if ("All" %in% input$database) {
                sum_table <- sum_table
                
            } else {
                selected_DBs <- input$database
                selected_DBs <- replace(selected_DBs, selected_DBs=='NHPRTR', input$NHPRTR_merge)
                
                sum_table <- sum_table[sum_table$Database %in% selected_DBs, ]
                
            }
            
            if ("All" %in% input$species) {
                sum_table <- sum_table
                
            } else {
                sum_table <- sum_table[sum_table$Species %in% input$species, ]
                
            }
            

            return(sum_table)
            
        }
    }
    
    
    macaque_DB_mean_funct <- function(){
        switch(input$NHPRTR_merge,                       #Not possible to use macaque_DB_summary() function since we need the E_gene(x) index to identify DB and Species
               NHPRTR_all_mac  = mean_DB(E_gene18()),
               NHPRTR_CMC = mean_DB(E_gene4()),
               NHPRTR_CMM = mean_DB(E_gene5()),
               NHPRTR_JMI = mean_DB(E_gene19()),
               NHPRTR_RMI = mean_DB(E_gene6()))
    }

    
    summary_enriched <- function(x){
        
        macaque_mean_DB <- macaque_DB_mean_funct()

        DIS_Atlas_Human <-  mean_DB(E_gene20())[-3]
        GTEx <- mean_DB(E_gene1())[-3]  #Remove TPM/CPM.. column
        HPA <- mean_DB(E_gene2())[-3]
        BioGPS_H <- mean_DB(E_gene9())[-3]
        DIS_Atlas_Monkey <-  mean_DB(E_gene14())[-3]
        NHPRTR_Macaque <- macaque_mean_DB[-3] 
        DIS_Atlas_Dog <-  mean_DB(E_gene15())[-3]
        BarkBase <- mean_DB(E_gene3())[-3]
        Minipig_Atlas <- mean_DB(E_gene11())[-3]
        DIS_Atlas_Rat <-  mean_DB(E_gene16())[-3]
        Ratlas <- mean_DB(E_gene10())[-3]
        Rat_MTAB6081 <- mean_DB(E_gene13())[-3]
        DIS_Atlas_Mouse <-  mean_DB(E_gene17())[-3]
        BioGPS_M <- mean_DB(E_gene7())[-3]
        BioGPS_M2 <- mean_DB(E_gene8())[-3]
        Mouse_MTAB6081 <- mean_DB(E_gene12())[-3]
        
        summary_enriched <- data.frame(rbind(
            DIS_Atlas_Human,
            GTEx,
            HPA,
            BioGPS_H,
            DIS_Atlas_Monkey,
            NHPRTR_Macaque,
            DIS_Atlas_Dog,
            BarkBase,
            Minipig_Atlas,
            DIS_Atlas_Rat,
            Ratlas,
            Rat_MTAB6081,
            DIS_Atlas_Mouse,
            BioGPS_M,
            BioGPS_M2,
            Mouse_MTAB6081
        )
        )
        
        ZS <- as.numeric(input$ZScore_cutoff)    #To avoid some filter done wrong since it consider the value as a character
        
        summary_enriched <- summary_enriched %>% 
            dplyr::filter(Enrichment > ZS)
        
        validate(need(nrow(summary_enriched) > 0, message = 'No enriched tissues for this Gene'))
        
        ####PLACE TO INTRODUCE THE ***
        
        if ("All" %in% input$database) {
            summary_enriched <- summary_enriched
        } else {
            selected_DBs <- input$database
            selected_DBs <- replace(selected_DBs, selected_DBs=='NHPRTR', NHPRTR_Macaque$Database)      
            summary_enriched <- summary_enriched[summary_enriched$Database %in% selected_DBs,]}
        
        if ("All" %in% input$tissue) {summary_enriched <- summary_enriched
        } else {summary_enriched <- summary_enriched[summary_enriched$Tissue_type %in% input$tissue,]}
        
        if ("All" %in% input$species) {summary_enriched <- summary_enriched
        } else {summary_enriched <- summary_enriched[summary_enriched$Species %in% input$species,]}
        
        
        
        summary_enriched <- summary_enriched %>%
            dplyr::select(groupTissue, Species, Expression, Enrichment, Database)
        
        
        return(summary_enriched)   
        
    }
    
    tissueSpecies <- reactive({
        
        tissueSpecies <- summary_enriched()
        
        tissueSpecies$Species <- ifelse (tissueSpecies$Enrichment>3,  paste0(tissueSpecies$Species, "***"), 
                                         ifelse (tissueSpecies$Enrichment>2, paste0(tissueSpecies$Species, "**"), 
                                                 ifelse (tissueSpecies$Enrichment>1, paste0(tissueSpecies$Species, "*"), as.character(tissueSpecies$Species))))
        tissueSpecies <- tissueSpecies[,-c(4)]
        tissueSpecies <- tissueSpecies %>% 
            dplyr::group_by(groupTissue, Species, Expression) %>% 
            summarise_all(funs(trimws(paste(., collapse = ', ')))) %>% 
            ungroup() %>% 
            dplyr::mutate(Species = paste0(Species," (", Database, ")")) %>% 
            dplyr::select(-Database) %>% 
            arrange(match(Expression, c('high', 'moderate', 'low', 'below cutoff'))) 
        
        
        tissueSpecies <- pivot_wider(tissueSpecies, names_from = Expression, values_from = Species, values_fn = toString)  
        
        colnames(tissueSpecies) <- c("Tissue", paste0(colnames(tissueSpecies)[2:length(tissueSpecies)], " expression"))
        
        return(tissueSpecies) 
        
    })
    
    
    
    speciesTissue <- function() {
        
        speciesTissue <- summary_enriched()
        
        speciesTissue$groupTissue <- ifelse (speciesTissue$Enrichment>3,  paste0(speciesTissue$groupTissue, "***"), 
                                             ifelse (speciesTissue$Enrichment>2, paste0(speciesTissue$groupTissue, "**"), 
                                                     ifelse (speciesTissue$Enrichment>1, paste0(speciesTissue$groupTissue, "*"), as.character(speciesTissue$groupTissue))))
        speciesTissue <- speciesTissue[-4]
        
        speciesTissue <- speciesTissue %>% 
            dplyr::group_by(groupTissue, Species, Expression) %>% 
            summarise_all(funs(trimws(paste(., collapse = ', ')))) %>% 
            ungroup() %>% 
            dplyr::mutate(groupTissue = paste0(groupTissue," (", Database, ")")) %>% 
            dplyr::select(-Database) %>% 
            arrange(match(Expression, c('high', 'moderate', 'low', 'below cutoff')))
        
        speciesTissue <- pivot_wider(speciesTissue, names_from = Expression, values_from = groupTissue, values_fn = toString) %>% 
            arrange(factor(Species, levels = c("Homo sapiens", "Nonhuman primate", "Canis familiaris", "Sus scrofa domesticus", "Rattus norvegicus", "Mus musculus")))
        
        colnames(speciesTissue) <- c("Species", paste0(colnames(speciesTissue)[2:length(speciesTissue)], " expression"))
        
        return(speciesTissue)
        
    }
    
    
    

    #-------------------------------------   H E A T M A P   ----------------------------------------


    heatmap_sum <- reactive({
        
        macaque_mean_DB <- macaque_DB_mean_funct()
        
        
        heatmap_sum <- bind_rows(
            
            mean_DB(E_gene1())[-3],
            mean_DB(E_gene2())[-3],
            mean_DB(E_gene3())[-3],
            mean_DB(E_gene7())[-3],
            mean_DB(E_gene8())[-3],
            mean_DB(E_gene9())[-3],
            mean_DB(E_gene10())[-3],
            mean_DB(E_gene11())[-3],
            mean_DB(E_gene12())[-3],
            mean_DB(E_gene13())[-3],
            mean_DB(E_gene14())[-3],
            mean_DB(E_gene15())[-3],
            mean_DB(E_gene16())[-3],
            mean_DB(E_gene17())[-3],
            macaque_mean_DB[-3],
            mean_DB(E_gene20())[-3]
            
        )
        
        if ("All" %in% input$database) {
            heatmap_sum <- heatmap_sum
            
        } else {
            selected_DBs <- input$database
            selected_DBs <- replace(selected_DBs, selected_DBs=='NHPRTR', input$NHPRTR_merge)
            heatmap_sum <- heatmap_sum[heatmap_sum$Database %in% selected_DBs, ]}
        
        
        if ("All" %in% input$tissue) {heatmap_sum <- heatmap_sum
        } else {heatmap_sum <- heatmap_sum[heatmap_sum$Tissue_type %in% input$tissue,]}
        
        if ("All" %in% input$species) {heatmap_sum <- heatmap_sum
        } else {heatmap_sum <- heatmap_sum[heatmap_sum$Species %in% input$species,]}
        
        #Select only the tissues where at least one Enrichment > 0 and then subset the data to plot all values where the tissue is for all DB
        
        heatmap_sum_ZS0 <- heatmap_sum %>% 
            dplyr::filter(Enrichment >= as.numeric(input$ZScore_cutoff))
        
        heatmap_sum <- heatmap_sum[heatmap_sum$groupTissue %in% heatmap_sum_ZS0$groupTissue ,]   
    })
    
    output$heatmap <- renderPlot({
        
        heatmap_plot(circle_size=1, axis_size=14, heatmap_sum())
        
    })
    
    
    
    
    # ----------------------------- Output Variables ----------------------------------

    
    observeEvent(input$reset_input, {
        reset("reset")
    })
    
    
    output$gene_descr <- renderText({
        unique(geneID$GENENAME[geneID$SYMBOL == input$geneName])
    })
    
    
    output$gene_descr2 <- output$gene_descr3 <- output$gene_descr4 <- renderText({
        
        paste0(input$geneName, ": ", unique(geneID$GENENAME[geneID$SYMBOL == input$geneName]))
        
    })
    
    output$tissue2 <- output$tissue3 <- output$tissue4 <- renderText({
        paste0(input$tissue, ",")
    })
    
    
    plotGTEx <- eventReactive(input$Submit, {
        
        ##Added intermidiate step to avoid updating plots when selecting tissues without Submit
        graph(E_gene1(), input_tissue = input$tissue, scale = scaleY())
        
    })
    
    output$graph_GTEx <- renderPlot({
        plotGTEx()
    })
    
    
    plotHPA <- eventReactive(input$Submit, {
        graph(E_gene2(), input_tissue = input$tissue, scale = scaleY())
        
    })
    
    output$graph_HPA <- renderPlot({
        plotHPA()
    })
    
    
    plotBarkBase <- eventReactive(input$Submit, {
        graph(E_gene3(), input_tissue = input$tissue, scale = scaleY())
        
    })
    
    output$graph_BarkBase <- renderPlot({
        plotBarkBase()
    })
    
    

    macaque_DB <- eventReactive(list(input$NHPRTR_merge, input$Submit), {
        genDB <- macaque_DB_summary()
        graph(genDB, input_tissue = input$tissue, scale = scaleY())            
        
    })
    
    
    
    output$graph_macaque <- renderPlot({
        macaque_DB()
    }) 
    
    
    plotBioGPS_Mouse <- eventReactive(c(input$probeBioGPS_Mouse, input$Submit), {
        #Added logic to execute automatically when probe is updated.
        graph(E_gene7(), input_tissue = input$tissue, scale = scaleY())
    })
    
    output$graph_BioGPS_Mouse <- renderPlot({
        plotBioGPS_Mouse()
    })
    
    
    plotBioGPS_Mouse2 <- eventReactive(c(input$probeBioGPS_Mouse2, input$Submit), {
        graph(E_gene8(), input_tissue = input$tissue, scale = scaleY())
    })
    
    output$graph_BioGPS_Mouse2 <- renderPlot({
        plotBioGPS_Mouse2()
    })
    
    plotBioGPS_Human <- eventReactive(c(input$probeBioGPS_Human, input$Submit), {
        graph(E_gene9(), input_tissue = input$tissue, scale = scaleY())
    })
    
    output$graph_BioGPS_Human <- renderPlot({
        plotBioGPS_Human()
    })
    
    
    plotRatlas <- eventReactive(input$Submit, {
        graph(E_gene10(), input_tissue = input$tissue, scale = scaleY())
    })
    
    output$graph_Ratlas <- renderPlot({
        plotRatlas()
    })
    
    plotMinipig_Atlas <- eventReactive(input$Submit, {
        graph(E_gene11(), input_tissue = input$tissue, scale = scaleY())
    })
    
    output$graph_Minipig_Atlas <- renderPlot({
        plotMinipig_Atlas()
    })
    
    plotMouse_MTAB6081 <- eventReactive(input$Submit, {
        graph(E_gene12(), input_tissue = input$tissue, scale = scaleY())
    })
    
    output$graph_Mouse_MTAB6081 <- renderPlot({
        plotMouse_MTAB6081()
    })
    
    plotRat_MTAB6081 <- eventReactive(input$Submit, {
        graph(E_gene13(), input_tissue = input$tissue, scale = scaleY())
    })
    
    output$graph_Rat_MTAB6081 <- renderPlot({
        plotRat_MTAB6081()
    })
    
    plotDIS_Monkey <- eventReactive(c(input$probeDIS_Monkey, input$Submit), {
        graph(E_gene14(), input_tissue = input$tissue, scale = scaleY())
    })
    
    output$graph_DIS_Monkey <- renderPlot({
        plotDIS_Monkey()
    })
    
    plotDIS_Dog <- eventReactive(c(input$probeDIS_Dog, input$Submit), {
        graph(E_gene15(), input_tissue = input$tissue, scale = scaleY())
    })
    
    output$graph_DIS_Dog <- renderPlot({
        plotDIS_Dog()
    })
    
    plotDIS_Rat <- eventReactive(c(input$probeDIS_Rat, input$Submit), {
        graph(E_gene16(), input_tissue = input$tissue, scale = scaleY())
    })
    
    output$graph_DIS_Rat <- renderPlot({
        plotDIS_Rat()
    }) 
    
    plotDIS_Mouse <- eventReactive(c(input$probeDIS_Mouse, input$Submit), {
        graph(E_gene17(), input_tissue = input$tissue, scale = scaleY())
    })
    
    output$graph_DIS_Mouse <- renderPlot({
        plotDIS_Mouse()
    })
    
    
    plotDIS_Human <- eventReactive(c(input$probeDIS_Human, input$Submit), {
        graph(E_gene20(), input_tissue = input$tissue, scale = scaleY())
    })
    
    output$graph_DIS_Human <- renderPlot({
        plotDIS_Human()
    })
    
    
    
    
    output$tissueExp <- DT::renderDataTable(
        DB_summary(),
        
        options = list(
            dom = 't',
            pageLength = 25,
            initComplete = JS(
                "function(settings, json) {",
                "$(this.api().table().header()).css({'background-color': '#cce6ff', 'color': 'black'});",
                "}"
            ),
            autoWidth = FALSE,
            columnDefs = list(list(
                className = 'dt-center', targets = 1:5
            ))
        ),
        rownames = FALSE
        
    )
    
    output$tissueSpecies2 <- DT::renderDataTable(
        tissueSpecies(),
        options = list(
            "pageLength" = 100,
            dom = 't',
            
            initComplete = JS(
                "function(settings, json) {",
                "$(this.api().table().header()).css({'background-color': '#cce6ff', 'color': 'black'});",
                "}"
            ),
            
            autoWidth = FALSE
            
        ),
        rownames = FALSE
        
        
    )
    
    output$speciesTissue2 <- DT::renderDataTable(
        speciesTissue(),
        options = list(
            dom = 't',
            
            initComplete = JS(
                "function(settings, json) {",
                "$(this.api().table().header()).css({'background-color': '#cce6ff', 'color': 'black'});",
                "}"
            ),
            autoWidth = FALSE
            
        ),
        rownames = FALSE
    )
    
    
    
    output$geneExp_GTEx <- renderDataTable({
        if (input$geneName != "Select Gene") {
            singleDB(E_gene1(), input$tissue)
        }
    } , rownames = FALSE)
    
    output$geneExp_HPA <- renderDataTable({
        if (input$geneName != "Select Gene") {
            singleDB(E_gene2(), input$tissue)
        }
    } , rownames = FALSE)
    
    
    output$geneExp_BarkBase <- renderDataTable({
        if (input$geneName != "Select Gene") {
            singleDB(E_gene3(), input$tissue)
            
        }
    } , rownames = FALSE)
    
    
    output$geneExp_MAC <- renderDataTable({
        if (input$geneName != "Select Gene") {
            singleDB(E_gene18(), input$tissue)
            
        }
    } , rownames = FALSE)
    
    
    output$geneExp_CMC <- renderDataTable({
        if (input$geneName != "Select Gene") {
            singleDB(E_gene4(), input$tissue)
            
        }
    } , rownames = FALSE)
    
    
    output$geneExp_CMM <- renderDataTable({
        if (input$geneName != "Select Gene") {
            singleDB(E_gene5(), input$tissue)
            
        }
    } , rownames = FALSE)
    
    
    output$geneExp_RMI <- renderDataTable({
        if (input$geneName != "Select Gene") {
            singleDB(E_gene6(), input$tissue)
            
        }
    } , rownames = FALSE)
    
    
    output$geneExp_JMI <- renderDataTable({
        if (input$geneName != "Select Gene") {
            singleDB(E_gene19(), input$tissue)
            
        }
    } , rownames = FALSE)
    
    
    output$geneExp_BioGPS_Mouse <- renderDataTable({
        if (input$geneName != "Select Gene") {
            singleDB(E_gene7(), input$tissue)
            
        }
    } , rownames = FALSE)
    
    
    output$geneExp_BioGPS_Mouse2 <- renderDataTable({
        if (input$geneName != "Select Gene") {
            singleDB(E_gene8(), input$tissue)
            
        }
    } , rownames = FALSE)
    
    
    output$geneExp_BioGPS_Human <- renderDataTable({
        if (input$geneName != "Select Gene") {
            singleDB(E_gene9(), input$tissue)
            
        }
    } , rownames = FALSE)
    
    output$geneExp_Ratlas <- renderDataTable({
        if (input$geneName != "Select Gene") {
            singleDB(E_gene10(), input$tissue)
        }
    } , rownames = FALSE)
    
    
    output$geneExp_Minipig_Atlas <- renderDataTable({
        if (input$geneName != "Select Gene") {
            singleDB(E_gene11(), input$tissue)
        }
    } , rownames = FALSE)
    
    
    output$geneExp_Mouse_MTAB6081 <- renderDataTable({
        if (input$geneName != "Select Gene") {
            singleDB(E_gene12(), input$tissue)
        }
    } , rownames = FALSE)
    
    
    output$geneExp_Rat_MTAB6081 <- renderDataTable({
        if (input$geneName != "Select Gene") {
            singleDB(E_gene13(), input$tissue)
        }
    } , rownames = FALSE)
    
    
    output$geneExp_Monkey_DIS_Atlas <- renderDataTable({
        if (input$geneName != "Select Gene") {
            singleDB(E_gene14(), input$tissue)
        }
    } , rownames = FALSE)
    
    output$geneExp_Dog_DIS_Atlas <- renderDataTable({
        if (input$geneName != "Select Gene") {
            singleDB(E_gene15(), input$tissue)
        }
    } , rownames = FALSE)
    
    
    output$geneExp_Rat_DIS_Atlas <- renderDataTable({
        if (input$geneName != "Select Gene") {
            singleDB(E_gene16(), input$tissue)
        }
    } , rownames = FALSE) 
    
    output$geneExp_Mouse_DIS_Atlas <- renderDataTable({
        if (input$geneName != "Select Gene") {
            singleDB(E_gene17(), input$tissue)
        }
    } , rownames = FALSE)
    
    output$geneExp_Human_DIS_Atlas <- renderDataTable({
        if (input$geneName != "Select Gene") {
            singleDB(E_gene20(), input$tissue)
        }
    } , rownames = FALSE)
    
    
    
    output$sessionInfo <- renderPrint({
        capture.output(sessionInfo())
        
    })
    
    
    
    #----------------------------------- Welcome pages -----------------------------------

    
    
    output$aboutDB <- DT::renderDataTable(
        DB_Used(),
        
        options = list(
            dom = 't',
            pageLength = 25,
            
            initComplete = JS(
                "function(settings, json) {",
                "$(this.api().table().header()).css({'background-color': '#cce6ff', 'color': 'black'});",
                "}"
            ),
            autoWidth = FALSE,
            columnDefs = list(list(
                className = 'dt-center', targets = 1:5
            ))
        ),
        rownames = FALSE
        
    )
    
    

    
    # -------------------------------------- Download ----------------------------------------

    
    output$download <- downloadHandler(
        filename = function() {
            paste0(format(Sys.time(), "%Y-%m-%d"), "_", input$geneName, sep = '.', switch(
                input$format,
                PDF = 'pdf',
                HTML = 'html',
                Word = 'docx'
            ))
        },
        
        content = function(file) {
            withProgress(message = 'Rendering, please wait!',
                         detail = 'Progress bar will disappear when finished...',{
                             
                             src <- normalizePath('report2.Rmd')                           
                             src2 <- normalizePath('template.docx')

                             # temporarily switch to the temp dir, in case you do not have write
                             # permission to the current working directory
                             owd <- setwd(tempdir())
                             on.exit(setwd(owd))
                             file.copy(src, 'report2.Rmd', overwrite = TRUE)
                             file.copy(src2, 'template.docx', overwrite = TRUE)
                             params <- list(
                                 n = E_gene1()$tpm,
                                 gene = input$geneName,
                                 DB_selected = input$database,
                                 microarrayCutoff = input$microarray_cutoff,
                                 ZScoreCutoff = as.numeric(input$ZScore_cutoff),
                                 macaque_select = input$NHPRTR_merge,
                                 TEP_Version = TEP_Version,
                                 species_selected = input$species
                             )
                             
                             library(rmarkdown)
                             out <- render('report2.Rmd', switch(
                                 input$format,
                                 PDF = pdf_document(number_sections = TRUE),
                                 HTML = html_document(number_sections = TRUE),
                                 Word = word_document(reference_docx = "template.docx") 
                             ))
                             file.rename(out, file)
                         })
        }
    )
    
    

    
    output$dl <- downloadHandler(
        
        filename = function() {
            
            paste0(paste0(format(Sys.time(), "%Y-%m-%d"), "_", input$geneName,"_raw_data", ".zip"))
        },
        
        content = function(file) {
            
            df <- list(GTEx = E_gene1(), HPA = E_gene2(), BarkBase = E_gene3(), NHPRTR_CMC = E_gene4(), NHPRTR_CMM = E_gene5(), NHPRTR_RMI = E_gene6(),
                       BioGPS_Mouse = E_gene7(), BioGPS_Mouse2 = E_gene8(), BioGPS_Human = E_gene9(), Ratlas = E_gene10(), Minipig_Atlas = E_gene11(),
                       Mouse_MTAB6081 = E_gene12(), Rat_MTAB6081 = E_gene13(), DIS_Atlas_Monkey = E_gene14(), DIS_Atlas_Dog = E_gene15(), DIS_Atlas_Rat = E_gene16(),
                       DIS_Atlas_Mouse = E_gene17(), NHPRTR_all_mac = E_gene18(), NHPRTR_JMI = E_gene19(), DIS_Atlas_Human = E_gene20())
            
            list_with_nrow <- sapply(df, function(x) nrow(x)>0)   #Remove DBs with 0 rows even if they were selected
            df <- df[list_with_nrow]
            
            DB_selected <- DB_summary()$Databases
            non_selected_DB <- setdiff(names(df), DB_selected)
            
            df[non_selected_DB] <- NULL
            
            fs <- c()
            
            setwd(tempdir())
            
            for (i in 1:length(names(df))){
                path <- paste0(names(df[i]), ".csv")    #Define file name
                write.csv(df[[i]], path)                #write data at previous file name
                fs <- c(fs,path)                        #store list of files to be zipped
                
            }
            
            # file.copy(from = "report2.Rmd", to = "report2.Rmd", overwrite = TRUE)
            
            fs <- c(fs,"report2.Rmd")
            # browser()
            zip(zipfile=file, files=fs)
        },
        
        contentType = "application/zip"
        
    )
    
    
})
