import csv

files = ["Autism.tsv", "Dystonia_complex.tsv", "Dystonia_isolated_combined.tsv", 
         "Dystonia_Superpanel.tsv", "Early-onset_Parkinson_disease.tsv", "Paroxysmal_Dyskinesia.tsv"]

for i in range(len(files)):
    with open(files[i]) as fd:
        #read the "dirty" data
        rd = csv.reader(fd, delimiter="\t", quotechar='"')
        #create a new .txt file to write to
        newfile = open(files[i].strip(".tsv") + "_PanelApp.txt", "a")
        newfile.write("GENE,EVIDENCE\n")
        j = 0
        for row in rd:
            if j == 0:
                j += 1
                continue
            else:
                j += 1
                #write the name of the gene and whether the evidence is strong/moderate/low
                evidence = row[3]
                if "Expert Review Green" in evidence:
                    evidence = "high"
                elif "Expert Review Amber" in evidence:
                    evidence = "moderate"
                elif "Expert Review Red" in evidence:
                    evidence = "low"
                else:
                    evidence = ""
                newfile.write(row[2] + "," + evidence + "\n")