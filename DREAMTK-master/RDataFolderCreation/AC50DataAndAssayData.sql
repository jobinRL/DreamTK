/*Run this script to get the CSV files for ac50_data, assay_data
  NOTE: the file paths are left default due to the reason that mysql does not have access to write to them. I will still make sure that in the R script
  to improt that there will be no need to copy paste those files to different locations. But if you need to run it more than once you will need
  to delete them. as mysql does not want to delete them. Chem_data will be ran thru the R file directly and it is assumed that you have access to the data
  for the other models that are not in this database.
*/
DROP TABLE IF EXISTS testgabriel.AssayHit;
CREATE TABLE testgabriel.AssayHit AS       
	(SELECT mc5.hitc, chemicalTable.casn, mc4.aeid, mc5.modl_ga, ace.assay_component_endpoint_name
		FROM invitrodb_v2.mc5 AS mc5, invitrodb_v2.mc4 AS mc4, invitrodb_v2.sample AS sampleTable, invitrodb_v2.chemical AS chemicalTable, invitrodb_v2.assay_component_endpoint AS ace
		WHERE mc5.m4id = mc4.m4id AND sampleTable.spid = mc4.spid AND sampleTable.chid = chemicalTable.chid and mc5.aeid = ace.aeid and   chemicalTable.casn = "100-00-5" order by casn, aeid
	);

DROP TABLE IF EXISTS testgabriel.AssayHitWCounts;
CREATE TABLE testgabriel.AssayHitWCounts AS
    (SELECT a.aeid, a.casn, b.assay_count, hitc, a.modl_ga FROM  testgabriel.AssayHit as a,
										(SELECT count(casn) as assay_count, casn, aeid FROM testgabriel.AssayHit
											GROUP BY casn, aeid
										) as b where a.aeid = b.aeid and b.casn = a.casn
	);
 

DROP TABLE IF EXISTS testgabriel.temp_table;
CREATE TABLE testgabriel.temp_table as SELECT aeid, casn, hitc, min(modl_ga) as ac50
													FROM testgabriel.assayhitwcounts
                                                    where hitc = 1
                                                    group by casn, aeid
													having sum(hitc) >= (count(*)/2);

-- Add constraint
ALTER TABLE testgabriel.temp_table ADD UNIQUE(aeid, casn);

-- Copy data
INSERT IGNORE INTO testgabriel.temp_table SELECT aeid, casn, hitc, min(modl_ga) as ac50
													FROM testgabriel.assayhitwcounts
                                                    where hitc = 0
                                                    group by casn, aeid
													having sum(hitc) <= (count(*)/2);


INSERT IGNORE INTO testgabriel.temp_table SELECT * from testgabriel.assayhit where aeid != 0;

SELECT "casn","aeid","hitc", "ac50", "ac_top","ac_cutoff"
UNION ALL
SELECT tmp.CASN, tmp.aeid, tmp.hitc, ah. , ah. FROM testgabriel.temp_table as tmp NATURAL JOIN testgabriel.assayhit as ah
WHERE tmp.ac50 = ah.modl_ga
INTO OUTFILE '..\ac50_data.csv'
FIELDS TERMINATED BY ','
LINES TERMINATED BY '\n'
;

/*Assay_data*/
#creating table to get the gene information of an assay.
#https://fellowtuts.com/mysql/concatenate-multiple-mysql-rows-into-one-field-using-group_concat/
#we are using this because some assay component endpoints have more than 1 gene target. They will be separated by commas. the concat is used to add a space between the commas for esthetic reasons.
DROP TABLE IF EXISTS testgabriel.AssayGene;
CREATE TABLE testgabriel.AssayGene as (Select ace.aeid, GROUP_CONCAT(CONCAT(' ',gene.gene_name)) as gene_name, GROUP_CONCAT(concat(' ',gene.gene_symbol)) as gene_symbol
		FROM invitrodb_v2.assay_component_endpoint as ace, invitrodb_v2.intended_target as IT, invitrodb_v2.gene as gene
        where it.target_id = gene.gene_id and ace.aeid = it.aeid
        group by aeid
        order by aeid
        );
/* we have inserted everything that has a value, so all that's left is assays that don't have a gene so we can just add nulls for those
So with this in mind and with the fact that we have small tables, let's just add everything onto it with nulls. THIS IS EXTREMELY HACKY. do not include this in any
code that will make it into a live version. instead you should make sure that it's not in the initial table... but that doesn't matter
*/


ALTER TABLE testgabriel.AssayGene ADD UNIQUE(aeid);
INSERT IGNORE INTO testgabriel.AssayGene SELECT aeid, NULL as gene_name, NULL as gene_symbol FROM invitrodb_v2.assay_component_endpoint;

Select "aeid","assay_name", "assay_component_name","assay_component_endpoint_name","organism","tissue","cell_short_name","biological_process_target","intended_target_family","intended_target_family_sub","gene_name","gene_id"
UNION ALL
SELECT ace.aeid, assay.assay_name, ac.assay_component_name, ace.assay_component_endpoint_name, assay.organism, assay.tissue, assay.cell_short_name, ac.biological_process_target, ace.intended_target_family, ace.intended_target_family_sub, gene.gene_name, gene.gene_symbol
FROM  testgabriel.assaygene as gene NATURAL JOIN invitrodb_v2.assay_component_endpoint as ace NATURAL JOIN invitrodb_v2.assay_component as ac NATURAL JOIN invitrodb_v2.assay as assay
INTO OUTFILE '..\assay_data.csv'
FIELDS TERMINATED BY ';'
LINES TERMINATED BY '\n'
;
/*Dropping the unnecessary tables created.*/
DROP TABLE IF EXISTS testgabriel.AssayGene;
DROP TABLE IF EXISTS testgabriel.temp_table;
DROP TABLE IF EXISTS testgabriel.AssayHit;
DROP TABLE IF EXISTS testgabriel.AssayHitWCounts;
/*end of the script to get the data from the sql table*/