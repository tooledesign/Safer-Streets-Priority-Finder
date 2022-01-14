-- backup values 
UPDATE 
    static.national_tracts
SET 
    outdated_pfm_values = pbm_value; 

-- count total 
SELECT 
    COUNT(*) 
FROM 
    static.national_tracts;
-- 73666 (spooky number)

-- count match
SELECT 
    COUNT(*)
FROM 
    static.national_tracts n
WHERE 
    EXISTS( SELECT 
                1 
            FROM 
                 static.pfm_corrected_predvalyearly_20220113_js j
            WHERE 
                j.geoid = n.geoid)
;
--72734, 932 fewer

UPDATE 
    static.national_tracts c
SET 
    pbm_value = j.predvalyearly
FROM 
    static.pfm_corrected_predvalyearly_20220113_js j
WHERE 
    c.geoid = j.geoid
AND  
    EXISTS( SELECT 
                1 
            FROM 
                 static.pfm_corrected_predvalyearly_20220113_js j
            WHERE 
                j.geoid = n.geoid); 
-- updated 72734

-- Confirmed that 931 nulls existed in the original pbm values that were not solved by this update. 
-- This update fixed 848 pbm values that were incorrected filled with 0 expected fatalities. Two still remain.