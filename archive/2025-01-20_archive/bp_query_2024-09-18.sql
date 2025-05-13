/****** Fetching relevant corpora
		Project: Diversity in Philosophy of Economics
		created by F. Claveau on 2023-03-21 
		updates on:
			- 2023-11-07 to add philosophy
			- 2024-02-26 to add science studies with philosophy
		
		
		SETUP
		Choose: Query -> Results to file
            Query options -> Text -> Tab delimited
		You can easily save on onedrive and fetch the data elsewhere
		******/


/*  Economics first */

-- The articles (to articles.tsv)

SELECT art.*, Revue, Abbrev_11
  FROM [WoS].[pex].[Article] as art
  JOIN [WoS].[pex].[Liste_revue] as rev
  on art.Code_Revue = rev.Code_Revue
  WHERE art.Code_Discipline = 119

-- The references (to references.tsv)
-- Note that a big bunch of columns are left out because they have no value (either NULL or 0) for most rows
-- Those rows are:      [Patent_No],[Assignee],[Art_No],[Occurrence_Order],[Is_Unicode],[Is_Truncated_Cited_Author],
--                      [Is_Truncated_Cited_Title],[Is_Truncated_Cited_Work],[Is_Truncated_Page],
--                      [Is_Truncated_Year],[Is_Truncated_Volume]
-- Occurence_order is populated quite a lot (11% of rows), but not useful for us (there is already an Ordre column)
-- Exactly 26009 out of 14377801 rows have at least one non-null value for the other  left-out columns (0.2% of rows)

SELECT ref.[OST_BK]
      ,ref.[Ordre]
      ,ref.[UID]
      ,ref.[UID_Ref]
      ,ref.[Cited_Author]
      ,ref.[Year]
      ,ref.[Volume]
      ,ref.[Page]
      ,ref.[Doi]
      ,ref.[Cited_Title]
      ,ref.[Cited_Work]
  FROM [WoS].[dbo].[Reference] as ref
  JOIN [WoS].[pex].[Article] as art
  ON ref.OST_BK = art.OST_BK
  WHERE art.Code_Discipline = 119 

-- The abstracts  (to abstracts.tsv)
SELECT abst.*
  FROM [WoS].[dbo].[Abstract] as abst
  LEFT JOIN [WoS].[pex].[Article] as art
  ON abst.OST_BK = art.OST_BK
  WHERE art.Code_Discipline = 119

  
-- The authors (first table) (to authors.tsv)
SELECT  contrib.*
  FROM [WoS].[dbo].[Summary_Name] as contrib
  LEFT JOIN [WoS].[pex].[Article] as art
  ON contrib.OST_BK = art.OST_BK
  WHERE art.Code_Discipline = 119
  
-- The authors (second table) (to authors_other_table.tsv)
SELECT  contrib.*
  FROM [WoS].[dbo].[Address_Name] as contrib
  LEFT JOIN [WoS].[pex].[Article] as art
  ON contrib.OST_BK = art.OST_BK
  WHERE art.Code_Discipline = 119

   
-- The contributors (third table) (to contributors.tsv)
SELECT  contrib.*
  FROM [WoS].[dbo].[Contributor] as contrib
  LEFT JOIN [WoS].[pex].[Article] as art
  ON contrib.OST_BK = art.OST_BK
  WHERE art.Code_Discipline = 119


-- The addresses (to address.tsv)
SELECT contrib.*
  FROM [WoS].[dbo].Address as contrib
  LEFT JOIN [WoS].[pex].[Article] as art
  ON contrib.OST_BK = art.OST_BK
  WHERE art.Code_Discipline = 119

-- The organizations (to organizations.tsv)
SELECT contrib.*
  FROM [WoS].[dbo].Address_Organization as contrib
  LEFT JOIN [WoS].[pex].[Article] as art
  ON contrib.OST_BK = art.OST_BK
  WHERE art.Code_Discipline = 119

-- Alternative address and organization (to alternative_address_and_org.tsv)
SELECT contrib.*
  FROM [WoS].[pex].[Adresse] as contrib
  LEFT JOIN [WoS].[pex].[Article] as art
  ON contrib.OST_BK = art.OST_BK
  WHERE art.Code_Discipline = 119



/* Second Philosophy and science studies  */

-- The articles

SELECT art.*, Revue, Abbrev_11
  FROM [WoS].[pex].[Article] as art
  JOIN [WoS].[pex].[Liste_revue] as rev
  on art.Code_Revue = rev.Code_Revue
  WHERE art.Code_Discipline = 139  -- code for philosophy
   OR  art.Code_Discipline = 126 -- Science studies

-- The references

SELECT top 100 ref.[OST_BK]
      ,ref.[Ordre]
      ,ref.[UID]
      ,ref.[UID_Ref]
      ,ref.[Cited_Author]
      ,ref.[Year]
      ,ref.[Volume]
      ,ref.[Page]
      ,ref.[Doi]
      ,ref.[Cited_Title]
      ,ref.[Cited_Work]
  FROM [WoS].[dbo].[Reference] as ref
  JOIN [WoS].[pex].[Article] as art
  ON ref.OST_BK = art.OST_BK
  WHERE art.Code_Discipline = 139  -- code for philosophy
   OR  art.Code_Discipline = 126 -- Science studies

-- info refs in art table

SELECT art2.*, Revue, Abbrev_11
	FROM [WoS].[pex].[Article] as art2
	JOIN [WoS].[pex].[Liste_revue] as rev
	on art2.Code_Revue = rev.Code_Revue
	WHERE UID IN (SELECT DISTINCT ref.[UID_Ref]
					FROM [WoS].[dbo].[Reference] as ref  JOIN [WoS].[pex].[Article] as art
					ON ref.OST_BK = art.OST_BK
					WHERE art.Code_Discipline = 139  -- code for philosophy
					      OR  art.Code_Discipline = 126 -- Science studies
						  )


-- The abstracts 
SELECT abst.*
  FROM [WoS].[dbo].[Abstract] as abst
  LEFT JOIN [WoS].[pex].[Article] as art
  ON abst.OST_BK = art.OST_BK
  WHERE art.Code_Discipline = 139  -- code for philosophy
   OR  art.Code_Discipline = 126 -- Science studies

-- The authors (first table)
SELECT contrib.*
  FROM [WoS].[dbo].[Summary_Name] as contrib
  LEFT JOIN [WoS].[pex].[Article] as art
  ON contrib.OST_BK = art.OST_BK
  WHERE art.Code_Discipline = 139  -- code for philosophy
   OR  art.Code_Discipline = 126 -- Science studies
  
-- The authors (second table, authors_other_table)
SELECT contrib.*
  FROM [WoS].[dbo].[Address_Name] as contrib
  LEFT JOIN [WoS].[pex].[Article] as art
  ON contrib.OST_BK = art.OST_BK
  WHERE art.Code_Discipline = 139  -- code for philosophy
   OR  art.Code_Discipline = 126 -- Science studies

-- The contributors (third table) (to contributors.tsv)
SELECT  contrib.*
  FROM [WoS].[dbo].[Contributor] as contrib
  LEFT JOIN [WoS].[pex].[Article] as art
  ON contrib.OST_BK = art.OST_BK
  WHERE art.Code_Discipline = 139  -- code for philosophy
   OR  art.Code_Discipline = 126 -- Science studies

-- The addresses
SELECT contrib.*
  FROM [WoS].[dbo].Address as contrib
  LEFT JOIN [WoS].[pex].[Article] as art
  ON contrib.OST_BK = art.OST_BK
  WHERE art.Code_Discipline = 139  -- code for philosophy
   OR  art.Code_Discipline = 126 -- Science studies

-- The organizations
SELECT contrib.*
  FROM [WoS].[dbo].Address_Organization as contrib
  LEFT JOIN [WoS].[pex].[Article] as art
  ON contrib.OST_BK = art.OST_BK
  WHERE art.Code_Discipline = 139  -- code for philosophy
   OR  art.Code_Discipline = 126 -- Science studies

-- Alternative address and organization
SELECT contrib.*
  FROM [WoS].[pex].[Adresse] as contrib
  LEFT JOIN [WoS].[pex].[Article] as art
  ON contrib.OST_BK = art.OST_BK
  WHERE art.Code_Discipline = 139  -- code for philosophy
   OR  art.Code_Discipline = 126 -- Science studies
