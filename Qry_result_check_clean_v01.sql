SELECT
	   s.Organisasjonsnummer as orgn
	  ,CAST(CONCAT(s.InternSelskapsId, YEAR(CAST(gd.HistorikkAar as date))) AS bigint) AS [id.y]
	  ,s.InternSelskapsId as id
	  ,YEAR(CAST([HistorikkAar] as date)) as 'y'
	  ,s.Selskapsnavn as comp
	  ,v.Variabelnavn as var
      ,TRY_CONVERT(decimal(38, 12), REPLACE([Verdi], ',', '.')) AS value -- for korrekt håndtering av komma
  FROM [NVE_DWH].[Inntektsrammer].[faktaGrunnlagsdata] gd

	LEFT JOIN
		NVE_DWH.Inntektsrammer.dimType t on gd.Type_Id = t.Type_Id
	LEFT JOIN
		NVE_DWH.Inntektsrammer.dimKjoereIndeks ki on gd.KjoereIndeks_Id = ki.KjoereIndeks_Id
	LEFT JOIN
		NVE_DWH.Inntektsrammer.dimSelskap s on gd.Selskap_Id = s.Selskap_Id
	LEFT JOIN
		NVE_DWH.Inntektsrammer.dimVariabel v on gd.Variabel_Id = v.Variabel_Id
	WHERE
			t.Beskrivelse = 'Varsel' --- Må ha egen logikk på om denne skal hente varsel eller vedtak
		AND
			t.
		AND
			YEAR(CAST([Dato_Id] as date)) = 2023 -- må ersattes med y.cb-1
	ORDER BY InternSelskapsId, y