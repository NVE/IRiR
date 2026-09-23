SELECT [Grunnlagsdata_Id]
      ,[Kjoering_Id]
	  ,s.Organisasjonsnummer
	  ,s.InternSelskapsId
	  ,s.Selskapsnavn
      ,gd.[Type_Id]
      ,gd.[Variabel_Id]
	  ,v.Variabelnavn
      ,YEAR(CAST([Dato_Id] as date)) as 'y.cb'
      ,YEAR(CAST([HistorikkAar] as date)) as 'y'
      ,gd.[Selskap_Id]
      ,[Verdi]
      ,gd.[KjoereIndeks_Id]
      ,gd.[AuditKey]
	  ,ki.KjoereIndeks
	  ,t.Beskrivelse
	  ,t.ErNyesteType
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
			t.Beskrivelse = 'Vedtak' --- Må ha egen logikk på om denne skal hente varsel eller vedtak
		AND
			YEAR(CAST([Dato_Id] as date)) = 2022 -- må ersattes med y.cb-1
		AND
			t.ErNyesteType = 0
	ORDER BY InternSelskapsId, y