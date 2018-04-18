(defrule regla_inicial
	"Comprobacion del TAC"
	=>
	(printout t crlf "SISTEMA DE DIAGNÓSTICO, ESTADIFICACIÓN Y TRATAMIENTO DEL CÁNCER DE PULMÓN" crlf "---------------------------------------------------------------------" crlf crlf)
	(printout t "¿El paciente se ha realizado una Tomografia axial computarizada (TAC)?" crlf)
  (assert (hayTAC (read)))
)
	(defrule tacsi
		?hayTAC <- (hayTAC ?hayTAC-read&si)
         =>
        	(retract ?hayTAC)
	)

	(defrule tacno
	 		?hayTAC <- (hayTAC ?hayTAC-read&no)
         =>
          (retract ?hayTAC)
          (printout t "¿El paciente se ha realizado un PET-TAC?" crlf)
          (assert(hayPETAC (read)))
	)
		(defrule ptacsi
	 		?hayPETAC <- (hayPETAC ?hayPETAC-read&si)
        	=>
      	 	(retract ?hayPETAC)
		)
		(defrule ptacsno
	 		?hayPETAC <- (hayPETAC ?hayPETAC-read&no)
         	=>
          (retract ?hayPETAC)
	       	(printout t "El paciente debe tener realizada una prueba TAC o PET-TAC para poder ser evaluado por el Sistema." crlf)
		)

(defrule regla_T
	"Determinacion del Tamaño del Tumor Primario"
	=>
		(printout t crlf "Determinación del Tamaño del Tumor Primario (T)" crlf "-----------------------------------------------------------------" crlf)
		(printout t "¿Se visualizan masas en el TAC?" crlf)
		(assert (haymasas (read)))
)
	(defrule hay_masas
		?haymasas <- (haymasas ?maymasas-read&si)
		=>
		(retract ?haymasas)
		(printout t "Indique el tamaño de las masas en cm de entre los siguientes: <2, 2-3, 3-5, 5-7, >7" crlf)
		(assert (tamano_masas (read)))
		(printout t "¿Las masas invaden mediastino, corazón, grandes vasos, tráquea, nervio recurrente, esofago, cuerpo vertebral, carina o existen nódulos separados del tumor primario?" crlf)
		(assert (invaden_masas (read)))
	)
		(defrule masa_2cm
			?tamano_masas <- (tamano_masas ?tamano_masas-read&<2)
			?invaden_masas <- (invaden_masas ?invaden_masas-read&no)
			=>
				(retract ?tamano_masas)
				(retract ?invaden_masas)
				(assert (t T1A))
		)
		(defrule masa_2-3cm
			?tamano_masas <- (tamano_masas ?tamano_masas-read&2-3)
			?invaden_masas <- (invaden_masas ?invaden_masas-read&3-5)
			=>
				(retract ?tamano_masas)
				(retract ?invaden_masas)
				(assert (t T1B))
		)
		(defrule masa_3-5cm
			?tamano_masas <- (tamano_masas ?tamano_masas-read&3-5)
			?invaden_masas <- (invaden_masas ?invaden_masas-read&no)
			=>
				(retract ?tamano_masas)
				(retract ?invaden_masas)
				(assert (t T2A))
		)
		(defrule masa_5-7cm
			?tamano_masas <- (tamano_masas ?tamano_masas-read&5-7)
			?invaden_masas <- (invaden_masas ?invaden_masas-read&no)
			=>
				(retract ?tamano_masas)
				(retract ?invaden_masas)
				(assert (t T2B))
		)
		(defrule masa_7cm
			?tamano_masas <- (tamano_masas ?tamano_masas-read&>7)
			?invaden_masas <- (invaden_masas ?invaden_masas-read&no)
			=>
				(retract ?tamano_masas)
				(retract ?invaden_masas)
				(assert (t T3))
		)
		(defrule masas_invaden
			?tamano_masas <- (tamano_masas ?tamano_masas-read&<2|2-3|3-5|5-7|>7)
			?invaden_masas <- (invaden_masas ?invaden_masas-read&si)
			=>
				(retract ?tamano_masas)
				(retract ?invaden_masas)
				(assert (t T4))
		)
	(defrule no_hay_masas
		?haymasas <- (haymasas ?haymasas-read&no)
	=>
		(retract ?haymasas)
		(printout t "¿El paciente se ha realizado una Broncoscopia?" crlf)
		(assert (broncoscopia (read)))
	)
		(defrule broncoscopia_realizada
			?broncoscopia <- (broncoscopia ?broncoscopia-read&si)
		=>
			(retract ?broncoscopia)
			(printout t "¿La Broncoscopia ha resultado positiva?" crlf)
			(assert (broncos_posit (read)))
		)
			(defrule broncoscopia_positiva
				?broncos_posit <- (broncos_posit ?broncos_posit-read&si)
			=>
				(retract ?broncos_posit)
				(assert (t Tis))
			)
			(defrule broncoscopia_negativa
				?broncos_posit <- (broncos_posit ?broncos_posit-read&no)
			=>
				(retract ?broncos_posit)
				(assert (t T0))
			)
		(defrule broncoscopia_no_realizada
			?broncoscopia <- (broncoscopia ?broncoscopia-read&no)
		=>
			(retract ?broncoscopia)
			(printout t "El paciente debe tener realizada una broncoscopia para poder ser evaluado por el Sistema." crlf)
		)

		(defrule regla_N
			"Determinacion de la Afectacion del Ganglio Linfático"
			=>
			(printout t crlf "Determinación de la Afectacion del Ganglio Linfático (N)" crlf "-----------------------------------------------------------------" crlf)
			(printout t "¿Se ven ganglios aumentados en el TAC?" crlf)
			(assert (ganglios (read)))
		)
			(defrule hay_ganglios
				?ganglios <- (ganglios ?ganglios-read&si)
				=>
					(retract ?ganglios)
					(printout t "¿Es posible Biopsiar?" crlf)
					(assert (biopsia (read)))
			)
				(defrule biopsia_si
					?biopsia <- (biopsia ?biopsia-read&si)
					=>
						(retract ?biopsia)
						(printout t "¿Existe Tumor?" crlf)
						(assert (tumor (read)))
				)
					(defrule hay_tumor
						?tumor <- (tumor ?tumor-read&si)
						=>
							(retract ?tumor)
							(printout t crlf "Seleccione el tipo de Ganglios en el Tumor (Escriba 1, 2 ó 3)" crlf)
							(printout t "1. Ganglios peribronquiales, hiliares o intrapulmonares homolaterales" crlf)
							(printout t "2. Ganglios mediastínicos homolaterales o subcarinales" crlf)
							(printout t "3. Ganglios contralaterales o supraclaviculares" crlf)
							(assert (tipo_ganglios (read)))
						)
							(defrule peribronquiales
								?tipo_ganglios <- (tipo_ganglios ?tipo_ganglios-read&1)
								=>
									(retract ?tipo_ganglios)
									(assert (n N1))
							)
							(defrule mediastinicos
								?tipo_ganglios <- (tipo_ganglios ?tipo_ganglios-read&2)
								=>
									(retract ?tipo_ganglios)
									(assert (n N2))
							)
							(defrule contralaterales
								?tipo_ganglios <- (tipo_ganglios ?tipo_ganglios-read&3)
								=>
									(retract ?tipo_ganglios)
									(assert (n N3))
							)
					(defrule no_hay_tumor
						?tumor <- (tumor ?tumor-read&no)
						=>
							(retract ?tumor)
							(assert (n N0))
					)
				(defrule biopsia_no
					?biopsia <- (biopsia ?biopsia-read&no)
					=>
						(retract ?biopsia)
						(assert (n Nx))
				)
			(defrule no_hay_ganglios
				?ganglios <- (ganglios ?ganglios-read&no)
				=>
					(retract ?ganglios)
					(assert (n N0))
			)
		(defrule regla_M
			"Determinacion de la presencia de Metastasis"
			=>
			(printout t crlf "Determinación de la Presencia de Metastasis (M)" crlf "-----------------------------------------------------------------" crlf)
			(printout t "¿Se ve Metástasis en el TAC?" crlf)
			(assert (metastasis (read)))
		)
			(defrule hay_metastasis
				?metastasis <- (metastasis ?metastasis-read&si)
				=>
					(retract ?metastasis)
					(assert (m M1))
			)
			(defrule no_hay_metastasis
				?metastasis <- (metastasis ?metastasis-read&no)
				=>
					(retract ?metastasis)
					(assert (m M0))
			)
		; Determinacion del tratamiento
	(defrule regla_F
		"Determinacion del Tratamiento"
		=>
		(printout t crlf "Tratamiento Recomendado por el Sistema" crlf "-----------------------------------------------------------------" crlf)
	)
		(defrule tratamiento1
				(t T1A)
			=>
				(printout t "Funciona" crlf)

		)
