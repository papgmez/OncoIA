(defrule regla_inicial
	"Comprobacion del TAC"
	(initial-fact)
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
				(assert (t t1A))
		)
		(defrule masa_2-3cm
			?tamano_masas <- (tamano_masas ?tamano_masas-read&2-3)
			?invaden_masas <- (invaden_masas ?invaden_masas-read&no)
			=>
				(retract ?tamano_masas)
				(retract ?invaden_masas)
				(assert (t t1B))
		)
		(defrule masa_3-5cm
			?tamano_masas <- (tamano_masas ?tamano_masas-read&3-5)
			?invaden_masas <- (invaden_masas ?invaden_masas-read&no)
			=>
				(retract ?tamano_masas)
				(retract ?invaden_masas)
				(assert (t t2A))
		)
		(defrule masa_5-7cm
			?tamano_masas <- (tamano_masas ?tamano_masas-read&5-7)
			?invaden_masas <- (invaden_masas ?invaden_masas-read&no)
			=>
				(retract ?tamano_masas)
				(retract ?invaden_masas)
				(assert (t t2B))
		)
		(defrule masa_7cm
			?tamano_masas <- (tamano_masas ?tamano_masas-read&>7)
			?invaden_masas <- (invaden_masas ?invaden_masas-read&no)
			=>
				(retract ?tamano_masas)
				(retract ?invaden_masas)
				(assert (t t3))
		)
		(defrule masas_invaden
			?tamano_masas <- (tamano_masas ?tamano_masas-read&<2|2-3|3-5|5-7|>7)
			?invaden_masas <- (invaden_masas ?invaden_masas-read&si)
			=>
				(retract ?tamano_masas)
				(retract ?invaden_masas)
				(assert (t t4))
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
				(assert (t tis))
			)
			(defrule broncoscopia_negativa
				?broncos_posit <- (broncos_posit ?broncos_posit-read&no)
			=>
				(retract ?broncos_posit)
				(assert (t t0))
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
									(assert (n n1))
							)
							(defrule mediastinicos
								?tipo_ganglios <- (tipo_ganglios ?tipo_ganglios-read&2)
								=>
									(retract ?tipo_ganglios)
									(assert (n n2))
							)
							(defrule contralaterales
								?tipo_ganglios <- (tipo_ganglios ?tipo_ganglios-read&3)
								=>
									(retract ?tipo_ganglios)
									(assert (n n3))
							)
					(defrule no_hay_tumor
						?tumor <- (tumor ?tumor-read&no)
						=>
							(retract ?tumor)
							(assert (n n0))
					)
				(defrule biopsia_no
					?biopsia <- (biopsia ?biopsia-read&no)
					=>
						(retract ?biopsia)
						(assert (n nx))
				)
			(defrule no_hay_ganglios
				?ganglios <- (ganglios ?ganglios-read&no)
				=>
					(retract ?ganglios)
					(assert (n n0))
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
					(assert (m m1))
			)
			(defrule no_hay_metastasis
				?metastasis <- (metastasis ?metastasis-read&no)
				=>
					(retract ?metastasis)
					(assert (m m0))
			)
			(defrule regla_Tratamiento0
			"Determinacion del Tratamiento del Paciente"
			=>
				(printout t crlf "Determinacion del Tratamiento" crlf "-----------------------------------------------------------------" crlf)
				(printout t "¿El paciente es operable?" crlf)
				(assert (operable (read)))
			)
			(defrule regla_Tratamiento1
			"Determinacion del Tratamiento del Paciente"
			=>
				(printout t "¿El Tumor es Resecable?" crlf)
				(assert (resecable (read)))
			)
	(defrule regla_t1
		"T1A N0 M0, T1B N0 M0 ó T2A N0 M0, operable y resecable"
		(or (t t1A)
				(t t1B)
				(t t2A))
		(n n0)
		(m m0)
		?operable <- (operable ?operable-read&si)
		?resecable <- (resecable ?resecable-read&si)
		=>
		(retract ?operable)
		(retract ?resecable)
		;(retract t)
		;(retract n)
		;(retract m)
		(printout t crlf "El Tratamiento Recomendado por el Sistema es ")
		(printout t "Reseccion del Tumor y Linfadenectomia Parcial." crlf crlf)
	)
	(defrule regla_t2
		"T1A N0 M0, T1B N0 M0 ó T2A N0 M0 y no operable"
		(or (t t1A)
				(t t1B)
				(t t2A))
		(n n0)
		(m m0)
		?operable <- (operable ?operable-read&no)
		?resecable <- (resecable ?resecable-read&si|no)
		=>
		(retract ?operable)
		(retract ?resecable)
		;(retract t)
		;(retract n)
		;(retract m)
		(printout t crlf "El Tratamiento Recomendado por el Sistema es ")
		(printout t "Radioterapia Radical sobre el Tumor y las Cadenas Ganglionares." crlf crlf)
	)
	(defrule regla_t3
		"T1A N0 M0, T1B N0 M0 ó T2A N0 M0, operable y no resecable"
		(or (t t1A)
				(t t1B)
				(t t2A))
		(n n0)
		(m m0)
		?operable <- (operable ?operable-read&si)
		?resecable <- (resecable ?resecable-read&no)
		=>
		(retract ?operable)
		(retract ?resecable)
		;(retract t)
		;(retract n)
		;(retract m)
		(printout t crlf "El Tratamiento Recomendado por el Sistema es ")
		(printout t "Quimioterapia Preoperatoria, Cirugia y Quimioterapia Adyuvante." crlf crlf)
	)
	(defrule regla_t4
		"Tis N0 M0"
		(t tis)
		(n n0)
		(m m0)
		?operable <- (operable ?operable-read&si|no)
		?resecable <- (resecable ?resecable-read&si|no)
		=>
		(retract ?operable)
		(retract ?resecable)
		;(retract t)
		;(retract n)
		;(retract m)
		(printout t crlf "El Tratamiento Recomendado por el Sistema es ")
		(printout t "Resecion Conservadora." crlf crlf)
	)
	(defrule regla_t5
		"Tis N1 M0, T1A/T1B N1 M0, T2A N1 M0, T2B N0 M0, T2B N1 M0 ó T3 N0 M0, operable y resecable"
		(or ((t tis) (n n1) (m m0))
				((t t1A) (n n1) (m m0))
				((t t1B) (n n1) (m m0))
				((t t2A) (n n1) (m m0))
				((t t2B) (n n0) (m m0))
				((t t2B) (n n1) (m m0))
				((t t3) (n n0) (m m0))
		)
		?operable <- (operable ?operable-read&si)
		?resecable <- (resecable ?resecable-read&si)
		=>
		(retract ?operable)
		(retract ?resecable)
		;(retract t)
		;(retract n)
		;(retract m)
		(printout t crlf "El Tratamiento Recomendado por el Sistema es ")
		(printout t "Reseccion del Tumor, Linfadenectomia y Quimioterapia Adyuvante." crlf crlf)
	)
	(defrule regla_t6
		"Tis N1 M0, T1A/T1B N1 M0, T2A N1 M0, T2B N0 M0, T2B N1 M0 ó T3 N0 M0, operable y no resecable"
		(or ((t tis) (n n1) (m m0))
				((t t1A) (n n1) (m m0))
				((t t1B) (n n1) (m m0))
				((t t2A) (n n1) (m m0))
				((t t2B) (n n0) (m m0))
				((t t2B) (n n1) (m m0))
				((t t3) (n n0) (m m0))
		)
		?operable <- (operable ?operable-read&si)
		?resecable <- (resecable ?resecable-read&no)
		=>
		(retract ?operable)
		(retract ?resecable)
		;(retract t)
		;(retract n)
		;(retract m)
		(printout t crlf "El Tratamiento Recomendado por el Sistema es ")
		(printout t "Quimioterapia preoperatoria, Cirugia y Quimioterapia Adyuvante." crlf crlf)
	)
	(defrule regla_t7
		"Tis N1 M0, T1A/T1B N1 M0, T2A N1 M0, T2B N0 M0, T2B N1 M0 ó T3 N0 M0 y no operable"
		(or ((t tis) (n n1) (m m0))
				((t t1A) (n n1) (m m0))
				((t t1B) (n n1) (m m0))
				((t t2A) (n n1) (m m0))
				((t t2B) (n n0) (m m0))
				((t t2B) (n n1) (m m0))
				((t t3) (n n0) (m m0))
		)
		?operable <- (operable ?operable-read&no)
		?resecable <- (resecable ?resecable-read&si|no)
		=>
		(retract ?operable)
		(retract ?resecable)
		;(retract t)
		;(retract n)
		;(retract m)
		(printout t crlf "El Tratamiento Recomendado por el Sistema es ")
		(printout t "Radioterapia Radical sobre el Tumor y las Cadenas Ganglionares." crlf crlf)
	)
	(defrule regla_t8
		"Tis/T1/T2 N2 M0, T3 N1/N2 M0 ó T4 N0/N1 M0, operable y tanto resecable como no resecable"
		(or (((or (t tis) ((or (t t1A) (t t1B))) ((or (t t2A) (t t2B))))) (n n2) (m m0))
				((t t3) ((or (n n1) (n n2))) (m m0))
				((t t4) ((or (n n0) (n n1))) (m m0))
		)
		?operable <- (operable ?operable-read&si)
		?resecable <- (resecable ?resecable-read&si|no)
		=>
		(retract ?operable)
		(retract ?resecable)
		;(retract t)
		;(retract n)
		;(retract m)
		(printout t crlf "El Tratamiento Recomendado por el Sistema es ")
		(printout t "Quimioterapia, Radioterapia y re-estadificar." crlf crlf)
		)
	(defrule regla_t9
		"Tis/T1/T2 N2 M0, T3 N1/N2 M0 ó T4 N0/N1 M0 y no operable"
		(or (((or (t tis) ((or (t t1A) (t t1B))) ((or (t t2A) (t t2B))))) (n n2) (m m0))
				((t t3) ((or (n n1) (n n2))) (m m0))
				((t t4) ((or (n n0) (n n1))) (m m0))
		)
		?operable <- (operable ?operable-read&no)
		?resecable <- (resecable ?resecable-read&si|no)
		=>
		(retract ?operable)
		(retract ?resecable)
		;(retract t)
		;(retract n)
		;(retract m)
		(printout t crlf "El Tratamiento Recomendado por el Sistema es ")
		(printout t "Quimioterapia y Radioterapia Concomitante." crlf crlf)
		)
	(defrule regla_t10
		"T4 N2 M0 ó Tis/T1/T2/T3/T4 N3 M0, operable y resecable"
		(or ((t t4) (n n2) (m m0))
				(((or (t tis) ((or (t t1A) (t t1B))) ((or (t t2A) (t t2B))) (t t3) (t t4))) (n n3) (m m0))
		)
		?operable <- (operable ?operable-read&si)
		?resecable <- (resecable ?resecable-read&si)
		=>
		(retract ?operable)
		(retract ?resecable)
		;(retract t)
		;(retract n)
		;(retract m)
		(printout t crlf "El Tratamiento Recomendado por el Sistema es ")
		(printout t "Quimioterapia y Radioterapia Concomitante o Secuencial." crlf crlf)
		)
	(defrule regla_t11
		"T4 N2 M0 ó Tis/T1/T2/T3/T4 N3 M0, operable y no resecable"
		(or ((t t4) (n n2) (m m0))
				(((or (t tis) ((or (t t1A) (t t1B))) ((or (t t2A) (t t2B))) (t t3) (t t4))) (n n3) (m m0))
		)
		?operable <- (operable ?operable-read&si)
		?resecable <- (resecable ?resecable-read&no)
		=>
		(retract ?operable)
		(retract ?resecable)
		;(retract t)
		;(retract n)
		;(retract m)
		(printout t crlf "El Tratamiento Recomendado por el Sistema es ")
		(printout t "Pleurodesis y Tratamiento M1." crlf crlf)
		)
	(defrule regla_t12
		"T4 N2 M0 ó Tis/T1/T2/T3/T4 N3 M0 y no operable"
		(or ((t t4) (n n2) (m m0))
				(((or (t tis) ((or (t t1A) (t t1B))) ((or (t t2A) (t t2B))) (t t3) (t t4))) (n n3) (m m0))
		)
		?operable <- (operable ?operable-read&no)
		?resecable <- (resecable ?resecable-read&si|no)
		=>
		(retract ?operable)
		(retract ?resecable)
		;(retract t)
		;(retract n)
		;(retract m)
		(printout t crlf "El Tratamiento Recomendado por el Sistema es ")
		(printout t "Quimioterapia y Radioterapia Concomitante o Secuencial." crlf crlf)
	)
	(defrule regla_t13
		"Tis/T1/T2/T3/T4 N0/N1/N2/N3 M1"
		((or (t tis) ((or (t t1A) (t t1B))) ((or (t t2A) (t t2B))) (t t3) (t t4)))
		((or (n n0) (n n1) (n n2) (n n3))) 
		(m m1)
		?operable <- (operable ?operable-read&no)
		?resecable <- (resecable ?resecable-read&si|no)
		=>
		(retract ?operable)
		(retract ?resecable)
		;(retract t)
		;(retract n)
		;(retract m)
		(printout t crlf "El Tratamiento Recomendado por el Sistema es ")
		(printout t "Quimioterapia y Radioterapia Concomitante o Secuencial." crlf crlf)
	)
