library(googlesheets4)
library(tidyverse)
library(sf)

libDB <- "C:/Users/micha/CPAL Dropbox/"

repairDallas <- read_sheet(ss = "https://docs.google.com/spreadsheets/d/1dbQ-Vv01X3pDB_AIhp9C77Fs16o73W1o8y_nOU_UVb8/",
                           sheet = "RWJ Housing Quality Draft",
                           col_names = TRUE)

names(repairDallas)
#rio::export(surveynames, "Data/Survey Question Column Names.csv")

repairRename <- repairDallas %>%
  rename(
    streetname	=		"What street do you live on?\n\n�En qu� calle vive usted?" 		,
    streetnum	=		"What is the street number of this house?

�Cu�l es el n�mero de la calle de esta casa?"		,
    age	=		"What is your age?

�Qu� edad tiene?"		,
    gender	=		"Which of the following best describes your gender identity?

�Cu�l de las siguientes opciones se adecua m�s a su identidad de g�nero?"		,
    ethnicity	=		"Which of the following best describes your Ethnicity?

�Cu�l de las siguientes opciones describe mejor su origen �tnico?"		,
    race	=		"Which of the following best describes your Race?

�Cu�l de las siguientes describe mejor su raza?"		,
    tot_adults	=		"How many adults, including yourself, currently live or stay in your home?

�Cu�ntos adultos, incluyendo usted mismo, viven o se quedan actualmente en su hogar?"		,
    over65	=		"Are you or any of the adults living or staying in the home 65 or older?

�Es usted o alguno de los adultos que viven o se quedan en el hogar de 65 a�os o m�s?"		,
    tot_u18	=		"How many children under 18 currently live or stay in your home?

�Cu�ntos ni�os menores de 18 a�os viven o permanecen actualmente en su hogar?"		,
    pregnant	=		"Are you or is a member of your household currently pregnant?

�Usted, o alg�n miembro de su hogar, est� actualmente embarazada?"		,
    tot_u6	=		"Are any of the children living or staying in your home 5 or younger?

�Alguno de ni�os que vive o se queda en su casa tiene 5 a�os o menos?"		,
    frequent_u6	=		"If no children younger than 6 live full time in the home, do any children younger than 6 visit here frequently? 

Si ning�n ni�o menor de 6 a�os vive a tiempo completo en el hogar, �alg�n ni�o menor de 6 a�os le visita con frecuencia?"		,
    income_hh1	=		"To the best of your knowledge, what is your total before-tax household income over the past year?

A lo mejor de su conocimiento, �cu�l fue su ingreso familiar total antes de impuestos durante el a�o pasado?...13"		,
    income_hh2	=		"To the best of your knowledge, what is your total before-tax household income over the past year?

A lo mejor de su conocimiento, �cu�l fue su ingreso familiar total antes de impuestos durante el a�o pasado?...14"		,
    income_hh3	=		"To the best of your knowledge, what is your total before-tax household income over the past year?

A lo mejor de su conocimiento, �cu�l fue su ingreso familiar total antes de impuestos durante el a�o pasado?...15"		,
    income_hh4	=		"To the best of your knowledge, what is your total before-tax household income over the past year?

A lo mejor de su conocimiento, �cu�l fue su ingreso familiar total antes de impuestos durante el a�o pasado?...16"		,
    income_hh5	=		"To the best of your knowledge, what is your total before-tax household income over the past year?

A lo mejor de su conocimiento, �cu�l fue su ingreso familiar total antes de impuestos durante el a�o pasado?...17"		,
    income_hh6	=		"To the best of your knowledge, what is your total before-tax household income over the past year?

A lo mejor de su conocimiento, �cu�l fue su ingreso familiar total antes de impuestos durante el a�o pasado?...18"		,
    income_hh7	=		"To the best of your knowledge, what is your total before-tax household income over the past year?

A lo mejor de su conocimiento, �cu�l fue su ingreso familiar total antes de impuestos durante el a�o pasado?...19"		,
    income_hh8	=		"To the best of your knowledge, what is your total before-tax household income over the past year?

A lo mejor de su conocimiento, �cu�l fue su ingreso familiar total antes de impuestos durante el a�o pasado?...20"		,
    home_status	=		"Which of the following best describes the status of this home?

Cu�l de las siguientes opciones describe mejor el estado de este hogar?"		,
    home_title	=		"Do you have the title to the property?

�Tiene t�tulo sobre la propiedad?"		,
    home_insurance	=		"Do you have homeowners insurance?

�Tiene seguro de propietario de vivienda?"		,
    homesteadexempt	=		"Have you filed for a homestead exemption for your property taxes?

�Ha solicitado una exenci�n de vivienda para sus impuestos a la propiedad?"		,
    rent_lease	=		"Do you have an active lease for the home that you are renting?

�Tiene un contrato de arrendamiento activo para la casa que est� alquilando?"		,
    rent_insurance	=		"Do you have renter's insurance?

�Tiene seguro de inquilino?"		,
    home_1979	=		"To the best of your knowledge, was this home built before 1979?

�Esta casa fue construida antes de 1979?"		,
    water_damage	=		"Have you noticed any of the following in the past year: wet spots in the walls or ceiling, leaks, dampness, or mold?

�Ha notado alguno de los siguientes en el �ltimo a�o: manchas h�medas en las paredes o el techo, fugas, humedad o moho?"		,
    water_evidence	=		"Please describe the evidence of water damage that you have seen.

Por favor, describa la evidencia de da�os por agua que haya visto."		,
    water_repairs	=		"Have you made any repairs to address these issues?\n\n�Ha hecho alguna reparaci�n para abordar estos problemas?...30",
    water_llreport	=		"Have you reported these issues to your landlord?\n\n�Ha informado de estos problemas a su arrendador?...31"		,
    water_llrepair	=		"Has your landlord addressed these issues?\n\n�Su propietario ha abordado estos problemas?...32"		,
    safety_smoke	=		"Do you have a working smoke detector?

�Tiene un detector de humo en funcionamiento?"		,
    safety_carbonmono	=		"Do you have a working carbon monoxide detector? 

�Tiene un detector de mon�xido de carbono en funcionamiento?"		,
    safety_extinguisher	=		"Do you have a working fire extinguisher? 

�Tiene un extintor de incendios en funcionamiento?"		,
    hazards_fall	=		"Are there any hazards in the home that could cause a fall or injury?

�Hay alg�n peligro en el hogar que pueda causar una ca�da o lesi�n?"		,
    hazards_other	=		"What kinds of hazards are you worried about?

�Qu� tipo de peligros le preocupan?"		,
    hazards_repairs	=		"Have you made any repairs to address these issues?\n\n�Ha hecho alguna reparaci�n para abordar estos problemas?...38" 		,
    hazards_llreport	=		"Have you reported these issues to your landlord?\n\n�Ha informado de estos problemas a su arrendador?...39"		,
    hazards_llrepair	=		"Has your landlord addressed these issues?\n\n�Su propietario ha abordado estos problemas?...40"		,
    pests_service	=		"Does your landlord or property manager provide regular preventative service to manage rodents or bugs?

�Su propietario o administrador de la propiedad proporciona un servicio preventivo regular para controlar roedores o insectos?"		,
    pests_llreport	=		"Have you ever asked your landlord to set traps or call an exterminator for rodents or bugs?

�Alguna vez le ha pedido al propietario que ponga trampas o llame a un exterminador de roedores o insectos?"		,
    pests_traps	=		"Do you ever have to set traps or call an exterminator for rodents or bugs?

�Alguna vez ha tenido que poner trampas o llamar a un exterminador de roedores o insectos?"		,
    pests_worry	=		"What pests are you worried about?

�Qu� plagas le preocupan?"		,
    smoke_live	=		"Does a smoker live in the home?

�Un fumador vive en casa?"		,
    smoke_inside	=		"Do they smoke inside the home?

�Fuman dentro de la casa?"		,
    gas_source	=		"To the best of your knowledge, do any of the following items in your home use gas as a heating source?

Seg�n su leal saber y entender, �alguno de los siguientes elementos en su hogar usa gas como fuente de calefacci�n?"		,
    windows_open	=		"Do your windows open?

�Las ventanas pueden abrirse?"		,
    airfilter	=		"Do you know the last time your air filter was changed?

�Sabe cu�ndo fue la �ltima vez que cambiaron el filtro de aire?"		,
    airfilter_change	=		"When was the last time your air filter was changed?

�Cu�ndo fue la �ltima vez que se cambi� el filtro de aire?"		,
    tempsummer	=		"How do you keep your home cool in the summer?

C�mo mantiene su casa fresca en verano? Marque todas las que quiera."		,
    comfortheat	=		"On the hottest days in the summer, are you comfortable in your home?

En los d�as m�s calurosos del verano, �se siente c�modo(a) en su casa?"		,
    tempwinter	=		"How do you keep your home warm in the winter?

�C�mo mantiene su casa caliente en invierno? Marque todas las opciones que correspondan."		,
    comfortcold	=		"On the coldest days in the winter, are you comfortable in your home?

En los d�as m�s fr�os del invierno, �se siente c�modo(a) en su casa?"		,
    hotwater	=		"When you take a shower, do you have sufficient hot water?

Cuando se ducha, �tiene suficiente agua caliente?"		,
    repairs_needed	=		"Are there any repairs needed in the interior or exterior of your home?

�Se necesitan reparaciones en el interior o exterior de su casa?"		,
    repairs_interior	=		"In your judgment, what repairs are needed in the interior of your home?

A su juicio, �qu� reparaciones se necesitan en el interior de su hogar?"		,
    repairs_exterior	=		"In your judgement, what repairs are needed on the exterior of your home?

A su juicio, �qu� reparaciones se necesitan en el exterior de su casa?"		,
    repairs_completed	=		"Have you made any repairs to address these issues?\n\n�Ha hecho alguna reparaci�n para abordar estos problemas?...59"		,
    repairs_llreport	=		"Have you reported these issues to your landlord?\n\n�Ha informado de estos problemas a su arrendador?...60"		,
    repairs_llrepair	=		"Has your landlord addressed these issues?\n\n�Su propietario ha abordado estos problemas?...61"		,
    request_response	=		"In general, how responsive is your landlord when you make a service request?

En general, �qu� tan receptivo es su propietario cuando realiza una solicitud de servicio?"		,
    hrp_available	=		"Are you aware of any specific home repair programs that may be available to you?

�Conoce alg�n programa espec�fico de reparaci�n de viviendas que pueda estar disponible para usted?"		,
    hrp_knowledge	=		"Please list any information you have about the home repairs program you are aware of.

Por favor, enumere cualquier informaci�n que tenga sobre el programa de reparaciones en el hogar que conoce."		,
    hrp_information	=		"In what ways do you receive news and information about home repair or other economic resources or programs?

�De qu� manera recibe noticias e informaci�n sobre la reparaci�n del hogar u otros recursos o programas econ�micos?"		,
    hrp_inspection	=		"In the future, would you be comfortable with an interior inspection of the home? In other words, would you be comfortable if someone came inside the home to identify and record possible repair needs?

En el futuro, �se sentir�a c�modo con una inspecci�n interior de la casa? En otras palabras, �se sentir�a c�modo si alguien entrara a la casa para identificar y registrar posibles necesidades de reparaci�n?"		,
    giftcard	=		"Before giving the gift card to the resident, please enter the last 4 digits on the card for tracking purposes.

Antes de entregar la tarjeta de regalo al residente, por favor ingrese los �ltimos 4 d�gitos de la tarjeta para fines de seguimiento."		,
    user_id	=		user_id		,
    house	=		house		,
    street	=		street		,
    hh_size	=		hh_size		,
    score	=		score		,
    submitted	=		'Submitted At'		,
    token	=		Token
  )

repairPivot <- repairRename %>%
  mutate(income_hh0 = ifelse(!is.na(income_hh1), TRUE, FALSE)+ifelse(!is.na(income_hh2), TRUE, FALSE)+ifelse(!is.na(income_hh3), TRUE, FALSE)+ifelse(!is.na(income_hh4), TRUE, FALSE)+ ifelse(!is.na(income_hh5), TRUE, FALSE)+ifelse(!is.na(income_hh6), TRUE, FALSE)+ifelse(!is.na(income_hh7), TRUE, FALSE)+ifelse(!is.na(income_hh8), TRUE, FALSE),
         income_hh0 = as.character(income_hh0)) %>%
  pivot_longer(cols = starts_with("income_hh"),
               names_to = "householdsize",
               names_prefix = "income_hh",
               values_to = "incomeband") %>%
  filter(!is.na(incomeband) & incomeband != "1") %>%
  mutate(incomeband = ifelse(incomeband == "0", NA, incomeband),
         incomethreshold = ifelse(str_detect(incomeband, "0 to "), TRUE, FALSE))

rio::export(repairPivot, "Data/Repairing Dallas Survey Responses.csv")