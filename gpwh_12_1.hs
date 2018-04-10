type FirstName = String
type LastName = String
type Age = Int
type Height = Int

patientInfo :: PatientName -> Age -> Height -> String
patientInfo patientName age height = firstName patientName ++ " " ++ lastName patientName ++ " " ++ show age ++ " " ++ show height

type PatientName = (String,String)

firstName :: PatientName -> String
firstName patient = fst patient

lastName :: PatientName -> String
lastName patient = snd patient



