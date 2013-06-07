-module(c_klha_jwst). 
-export([]). 


collect(FunSend, KpiNames) ->
	Data1 = FunSend(3, 0, 2)
