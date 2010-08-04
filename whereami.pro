Function WhereAmI
;returns the directory location of the program file containing the routine
;that calls this function.
help,calls=a
callerfile=a[1]
p0=strpos(callerfile,'<')
IF !VERSION.OS_FAMILY EQ 'unix' THEN slash='/' ELSE slash='\'
IF FLOAT(!VERSION.RELEASE) LT 5.3 Then p1=RSTRPOS(callerfile,slash ) $
ELSE p1=strpos(callerfile,slash,/REVERSE_SEARCH)
result=strmid(callerfile,p0+1,p1-p0)
return,result
end
