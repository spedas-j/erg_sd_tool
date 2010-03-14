PRO sd_time, t, quiet=quiet

;Initialize !SDARN system variable
sd_init

;No argument -> print the current time for plotting
npar= n_params()
if npar eq 0 then begin
  print, 'plot_time: '+time_string(!sdarn.sd_polar.plot_time )
  return
endif

;Adopt only the 1st element if mistakenly given as an array
t = t[0]

;Set the plot time
CASE (size(t,/type)) OF
  7 : BEGIN  ;string
    !sdarn.sd_polar.plot_time = time_double(t)
  END
  5 : BEGIN  ;double-precision floating 
    !sdarn.sd_polar.plot_time = time_double(t)
  END
  2 : BEGIN  ;integer, interpreted as 'hhmm'
    if t lt 0 or t gt 2400 then begin
      dprint, 'Invalid sd time'
      return
    endif
    get_timespan, tr & ts = time_struct(tr[0])
    hh = t / 100 & mm = t mod 100 
    time = time_string(ts, tfor='YYYY-MM-DD')+'/'+string(hh,mm,'(I2.2,":",I2.2)')
    !sdarn.sd_polar.plot_time = time_double(time)
  end
  3 : BEGIN  ;integer, interpreted as 'hhmm'
    if t lt 0 or t gt 2400 then begin
      dprint, 'Invalid sd time'
      return
    endif
    get_timespan, tr & ts = time_struct(tr[0])
    hh = t / 100 & mm = t mod 100 
    time = time_string(ts, tfor='YYYY-MM-DD')+'/'+string(hh,mm,'(I2.2,":",I2.2)')
    !sdarn.sd_polar.plot_time = time_double(time)
  end
  
  
  ELSE: BEGIN
    dprint, 'Invalid sd time'
    return
  END
ENDCASE

if ~keyword_set(quiet) then print, 'plot_time: '+time_string(!sdarn.sd_polar.plot_time)


return
end
