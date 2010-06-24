;+
; :DESCRIPTION:
;    Initialize the environment for the 2D plot
;
; :NOTE:
;    The AACGM DLM needs to be installed properly in advance.
;
; :AUTHOR: T. Hori
; :HISTORY: 
;       2010/03/10: Created
; $LastChangedBy: $
; $LastChangedDate: $
; $LastChangedRevision: $
; $URL: $
;-
pro sd_init, reset=reset

defsysv,'!sdarn',exists=exists
if not keyword_set(exists) then begin

  defsysv,'!sdarn', $
    { $
      init: 0 $
      ,sd_polar: { $
                  plot_time: 0.D $
                } $
    }
    
endif

if keyword_set(reset) then !sdarn.init=0

if !sdarn.init ne 0 then return


!sdarn.init = 1

;Load the S-H coefficients for AACGM
aacgm_load_coef, 2005

return
end
