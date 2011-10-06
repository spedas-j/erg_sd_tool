;+
; PROCEDURE aacgmloadcoef 
; 
; :PURPOSE:
; A wrapper procedure to choose AACGM DLM or IDL-native routines 
; to load the coefficients of AACGM conversion. Actually this procedure 
; does load the coefficients for given year if AACGM DLM is available. 
; 
; The wrapper procedures/functions check !sdarn.aacgm_dlm_exists 
; (if not defined, then define it by sd_init) to select appropriate 
; AACGM routines (DLM, or IDL native ones attached to TDAS). 
; 
; :Params:
;   year:   4-digit year for which the AACGM coefficients are loaded.
; 
; :Examples:
;   aacgmloadcoef, 2005
;   
; :AUTHOR: 
;   Tomo Hori (E-mail: horit@stelab.nagoya-u.ac.jp)
;   
; :HISTORY:
;   2011/10/04: created and got through the initial bug fixes
;
; $LastChangedBy: $
; $LastChangedDate: $
; $LastChangedRevision: $
; $URL: $
;-
pro aacgmloadcoef, year

;Initialize !sdarn if not defined
help, name='!sdarn',out=out
if out eq '' then sd_init

;Only AACGM DLM has a subroutine to load the S-H coefficients for given year
if !sdarn.aacgm_dlm_exists then begin 
  ;print, 'using AACGM_DLM'
  aacgm_load_coef, year
endif 

return
end


