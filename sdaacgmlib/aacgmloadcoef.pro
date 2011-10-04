;+
; PROCEDURE sd_aacgmlib 
; 
; :DESCRIPTION:
; A dummy procedure to compile the AACGM wrapper procedure/function 
; defined below by just type "sd_aacgmlib" 
; 
; The wrapper procedures/functions check !sdarn.aacgm_dlm_exists 
; (if not defined, then define it by sd_init) to select appropriate 
; AACGM routines (DLM, or IDL native ones attached to TDAS). 
; 
; :AUTHOR: 
;   Tomo Hori (E-mail: horit@stelab.nagoya-u.ac.jp)
;   
; :HISTORY:
;   2010/12/02: created and got through the initial bug fixes
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


