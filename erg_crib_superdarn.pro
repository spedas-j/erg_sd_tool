;+
; :DESCRIPTION:
;    A crib sheet to demonstrate how procedures/functions for 
;    SuperDARN data work.
;
;    You can run this crib sheet by copying&pasting each commands 
;    below into a IDL window. 
;    Or just compile and run through all by the following command: 
;    .run erg_crib_superdarn.pro
;
; :AUTHOR: T. Hori
; :HISTORY:
;    2010/06/24: Created
;
; $LastChangedBy: $
; $LastChangedDate: $
; $LastChangedRevision: $
; $URL: $
;-

;Initialize 
thm_init 

;Set the date and time for loading data
timespan, '2007-06-21', 1, /day 

;Load the data
erg_load_sdfit, site='hok' 

;View the loaded data names
tplot_names

;Plot some data
window, 0, xsize=600, ysize=750
tplot, ['sd_hok_pwr_0', 'sd_hok_vlos_0', 'sd_hok_spec_width_0']

;Change the time range of the plot
tlimit, ['2007-06-21/13:00','2007-06-21/15:00']

;Divide the data into those for separate beams
splitbeam, ['sd_hok_pwr_0', 'sd_hok_vlos_0', 'sd_hok_spec_width_0']

;Plot data for a specific beam
tplot, ['sd_hok_pwr_0_azim04', 'sd_hok_vlos_0_azim04', $
        'sd_hok_spec_width_0_azim04']

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Commands for 2-dimensional plotting on the ground map are 
; still being worked on, comming up soon!
;

end

