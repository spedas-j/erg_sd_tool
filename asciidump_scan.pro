pro asciidump_scan, tvars, dir=dir

  npar = n_params()
  if npar ne 1 then return  ;Exit unless only one argument is given.
  if strlen((tnames(tvars))[0]) lt 6 then return ;Exit if given tplot variable does not exist.
  
  if ~keyword_set(dir) then dir = 'asciidump'
  if ~file_test(dir, /dir) then file_mkdir, dir
  cd, current=cwdir
  dirpath = filepath( dir, sub='', root_dir=cwdir )
  
  tvar = tvars
  scanarr = get_scan_struc_arr(tvar)
  
  for n=0L, 0 do begin
  
    time = scanarr.x[n]
    scan = reform( scanarr.y[n,*,*] )
    fn = tvar+'_'+time_string(time,tfor='YYYYMMDDhhmm')+'.dat'
    fpath = filepath( fn, sub='', root_dir=dirpath )
    openw, fp, fpath, /get_lun
    printf, fp, '# beam  range_gate  LOSV  MLTdir-bmdir_angle'
    for bm=0, n_elements(scan[0,*])-1 do begin
      for rg=0, n_elements(scan[*,0])-1 do begin
        printf, fp, bm, rg, scan[rg,bm], $
          format='(I2,1X,I3,1X,F7.1)'
      endfor
    endfor
    
    free_lun, fp
    
  endfor
  
  
  
  
  return
end
