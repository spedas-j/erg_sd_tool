pro aacgmfindcoeffile, prefix, coefyrlist

  map2d_init
  
  prefix0 = getenv('AACGM_DAT_PREFIX')
  
  if strlen(prefix0) gt 1 then begin  ; If AACGM_DAT_PREFIX env. variable is set
  
    if strpos(prefix0, '\\') ne -1 then begin ;Windows path
      prefix = strjoin( strsplit(prefix0, '\\', /ext), '/' )
    endif else prefix = prefix0
    
  endif else begin  ; If not set, then set the coef. dir in SPEDAS tree.
  
    cmd_paths = strsplit( !path, ';', /ext )
    for i=0L, n_elements(cmd_path)-1 do begin
      cmd_path = cmd_paths[i]
      if file_test(cmd_path+'/aacgmidl.pro') then break
    endfor
    if i eq n_elements(cmd_path) then begin
      print, 'Cannot find aacgmidl.pro!'
      print, 'Seems that the SPEDAS tree is not properly installed!'
      prefix = '' & coefyrlist = [0]
      return
    endif
    
    prefix = cmd_path+'/coef/aacgm_coeffs'
    
  endelse
  
  
  coeffpath = file_search(prefix+'????.asc', /fold_case)
  if strlen(coeffpath[0]) lt 1 then begin
    print, 'Cannot find any AACGM coefficient file!'
    coefyrlist = [0]
    return
  endif
  
  coefyrlist = fix( strmid( coeffpath, 7, 4, /rev ) )
  
  if !map2d.aacgm_dlm_exists and strlen(prefix0) lt 1 then begin
    if !version.os_family ne 'Windows' then begin
      setenv, 'AACGM_DAT_PREFIX='+prefix
    endif else begin
      setenv, 'AACGM_DAT_PREFIX='+strjoin( strsplit(prefix,'/',/ext), '\\' )
    endelse
  endif
  
  return
end

