PRO get_fixed_range_gate, vn, beam=beam, range_gate=rgate
  
  ;Check the arguments and keywords
  npar = n_params()
  if npar ne 1 then return
  if ~keyword_set(beam) or ~keyword_set(rgate) then return
  beam = fix(beam) & rgate = fix(rgate)
  if beam lt 0 or beam gt 22 or rgate lt 0 or rgate gt 220 then return
  if (tnames(vn))[0] eq '' then return
  
  ;strings consisting of variable names
  prefix = strmid(vn, 0,7) ;e.g, 'sd_hok_'
  suf = strmid(vn,0,1,/reverse) 
  azm_vn = prefix+'azim_no_'+suf
  
  ;Get data from tplot vars
  get_data, vn, data=d, dl=var_dl, lim=var_lim
  vartime = d.x & var = d.y & var_v = d.v
  get_data, azm_vn, data=d 
  azmno = d.y 
  
  if beam gt max(azmno,/nan) or rgate ge n_elements(var[0,*]) then begin
    print, 'Given beam no (',beam,') or rgate no (',rgate,') is out of range'
    return
  endif
  
  idx_bm = where( azmno eq beam )
  newtime = vartime[idx_bm]
  newvar = var[idx_bm,rgate]
  if (size(var_v))[0] eq 2 then new_v=var_v[idx_bm,*] else new_v=var_v
  
  newvn = vn +'_bm'+string(beam,'(I02)')+'rg'+string(rgate,'(I03)')
  store_data, newvn, $
    data={x:newtime, y:newvar, v:new_v}, $
    lim={ytitle:var_lim.ztitle, yrange:var_lim.zrange}
   
  return
end
