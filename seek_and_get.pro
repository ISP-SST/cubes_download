; docformat = 'rst'

;+
; Search for datacubes in SVO database and dowload them.
; 
; :Categories:
;
;    SST archive
; 
; 
; :Author:
; 
;    Oleksii Andriienko, Institute for Solar Physics
;
; 
; :Keywords:
;
;    dir : in, optional, type=string
;
;       The directory where to download files.
;
;    any : in, optional, type=bool
;
;       Skip selection menus (except of instruments).
; 
;    no_proprietary : in, optional, type=boolean
;
;       Don't show proprietary cubes.
;
;    verbose : in, optional, type=boolean
;
;       Print more info.
;
;    nodownload : in, optional, type=boolean
;
;       Don't actually download cubes.
;
;    verbose : in, optional, type=bool
;
;       Print more info.
; 
; 
; :History:
; 
;    2020-12-04 : OA. First version.
; 
;-
pro seek_and_get, dir = dir, any=any, no_proprietary=no_proprietary, verbose=verbose, nodownload=nodownload

  print, "Choose an instrument:"
  print
  print," [0]: CRISP"
  print," [1]: CHROMIS"
;;  print," [2]: TRIPPEL"
  print
  read, " > ", ans
  case ans of
     0: instrument = 'CRISP'
     1: instrument = 'CHROMIS'
;;     2: intrument = 'TRIPPEL'
  endcase

;;  dataset = (get_datasets(name = instrument))[0]

  print
  if ~keyword_set(any) then begin
     print, "Enter dates of observations (press 'Enter' for any): "
     print
     date_beg = ''
     date_end = ''
     read," Begin date (YYYY-MM-DD) > ", date_beg
     if date_beg eq '' then date_beg = '1972-01-01'  
     print
     read," End date (YYYY-MM-DD) > ", date_end
     if date_end eq '' then date_end = '2072-01-01'

     print
     print, "Enter time of observations (press 'Enter' for any): "
     print
     time_beg = ''
     time_end = ''
     read," Begin time (hh:mm:ss) > ", time_beg
     if time_beg eq '' then time_beg = '00:00:01'
     print
     read," End time (hh:mm:ss) > ", time_end
     if time_end eq '' then time_end = '23:59:59'

     print
     print, "Enter wavelengths (press 'Enter' for any): "
     print
     wavelnth_min = ''
     wavelnth_max = ''
     read, "Wavelength min, nm > ",wavelnth_min
     if wavelnth_min eq '' then wavelnth_min = '300' 
     print
     read,"Wavelength max, nm > ",wavelnth_max
     if wavelnth_max eq '' then wavelnth_max = '1500'

     if instrument eq 'CRISP' then begin
        print
        print, "Choose type of data:"
        print
        print," [0]: Polarimetric"
        print," [1]: Non-polarimetric"
        print," [2]: Any"
        print
        read," > ", ans
        case ans of
           0: naxis4 = 4
           1: naxis4 = 1
           2: naxis4 = 0
        endcase
     endif else naxis4 = 0
  endif else begin
     date_beg = '1972-01-01'
     date_end = '2072-01-01'
     time_beg = '00:00:01'
     time_end = '23:59:59'
     wavelnth_min = '200'
     wavelnth_max = '2000'
     naxis4 = 0
  endelse

  ;; Add solar coordinates selection
  
  ;; Get the current date
  time = Systime(UTC=Keyword_Set(utc))
  day = String(StrMid(time, 8, 2), Format='(I2.2)') ; Required because UNIX and Windows differ in time format.
  month = StrUpCase(Strmid(time, 4, 3))
  year = Strmid(time, 20, 4)
  months = ['JAN', 'FEB', 'MAR', 'APR', 'MAY', 'JUN', 'JUL', 'AUG', 'SEP', 'OCT', 'NOV', 'DEC']
  m = (where(months EQ month)) + 1
  current_date =  year + '-' + string(m, FORMAT='(I2.2)') + '-' + day

  dataset = get_datasets(name = instrument)
  offset = 0
  limit = 10
  credentials = 0B
  repeat begin
     ;; unfortunately filters ( like - wavelnth={min:'700',max:'900'} ) don't work with get_meta_datas
     metadatas = get_meta_datas(dataset[0], limit = limit, offset = offset)
     Nmetas = n_elements(metadatas)

     for i = 0, Nmetas-1 do begin
        metadata = metadatas[i]
        obs_date = (strsplit(metadata.date_obs,'T',/extract))[0]
        if obs_date lt date_beg or obs_date gt date_end then continue

        t_beg = (strsplit(metadata.date_beg,'T',/extract))[1]
        t_end = (strsplit(metadata.date_end,'T',/extract))[1]
        if t_beg gt time_end or t_end lt time_beg then continue

        if metadata.wavelnth lt double(wavelnth_min) or metadata.wavelnth gt double(wavelnth_max) then continue

        if naxis4 ne 0 then if metadata.naxis4 ne naxis4 then continue
        if metadata.naxis4 eq 4 then pol = 'YES' else pol = 'NO'

        release_date = metadata.release
        if release_date gt current_date then begin
           propr = 'YES'
           if ~credentials then begin
              if ~keyword_set(no_proprietary) then begin
                 print
                 print, 'There are proprietary data among datasets.'
                 print
                 user_name=''
                 user_passwd=''
                 reply=''
                 read, 'Do you want to proceed to authentication? (Y/N) ', reply
                 if strlowcase(reply) eq 'y' then begin
                    read, 'Username: ', user_name
                    bb = string(13B)
                    outline = string(replicate(32B, 25))
                    ll = 'Password: '
                    strput,outline, ll
                    print,bb,outline, FORMAT = '(A,A,$)'
                    repeat begin
                       q=get_kbrd(/KEY_NAME)
                       if byte(q) ne 10 then begin
                          ll += '*'
                          strput, outline, ll
                          user_passwd += q
                          print,bb,outline, FORMAT = '(A,A,$)'
                       endif
                    endrep until byte(q) eq 10
                    ;;read, 'Password: ', user_passwd
                    credentials = 1B
                 endif else begin
                    print, 'This data are proprietary. For more information about data policy please read  https://dubshen.astro.su.se/wiki/index.php/Science_data.'
                    print, 'You will see only open data from now on.'
                    no_proprietary = 1B
                 endelse
              endif
           endif
           if ~credentials or keyword_set(no_proprietary) then continue
        endif else propr = 'NO'

        a_set = {set, no:0, d_obs:'', t_beg:'', t_end:'', wavelnth:'', polar:'', propr:''}
        a_set.no = i
        a_set.d_obs = (strsplit(metadata.date_obs,'T',/extract))[0]
        a_set.t_beg = (strsplit(t_beg,'.',/extract))[0]
        a_set.t_end = (strsplit(t_end,'.',/extract))[0]
        a_set.wavelnth = string(metadata.wavelnth,format='(f7.2)')
        a_set.polar = pol
        a_set.propr = propr

        if n_elements(sets) eq 0 then sets=[a_set] else sets = [sets, a_set]
     endfor

     Nsets = n_elements(sets)
     if Nsets ne 0 then begin
        print
        print,"No.    date_obs      time_beg     time_end      wavelnth   polarimetric   proprietary"
        for j = 0, Nsets-1 do begin           
           print
           ;; offset is incremented in get_meta_datas, we have to reduce it here
           print, '[',strtrim(j+offset-limit, 2),']   ', sets[j].d_obs, '     ', sets[j].t_beg, '     ', sets[j].t_end, '      ', sets[j].wavelnth, '         ', sets[j].polar, '          ', sets[j].propr
        endfor
        print
        ans = ''
        read,"  Select datasets (like: 2,4-7; * - for all; q - quit) or press Enter for none > ", ans
        if strlowcase(ans) eq 'q' then return
        if ans ne '' then begin
           if ans eq '*' then selected = indgen(Nsets) else selected = expand_range(ans)-offset+limit
           for l = 0, n_elements(selected)-1 do begin
              set_no = sets[selected[l]].no
              if ~credentials and ~keyword_set(no_proprietary) then $
                 fname = download_sst_data(metadatas[set_no], dir=dir, verbose=verbose, nodownload=nodownload, /no_spectral) $
              else $
                 fname = download_sst_data(metadatas[set_no], dir=dir, user_name=user_name, user_passwd=user_passwd, verbose=verbose,nodownload=nodownload, /no_spectral)
              if fname eq '' then begin
                 print, 'No download.'
              endif else begin
                 print, 'Downloaded file: ', fname
              endelse
           endfor
        endif ;;else print,'You have not selected any set.'
     endif

     sets=[]
     
  endrep until Nmetas eq 0

end
