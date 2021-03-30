; docformat = 'rst'

;+
; Download SST CRISP or CHROMIS data from the prototype SVO. Based on
; the download_data function by Benjamin Mampaey.
; 
; :Categories:
;
;    SST archive
; 
; 
; :Author:
; 
;    Mats Löfdahl, Institute for Solar Physics
; 
; 
; :Returns:
;
;    The path to the downloaded file.
; 
; :Params:
; 
;    meta_data : in, type=struct
; 
;       A meta_data struct as returned by the get_meta_datas function.
; 
; 
; :Keywords:
; 
;    user_name : in, optional, type=string
;
;       A user name used when downloading proprietary
;       data.
;
;    user_passwd : in, optional, type=string
;
;       A user password used when downloading proprietary
;       data.
;
;    dir : in, optional, type=string
;
;       The directory where to download the file.
;
;    no_spectral : in, optional, type=boolean
;
;       Don't try to download a spectral cube.
;
;    verbose : in, optional, type=boolean
;   
;       Print more info.
; 
; 
; :History:
; 
;    2019-10-16 : MGL. First version.
;
;    2020-12-03 : OA. Second version.
; 
;-
function download_sst_data, meta_data $
                            , no_spectral = no_spectral $
                            , user_name = user_name $
                            , user_passwd = user_passwd $
                            , dir = dir $
                            , nodownload = nodownload $ 
                            , verbose = verbose

  IF N_ELEMENTS(dir) EQ 0 THEN dir = '.'

  pth_split = strsplit(meta_data.data_location.file_path, '/', /EXTRACT)
  print, meta_data.data_location
  ;data_date = pth_split[0]
  instrument = pth_split[1]
  im_filename =  meta_data.FILENAME
  fnm_split = strsplit(im_filename,'_',/extract)
  data_date=(strsplit(fnm_split[2],'T',/extract))[0]
  url_split = strsplit(meta_data.data_location.file_url, '/', /extract)
  url_scheme   = strmid(url_split[0],0,strlen(url_split[0])-1) ; Remove trailing colon.
  url_hostname = url_split[1]
  if url_hostname eq 'dubshen.isf.astro.su.se' then url_hostname = 'dubshen.astro.su.se'
  url_path = 'data'
  
  if strmatch(im_filename, '*_im.fits') then $
     ;; There might be a spectral version of the cube.
     sp_filename = file_basename(im_filename, '_im.fits')+'_sp.fits'
  
  ;; Check RELEASE tag to see if authentication is in fact needed 
  tags = tag_names(meta_data)
  pos = where(tags eq 'RELEASE', Nrelease)

  if Nrelease eq 1 then begin
     
     ;; Get the current date
     time = Systime(UTC=Keyword_Set(utc))
     day = String(StrMid(time, 8, 2), Format='(I2.2)') ; Required because UNIX and Windows differ in time format.
     month = StrUpCase(Strmid(time, 4, 3))
     year = Strmid(time, 20, 4)
     months = ['JAN', 'FEB', 'MAR', 'APR', 'MAY', 'JUN', 'JUL', 'AUG', 'SEP', 'OCT', 'NOV', 'DEC']
     m = (Where(months EQ month)) + 1
     current_date =  year + '-' + string(m, FORMAT='(I2.2)') + '-' + day

     ;; Is today before the RELEASE date?
     release_date = meta_data.release
     if current_date lt release_date then begin
        authentication = 3      ; Either basic or digest is used depending on the mode preferred by the remote server.

        if ~keyword_set(user_name) || ~keyword_set(user_passwd) then begin
           print
           print, 'These data are proprietary until '+meta_data.RELEASE
           print
           user_name=''
           user_passwd=''
           reply=''
           read, 'Do you want to proceed to authentication? (Y/N) ', reply
           if reply eq 'Y' then begin
              read, 'Username: ', user_name
              read, 'Password: ', user_passwd
           endif else begin
              print, 'You need to ask the data owners if they want to share the data before '+meta_data.RELEASE
              print, 'In that case they will send you credentials.'
              print, "The owner's contact info should be in the meta data's RELEASEC keyword:"
              print
              print, meta_data.RELEASEC
              return, ''
           endelse
        endif
     endif else authentication = 0
     
  endif else authentication = 0

  if authentication eq 0 then begin
     if strmatch(im_filename, '*_im.fits') and ~keyword_set(no_spectral) then begin
        ;; There might be a spectral version of the cube.
        print
        print, 'Will first try to download the spectral version of the cube.'
        url_query = 'date='+data_date+'&instrument='+instrument+'&filename='+sp_filename
        if keyword_set(nodownload) then begin
           print, 'Called with /nodownload'
           print, '    URL='+url_scheme+'://'+url_hostname+'/'+url_path
           print, '    query= ',url_query
           print, '    Local filename='+dir + PATH_SEP() + sp_filename 
        endif else begin
           print, '    URL='+url_scheme+'://'+url_hostname+'/'+url_path
           print, '    query= ',url_query
           url = OBJ_NEW('IDLnetUrl', authentication=authentication $
                         , url_hostname=url_hostname $
                         , url_path=url_path $
                         , url_scheme=url_scheme $
                         , url_query=url_query $     
                         , VERBOSE = verbose)
           fnm = dir + PATH_SEP() + sp_filename
           aaa = url->Get(FILENAME=fnm)
           if aaa ne '' then print, 'It should now be downloaded as '+aaa else print, "There doesn't seem to be one"
           openr, lun, fnm, /get_lun
           hd = bytarr(6)
           readu,lun,hd
           free_lun,lun
           if string(hd) ne 'SIMPLE' then begin ;; it's not a data cube file
              if strlowcase(string(hd)) eq '<html>' then begin
                 print & print, "Download of the datacube has failed."
                 print, "The server reply is:"
                 spawn, 'cat '+fnm
                 spawn, 'mv '+fnm+' download_status.html'
                 ;; Don't return try to download _im cube
              endif else begin
                 print, "Something is wrong."
                 stop
              endelse
           endif
           obj_destroy, url
        endelse
     endif
     print

     url_query = 'date='+data_date+'&instrument='+instrument+'&filename='+im_filename
     if keyword_set(nodownload) then begin
        print, 'Called with /nodownload'
        print, '    URL='+url_scheme+'://'+url_hostname+'/'+url_path
        print, '    query= ',url_query
        print, '    Local filename='+dir + PATH_SEP() + im_filename 
        aaa = ''
     endif else begin
        print, '    URL='+url_scheme+'://'+url_hostname+'/'+url_path
        print, '    query= ',url_query            
        url = OBJ_NEW('IDLnetUrl', authentication=authentication $
                      , url_hostname=url_hostname $
                      , url_path=url_path $
                      , url_scheme=url_scheme $
                      , url_query=url_query $     
                      , VERBOSE = verbose)
        fnm = dir + PATH_SEP() + im_filename
        aaa = url->Get(FILENAME=fnm)
        if aaa ne '' then print, 'It should now be downloaded as '+aaa else print, "There doesn't seem to be one"
        openr, lun, fnm, /get_lun
        hd = bytarr(6)
        readu,lun,hd
        free_lun,lun
        if string(hd) ne 'SIMPLE' then begin ;; it's not a data cube file
           if strlowcase(string(hd)) eq '<html>' then begin
              print & print, "Download of the datacube has failed."
              print, "The server reply is:"
              spawn, 'cat '+fnm
              spawn, 'mv '+fnm+' download_status.html'
              obj_destroy, url
              return,''
           endif else begin
              print, "Something is wrong."
              stop
              return,''
           endelse
        endif
        obj_destroy, url
     endelse
     

     return, aaa


  endif else begin
     
     ;; Documentation:
     ;; https://www.harrisgeospatial.com/docs/idlneturl_properties.html
     ;; and https://www.harrisgeospatial.com/docs/IDLnetURL__Get.html

     if strmatch(im_filename, '*_im.fits') and ~keyword_set(no_spectral) then begin
        ;; There might be a spectral version of the cube.
        print, 'Will first try to download the spectral version of the cube.'
        url_query = 'date='+data_date+'&instrument='+instrument+'&filename='+sp_filename
        if keyword_set(nodownload) then begin
           print, 'Called with /nodownload'
           print, '    URL='+url_scheme+'://'+url_hostname+'/'+url_path
           print, '    query= ',url_query
           print, '    Local filename='+dir + PATH_SEP() + sp_filename 
        endif else begin
           print, '    URL = '+url_scheme+'://'+url_hostname+'/'+url_path
           print, '    query = ',url_query
           url = OBJ_NEW('IDLnetUrl', authentication=authentication $
                         , url_hostname=url_hostname $
                         , url_path=url_path $
                         , url_scheme=url_scheme $
                         , url_username=user_name $
                         , url_password=user_passwd $
                         , url_query=url_query $     
                         , VERBOSE = verbose)
           fnm = dir + PATH_SEP() + sp_filename
           aaa = url->Get(FILENAME=fnm)
           if aaa ne '' then print, 'It should now be downloaded as '+aaa else print, "There doesn't seem to be one"
           openr, lun, fnm, /get_lun
           hd = bytarr(6)
           readu,lun,hd
           free_lun,lun
           if string(hd) ne 'SIMPLE' then begin ;; it's not a data cube file
              if strlowcase(string(hd)) eq '<html>' then begin
                 print & print, "Download of the datacube has failed."
                 print, "The server reply is:"
                 spawn, 'cat '+fnm
                 spawn, 'mv '+fnm+' download_status.html'
                 ;; Don't return try to download _im cube
              endif else begin
                 print, "Something is wrong."
                 stop
                 return,''
              endelse
           endif
           obj_destroy, url
        endelse
     endif
     print
     print, 'Will now download the cube.'
     url_query = 'date='+data_date+'&instrument='+instrument+'&filename='+im_filename
     if keyword_set(nodownload) then begin
        print, 'Called with /nodownload'
        print, '    URL = '+url_scheme+'://'+url_hostname+'/'+url_path
        print, '    query = ',url_query
        print, '    Local filename = '+dir + PATH_SEP() + im_filename 
        aaa = ''
     endif else begin
        print, '    URL = '+url_scheme+'://'+url_hostname+'/'+url_path
        print, '    query = ',url_query
        url = OBJ_NEW('IDLnetUrl', authentication=authentication $
                      , url_hostname=url_hostname $
                      , url_path=url_path $
                      , url_scheme=url_scheme $
                      , url_username=user_name $
                      , url_password=user_passwd $
                      , url_query=url_query $     
                      , VERBOSE = verbose)
        fnm = dir + PATH_SEP() + sp_filename
        aaa = url->Get(FILENAME=fnm)
        if aaa ne '' then print, 'It should now be downloaded as '+aaa else print, "Download seems to have failed"
        openr, lun, fnm, /get_lun
        hd = bytarr(6)
        readu,lun,hd
        free_lun,lun
        if string(hd) ne 'SIMPLE' then begin ;; it's not a data cube file
           if strlowcase(string(hd)) eq '<html>' then begin
              print & print, "Download of the datacube has failed."
              print, "The server reply is:"
              spawn, 'cat '+fnm
              spawn, 'mv '+fnm+' download_status.html'
              obj_destroy, url
              return,''
           endif else begin
              print, "Something is wrong."
              stop
              return,''
           endelse
        endif
        obj_destroy, url
        return,aaa
     endelse
  endelse
end
