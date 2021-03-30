; docformat = 'rst'

;+
; Expand a comma separated list of subranges given as a string into an
; integer array.
;
; Subranges can be of three kinds:
;    1. Single numbers, expands to the number..
;    2. Dash separated numbers, '10-15' --> [10,11,12,13,14,15].
; 
; :Categories:
;
;    SST archive
; 
; 
; :Author:
; 
;    Mats Löfdahl, ISP
; 
; 
; :Returns:
; 
;    An integer array.
; 
; :Params:
; 
;    st_in : in, type=str
; 
;      This is the comma and dash separated list.
; 
; :History:
; 
;     2004-03-25 : MGL. First version (in ANA).
;
;     2009-06-11 : MGL. Ported to IDL.
;
;     2013-10-03 : MGL. Renamed to the red_ namespace.
; 
;     2014-01-22 : MGL. Adapt to string functions moved to the str_
;                  namespace. 
; 
;     2017-06-29 : MGL. Use rdx_str2ints, will accept colon syntax
;                  with increments.
;
;     2020-02-04 : OA. Renamed to the expand_range. Excluded
;                  rdx_str2ints. Imported red_strcount and red_strskp.
; 
;-

; ANA's strskp function
;
; 2014-01-22 : MGL. Renamed for inclusion in the red_ namespace.
;
; 2014-03-05 : THI. Handle both arrays of strings and single strings.
function red_strskp,st,del

    pos = [strpos(st,del)]

    if size(pos,/dim) gt 0 then begin
        return,strmid(st,transpose(pos+(pos ge 0)*strlen(del)))
    end else begin
       return,st
    end

 end

; Limited form of ANA's strcount: Counts the number of
; occurences of a character in a string.
; Could make it more general using strskp.
;
; 2014-01-22 : MGL. Renamed for inclusion in the red_ namespace.
;
; 2014-03-05 : THI. Handle both arrays of strings and single strings.
;
; 2014-04-02 : MGL. Return scalar if input was a single string. 
;
function red_strcount,st,pat1

  if strlen(pat1) eq 0 then return,0

  ;; pat1 is not a single character

  cnt = intarr(size([st],/dim))
  st0 = st
  st1 = red_strskp(st0,pat1)

  while not array_equal(st0,st1) do begin
     
     cnt += (st0 ne st1)

     st0 = st1
     st1 = red_strskp(st0,pat1)
 
     ;print,cnt,' "',st0,'"   "',st1,'"'

  end

  if size(st, /n_dim) eq 0 then cnt = cnt[0]

  return,cnt


end

function expand_range, st_in
  
  st=st_in  ; Non-destructive
  
  n_commas = red_strcount(st,',')
  n_dashes = red_strcount(st,'-')
  n_sep = n_commas+n_dashes
  
  st=st+',-'
  
  range = [long(st)]
  for i=0,n_sep-1 do begin
    if strpos(st,',') lt strpos(st,'-') then begin
      ;; Comma: Just append the number
      st=red_strskp(st,',')
      range = [range,long(st)]
    end else begin
      ; Dash, append range
      st=red_strskp(st,'-')
      first_number = range((size(range,/dimensions))[0]-1)
      last_number  = long(st)
      new_range = IndGen(last_number-first_number) + long(first_number+1)
      range = [range,new_range]
    end
  end
  
  return, range

end




