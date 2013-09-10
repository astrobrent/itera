;+
; NAME:
;
;   plot_ITERAgrid
;    
;
; PURPOSE:
;
;   This program is the main plotting routine for ITERA, but can be
;   used to plot pre-existing grids of data. Given 4 2D arrays
;   (X1,X2,Y1,Y2) it plots (or overplots) the grid of log10(X1/X2)
;   versus log10(Y1/Y2). The grid is colored such that, for data
;   X1(a,b), lines of constant a are blue-white shaded, and lines of
;   constant b are red-white shaded.
;
; CALLING SEQUENCE:
;
;   plot_ITERAgrid(X1,X2,Y1,Y2)
;
; INPUTS
;
;
; KEYWORD PARAMETERS:
;
;
; EXTERNAL:
;
; itera_colorbar    
;
; REQUIREMENTS
; 
;
;
; MODIFICATION HISTORY:
;   Brent Groves, July 2009
;
;##############################################################################
;
; LICENSE
;
;  Copyright (C) 2009 Brent Groves
;
;  This file is part of ITERA.
;
;  ITERA is free software; you can redistribute it and/or modify it
;  under the terms of the GNU General Public License as published by
;  the Free Software Foundation; either version 3 of the licence, or
;  (at your option) any later version.
;
;  ITERA is distributed in the hope that it will be useful, but
;  WITHOUT ANY WARRANTY; without even the implied warranty of
;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;  General Public License for more details.
;
;  You should have received a copy of the GNU General Public License
;  along with ITERA (gpl.txt).  If not, see
;  http://www.gnu.org/licenses/. 
;
;##############################################################################
;
PRO plot_iteragrid, x1plot,x2plot,y1plot,y2plot, $
                  xrange=xr, xtitle=xtit, $
                  yrange=yr,ytitle=ytit,  $
                  p1values=p1vals,p1title=p1title,p1range=p1range,$ 
                  p2values=p2vals,p2title=p2title,p2range=p2range,$
                  grid_vals=grid_vals,plot_ct=plot_ct,overplot=overplot,$
                  _EXTRA=extra

 Catch, theError
 IF theError NE 0 THEN BEGIN
   Catch, /Cancel
   Help, /Last_Message, Output=theErrorMessage
   FOR j=0,N_Elements(theErrorMessage)-1 DO BEGIN
      Print, theErrorMessage[j]
   ENDFOR
   RETURN
 ENDIF
 IF (n_elements(X1plot) NE n_elements(X2plot)) AND $
    (n_elements(Y1plot) NE n_elements(Y2plot)) THEN $
      MESSAGE,'Input values of X1, X2, Y1, and Y2 must be equal arrays' 

 IF n_elements(xr) NE 2 THEN xr=[-3,3]
 IF n_elements(yr) NE 2 THEN yr=[-3,3]
 IF N_elements(overplot) EQ 0 THEN overplot=0
;
; Set Up Colour Table
;
 TVLCT,R_old,G_old,B_old,/GET ; save current system
 gridcolor=lonarr(256,3)
 gridcolor[128:254,0]=255 ; Red
 gridcolor[0:127,1]=2L*indgen(128) ; Green
 gridcolor[128:254,1]=255-2L*indgen(127) ; Green 
 gridcolor[1:127,2]=255 ;Blue  
 gridcolor[0,*]=0  ;Black
 gridcolor[255,*]=255 ;white
 tvlct,gridcolor
;
; Set up grid
; 
  X1=X1plot
  X2=X2plot
  Y1=Y1plot
  Y2=Y2plot
 IF n_elements(p1range) NE 0 AND n_elements(p1vals) NE 0 THEN BEGIN
  p1index=where(p1vals GE p1range[0] AND p1vals LE p1range[1],n_p1)
  IF n_p1 EQ 0 THEN RETURN
  p1vals=p1vals[p1index]
  X1=X1[*,p1index]
  X2=X2[*,p1index]
  Y1=Y1[*,p1index]
  Y2=Y2[*,p1index]
 ENDIF
 IF n_elements(p2range) NE 0 AND n_elements(p2vals) NE 0 THEN BEGIN
;  IF size(p2vals,/type) EQ 7 THEN BEGIN
;   FOR i=0,n_elements(p2vals)-1 DO BEGIN
;      p2test=VALID_NUM(STRMID(p2vals[i],0,STRLEN(p2vals[i])-2),p2number)
;      p2vals[i]=(p2test)? p2number:p2vals[i]
;   ENDFOR
;  ENDIF
  p2index=where(p2vals GE p2range[0] AND p2vals LE p2range[1],n_p2)
  IF n_p2 EQ 0 THEN RETURN
  p2vals=p2vals[p2index]
  X1=X1[p2index,*]
  X2=X2[p2index,*]
  Y1=Y1[p2index,*]
  Y2=Y2[p2index,*]
 ENDIF
 
 gridsize=SIZE(x1)
 n_param1=(gridsize[0] EQ 2) ? gridsize[2] : 1
 n_param2=(gridsize[0] NE 0) ? gridsize[1] : 1
 xdata=MAKE_ARRAY(/FLOAT,DIMENSION=[n_param2,n_param1])
 xdata[*]=-99
 ydata=xdata
;
; Set up plot
;
 oldchar=!P.CHARSIZE
 IF !D.NAME NE 'PS' THEN !P.CHARSIZE=1.3
 IF !P.THICK EQ 0 THEN !P.THICK=1.0
 IF overplot EQ 0 THEN $
   plot,xdata[0,*],ydata[0,*],/NoData,$
     xsty=1,xtitle=xtit,xrange=xr,$
     ysty=1,ytitle=ytit,yrange=yr,$
     position=[0.1,0.1,0.8+(~keyword_set(plot_ct))*0.15,0.96]
;
; Plot lines of constant parameter1 
;
; Set up labels
 IF N_ELEMENTS(p1vals) EQ 0 THEN BEGIN
   n_ct=n_param1-1
   p1vals=STRING(indgen(n_param1)+1,FORMAT='(i3)')
   p1title='Parameter 1'
 ENDIF ELSE n_ct=n_elements(p1vals)-1
 p1ind=sort(p1vals)
 p1vals=p1vals[p1ind]
 step=n_ct/10L+1
 labels=STRCOMPRESS(STRING(p1vals,FORMAT='(G9.2)'),/REMOVE_ALL)
 cticks=labels[0:n_ct:step] 
 n_ticks=n_elements(cticks)-1
 cticks[n_ticks]=labels[n_ct]
;
; Do plot
; !P.THICK=!P.THICK/2
 cindex=(FLOAT(n_param1-1) NE 0) ? FIX(124/FLOAT(n_param1-1)) : 124
 FOR i=0,n_param1-1 DO BEGIN 
   safe=where(X1[*,p1ind[i]] NE 0 AND X2[*,p1ind[i]] NE 0 AND $
              Y1[*,p1ind[i]] NE 0 AND Y2[*,p1ind[i]] NE 0,n_safe,$
              COMPLEMENT=wrong)
   IF n_safe GT 1 THEN BEGIN 
     xdata=REFORM(ALOG10(x1[safe,p1ind[i]]/x2[safe,p1ind[i]]))
     ydata=REFORM(ALOG10(y1[safe,p1ind[i]]/y2[safe,p1ind[i]]))
     oplot,xdata,ydata,color=254-i*cindex
     IF keyword_set(grid_vals) AND (i MOD (2*step)) EQ 0 THEN BEGIN
       xyouts, xdata[0],ydata[0],cticks[i/step],$
               alignment=1.0*(xdata[1] GT xdata[0])
     ENDIF
   ENDIF ELSE IF n_safe EQ 1 THEN BEGIN
     xdata=REFORM(ALOG10(x1[safe:safe,p1ind[i]]/x2[safe:safe,p1ind[i]]))
     ydata=REFORM(ALOG10(y1[safe:safe,p1ind[i]]/y2[safe:safe,p1ind[i]]))
     oplot,xdata,ydata,psym=6,color=254-i*cindex
   ENDIF
 ENDFOR
 IF overplot EQ 0 AND keyword_set(plot_ct) THEN BEGIN
   IF n_ticks GT 0 THEN BEGIN
     itera_colorbar,divisions=n_ticks,bottom=130,/Vertical,NCOLORS=124,$
       POSITION=[0.93,0.52,0.96,0.97],charsize=0.9,$
       TICKLEN=-0.1,TICKNAMES=cticks,title=p1title,/REVERSE
   ENDIF ELSE BEGIN 
      xyouts,0.87,0.78,p1title,color=254,/NORMAL
      xyouts,0.87,0.75,cticks,color=254,/NORMAL
   ENDELSE
 ENDIF 
;
; Plot lines of constant parameter2
;
; Set up Labels
 IF N_ELEMENTS(p2vals) EQ 0 THEN BEGIN
   n_ct=n_param2-1
   p2vals=indgen(n_param2)+1
   p2title='Parameter 2'
 ENDIF ELSE n_ct=n_elements(p2vals)-1
 p2ind=sort(p2vals)
 p2vals=p2vals[p2ind]
 step=n_ct/10L+1 
 labels=(size(p2vals,/type) EQ 7)? p2vals : $
        STRCOMPRESS(STRING(p2vals,FORMAT='(G9.2)'),/REMOVE_ALL)
 cticks=labels[0:n_ct:step]
 n_ticks=n_elements(cticks)-1
 cticks[n_ticks]=labels[n_ct]  
;
; Do Plot
 !P.THICK=2*!P.THICK
 cindex=(FLOAT(n_param2-1) NE 0) ? FIX(124/FLOAT(n_param2-1)) : 124
 FOR i=0,n_param2-1 DO BEGIN 
   safe=where(X1[p2ind[i],*] NE 0 AND X2[p2ind[i],*] NE 0 AND $
              Y1[p2ind[i],*] NE 0 AND Y2[p2ind[i],*] NE 0,n_safe,$
              COMPLEMENT=wrong)
   IF n_safe GT 1 THEN BEGIN 
     xdata=ALOG10(x1[p2ind[i],safe]/x2[p2ind[i],safe])
     ydata=ALOG10(y1[p2ind[i],safe]/y2[p2ind[i],safe])
     oplot,xdata,ydata,color=i*cindex+1
     IF keyword_set(grid_vals) AND (i MOD (2*step)) EQ 0 THEN BEGIN
       max_i=n_elements(xdata)-1
       xyouts, xdata[max_i],ydata[max_i]+0.2*(ydata[max_i]-ydata[max_i-1]),cticks[i/step],$
               alignment=0.0
     ENDIF
   ENDIF ELSE IF n_safe EQ 1 THEN BEGIN
     xdata=REFORM(ALOG10(x1[p2ind[i],safe:safe]/x2[p2ind[i],safe:safe]))
     ydata=REFORM(ALOG10(y1[p2ind[i],safe:safe]/y2[p2ind[i],safe:safe]))
     oplot,xdata,ydata,psym=4,color=i*cindex+1
   ENDIF
 ENDFOR
!P.THICK=!P.THICK/2
 IF overplot EQ 0 AND keyword_set(plot_ct) THEN BEGIN
   IF n_ticks GT 0 THEN BEGIN
      itera_colorbar,divisions=n_ticks,bottom=1,/Vertical,$
       POSITION=[0.93,0.08,0.96,0.48],charsize=0.9,NCOLORS=124,$
       TICKLEN=-0.1,TICKNAMES=cticks,title=p2title
   ENDIF ELSE BEGIN 
      xyouts,0.87,0.28,p2title,color=1,/NORMAL
      xyouts,0.87,0.25,cticks,color=1,/NORMAL
   ENDELSE
 ENDIF
 TVLCT,R_old,G_old,B_old ; restore original system
 !P.CHARSIZE=oldchar
 RETURN
END
