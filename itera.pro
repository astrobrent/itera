;+
; NAME:
;
;   ITERA : IDL Tool for Emission-line Ratio Analysis
;    
; VERSION:
;
;   1.1 (January, 2012)
;
; PURPOSE:
;
;   To create emission line diagnostic diagrams of dominant ionized
;   and atomic emission lines using public photoionization models (ie
;   MAPPINGS III) for comparison with observations.
;
; REFERENCE:
; 
;   Still being written
;
; CATEGORY:
;
;   Emission lines, Shock ionization, photoionization
;
; CALLING SEQUENCE:
;
;   itera [,max_models=#]
;
; OPTIONAL INPUTS:
;
;   max_models: This determines the maximum number of models read to be
;   plotted in the ITERA widow. By default this is 10 models. This is to
;   limit the memory taken over by ITERA and prevent clutered
;   diagrams. This can  altered if multiple models are necessary.
;
; EXTERNAL:
;
;   Uses the following associated ITERA external programs:
;     READ_LINELIST
;     READ_ITERAMODELSERIES
;     POPULATE_MODELTREE
;     PLOT_ITERAGRID
;     ITERA_GET_LINEFLUX
;     
;
; REQUIREMENTS
; 
;   For optimal use requires the downloading and installation of
;   published or user made models.
;
; MODIFICATION HISTORY:
;   Brent Groves, July 2009
;   Evolved from the SHOCKPLOT code by Mark Allen
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
;---------------------------------------------------------------------------
; itera_Cleanup: Cleans up all pointers that use  memory upon the
;                closure of the widget.  This procedure is called when
;                the top-level base widget is destroyed.
;---------------------------------------------------------------------------
PRO itera_Cleanup,top
 On_Error,2
 Widget_Control, top, Get_UValue=info, /No_Copy
 IF N_Elements(info) EQ 0 THEN BEGIN 
   Heap_gc,/VERBOSE
   RETURN
 ENDIF 
 modelindex=WHERE(PTR_VALID(info.p_models),num_models)
 IF num_models NE 0 THEN BEGIN
   FOR i=0L,num_models-1 DO BEGIN
     IF size(*info.p_models[modelindex[i]],/TYPE) EQ 8 THEN $
       PTR_FREE,(*info.p_models[modelindex[i]]).models
   ENDFOR
 ENDIF
 PTR_FREE,info.p_data
 PTR_FREE,info.p_models
 PTR_FREE,info.addXval,info.addYval,info.addlabel
 PTR_FREE,info.modellist
 PTR_FREE,info.X1grid,info.X2grid,info.Y1grid,info.Y2grid
 RETURN
END

;---------------------------------------------------------------------------
; itera_filemenu_handler: Handles events from the File button in the
;                         menu; save grid, print grid and exit
;---------------------------------------------------------------------------
PRO itera_filemenu_handler, event
 On_Error,2
; Get the UValue of activated widget
 widget_control, event.id, get_uvalue=uvalue
 CASE uvalue OF
   'save':BEGIN
     widget_control, event.top, get_uvalue=info,/no_copy
     IF ~info.gridexist THEN BEGIN ; dont start
        widget_control, event.top, set_uvalue=info,/no_copy
        RETURN
     ENDIF
     savebaseID = widget_base(/col,TITLE="Save Line Ratio Data",$
                           /FLOATING,GROUP_LEADER=event.top,$
                           EVENT_PRO='itera_savegrid_event')
     save_prefix=CW_FIELD(savebaseID,TITLE="File Prefix:",Value="iteramodel")
     savebuttonID = widget_base(savebaseID,/row,/Align_Center)
     Save_Enter=Widget_Button(savebuttonID,Value='Save',Uvalue='save',$
                               Xsize=60)
     save_Cancel=Widget_Button(savebuttonID,Value='Cancel',UValue='cancel',$
                               Xsize=60)
     WIDGET_CONTROL,savebaseID,/realize
     WIDGET_CONTROL,savebaseID,Set_Uvalue={info:info,prefixID:save_prefix}
     XMANAGER,'itera_savegrid', savebaseID
     widget_control, event.top, set_uvalue=info,/no_copy
   END
   'print':BEGIN
     widget_control, event.top, get_uvalue=info,/no_copy
     IF ~info.gridexist THEN BEGIN ; dont start
        widget_control, event.top, set_uvalue=info,/no_copy
        RETURN
     ENDIF
     printbaseID = widget_base(/col,TITLE="Print to Postscript File",$
                           /FLOATING,GROUP_LEADER=event.top,$
                           EVENT_PRO='itera_Printfile_Handler')
     CD,CURRENT=currentdir
     printnameID=CW_FILESEL(printbaseID,/WARN_EXIST,PATH=currentdir,$
                 /FILENAME,/SAVE,FILTER=[".eps"])
     WIDGET_CONTROL,printnameID,Set_Value='itera.eps'
     WIDGET_CONTROL,printbaseID,/realize
     WIDGET_CONTROL,printbaseID,Set_Uvalue=info
     XMANAGER,'itera_Printfile', printbaseID
     widget_control, event.top, set_uvalue=info,/no_copy
   END
   'exit': BEGIN
     itera_cleanup, event.top
     widget_control, event.top, /destroy
   END
   ELSE: print, 'Unknown Event...'     
 ENDCASE
 RETURN
END

;---------------------------------------------------------------------------
; itera_save_grid_event: This procedure saves the four selected lines
;                        (or sums of lines) used for the ratios for
;                        all selected models in an easy-to-use ascii
;                        table for use with other programs. 
;---------------------------------------------------------------------------
PRO itera_savegrid_event,event
 On_error,2
 widget_control, event.id, get_UValue=UValue
 CASE UValue OF
  'save':BEGIN
    widget_control, event.top, get_UValue=state,/No_copy
    Widget_Control,state.prefixID, get_Value=prefix
    modelindex=WHERE(PTR_VALID(state.info.p_models),n_models)
    filename=STRARR(n_models)
;
; Get line names (ion & wavelength) and compress if the same ion
    titles=STRARR(4)
    list=[state.info.X1list,state.info.X2list,$
          state.info.Y1list,state.info.Y2list]
    FOR i=0L,3 DO BEGIN
      Widget_control,list[i],Get_UValue=U_index
      linenames=state.info.lines[U_index]
      j=0L
      wavel=STRARR(n_elements(linenames))
      WHILE (n_elements(linenames) GT 1) AND (j LT  n_elements(linenames)) $
        DO BEGIN
        ion=STRMID(linenames[j],0,strpos(linenames[j],' '))
        match=WHERE(STRMATCH(linenames,ion+"*"),num,complement=nomatch)
        IF num NE 0 THEN BEGIN 
         FOR k=0L,num-1 DO $
          wavel[k]=STRMID(linenames[match[k]],$
              strpos(linenames[match[k]],' ')+1,STRLEN(linenames[match[k]]))
         newline=ion+STRCOMPRESS(STRING(wavel,FORMAT='(5(a9,:,","))'),/REMOVE)
        ENDIF
        linenames= (nomatch[0] NE -1) ? [newline,linenames[nomatch]] : newline
        j+=1
      ENDWHILE
      titles[i]=STRCOMPRESS(STRING(linenames,FORMAT='(5(a20,:," +"))'))
    ENDFOR
;
; Loop through model grids
    FOR i=0,n_models-1 DO BEGIN
; Set Up Paramter names and values
      paramnames=tag_names(*((*state.info.p_models[modelindex[i]]).models)[0])
; first parameter (ie V)
      maxindex=0
      maxparam1=-1
      FOR j=0,n_elements((*state.info.p_models[modelindex[i]]).models)-1 $
        DO BEGIN
        param1=(*(*state.info.p_models[modelindex[i]]).models[j]).(5)
        maxparam1=(n_elements(param1) GT maxindex)? param1 : maxparam1
      ENDFOR
      p1title=paramnames[5]
; Second parameter (ie B)  
      param2=(*state.info.p_models[modelindex[i]]).(state.info.n_para2)
      p2vals=(size(param2,/type) EQ 7)? $
         param2:STRING(param2,FORMAT='(E12.4)')
      p2title=paramnames[state.info.n_para2+1]
; Open file and loop through grid
      filename[i]=prefix+STRING(i+1,FORMAT='("_grid",i1,".txt")')
      x1grid=*(state.info.x1grid[i])
      x2grid=*(state.info.x2grid[i])
      y1grid=*(state.info.y1grid[i])
      y2grid=*(state.info.y2grid[i])
      openw,lun,filename[i],/GET_LUN
      printf,lun,"% itera v0.1 Model Grids"
      printf,lun,"%"
      printf,lun,"% FORMAT: parameter 2, parameter 1 (as labelled), X1, X2, Y1, Y2"
      printf,lun,"%         with emission lines given as ratios with respect to H-beta 4861"
      printf,lun,"%"
      printf,lun,"X1 = ",titles[0]
      printf,lun,"X2 = ",titles[1]
      printf,lun,"Y1 = ",titles[2]
      printf,lun,"Y2 = ",titles[3]
      printf,lun,p2title,p1title,"X1","X2","Y1","Y2",FORMAT='(6(a12,:,1x))'
      printf,lun,FORMAT='(80("-"))'
      FOR j=0L,n_elements(param2)-1 DO BEGIN
        FOR k=0L, n_elements(maxparam1)-1 DO $
          printf,lun,param2[j],maxparam1[k],$
                 x1grid[j,k],x2grid[j,k],y1grid[j,k],y2grid[j,k],$
                 FORMAT='(a12,1x,5(E12.4,:,1x))'
      ENDFOR
      close,lun
      free_lun,lun
    ENDFOR
    CD,CURRENT=dirname
    files_text="Saving as : "+filename[0]
    FOR j=1L,n_elements(filename)-1 DO $
        files_text=[files_text,"            "+filename[j]]
    files_text=[files_text,"in directory : "+dirname]
    ok = dialog_message(files_text, dialog_parent=event.top,/INFO,$
                        TITLE="Saving Files...")    
    widget_control, event.top, set_UValue=state,/No_copy
    widget_control, event.top, /destroy
  END
  'cancel':BEGIN
    widget_control, event.top, /destroy
  END
  ELSE: RETURN
 ENDCASE
 RETURN
END

;---------------------------------------------------------------------------
; itera_printfile_event: Handles the printing of the current plot in
;                        the draw window to an encapsulated
;                        postscript file.
;---------------------------------------------------------------------------
PRO itera_printfile_event,event
On_error,2
 CASE event.DONE OF
  0: BEGIN
  END
  1: BEGIN
    WIDGET_CONTROL, event.id, GET_VALUE=filename
    WIDGET_CONTROL, event.top, GET_UVALUE=info,/No_copy
;
; store original device parameters
     old_device=!D.NAME
     thin1=!P.thick
     thin2=!P.charthick
     thin3=!X.thick
     thin4=!Y.thick
     thin5=!Z.thick
     set_plot,'ps'
;
; set up postscript device
     DEVICE, SET_FONT='Helvetica', /TT_FONT
     device,filename=filename,bits=8,/color,/encapsulated
     !P.thick=2.0
     !P.charthick=2.0
     !X.thick=2.0
     !Y.thick=2.0
     !Z.thick=2.0      
;
; Finally Do plot
     itera_doplot,info,/postscript
;
; return original device parameters
     device,/close
     set_plot,old_device
     !P.thick=thin1
     !P.charthick=thin2
     !X.thick=thin3
     !Y.thick=thin4
     !Z.thick=thin5
;
; Finally destroy Popup Window
     WIDGET_CONTROL, event.top, SET_UVALUE=info,/No_copy
     WIDGET_CONTROL, event.top, /DESTROY
  END
  2: WIDGET_CONTROL, event.top, /DESTROY
 ENDCASE
 RETURN
END

;---------------------------------------------------------------------------
; itera_linelist_menu_handler: Handle events from the
;                              Emission-Line-List menu button, setting
;                              the lines shown in the 4 line lists.
;---------------------------------------------------------------------------
PRO itera_linelist_menu_handler, event
 On_Error,2

; Get the UValue of activated widget
widget_control, event.id, get_uvalue=uvalue
Widget_Control, event.top, Get_UValue=info, /no_copy
;
;Clear set button
;
FOR i=0,n_elements(info.linerangeIDs)-1 DO $
 Widget_Control, info.linerangeIDs[i],SET_BUTTON=0  
n_lines=n_elements(info.lines)

CASE uvalue of
'full'   : line_selection = indgen(n_lines)
'select1': line_selection =  $
;[1109, 1112, 1124, 1126, 1157, $
;      1160, 1187, 1188, 1189, 1190, 1191, 1192, 1193, $
;      1194, 1195, 1196, 1197, 1198, 1204, 1209, 1210, $
;      1218, 1220, 1221, 1226, 1227, 1228, 1247, 1248, $
;      1277, 1303, 1331, 1354, 1345, 1346, 1368, 1369, $
 [     1377, 1384, 1393, 1400, 1404, 1405, 1418, 1422, $
      1432, 1434, 1435, 1439, 1440, 1453, 1454, 1456, 1457, $
      1498, 1507, 1626, 1637, 1658, 1670, 1664, 1675, $
      1686, 1690, 1694, 1698, 1709 ]
'Lyman'  : line_selection = WHERE(info.linewave GT 1025)
'UV'     : line_selection = WHERE(info.linewave GT 950 $
                              AND info.linewave LT 3500)
'Optical': line_selection = WHERE(info.linewave GE 3500  $
                              AND info.linewave LT 9000)
'NIR'    : line_selection = WHERE(info.linewave GE 9000 $
                              AND info.linewave LT 50000)
'FIR'    : line_selection = WHERE(info.linewave GE 50000)
ENDCASE
;
;Set the button
Widget_Control,event.id,set_button=1
; use the line selection to make the options and option values for value and
; UValues of the line selection widgets
selected_range = info.lines[line_selection]
selected_range_indices = line_selection

; re-set all the line selection widgets with the new line list
Widget_Control, info.y1id, Set_Value = selected_range
Widget_Control, info.y1id, Set_UValue = selected_range_indices
Widget_Control, info.y2id, Set_Value = selected_range
Widget_Control, info.y2id, Set_UValue = selected_range_indices
Widget_Control, info.x1id, Set_Value = selected_range
Widget_Control, info.x1id, Set_UValue = selected_range_indices
Widget_Control, info.x2id, Set_Value = selected_range
Widget_Control, info.x2id, Set_UValue = selected_range_indices

Widget_Control, event.top, Set_UValue=info, /no_copy
RETURN
END 

;---------------------------------------------------------------------------
;std_diags_menu_handler: Handles events from the standard diagnostics
;                        menu button, allowing quick selection of
;                        emission lines for standard diagnostic diagrams.
;---------------------------------------------------------------------------
PRO std_diags_menu_handler,event
 On_Error,2
;
; Get Line Handler IDs
 widget_control, event.top, get_uvalue=info,/NO_COPY
 X1grid=WHERE(tag_names(info) EQ 'X1GRID')
 GridIDs=[X1grid,X1grid+1,X1grid+2,X1grid+3]
 ListIDs=[info.x1list,info.x2list,info.y1list,info.y2list]
 clearIDs=[info.X1clear,info.X2clear,info.Y1clear,info.Y2clear]
 lines=info.lines
 modelindex=WHERE(PTR_VALID(info.p_models),num_models)
 widget_control, event.top, set_uvalue=info,/NO_COPY
;
; Clear previous line data
 FOR i=0L,3 DO BEGIN
   pseudo_event={ID:clearIDs[i], top:event.top,Handler:0L,Select:1L}
   itera_Clear_line_handler,pseudo_event
 ENDFOR 
; Get selected Lines
 widget_control, event.id, get_uvalue=uvalue
;
; Set lines in lists (note - already empty
 FOR i=0L,3 DO BEGIN
   linenames=lines[uvalue.(i)]
   Widget_Control,listIDs[i], set_Value=linenames
   Widget_Control,listIDs[i], set_UValue=uvalue.(i)
 ENDFOR
;
; IF models are chosen Set values of grids and do plot
;
 IF num_models NE 0 THEN BEGIN
   widget_control, event.top, get_uvalue=info,/NO_COPY
   FOR i=0L,3 DO BEGIN
     FOR j=0L,num_models-1 DO BEGIN
       info.(gridIDs[i])[modelindex[j]] = $
          PTR_NEW(itera_get_lineflux(uvalue.(i),*info.p_models[modelindex[j]]))
     ENDFOR
   ENDFOR
   info.gridexist=1B 
   wset,info.windowID
   itera_doplot,info
   widget_control, event.top, set_uvalue=info,/NO_COPY
 ENDIF   
 RETURN
END

;--------------------------------------------------------------------------
; itera_helpmenu_handler: Handles events from the Help button in the
;                         menu, displaying both the version and the
;                         instructions.
;---------------------------------------------------------------------------
PRO itera_helpmenu_handler, event
 On_Error, 2
  widget_control, event.id, get_uvalue=uvalue

 CASE uvalue OF
   'about': begin
     about_text = ['itera version 1.0', $
                   'Brent Groves, December 2009',$
                   'Distributed under GNU GPLv3']
     ok = dialog_message(about_text,/information, dialog_parent=event.top)    
   end
   'instructions': begin
     work_dir=file_dirname(file_which('itera.pro',/include_current_dir) $
                       ,/mark_dir)
     xdisplayfile, work_dir+'INSTRUCTIONS.txt', height=32, group=event.top
    end
 ENDCASE

END 

;---------------------------------------------------------------------------
; itera_lineselection_handler: Handles events from the line selection
;                              widgets, reading in the selected line for
;                              the given box (numerator/denominator of
;                              one of the ratios), adding this line to
;                              the list box beside the selection box
;                              and finally plotting the grid if both
;                              models and lines are selected.
;---------------------------------------------------------------------------
PRO itera_lineselection_handler, event
On_Error,2
 widget_control, event.top, get_uvalue=info,/No_copy
 widget_control, event.id, get_uvalue=lineindex
 info_list=tag_names(info)
 CASE event.id OF
   info.x1ID: BEGIN
     chosen_list = info.X1list
     grid = WHERE(info_list EQ 'X1GRID')
   END
   info.x2ID: BEGIN
     chosen_list = info.X2list
     grid = WHERE(info_list EQ 'X2GRID')
   END
   info.y1ID: BEGIN
     chosen_list = info.Y1list
     grid = WHERE(info_list EQ 'Y1GRID')
   END
   info.y2ID: BEGIN
     chosen_list = info.Y2list
     grid = WHERE(info_list EQ 'Y2GRID')
   END
   ELSE:BEGIN 
     print,'No matching case found!!!'
     widget_control, event.top, set_uvalue=info,/No_copy
     RETURN
   END 
 ENDCASE
 linename=info.lines[lineindex[event.index]]
 Widget_Control,chosen_list, get_UValue=current_list
 IF current_list[0] EQ -1 THEN BEGIN
   sel_line_index=[lineindex[event.index]]
   sel_lines=[linename]
 ENDIF ELSE BEGIN
   sel_line_index=[current_list,lineindex[event.index]]
   current_lines=info.lines[current_list]
   sel_lines=[current_lines,linename]
 ENDELSE
;
; Set Lines in line list widgets
;
 Widget_Control,chosen_list, set_Value=sel_lines 
 Widget_Control,chosen_list, set_UValue=sel_line_index
;
; IF models are chosen Set values of grids 
;
 modelindex=WHERE(PTR_VALID(info.p_models),num_models)
 IF num_models NE 0 THEN BEGIN
   PTR_FREE,info.(grid)
   FOR i=0L,num_models-1 DO BEGIN
     info.(grid)[modelindex[i]] = $
        PTR_NEW(itera_get_lineflux(sel_line_index,*info.p_models[modelindex[i]]))
   
  ENDFOR
; Check that there are valid line selections for X1,X2,Y1,Y2
   Widget_control,info.X1list,Get_UValue=U_X1
   Widget_control,info.X2list,Get_UValue=U_X2
   Widget_control,info.Y1list,Get_UValue=U_Y1
   Widget_control,info.Y2list,Get_UValue=U_Y2
   IF (U_X1[0] NE -1) AND (U_X2[0] NE -1) AND (U_Y1[0] NE -1) AND $
      (U_Y2[0] NE -1) THEN info.gridexist=1B ; Fullgrid now exists
;
; If Data selected, determine values
   IF info.n_data NE 0 THEN get_obs_ratios, info
;
; Finally, Redo plot with new lines if possible
   wset,info.windowID
   itera_doplot,info
 ENDIF

 widget_control, event.top, set_uvalue=info,/No_copy
 RETURN
END

;---------------------------------------------------------------------------
; itera_clear_line_handler: Handles events from the line clear
;                           buttons. Clears all lines from the
;                           selected ratio denominator/numerator (ie
;                           X1, X2, Y1, Y2). 
;---------------------------------------------------------------------------
PRO itera_clear_line_handler,event
On_Error,2
 widget_control, event.top, get_uvalue=info ,/No_copy
 widget_control, event.id, get_value=value
 CASE value OF
  'Clear X1':BEGIN
    Widget_control,info.X1ID, Set_List_Select=-1    
    Widget_control,info.X1list,Set_Value=[' ']
    Widget_control,info.X1list,Set_UValue=[-1]   
    chosen_grid = info.X1grid
  END
  'Clear X2':BEGIN
    Widget_control,info.X2ID, Set_List_Select=-1    
    Widget_control,info.X2list,Set_Value=[' ']
    Widget_control,info.X2list,Set_UValue=[-1]   
    chosen_grid = info.X2grid
  END
  'Clear Y1':BEGIN
    Widget_control,info.Y1ID, Set_List_Select=-1    
    Widget_control,info.Y1list,Set_Value=[' ']
    Widget_control,info.Y1list,Set_UValue=[-1]   
    chosen_grid = info.Y1grid
  END
  'Clear Y2':BEGIN
    Widget_control,info.Y2ID, Set_List_Select=-1    
    Widget_control,info.Y2list,Set_Value=[' ']
    Widget_control,info.Y2list,Set_UValue=[-1]   
    chosen_grid = info.Y2grid
  END
 ENDCASE 
;
; IF models are chosen Unset values of grids 
;
 PTR_FREE,chosen_grid
 info.gridexist=0B ; Grid no longer exists for plot
;
; Reset added Datapoints
 *info.addXval=[-99]
 *info.addYval=[-99]
 *info.addlabel=''
;
; Clear plot window
;
 wset,info.windowID
 erase 
 widget_control, event.top, set_uvalue=info ,/No_copy
END

;---------------------------------------------------------------------------
; itera_modeltree_handler: Handles all events from the model selection
;                          widget. This includes directory expansion
;                          in the tree, model selection, and model
;                          description in the text box. If emission
;                          line ratios are selected, htis routine will
;                          also read in all relevant lines from the
;                          selected model and draw the grid in the
;                          window. 
;---------------------------------------------------------------------------
PRO itera_modeltree_handler,event
 On_Error,2
 widget_control, event.id, get_Uvalue=uvalue
 widget_control, event.id, get_value=modelname
 widget_control, event.top, get_uvalue=info ,/No_copy
; IF simply a Folder expansion, then return
 IF n_elements(uvalue) EQ 0 THEN BEGIN
   Widget_Control, event.id, Set_List_Select=*info.modellist
   Widget_Control, event.top, set_uvalue=info ,/No_copy
   RETURN
 ENDIF
 type=strmid(modelname,0,3)
; 
; If a model is selected again, this will "unselect" the model
;  and remove it from the grid 
;
 preselect=where(*info.modellist EQ event.id,complement=other_ids)
 IF preselect  NE -1 THEN BEGIN
   IF n_elements(preselect) GT 1 THEN BEGIN
     Widget_Control, event.id, Set_List_Select=*info.modellist
     Widget_Control, event.top, set_uvalue=info ,/No_copy     
     RETURN
   ENDIF
   IF n_elements(*info.modellist) EQ 1 THEN BEGIN
     Widget_Control, event.id, Set_List_Select=[-1L]
     Widget_Control, event.top, set_uvalue=info ,/No_copy
     itera_modelreset_handler,$
        {CLEAR_BUTTON, ID:0L, TOP:event.top, HANDLER:0L, SELECT:0}
   ENDIF ELSE IF type NE 'Obs' THEN BEGIN
     Widget_Control, (*info.modellist)[preselect[0]], Set_Tree_Select=0
     *info.modellist=(*info.modellist)[other_ids]
; Delete the selected model
     n_others=n_elements(other_ids)
     PTR_FREE,(*info.p_models[preselect[0]]).models
     PTR_FREE,info.p_models[preselect[0]]
     info.p_models[0:n_others-1]=info.p_models[other_ids]
; If lines selected, clear those as well
     Widget_control,info.X1list,Get_UValue=lines_X1
     Widget_control,info.X2list,Get_UValue=lines_X2
     Widget_control,info.Y1list,Get_UValue=lines_Y1
     Widget_control,info.Y2list,Get_UValue=lines_Y2
     IF (lines_X1[0] NE -1) THEN BEGIN
       PTR_FREE,info.X1grid[preselect[0]]
       info.X1grid[0:n_others-1]=info.X1grid[other_ids]
     ENDIF
     IF (lines_X2[0] NE -1) THEN BEGIN
       PTR_FREE,info.X2grid[preselect[0]]
       info.X2grid[0:n_others-1]=info.X2grid[other_ids]
     ENDIF
     IF (lines_Y1[0] NE -1) THEN BEGIN
       PTR_FREE,info.Y1grid[preselect[0]]
       info.Y1grid[0:n_others-1]=info.Y1grid[other_ids]
     ENDIF
     IF (lines_Y2[0] NE -1) THEN BEGIN
       PTR_FREE,info.Y2grid[preselect[0]]
       info.Y2grid[0:n_others-1]=info.Y2grid[other_ids]
     ENDIF
     wset,info.windowID
     itera_doplot,info
     Widget_Control, event.id, Set_List_Select=*info.modellist
     Widget_Control, event.top, set_uvalue=info ,/No_copy
   ENDIF ELSE BEGIN; Currently can't de-select Observational Data
     Widget_Control, event.id, Set_List_Select=*info.modellist
     Widget_Control, event.top, set_uvalue=info ,/No_copy    
   ENDELSE
   RETURN   
 ENDIF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Handle observational data seperately
;
 IF type EQ 'Obs'THEN BEGIN
 ; Update model index list with the newly selected data set index 
; Checking to see that the model index is not -1 (none selected)
   modellist = ((*info.modellist)[0] NE -1) ? [*info.modellist,event.id] $
                                       : [event.id]
;
; If there is more than one model set selected, make sure the 
; tree widget correctly shows this. Note that multiple only returns
; last selected index (but highlights more)
   FOR i=0,n_elements(modellist)-1 DO $
      Widget_Control, modellist[i], /Set_Tree_Select
   *info.modellist = modellist
;
; List data in modelselection window
  Widget_control,info.modellistID,Get_Value=oldmodeltype
  Widget_control,info.modellistID,Set_Value=$
         [modelname,"-----------------------",oldmodeltype]
;
; Read in new data
;
  datastr=' '
  comment='%'
  openr,lun,uvalue,/get_lun
  readf,lun,datastr
  comment=strmid(datastr,0,1)
  WHILE (comment EQ '%') AND (~EOF(lun)) DO BEGIN
    readf,lun,datastr
    comment=strmid(datastr,0,1)
  ENDWHILE
; first line lists line indices
  data=strsplit(datastr,STRING(9B)+",",/EXTRACT,count=n_lines)
  lineindices=LONG(data[1:n_lines-1])
  WHILE(~EOF(lun)) DO BEGIN
    readf,lun,datastr
    comment=strmid(datastr,0,1)
    IF comment NE '%' THEN BEGIN
      data=strsplit(datastr,STRING(9B)+",",/EXTRACT,count=n_linedata)
      IF n_linedata NE n_lines THEN BEGIN 
        IF n_linedata GT 1 THEN BEGIN 
          print,"Data format error... Skipping Line"
          print,data[0]
        ENDIF
      ENDIF ELSE BEGIN
       label=data[0]
       lineflux=DOUBLE(data[1:n_linedata-1])
       newdata={label:label,$
                lineflux:lineflux,$
                lineindex:lineindices}
       info.p_data[info.n_data]=PTR_NEW(newdata,/NO_COPY)
       info.n_data=info.n_data+1
      ENDELSE
    ENDIF
  ENDWHILE
  close,lun
  free_lun,lun
;
; now determine lines if the selected lines are chosen
;
  get_obs_ratios,info
;for i=0,info.n_data-1 DO help,/str,*info.p_data[i]

  wset,info.windowID
  itera_doplot,info
;
; Save info struct
  widget_control, event.top, set_uvalue=info ,/No_copy
  RETURN
 ENDIF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
; Model Data
;
; maximum number of allowed models is limited so check
;
 modelindex=WHERE(PTR_VALID(info.p_models),n_models)
 IF n_models  EQ info.maxmodels THEN BEGIN
   OK=DIALOG_MESSAGE($
      STRING(FORMAT='("Maximum of", i3," models allowed at one time")',info.maxmodels) $
      ,/ERROR,dialog_parent=event.id) 
   Widget_Control, event.id, Set_List_Select=*info.modellist
   Widget_Control, event.top, set_uvalue=info ,/No_copy
   RETURN
 ENDIF
; Update model index list with the newly selected model set index 
; Checking to see that the model index is not -1 (none selected)
 modellist = ((*info.modellist)[0] NE -1) ? [*info.modellist,event.id] $
                                       : [event.id]
;
; If there is more than one model set selected, make sure the 
; tree widget correctly shows this. Note that multiple only returns
; last selected index (but highlights more)
 FOR i=0,n_elements(modellist)-1 DO $
    Widget_Control, modellist[i], /Set_Tree_Select
 *info.modellist = modellist
;
; Read in new model set 
 newmodel=read_iteramodelseries(file_list=uvalue,modelsetname=modelname)
;
;list selected model in text box
 modelparams=STRARR(4)
 paramnames=tag_names(*newmodel.models[0])
 valuelabel=[' Zo',' H/cm^3','']
 FOR i=1,3 DO BEGIN 
  IF (i EQ info.n_para2) THEN $
     value=STRCOMPRESS(string(min(newmodel.(i))))+"--"+$
           STRCOMPRESS(string(max(newmodel.(i)))) $
  ELSE value=STRCOMPRESS(string((*newmodel.models[0]).(i+1)))
  IF (size((*newmodel.models[0]).(i+1),/type) NE 7) THEN $
     value=value+valuelabel[i-1]
  modelparams[i-1]=string(paramnames[i+1],FORMAT='(a9," : ")')+value
 ENDFOR
 value=STRCOMPRESS(string(min((*newmodel.models[0]).(5))))+" --"+$
       STRCOMPRESS(string(max((*newmodel.models[0]).(5)))) 
 modelparams[3]=string(paramnames[5],FORMAT='(a9," : ")')+value
 modeltype=[(*newmodel.models[0]).model_type,modelparams]

 Widget_control,info.modellistID,Get_Value=oldmodeltype
 Widget_control,info.modellistID,Set_Value=$
         [modeltype,"-----------------------",oldmodeltype]
;
; With model selected, check lines to see if selected,
; Then grids can be set
 Widget_control,info.X1list,Get_UValue=lines_X1
 Widget_control,info.X2list,Get_UValue=lines_X2
 Widget_control,info.Y1list,Get_UValue=lines_Y1
 Widget_control,info.Y2list,Get_UValue=lines_Y2
 IF (lines_X1[0] NE -1) THEN BEGIN
   info.X1grid[n_models] = $
        PTR_NEW(itera_get_lineflux(lines_X1,newmodel))
   X1exist=1B
 ENDIF ELSE X1exist=0B
 IF (lines_X2[0] NE -1) THEN BEGIN 
   info.X2grid[n_models] = $
        PTR_NEW(itera_get_lineflux(lines_X2,newmodel))
   X2exist=1B
 ENDIF ELSE X2exist=0B
 IF (lines_Y1[0] NE -1) THEN BEGIN 
   info.Y1grid[n_models] = $
        PTR_NEW(itera_get_lineflux(lines_Y1,newmodel))
   Y1exist=1B
 ENDIF ELSE Y1exist=0B
 IF (lines_Y2[0] NE -1) THEN BEGIN 
   info.Y2grid[n_models] = $
        PTR_NEW(itera_get_lineflux(lines_Y2,newmodel))
   Y2exist=1B
 ENDIF ELSE Y2exist=0B
 IF X1exist AND X2exist AND Y1exist AND Y2exist THEN info.gridexist=1B
;
; save model in empty pointer
 info.p_models[n_models]=PTR_NEW(newmodel,/NO_COPY)

; Finally, Redo plot with new lines if possible
 wset,info.windowID
 itera_doplot,info
;
; Save info struct

  widget_control, event.top, set_uvalue=info ,/No_copy
  RETURN
END

;---------------------------------------------------------------------------
; Get_Obs_ratios: when passed the info struct determines the x & Y
;       line ratios for the observational data points in p_data
;
;---------------------------------------------------------------------------
PRO get_obs_ratios, info
 On_Error,2
 Widget_control,info.X1list,Get_UValue=lines_X1
 Widget_control,info.X2list,Get_UValue=lines_X2
 Widget_control,info.Y1list,Get_UValue=lines_Y1
 Widget_control,info.Y2list,Get_UValue=lines_Y2
 X1val=1.d-99
 X2val=1.d-50
 Y1val=1.d-99
 y2val=1.d-50
 IF (lines_X1[0] NE -1) AND (lines_X2[0] NE -1) AND $
    (lines_Y1[0] NE -1) AND (lines_Y2[0] NE -1) THEN BEGIN
    FOR i=0,info.n_data-1 DO BEGIN ;loop over all data
     FOR lind=0,n_elements(lines_X1)-1 DO BEGIN 
       ind=where((*info.p_data[i]).lineindex EQ lines_X1[lind])
       IF ind[0] NE -1 THEN X1val=X1val+(*info.p_data[i]).lineflux[ind]
     ENDFOR
     FOR lind=0,n_elements(lines_X2)-1 DO BEGIN 
       ind=where((*info.p_data[i]).lineindex EQ lines_X2[lind])
       IF ind[0] NE -1 THEN X2val=X2val+(*info.p_data[i]).lineflux[ind]
     ENDFOR
     FOR lind=0,n_elements(lines_Y1)-1 DO BEGIN 
       ind=where((*info.p_data[i]).lineindex EQ lines_Y1[lind])
       IF ind[0] NE -1 THEN Y1val=Y1val+(*info.p_data[i]).lineflux[ind]
     ENDFOR
     FOR lind=0,n_elements(lines_Y2)-1 DO BEGIN 
       ind=where((*info.p_data[i]).lineindex EQ lines_Y2[lind])
       IF ind[0] NE -1 THEN Y2val=Y2val+(*info.p_data[i]).lineflux[ind]
     ENDFOR
     IF (*info.addXval)[0] EQ -99 THEN BEGIN
       *info.addXval=ALOG10(X1val/X2val)
       *info.addYval=ALOG10(Y1val/Y2val)
       *info.addlabel= (*info.p_data[i]).label    
     ENDIF ELSE BEGIN
       *info.addXval=[*info.addXval,ALOG10(X1val/X2val)]
       *info.addYval=[*info.addYval,ALOG10(Y1val/Y2val)]
       *info.addlabel=[*info.addlabel,(*info.p_data[i]).label]      
     ENDELSE
    ENDFOR
 ENDIF
 RETURN
END

;---------------------------------------------------------------------------
; itera_model_reset_handler: Clears all selected model sets and frees
;                            up associated pointers
;---------------------------------------------------------------------------
PRO itera_modelreset_handler,event
 On_Error,2
 widget_control, event.top, get_uvalue=info ,/No_copy
 Widget_control, info.modellistID,Set_Value=" "
;
; Exit if there's nothing to do (ie no models selected)
 IF *info.modellist EQ [-1L] THEN BEGIN 
  Widget_Control, event.top, set_uvalue=info ,/No_copy
  RETURN
 ENDIF
;
; Clear selected models
 FOR i=0,n_elements(*info.modellist)-1 DO $
   Widget_Control, (*info.modellist)[i], Set_Tree_Select=0
 *info.modellist = [-1L]
;
; If models are chosen Unset values of grids and free models
 modelindex=WHERE(PTR_VALID(info.p_models),num_models)
 IF num_models NE 0 THEN BEGIN
   FOR i=0L,num_models-1 DO BEGIN
     PTR_FREE,(*info.p_models[modelindex[i]]).models
   ENDFOR
 ENDIF
 PTR_FREE,info.X1grid,info.X2grid,info.Y1grid,info.Y2grid
 PTR_FREE,info.p_models
 PTR_FREE,info.p_data
 info.n_data=0L
;;
; Reset Parameter Ranges
 info.p1range=[-99.0,-99.0]
 info.p2range=[-99.0,-99.0]
;
; Reset Added Observational Data 
 *info.addXval=[-99]
 *info.addYval=[-99]
 *info.addlabel=''

 info.gridexist=0B ; Grid no longer exists for plot
;
; Clear plot window
 wset,info.windowID
 erase 
 Widget_Control, event.top, set_uvalue=info ,/No_copy
 RETURN
END

;---------------------------------------------------------------------------
; itera_label_handler: Sets whether the diagnostic diagram has colour
;                      tables for the grid, grid labels or no labels
;                      at all. 
;---------------------------------------------------------------------------
PRO itera_label_handler,event
 On_error,2
 widget_control, event.top, get_uvalue=info,/No_copy 
 widget_control, event.id, get_Uvalue=uvalue
 CASE uvalue OF
  'NONE': BEGIN
   info.plot_ct=0B
   info.gridvalues=0B
  END
  'CT': BEGIN
   info.plot_ct=1B
   info.gridvalues=0B
  END
  'GRID': BEGIN
   info.plot_ct=0B
   info.gridvalues=1B
  END
 ENDCASE
; Finally, Redo plot with labels
 wset,info.windowID
 itera_doplot,info
;
; Save info struct
 Widget_Control, event.top, set_uvalue=info ,/No_copy
 RETURN
END

;---------------------------------------------------------------------------
;
; itera_limitparameterrange_handler: Brings up a floating widget in
;                          which the range of the free parameters
;                          (param1 & selectedparam) can be altered.
;                          Once entered the grid with these new ranges
;                          will be re-plotted. 
;---------------------------------------------------------------------------
PRO itera_LimitParamRange_Handler,event
 On_error,2
 widget_control, event.top, get_uvalue=info,/No_copy 
 modelindex=WHERE(PTR_VALID(info.p_models),n_models)
;
; Exit if there's nothing to do (ie no models selected)
 IF n_models EQ 0 THEN BEGIN 
  Widget_Control, event.top, set_uvalue=info ,/No_copy
  RETURN
 ENDIF 
;
;Set Min and max values of the parameters from the first model
 basemodel=*info.p_models[modelindex[0]]
 paramnames=tag_names(*(basemodel.models)[0])
;  first parameter (ie V)
 maxindex=0
 maxparam1=-1
 FOR i=0,n_elements(basemodel.models)-1 DO BEGIN
   param1=(*(basemodel).models[i]).(5)
   maxparam1=(n_elements(param1) GT maxindex)? param1 : maxparam1
 ENDFOR
 minp1=MIN(maxparam1)
 maxp1=MAX(maxparam1)
;  Second parameter (ie B) 
 param2=basemodel.(info.n_para2)
 IF size(param2,/type) EQ 7 THEN param2=[0.0,0.0]
 minp2=MIN(param2)
 maxp2=MAX(param2)
 p1range=(info.p1range[0] NE -99.0) ? $
      info.p1range : [minp1,maxp1]
 p2range=(info.p2range[0] NE -99.0) ? $
      info.p2range : [minp2,maxp2] 
 MinP1s=STRING(p1range[0],FORMAT='(G10.4)')
 MaxP1s=STRING(p1range[1],FORMAT='(G10.4)')
 MinP2s=STRING(p2range[0],FORMAT='(G10.4)')
 MaxP2s=STRING(p2range[1],FORMAT='(G10.4)');
;
; Create Floating Widget
 PrangeID=Widget_Base(/col,TITLE='Parameter Range Limits',$
       /FLOATING,GROUP_LEADER=event.top,$
       EVENT_PRO='itera_SetParamRange_Event')
 PvalsID=Widget_Base(PrangeID,row=2)
 P1label=Widget_Label(PvalsID,Value=paramnames[5],ysize=30,xsize=70,$
                     /ALIGN_LEFT)
 P1min=Widget_Text(PvalsID,value=MinP1s,/Editable,xsize=10,/ALIGN_RIGHT)
 P1max=Widget_Text(PvalsID,value=MaxP1s,/Editable,xsize=10,/ALIGN_RIGHT)
 P2label=Widget_Label(PvalsID,Value=paramnames[info.n_para2+1],ysize=30,xsize=70,$
                     /ALIGN_LEFT)
 P2min=Widget_Text(PvalsID,value=MinP2s,/Editable,xsize=10,/ALIGN_RIGHT)
 P2max=Widget_Text(PvalsID,value=MaxP2s,/Editable,xsize=10,/ALIGN_RIGHT)
 P_set=Widget_Base(PrangeID,/row,/ALIGN_CENTER)
 P_OK=Widget_Button(P_set,Value='OK',Uvalue='OK',Xsize=60)
 P_Cancel=Widget_Button(P_set,Value='Cancel',UValue='Cancel',Xsize=60)
 Widget_Control,PrangeID,/Realize
;
; Set up State widget for reading new values  
 state={p1range:PTR_NEW(p1range,/No_Copy),$
        p2range:PTR_NEW(p2range,/No_Copy),$
        P1min:P1min,P1max:P1max,P2min:P2min,P2max:P2max}
 Widget_Control,PrangeID,Set_Uvalue=state
;
 Xmanager,'itera_SetParamRange',PrangeID
;
; Save returned ranges  
 info.p1range=[MIN([(*state.p1range),maxp1]),$
               MAX([(*state.p1range),minp1])]
 info.p2range=[MIN([(*state.p2range),maxp2]),$
               MAX([(*state.p2range),minp2])]
 PTR_FREE,state.p1range,state.p2range
; Finally, Redo plot with labels
 wset,info.windowID
 itera_doplot,info
;
; Save info struct
 Widget_Control, event.top, set_uvalue=info ,/No_copy
 RETURN
END

;---------------------------------------------------------------------------
;
; itera_SetParamRange_Event: This controls the floating Widget which
;                     limits the plotted parameter ranges of the model
;                     grid.
;---------------------------------------------------------------------------
PRO itera_SetParamRange_Event, event
 ON_Error,2
 widget_control, event.id, get_Value=Value
 CASE Value OF
   'OK':BEGIN
     Widget_Control,event.top,Get_UValue=state,/No_Copy
     Widget_Control,state.P1min, Get_Value=minP1s
     Widget_Control,state.P1max, Get_Value=maxP1s
     Widget_Control,state.P2min, Get_Value=minP2s
     Widget_Control,state.P2max, Get_Value=maxP2s
     *state.p1range=[FLOAT(minP1s),FLOAT(maxP1s)]
     *state.p2range=[FLOAT(minP2s),FLOAT(maxP2s)]
     Widget_Control,event.top,Set_UValue=state,/No_Copy     
     Widget_Control,event.top,/DESTROY
     RETURN
   END
   'Cancel':BEGIN
     Widget_Control,event.top,/DESTROY
     RETURN
   END
   ELSE: RETURN
 ENDCASE
 RETURN
END

;---------------------------------------------------------------------------
; itera_PlotRange_Handler: Reacts to the Plot range sliders below the
;                          draw window and sets X & Y range based on
;                          the sliders or text boxes. 
;---------------------------------------------------------------------------
PRO itera_PlotRange_Handler,event
 On_Error,2
 widget_control, event.top, get_uvalue=info ,/No_copy
 widget_control, event.id, get_Uvalue=uvalue
 widget_control, event.id, get_value=value
 CASE uvalue OF
  'Xmin':info.xrange=[value,info.xrange[1]]
  'Xmax':info.xrange=[info.xrange[0],value]
  'Ymin':info.yrange=[value,info.yrange[1]]
  'Ymax':info.yrange=[info.yrange[0],value]
 ENDCASE
 wset,info.windowID
 itera_doplot,info
 widget_control, event.top, set_uvalue=info ,/No_copy 
 RETURN
END

;---------------------------------------------------------------------------
; itera_doplot: This is a wrapper routine which sets up thegrids, and
;               the axes and grid titles. It also overplots any
;               observational data points added by the user.
;---------------------------------------------------------------------------
PRO itera_doplot,info,postscript=ps
 On_Error,2
; Return if the grid doesnt exist for some strange reason
 IF ~info.gridexist THEN RETURN 
 titles=strarr(4)
 modelindex=WHERE(PTR_VALID(info.p_models),num_models)
 x1 = info.x1grid
 x2 = info.x2grid
 y1 = info.y1grid
 y2 = info.y2grid

;
; Get line names (ion & wavelength) and compress if the same ion
 list=[info.X1list,info.X2list,info.Y1list,info.Y2list]
 FOR i=0L,3 DO BEGIN
   Widget_control,list[i],Get_UValue=U_index
   linenames=info.lines[U_index]
   j=0L
   wavel=STRARR(n_elements(linenames))
   WHILE (n_elements(linenames) GT 1) AND (j LT  n_elements(linenames)) $
     DO BEGIN
     ion=STRMID(linenames[j],0,strpos(linenames[j],' ')+1)
     match=WHERE(STRMATCH(linenames,ion+"*"),num,complement=nomatch)
     IF num NE 0 THEN BEGIN 
      FOR k=0L,num-1 DO $
       wavel[k]=STRMID(linenames[match[k]],$
               strpos(linenames[match[k]],' ')+1,STRLEN(linenames[match[k]]))
      newline=ion+STRCOMPRESS(STRING(wavel,FORMAT='(5(a9,:,","))'),/REMOVE)
     ENDIF
     linenames= (nomatch[0] NE -1) ? [newline,linenames[nomatch]] : newline
     j+=1
   ENDWHILE
   titles[i]=STRCOMPRESS(STRING(linenames,FORMAT='(5(a20,:," +"))'))
 ENDFOR
 xtitle="log!I10!N("+titles[0]+"/"+titles[1]+")"
 ytitle="log!I10!N("+titles[2]+"/"+titles[3]+")"
;
; Set Window (If not postscript) and plot the model
;
 IF ~KEYWORD_SET(ps) THEN wset,info.windowID
 FOR i=0L,num_models-1 DO BEGIN
; Set Up Paramter values for labels and Ranges
   paramnames=tag_names(*((*info.p_models[modelindex[i]]).models)[0])
; first parameter (ie V)
   maxindex=0
   maxparam1=-1
   FOR j=0,n_elements((*info.p_models[modelindex[i]]).models)-1 DO BEGIN
     param1=(*(*info.p_models[modelindex[i]]).models[j]).(5)
     maxparam1=(n_elements(param1) GT maxindex)? param1 : maxparam1
   ENDFOR
   p1title=paramnames[5]
; Second parameter (ie B)  
   param2=(*info.p_models[modelindex[i]]).(info.n_para2)
   p2title=paramnames[info.n_para2+1]
;
; Set Parameter Ranges
   IF info.p1range[0] EQ -99 THEN $
     p1range=[min(maxparam1),max(maxparam1)] $
   ELSE $
     p1range=info.p1range
   IF info.p2range[0] EQ -99 THEN $
     p2range=[min(param2),max(param2)] $
   ELSE $
     p2range=info.p2range

   plot_iteragrid,*x1[i],*x2[i],*y1[i],*y2[i],$
     xr=info.xrange,xtit=xtitle, $
     yr=info.yrange,ytit=ytitle,overplot=(i GT 0),$
     plot_ct=info.plot_ct,grid_vals=info.gridvalues,$
     p1values=maxparam1,p1title=p1title,p1range=p1range,$
     p2values=param2,p2title=p2title,p2range=p2range
 ENDFOR
;
; Add observational Data
 IF (*info.addXval)[0] NE -99 THEN BEGIN
     Xdata=(n_elements(*info.addXval) GT 1) ?  *info.addXval : $
            [(*info.addYval)[0],(*info.addYval)[0]]
     Ydata=(n_elements(*info.addYval) GT 1) ?  *info.addYval : $
            [(*info.addYval)[0],(*info.addYval)[0]]
     oplot,Xdata,ydata,psym=7
     IF (~info.hidelabel) THEN $
       xyouts,(*info.addXval),(*info.addYval),(*info.addlabel)
 ENDIF     
 IF ~KEYWORD_SET(ps) THEN BEGIN
; Now that the plot is drawn, we keep a copy of the graphics window, 
; which is needed for the rubber-band-box selection function for
; zooming in, and redrawing the plot with different x and y limits
; (as is done in draw_events)
   Wset,info.pixID
   Device, Copy = [0, 0, 600, 500, 0, 0, info.windowID]
;
; reset the graphics window to the main widget graphics window
   Wset, info.windowID
 ENDIF
 RETURN 
END

;---------------------------------------------------------------------------
; itera_draw_handler: This procedure enables the creation of "Zoom
;                     Boxes" on the draw window that set the
;                     plot ranges for the window zooming in or out on
;                     the plot. 
;---------------------------------------------------------------------------
PRO itera_Draw_Handler,event
 On_error,2
 Widget_Control, event.top, Get_UValue=info, /No_Copy
 IF ~info.gridexist THEN BEGIN
; Dont do anything if plot doesnt exist
   Widget_Control, event.top, Set_UValue=info, /No_Copy  
   RETURN 
 ENDIF
;
; What type of an event is this?
 possibleEventTypes = [ 'DOWN', 'UP', 'MOTION', 'SCROLL' ]
 thisEvent = possibleEventTypes(event.type)
 CASE thisEvent OF
   'DOWN': BEGIN
     Widget_Control, event.id, Draw_Motion_Events=1
     info.xzoom_s=event.x
     info.yzoom_s=event.y
   END
   'UP':BEGIN
     Widget_Control, event.id, Draw_Motion_Events=0    
     event.x = 0 > event.x < (599)
     event.y = 0 > event.y < (499)
;
; Make sure the user didn't just click in the window.
     IF info.xzoom_s EQ event.x OR info.yzoom_s EQ event.y THEN BEGIN
       Widget_Control, event.top, Set_UValue=info, /No_Copy
       RETURN
     ENDIF
;
; Get limits in plot coordinates 
     x = [info.xzoom_s, event.x]
     y = [info.yzoom_s, event.y]
     top_left_dev = [min(x),max(y)]
     bot_right_dev = [max(x),min(y)]
     top_left = convert_coord(top_left_dev,/device,/to_data)
     bot_right = convert_coord(bot_right_dev,/device,/to_data)
;
; Set the new limits
     Widget_Control, info.xminID, Set_Value=top_left[0]
     Widget_Control, info.xmaxID, Set_Value=bot_right[0]
     Widget_Control, info.yminID, Set_Value=bot_right[1]
     Widget_Control, info.ymaxID, Set_Value=top_left[1]
     info.xrange=[top_left[0],bot_right[0]]
     info.yrange=[bot_right[1],top_left[1]]
;
; Finally redo plot with new limits
     wset,info.windowID
     itera_doplot,info
   END
   'MOTION': BEGIN
; Most of the action in this event handler occurs here while we are waiting
; for an UP event to occur. As long as we don't get it, keep erasing the
; old zoom box and drawing a new one.
;
; Erase the old zoom box.
     WSet, info.windowID
     Device, Copy = [0, 0, 600, 500, 0, 0, info.pixID]
;
; Update the dynamic corner of the zoom box to the current cursor location.
     info.xzoom_d = event.x
     info.yzoom_d = event.y
     PlotS,[info.xzoom_s,info.xzoom_s,info.xzoom_d,info.xzoom_d,info.xzoom_s],$
           [info.yzoom_s,info.yzoom_d,info.yzoom_d,info.yzoom_s,info.yzoom_s],$
           /Device
   END
 ENDCASE
 Widget_Control, event.top, Set_UValue=info, /No_Copy
 RETURN
END

;---------------------------------------------------------------------------
; itera_button_handler: This si the routine that handles input from
;                       all buttons below the draw window;
;                range: Resets plot range (xmin-xmax,ymin-ymax) to
;                       [-3,3]
;                clear: Clears all data in ITERA; observational, model
;                       and plot
;                 quit: Exits ITERA
;             add_data: Allows the user to add observational data
;                       points 
;                
;                       User defined buttons can also be added here.
;---------------------------------------------------------------------------
PRO itera_Button_Handler,Event
 On_Error,2
 widget_control, event.top, get_uvalue=info,/No_copy 
 widget_control, event.id, get_Uvalue=uvalue
 widget_control, event.id, get_value=value
 CASE uvalue OF
  'range':BEGIN
   min=-3.0
   max=3.0
   info.xrange=[min,max]
   info.yrange=[min,max]
   Widget_control,info.xminID,Set_value=min
   Widget_control,info.xmaxID,Set_value=max
   Widget_control,info.yminID,Set_value=min
   Widget_control,info.ymaxID,Set_value=max
   wset,info.windowID
   itera_doplot,info
  END
  'clear':BEGIN
;
; Clear line data
    clearIDs=[info.X1clear,info.X2clear,info.Y1clear,info.Y2clear]
    widget_control, event.top, set_uvalue=info ,/No_copy 
    FOR i=0L,3 DO BEGIN
     pseudo_event={ID:clearIDs[i], top:event.top,Handler:0L,Select:1L}
     itera_Clear_line_handler,pseudo_event
    ENDFOR 
;
; Clear Model & Grid Data
    pseudo_event={ID:0L, top:event.top,Handler:0L,Select:1L}
    itera_modelreset_Handler,pseudo_event
; Reset X & Y ranges
   widget_control, event.top, get_uvalue=info ,/No_copy 
   min=-3.0
   max=3.0
   info.xrange=[min,max]
   info.yrange=[min,max]
   Widget_control,info.xminID,Set_value=min
   Widget_control,info.xmaxID,Set_value=max
   Widget_control,info.yminID,Set_value=min
   Widget_control,info.ymaxID,Set_value=max
  END
  'quit':BEGIN
     widget_control, event.top, set_uvalue=info ,/No_copy 
     itera_cleanup, event.top
     widget_control, event.top, /destroy
     RETURN
  END
  'adddata':BEGIN
;
; IF no grid to add data to, dont do anything
    IF ~info.gridexist THEN BEGIN
      Widget_Control, event.top, Set_UValue=info, /No_Copy
      RETURN
    ENDIF
   OK=0B
   state={Xval:PTR_NEW(/ALLOCATE_HEAP),$
         Yval:PTR_NEW(/ALLOCATE_HEAP),$
         datalabel:PTR_NEW(/ALLOCATE_HEAP),$
         OK:PTR_NEW(OK,/NO_COPY)}
   add_DataID=Widget_Base(/col,TITLE='Add Observational Data',$
       /FLOATING,GROUP_LEADER=event.top,$
       EVENT_PRO='itera_Add_Data_Event')
   dataformID=Widget_Base(add_DataID,/ROW,/ALIGN_CENTER)
   data_Point=Widget_Button(dataformID,Value='Add Data Point',$
                            Xsize=100)
   data_File=Widget_Button(dataformID,Value='Read Data File',$
                           Xsize=100)
   data_listID=Widget_Text(add_DataID,value="log(x1/x2) | log(y1/y2) | label",$
                           /SCROLL, xsize=50,ysize=10)
   data_set=Widget_Base(add_DataID,/row,/ALIGN_CENTER)
   data_Confirm=Widget_Button(data_set,Value='OK',Uvalue=info,Xsize=70)
   data_Cancel=Widget_Button(data_set,Value='Cancel',UValue='Cancel',Xsize=70)
   Widget_control,add_DataID,/REALIZE
   Widget_Control,add_DataID,Set_Uvalue=state
   Widget_control,data_Point,Set_Uvalue=data_listID
   Widget_control,data_File,Set_Uvalue=data_listID
;
   Xmanager,'itera_Add_Data',add_DataID,/No_Block
  END
  'hidelabels':BEGIN
   info.hidelabel=~info.hidelabel
   wset,info.windowID
   itera_doplot,info
  END
  ELSE: BEGIN 
   OK=Dialog_Message('Button Not Yet Implemented!',$
                     /ERROR, dialog_parent=event.top)
  END
 ENDCASE
 widget_control, event.top, set_uvalue=info,/No_copy 
 RETURN
END

;---------------------------------------------------------------------------
; itera_add_data_event: Controls the widget for adding user-defined
;                (observational) data points to the currently defined
;                plot window. Allows entry of data via file or widget
;                and displays a list of points entered so far (and can
;                accept or reject points). 
;---------------------------------------------------------------------------
PRO itera_Add_Data_Event, event
 ON_Error,2
 widget_control, event.id, get_Value=Value
 CASE Value OF
  'Add Data Point': BEGIN
   widget_control, event.top, get_UValue=state,/No_Copy
;
; Create a Floating Widget with which to enter a read value data point
   add_PointID=Widget_Base(/col,TITLE='Add Observational Point',$
       /FLOATING,GROUP_LEADER=event.top,$
       EVENT_PRO='itera_Add_Datapoint_Event')
   datavalsID=Widget_Base(add_PointID,row=3)
   Xlabel=Widget_Label(datavalsID,Value='log(X1/X2)',ysize=30,xsize=80,$
                     /ALIGN_LEFT)
   XvalID=Widget_Text(datavalsID,value='0.0',/Editable,xsize=15,/ALIGN_RIGHT)
   Ylabel=Widget_Label(datavalsID,Value='log(Y1/Y2)',ysize=30,xsize=80,$
                     /ALIGN_LEFT)
   YvalID=Widget_Text(datavalsID,value='0.0',/Editable,xsize=15,/ALIGN_RIGHT)
   dataLabel_label=Widget_Label(datavalsID,value='Data Label',$
                     ysize=30,xsize=80,/ALIGN_LEFT)
   dataLabelID=Widget_Text(datavalsID,value='',/Editable,xsize=15,/ALIGN_RIGHT)
   data_Pset=Widget_Base(add_PointID,/row,/ALIGN_CENTER)
   data_Enter=Widget_Button(data_Pset,Value='Enter',Uvalue='Enter',Xsize=70)
   data_Cancel=Widget_Button(data_Pset,Value='Cancel',UValue='Cancel',Xsize=70)
   Widget_Control,add_PointID,/Realize
   OK=0B
   state=CREATE_STRUCT('XvalID',XvalID,'YvalID',YvalID,$
                       'datalabelID',datalabelID,$
                       state)
   Widget_Control,add_PointID,Set_Uvalue=state
;
   Xmanager,'itera_Add_datapoint',add_PointID
   IF *state.OK THEN BEGIN
    Widget_control, event.id,get_Uvalue=listID
    listnew="log(x1/x2) | log(y1/y2) | label"
    FOR i=0,n_elements(*state.Xval)-1 DO $
      listnew=[listnew,$
          STRING((*state.Xval)[i],(*state.Yval)[i],FORMAT='(2(G10.3," | "))')+$
          (*state.datalabel)[i]]
    Widget_control,listID,set_Value=listnew
   ENDIF    
   widget_control, event.top, set_UValue=state,/No_Copy
  END
  'Read Data File':BEGIN
   Widget_Control,event.top,Get_UValue=state,/No_Copy
   readfbaseID = widget_base(/col,TITLE="Open Data File",$
                          /FLOATING,GROUP_LEADER=event.top)
   CD,CURRENT=currentdir
   readfnameID=CW_FILESEL(readfbaseID,PATH=currentdir,$
                           FILTER=[".txt","All Files"])
   WIDGET_CONTROL,readfbaseID,/realize
   WIDGET_CONTROL,readfbaseID,Set_Uvalue=state
   XMANAGER,'itera_Readfile', readfbaseID
   IF *state.OK THEN BEGIN
    Widget_control, event.id,get_Uvalue=listID
    listnew="log(x1/x2) | log(y1/y2) | label"
    FOR i=0,n_elements(*state.Xval)-1 DO $
      listnew=[listnew,$
          STRING((*state.Xval)[i],(*state.Yval)[i],FORMAT='(2(G10.3," | "))')+$
          (*state.datalabel)[i]]
    Widget_control,listID,set_Value=listnew
   ENDIF    
   Widget_Control,event.top,Set_UValue=state,/No_Copy     
  END
  'OK':BEGIN
   widget_control, event.top, get_UValue=state,/No_Copy
   widget_control, event.id, get_UValue=info,/No_Copy   
   IF *state.OK THEN BEGIN
    IF (*info.addXval)[0] EQ -99 THEN BEGIN
       *info.addXval=*state.Xval
       *info.addYval=*state.Yval       
       *info.addlabel=*state.datalabel      
     ENDIF ELSE BEGIN
       *info.addXval=[*info.addXval,*state.Xval]
       *info.addYval=[*info.addYval,*state.Yval]      
       *info.addlabel=[*info.addlabel,*state.datalabel]      
    ENDELSE
   ENDIF    
   wset,info.windowID
   itera_doplot,info
   PTR_FREE,state.Xval,state.Yval,state.datalabel,state.OK
   widget_control, event.top, set_UValue=state,/No_Copy
   widget_control, event.id, set_UValue=info,/No_Copy   
   widget_control, event.top,/DESTROY
  END
  'Cancel': BEGIN
   widget_control, event.top, get_UValue=state,/No_Copy
   PTR_FREE,state.Xval,state.Yval,state.datalabel,state.OK   
   widget_control, event.top,/DESTROY
  END
 ENDCASE
 RETURN
END

;---------------------------------------------------------------------------
; itera_add_datapoint_event: This allows the entry of the X and Y
;                     values and label for an observational datapoint.
;---------------------------------------------------------------------------
PRO itera_Add_Datapoint_Event, event
 ON_Error,2
 widget_control, event.id, get_Value=Value
 CASE Value OF
   'Enter':BEGIN
     Widget_Control,event.top,Get_UValue=state,/No_Copy
     Widget_Control,state.XvalID,Get_Value=Xval    
     Widget_Control,state.YvalID,Get_Value=Yval
     Widget_Control,state.datalabelID,Get_Value=datalabel
     IF ~*state.OK THEN BEGIN
      *state.Xval=FLOAT(Xval)
      *state.Yval=FLOAT(Yval)
      *state.datalabel=" "+datalabel    
      *state.OK=1B
    ENDIF ELSE BEGIN
      *state.Xval=[*state.Xval,FLOAT(Xval)]
      *state.Yval=[*state.Yval,FLOAT(Yval)]
      *state.datalabel=[*state.datalabel," "+datalabel]    
    ENDELSE

     Widget_Control,event.top,Set_UValue=state,/No_Copy     
     Widget_Control,event.top,/DESTROY
     RETURN
   END
   'Cancel':BEGIN
     Widget_Control,event.top,/DESTROY
     RETURN
   END
   ELSE: RETURN
 ENDCASE
 RETURN
END

;---------------------------------------------------------------------------
; itera_readfile_event: Controls the widget for the selection of a
;                ascii file containing a list of observational data
;                points (X & Y values and labels), and reads in file
;                when done (not cancelled). 
;---------------------------------------------------------------------------
PRO itera_Readfile_event,event
On_error,2
 CASE event.DONE OF
  0: BEGIN
  END
  1: BEGIN
    WIDGET_CONTROL, event.id, GET_VALUE=filename
    WIDGET_CONTROL, event.top, GET_UVALUE=state,/No_copy
    openr,lun,filename,/get_lun
    datatext=' '
    WHILE ~EOF(lun) DO BEGIN 
     readf,lun,datatext 
     data=STRSPLIT(datatext,/EXTRACT) 
     commenttxt=STRMID(data[0],0,1) 
     IF (commenttxt NE ';') AND (commenttxt NE '%') THEN BEGIN 
      IF n_elements(data) GT 2 THEN $
        datalabel=STRJOIN(data[2:n_elements(data)-1],' ') $
      ELSE datalabel='' 
      IF ~*state.OK THEN BEGIN
       *state.Xval=FLOAT(data[0])
       *state.Yval=FLOAT(data[1])
       *state.datalabel=" "+datalabel      
       *state.OK=1B
      ENDIF ELSE BEGIN
       *state.Xval=[*state.Xval,FLOAT(data[0])]
       *state.Yval=[*state.Yval,FLOAT(data[1])]
       *state.datalabel=[*state.datalabel," "+datalabel]
      ENDELSE
     ENDIF 
    ENDWHILE
    close,lun
    free_lun,lun
;
; Finally destroy Popup Window
    WIDGET_CONTROL, event.top, SET_UVALUE=state,/No_copy
    WIDGET_CONTROL, event.top, /DESTROY
  END
  2: BEGIN 
    WIDGET_CONTROL, event.top, /DESTROY
  END
 ENDCASE
 RETURN
END

;---------------------------------------------------------------------------
; itera_setmodelwidget: Sets up the model selection widget, grouping
;                       the indivdual models into groups of the
;                       same "second parameter" (n_para2) of the grid,
;                       with:     
;                         n_para2=1 = Metallicity/Abund.              
;                         n_para2=2 = Density                         
;                         n_para2=3 = Unique parameter (B, SB age etc)
;
;---------------------------------------------------------------------------
FUNCTION itera_SetModelWidget,ColID,n_para2
 On_Error,2
;
 work_dir=file_dirname(file_which('itera.pro',/include_current_dir) $
                       ,/mark_dir)
 CD,work_dir,CURRENT=orig_dir
;
; The model set selection widget
 model_setID=WIDGET_TREE(colID,Ysize=450 ,Frame=2,/multiple,$
                        EVENT_PRO='itera_Modeltree_Handler')
;
; Shock models first
 datadir='shockdata/'
 IF FILE_TEST(datadir,/DIR) THEN BEGIN 
   ShockID=WIDGET_TREE(model_setID,/FOLDER, VALUE='Shock Models')
   shockleafIDs=populate_modeltree(shockID,datadir,free_param=n_para2)
 ENDIF
;
; AGN models
 datadir='AGNdata/'
 IF FILE_TEST(datadir,/DIR) THEN BEGIN 
   AGNID=WIDGET_TREE(model_setID,/FOLDER, VALUE='AGN Models')
   AGNleafIDs=populate_modeltree(AGNID,datadir,free_param=n_para2)
 ENDIF
;
; Starburst models
 datadir='SBdata/'
 IF FILE_TEST(datadir,/DIR) THEN BEGIN 
   SBID=WIDGET_TREE(model_setID,/FOLDER, VALUE='Starburst Models')
   SBleafIDs=populate_modeltree(SBID,datadir,free_param=n_para2)
 ENDIF
;
; User models
 datadir='Usermodels/'
 IF FILE_TEST(datadir,/DIR) THEN BEGIN 
   UsermodelID=WIDGET_TREE(model_setID,/FOLDER, VALUE='User Models')
   UsermodelleafIDs=populate_modeltree(UsermodelID,datadir,free_param=n_para2)
 ENDIF
;
; User Observational Data
 datadir='Userdata/'
 IF FILE_TEST(datadir,/DIR) THEN BEGIN 
   UserdataID=WIDGET_TREE(model_setID,/FOLDER, VALUE='User Observational Data')
   datafiles=FILE_SEARCH(datadir+'*.data', COUNT=n_files,/FULLY_QUALIFY_PATH)
   dirend=strpos(datafiles[0],'/', /REVERSE_SEARCH)+1 ;all in same directory
   FOR i=0L, n_files-1 DO BEGIN
     dataname='Obs data: '+$
              strmid(datafiles[i],dirend,STRLEN(datafiles[i])-(dirend+5))
     UserdataleafIDs=WIDGET_TREE(UserdataID,value=dataname,uvalue=datafiles[i])
   ENDFOR
   Userdata_notset=WIDGET_TREE(UserdataID,value='Not Yet Implemented')
 ENDIF
 CD,orig_dir
 RETURN,model_setID
END


;---------------------------------------------------------------------------
; itera_changemodel_handler: This responds to the "Change 2nd
;                   parameter button", reading the slected
;                   model and resetting the model_select widget using
;                   the above routine.
;---------------------------------------------------------------------------
PRO itera_ChangeModel_Handler,event
 On_Error,2
;
; ClearPrevious  Model & Grid Data First
 pseudo_event={ID:0L, top:event.top,Handler:0L,Select:1L}
 itera_modelreset_Handler,pseudo_event
;
 widget_control, event.top, get_UValue=info,/No_Copy
; Destroy old Tree
 Widget_Control,info.model_setID,/destroy
; set up new tree with new parameter
 ModelsetID=itera_SetModelWidget(info.ColID,event.index+1)
 info.n_para2=event.index+1
 info.model_setID=ModelSetID
 Widget_Control,event.top,Set_UValue=info,/No_Copy     
 RETURN
END

;---------------------------------------------------------------------------
; itera: This is the main program, setting up all the program
;        variables and the widget itself.
;---------------------------------------------------------------------------
PRO itera,max_models=max_models,max_data=max_data
;
; This is the main program that sets up the widget
;
On_Error,2

; set up colour device - NOTE program leaves IDL with decomposed=0!!!
device, GET_DECOMPOSED=originalDCstate
device, DECOMPOSED=0
;
; Variable input: Maximum number of models and data displayed 
;  Reduce this to save memory

 IF (n_elements(max_models) EQ 0) THEN max_models=10
 IF (n_elements(max_data) EQ 0) THEN max_data=100

; Go to the itera directory
 work_dir=file_dirname(file_which('itera.pro',/include_current_dir) $
                       ,/mark_dir)

 CD,work_dir,CURRENT=orig_dir
;
; tlb= Top Level Base
 device,get_screen_size=screensize
 tlb = Widget_Base(/Row,mbar=menu,title='itera',$ ;/tlb_size_events,
     xoffset=fix(screensize[0]*0.05),yoffset=fix(screensize[1]*0.05))
;
; Set up Draw widget layout and draw widget
;
 col1ID = Widget_Base(tlb,Row=4)
 col2ID = Widget_Base(tlb,Row=4,/GRID_LAYOUT)
 col2a  = Widget_Base(col2ID,col=1)
 col2b  = Widget_Base(col2ID,col=1)
 col2c  = Widget_Base(col2ID,col=1)
 col2d  = Widget_Base(col2ID,col=1)
 col3ID = Widget_Base(tlb,col=1,xsize=270)
 col3a  = Widget_Base(col3ID,col=1,Ysize=450,xsize=268)
 col3b  = Widget_Base(col3ID,col=1,xsize=268)
 col4ID = Widget_Base(tlb,Col=1)
 drawID = Widget_Draw(col4ID, Xsize=600, Ysize=500, Button_Events=1 $
                   ,Event_Pro='itera_Draw_Handler')
 col4b      = Widget_Base(col4ID,Col=2)
 col4bleft  = Widget_Base(col4b,col=2,EVENT_PRO='itera_PlotRange_Handler')
 col4bright = Widget_Base(col4b,col=3,EVENT_PRO='itera_Button_Handler' $
                          ,/grid_layout)
;
; The File menu button
;
 fileID = Widget_Button(menu, value='File', /menu $
                      ,Event_Pro='itera_filemenu_handler')
 saveID = widget_button(fileID, value='Save Grids', uvalue='save') ;save button
 printID = widget_button(fileID, value='Print', uvalue='print') ;print button
 exitID = widget_button(fileID, value='Exit', uvalue='exit') ;exit button
;
; The Limit Line List menu button
;
 linerange_ID =  widget_button(menu, value='LineList', /menu, $
                        event_pro='itera_linelist_menu_handler')
             
 full = Widget_Button(linerange_ID, Uvalue='full',Value='Full line list', $
                     /checked_menu)
 UV_Opt_IR = Widget_Button(linerange_ID, Uvalue='select1',$
               Value='Selected  UV, Optical and IR lines', /checked_menu)
 Lyman_limit = Widget_Button(linerange_ID, UValue='Lyman',$
               Value='Below Lyman Limit', /checked_menu)
 UV = Widget_Button(linerange_ID, UValue='UV',$
               Value='UV lines [950--3500]', /checked_menu)
 Optical = Widget_Button(linerange_ID, UValue='Optical',$
               Value='Optical lines [3500--9000]', /checked_menu)
 NIR = Widget_Button(linerange_ID, UValue='NIR',$
               Value='Near IR lines [0.9--5um]', /checked_menu)
 FIR = Widget_Button(linerange_ID, UValue='FIR',$
               Value='Far IR lines [5um--10mm]', /checked_menu)
 linerangeIDs=[full,UV_Opt_IR,Lyman_limit,UV,Optical,NIR,FIR]

 Widget_Control, Lyman_limit, Set_Button=1
;
; Standard Diagrams Menu
;
 std_diags_menuID  = widget_button(menu, value='Std. Diagrams', /menu,  $
                     event_pro='Std_Diags_Menu_Handler')
 stddiag_file=work_dir+'stddiagram_list.txt'
 IF FILE_TEST(stddiag_file,/READ) THEN BEGIN
  list=' '
  std_end='-------------PUT YOUR LINES AFTER THIS----------------------------'
  n_diags=FILE_LINES(stddiag_file)-2
  std_diagID=lonarr(n_diags)
  openr,lun,stddiag_file,/get_lun
  readf,lun,list
  readf,lun,list
  j=0
;
; Standard diagrams
  WHILE (list NE std_end) DO BEGIN
   diagram=STRSPLIT(list,':',/EXTRACT)
   X1lines=FIX(STRSPLIT(diagram[0],',',/EXTRACT))
   X2lines=FIX(STRSPLIT(diagram[1],',',/EXTRACT))
   Y1lines=FIX(STRSPLIT(diagram[2],',',/EXTRACT))
   Y2lines=FIX(STRSPLIT(diagram[3],',',/EXTRACT))
   std_diagID[j]=WIDGET_BUTTON(std_diags_menuID,VALUE=diagram[4],$
        UVALUE={X1:X1lines,X2:X2lines,Y1:Y1lines,Y2:Y2lines})
   readf,lun,list
   j+=1
  ENDWHILE
;
; User defined diagrams
  FOR i=j,n_diags-1 DO BEGIN
   readf,lun,list
   diagram=STRSPLIT(list,':',/EXTRACT)
   X1lines=FIX(STRSPLIT(diagram[0],',',/EXTRACT))
   X2lines=FIX(STRSPLIT(diagram[1],',',/EXTRACT))
   Y1lines=FIX(STRSPLIT(diagram[2],',',/EXTRACT))
   Y2lines=FIX(STRSPLIT(diagram[3],',',/EXTRACT))
   std_diagID[i]=WIDGET_BUTTON(std_diags_menuID,VALUE=diagram[4],$
        UVALUE={X1:X1lines,X2:X2lines,Y1:Y1lines,Y2:Y2lines},$
        SEPARATOR=(i EQ j))
  ENDFOR
  close,lun
  free_lun,lun
 ENDIF
;
; The Help menu button
;
 help_menu = widget_button(menu, value='Help', /menu $
             , event_pro='itera_helpmenu_handler')
 help_about = widget_button(help_menu, value='About' $
              , uvalue='about' )        ; About button
 help_instr = widget_button(help_menu, value='Instructions' $
              , uvalue='instructions' ) ; Instructions Button
;
; Set up Line_selection widgets
;
 linelist = READ_LINELIST(file=linelistfile)

 line_wave=fltarr(max(linelist.index))
 line_wave[linelist.index]=linelist.wave_a

 anglist = WHERE(line_wave LT 1e4)
 miclist = WHERE(line_wave GE 1e4 AND line_wave LT 1e7)
 mmlist  = WHERE(line_wave GT 1e7)
 
 lines=strarr(max(linelist.index))
 lines[linelist.index] = linelist.element+linelist.ion 
 lines[anglist] = lines[anglist] + $
     STRING(FORMAT='(G7.5)',line_wave[anglist])+STRING(197B);put in Angstroms
 
 lines[miclist] = lines[miclist] + $
     STRING(FORMAT='(G7.5,"um")',line_wave[miclist]/1.0e4); put in micron

 lines[mmlist]=lines[mmlist] + $
     STRING(FORMAT='(G7.5,"mm")',line_wave[mmlist]/1.0e7); for mm
 
 select=where(line_wave GE 1025.7) ; start off above Ly-limit
   
; ratio_title = Widget_Label(col1_topID, Value='log(Y1/Y2) vs log(X1/X2)')
 X1_title = Widget_Label(col1ID, Value='X1')
 X1_ID = Widget_List(col1ID, Value=lines[select], Ysize=10, xsize=15, $
       Frame=1, UValue=select, $
       Event_Pro='itera_lineselection_handler')
 X2_title = Widget_Label(col1ID, Value='X2')
 X2_ID = Widget_List(col1ID, Value=lines[select], Ysize=10, xsize=15,$
       Frame=1, UValue=select, $
       Event_Pro='itera_lineselection_handler')
 Y1_title = Widget_Label(col1ID, Value='Y1')
 Y1_ID = Widget_List(col1ID, Value=lines[select], Ysize=10, xsize=15, $
       Frame=1, UValue=select, $
       Event_Pro='itera_lineselection_handler')
 Y2_title = Widget_Label(col1ID, Value='Y2')
 Y2_ID = Widget_List(col1ID, Value=lines[select], Ysize=10, xsize=15, $
       Frame=1, UValue=select, $
       Event_Pro='itera_lineselection_handler')

 X1_list = Widget_List(col2a, Value=[' '],$
       UValue=[-1],Ysize=8,xsize=15)
 X1_clear = Widget_Button(col2a, Value='Clear X1', $
       Event_Pro='itera_Clear_line_handler',ysize=26)

 X2_list = Widget_List(col2b, Value=[' '],$
       UValue=[-1],Ysize=8,xsize=15)
 X2_clear = Widget_Button(col2b, Value='Clear X2', $
       Event_Pro='itera_Clear_line_handler',ysize=26)

 Y1_list = Widget_List(col2c, Value=[' '],$
       UValue=[-1],Ysize=8,xsize=15)
 Y1_clear = Widget_Button(col2c, Value='Clear Y1', $
       Event_Pro='itera_Clear_line_handler',ysize=26)

 Y2_list = Widget_List(col2d, Value=[' '],$
       UValue=[-1],Ysize=8,xsize=15)
 Y2_clear = Widget_Button(col2d, Value='Clear Y2', $
       Event_Pro='itera_Clear_line_handler',ysize=26)
;
; The model set selection widget
 model_setID=itera_SetModelWidget(col3a,3)
 model_listID=Widget_text(col3b,ysize=5,xsize=40,Uvalue=-1,/scroll)
;
; A button to reset the model set selection to none
 model_resetID = Widget_Button(col3b, Value='Clear Selected Models', $
        Event_Pro='itera_modelreset_Handler',ysize=26)
;
; Buttons for labels
 labelID=Widget_Base(col3b,col=2,Frame=1)
 labeltext=WIDGET_LABEL(labelID,value='Labels:',ysize=30,/ALIGN_LEFT)
 labelbutsID= Widget_Base(labelID,col=3,/EXCLUSIVE,$
        Event_pro='itera_Label_handler')
 nolabelsID=Widget_Button(labelbutsID,ysize=26,Value='None',$
        uvalue='NONE')
 CT_ID=Widget_Button(labelbutsID,ysize=26,Value='Colours',$
        uvalue='CT')
 gridvalueID=Widget_Button(labelbutsID,ysize=26,Value='Grid Vals',$
        uvalue='GRID')
 WIDGET_CONTROL,nolabelsID,SET_BUTTON=1
;
; Text Widgets for Parameter ranges
 PrangeID=Widget_Button(col3b,Value='Limit Parameter Ranges',$
         EVENT_PRO='itera_LimitParamRange_Handler',ysize=26)
;
; Button to Change 2nd parameter
 changeID=Widget_Base(col3b,row=1,Frame=1,xsize=255,$
                     EVENT_PRO='itera_ChangeModel_Handler')
; changetext=WIDGET_LABEL(changeID,value='2nd Parameter for Grid:',$
;                         ysize=30,/ALIGN_LEFT)
; parameters=['0\Metallicity','0\Density','2\Unique Parameter']
; changelist=CW_PDMENU(changeID,parameters,/Column,/RETURN_INDEX)
 changelist=WIDGET_DROPLIST(changeID,Title='2nd Parameter for Grid:',$
            value=['Metallicity','Density','Unique Parameter'])
 WIDGET_CONTROL,changelist, Set_Droplist_Select=2
;
; Set the min/max values of the plot
;
 slider_min=-10
 slider_max=10
 min=-3.0
 max=3.0
 xrange=[min,max]
 yrange=[min,max]
;
 xminID = CW_FSLIDER(col4bleft, minimum=slider_min, maximum=slider_max, $
                     drag=0, Value=min, UValue='Xmin', Title='X Min', $
                     FORMAT='(G10.3)', Edit=1)
 yminID = CW_FSLIDER(col4bleft, minimum=slider_min, maximum=slider_max, $ 
                     drag=0, Value=min, UValue='Ymin', Title='Y Min', $
                     FORMAT='(G10.3)', Edit=1)
 xmaxID = CW_FSLIDER(col4bleft, minimum=slider_min, maximum=slider_max, $
                     drag=0, Value=max, UValue='Xmax', Title='X Max', $
                     FORMAT='(G10.3)', Edit=1)
 ymaxID = CW_FSLIDER(col4bleft, minimum=slider_min, maximum=slider_max, $
                     drag=0, Value=max, UValue='Ymax', Title='Y Max',$
                     FORMAT='(G10.3)', Edit=1)
;
; Optional Button area
;
;
; Button to reset plot range to default values
reset_rangeID=Widget_Button(col4bright, Value='Reset Range',uvalue='range',$
                           TOOLTIP='Reset X & Y ranges')
;
; Button to reset everything to default values
resetID = Widget_Button(col4bright, Value='Reset All',uvalue='clear',$
                      TOOLTIP='Reset X & Y and data ranges, models and lines')

; An example button that can be defined by the use
;userdef1ID = Widget_Button(col4bright, Value='User Defined',sensitive=0)
;
; Button to reset everything to default values
adddataID = Widget_Button(col4bright, Value='Add Data',$
                           uvalue='adddata',$
                           TOOLTIP='Add Observational data points')
; Button to reset everything to default values
hidelabelID = Widget_Button(col4bright, Value='Labels On/Off',$
                           uvalue='hidelabels',$
                           TOOLTIP='Hide/Show Observational data point labels')

; The Exit button
QuitID = Widget_Button(col4bright, Value='Quit',uvalue='quit')
;
; Realize the top level base
;
 Widget_Control, tlb, /Realize
; Get the window index of the plotting window
 Widget_Control, drawID, Get_Value=windowID
 Wset, windowID
 Widget_Control, tlb,TLB_GET_SIZE=widgetsize
 xsize_extra=widgetsize[0]-600 ;xsize-draw widget
 ysize_extra=widgetsize[1]-500 ;ysize-draw widget
;
; Set pixel map window for Zoom Box
   Window, XSize=600, YSize=500, /Pixmap,/FREE
   lun=!D.Window
; Set up info structure and store in the top level base
 info={top:tlb,$
       windowID:WindowID,$
       drawID:drawID, $
; Line List selection tools
       X1ID:X1_ID, X1list:X1_list, X1clear:X1_clear,$
       X2ID:X2_ID, X2list:X2_list, X2clear:X2_clear,$
       Y1ID:Y1_ID, Y1list:Y1_list, Y1clear:Y1_clear,$
       Y2ID:Y2_ID, Y2list:Y2_list, Y2clear:Y2_clear,$
; modelset IDs
       ColID:col3a,$
       model_setID:model_setID,$
       modellistID:model_listID, $
       n_para2:3,$ 
; linelist values
       lines:lines,$
       linewave:line_wave, $
       linerangeIDs:linerangeIDs,$ ; for setting check buttons
; Model data pointer, uses "Max_models" to define maximum num. of models
       maxmodels:max_models, $
       modellist:PTR_NEW([-1L]), $ ; No models originally selected
       p_models:PTRARR(max_models), $
       x1grid:PTRARR(max_models), $
       x2grid:PTRARR(max_models), $
       y1grid:PTRARR(max_models), $
       y2grid:PTRARR(max_models), $
; Added Observational points & data
       p_data:PTRARR(max_data), $
       n_data:0L, $
       addXval:PTR_NEW([-99]),$
       addYval:PTR_NEW([-99]),$
       addlabel:PTR_NEW(''),$
       hidelabel:0B, $
; Plot values
       gridexist:0B,$  ; check if grid exists
       xminID:xminID,$
       xmaxID:xmaxID,$
       yminID:yminID,$
       ymaxID:ymaxID,$
       xrange:xrange,$
       yrange:yrange, $
       plot_ct:0B,$
       gridvalues:0B,$
; Parameter Ranges
       P1range:[-99.0,-99.0],$
       P2range:[-99.0,-99.0],$       
; Zoom box
       xzoom_s:0, yzoom_s:0, $ ; coordinates of the static corner of zoom box
       xzoom_d:0, yzoom_d:0, $ ; coordinates of the static corner of zoom box
       pixID:lun $ ;ID of the pixel map in which plotwindow is stored
      }
 Widget_Control,tlb,Set_Uvalue=info,/no_Copy
;
 Xmanager, 'tlb', tlb, /no_block, Cleanup='itera_Cleanup'
;
; Return to original state
;
 CD,orig_dir

END
