;;**********************************************************************************
;;    Copyright (C) 2014 AIS Foundation.
;;
;;    This program is free software: you can redistribute it and/or modify
;;    it under the terms of the GNU General Public License as published by
;;    the Free Software Foundation, either version 3 of the License, or
;;    any later version.
;;
;;    This program is distributed in the hope that it will be useful,
;;    but WITHOUT ANY WARRANTY; without even the implied warranty of
;;    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;    GNU General Public License for more details.
;;
;;    You should have received a copy of the GNU General Public License
;;    along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;
;;***********************************************************************************/
;;  
;;  Name:     	AIS Application: RadIde
;;  Author:   	Michael Korns
;;  Project:  	Application for AIS RadIde
;;  Notes:		This application provides a Rapid Analytic Demo Inegrated Development Environment.
;;
;#ContextName=RadIde
;#heap=90
;#objects=10
;#aishelp=file:C:\Ais\onlinedocs\index.html
;#qthelp=file:C:\Qt\4.6.2\doc\html\index.html
;#mysqlhelp=www.mysql.com
;#script=yes

;; Check to see if the binary workspace has already been compiled.
(qt mainWindow: getAis:) ;; Refresh the _ais globals structure to replace the old workspace version.
(setq _path _ais.ScriptPath)
(setq _bin (append _ais.ExePath "/radide.bin"))
(setq _log (append _ais.ScriptPath "/ais.log"))
(qt console: maximize: (append "AIS RadIde [" _ais.ContextName "," (integer (/ _ais.Heap 1000000)) "," (integer (/ _ais.Objects 1000000)) "]")) ;; Allow the console to display progress msgs and errors 
(if (not (fileExists _bin)) (quit true))
 
;; Load the binary workspace and proceed.
LoadTheWorkspace::
(loadWorkspace _bin) ;; Load the radIde workspace with all of its global variables as they were at the time of saving.
(qt mainWindow: getAis:) ;; Refresh the _ais globals structure to replace the old workspace version which was just loaded.
(setq _bin (append _ais.ExePath "/radide.bin"))  ;; Refresh the _bin global variable to replace the old workspace version which was just loaded.
(setq _log (append _ais.ScriptPath "/ais.log"))  ;; Refresh the _log global variable to replace the old workspace version which was just loaded.
(setq _path _ais.ScriptPath)  ;; Refresh the _path global variable to replace the old workspace version which was just loaded.
(goto RunSartupScript:)
  
;; ***************
;; RunSartupScript
;; ***************
RunSartupScript::  
;;Register the ide manager used in this demo.
;;Show the basic QT console window only AFTER the ide manager has been installed.
(ideMgr.showConsoleIde (append "AIS RadIde [" _ais.ContextName "," (integer (/ _ais.Heap 1000000)) "," (integer (/ _ais.Objects 1000000)) "]"))
(writeln "Welcome to AIS RadIde")

;; Console testing code snipets (comment out those not used at the moment)
(qt console: setCommandText: {(defun foo(N) regs:(n) (loop for n from 0 until N do (writeln "[" n "]")))(foo 100)})
  

;; Make sure we start with all extra space garbage collected.
(gc compact:)	;; Free up as much space as is possible
