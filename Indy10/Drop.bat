@echo off
SET /P INDYROOT=Enter Indy SVN folder:
SET INDYLIB=%INDYROOT%\trunk\Lib

IF NOT EXIST %INDYLIB% GOTO NOTINDY

DEL /S /Q System\.
DEL /S /Q Core\.
DEL /S /Q Protocols\.

COPY %INDYLIB%\Core Core
COPY %INDYLIB%\System System
COPY %INDYLIB%\Protocols Protocols
DEL /s /q *.cfg* *.tmpl *.dpk *.bdsproj *.proj *.rc *.dproj *.ico *.resources *.res

GOTO SUCCESS

:NOTINDY
echo This is not a valid Indy10 path

:SUCCESS
echo Indy10 updated. Good luck.