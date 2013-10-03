@echo off

REM 
REM         Windows Script to build SwinGame from the command line
REM 

REM   	Move into script directory
pushd "%CD%"
cd /d "%~dp0"

REM
REM   	Determine program name
REM
set mydir="%~p0"
set APP_PATH=%mydir%
set mydir=%mydir:\=;%
	
for /F "tokens=* delims=;" %%i IN (%mydir%) DO call :LAST_FOLDER %%i
goto :COMPILE_LABEL

:LAST_FOLDER
if "%1"=="" (
  goto :EOF
)

set LAST=%1
SHIFT

goto :LAST_FOLDER

REM
REM		Compile the game
REM
:COMPILE_LABEL

set GAME_NAME=%LAST%
set OUT_DIR=.\bin
set TMP_DIR=.\tmp
set SRC_DIR=.\src
set LIB_DIR=.\lib
set LOG_FILE=.\out.log

SET SG_INC="-Fu.\lib\"

mkdir %TMP_DIR% > %LOG_FILE% 2>&1
mkdir %OUT_DIR% >> %LOG_FILE% 2>&1

echo --------------------------------------------------
echo           Creating %GAME_NAME%
echo           for Windows
echo --------------------------------------------------
echo   Running script from %APP_PATH%
echo   Saving output to %OUT_DIR%
echo --------------------------------------------------
echo   ... Creating %GAME_NAME%
   
fpc %SG_INC% -Mobjfpc -g -vw -Sh -FE%OUT_DIR% -FU%TMP_DIR% -Fu%LIB_DIR% -Fi%SRC_DIR% -o"%GAME_NAME%.exe" %SRC_DIR%/GameMain.pas >> %LOG_FILE%
if errorlevel 1 ( goto :FAILURE )

REM
REM		Copying resources
REM
echo   ... Copying Libraries
copy /y "%LIB_DIR%\*.dll" "%OUT_DIR%\" >> %LOG_FILE%

echo   ... Copying Resources
xcopy ".\Resources" "%OUT_DIR%\Resources" /D /E /C /R /I /K /Y >> %LOG_FILE%

goto :SUCCESS

REM
REM     Error handling
REM
:FAILURE

popd
echo   Error during build!  See below for output.
echo --------------------------------------------------
type %LOG_FILE%
echo --------------------------------------------------
goto :EOF

REM
REM		Return to starting directory
REM
:SUCCESS

popd
echo   Done...
echo --------------------------------------------------