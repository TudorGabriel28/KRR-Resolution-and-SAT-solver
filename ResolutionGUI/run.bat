@echo off
echo Starting Resolution Test Cases GUI...
echo.
cd /d "%~dp0"
java -cp build\classes resolutiongui.ResolutionGUI
pause
