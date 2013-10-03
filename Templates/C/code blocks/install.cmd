@echo off

xcopy . "%APPDATA%\codeblocks\UserTemplates\SwinGame\" /E /EXCLUDE:ExcludeList.txt /Y

pause