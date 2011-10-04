:: http://www.computing.net/answers/programming/batch-file-to-join-files-into-one-file/19150.html
@echo off

:: delete code.r if present
if exist c:\temp\code.r del c:\temp\code.r

:: create code.r
echo. >c:\temp\code.r

:: merge all R\*.r files
for /r %%a in (R\*.r) do (
	echo. >>c:\temp\code.r
	copy/b c:\temp\code.r+"%%a" c:\temp\code.r
)

:: delete sit.zip if present
if exist sit.zip del sit.zip

:: create zip
c:\Library\exe\7z.exe a -tzip sit.zip -mx9 c:\temp\code.r