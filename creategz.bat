:: http://www.computing.net/answers/programming/batch-file-to-join-files-into-one-file/19150.html
@echo off

:: clean up
::rmdir /S /Q SIT

:: delete code.r if present
if exist c:\temp\code.r del c:\temp\code.r

:: create code.r
echo. >c:\temp\code.r

::copy/b c:\temp\code.r+Readme.txt c:\temp\code.r

:: merge all R\*.r files
for %%a in (R\*.r) do (
	echo. >>c:\temp\code.r
	copy/b c:\temp\code.r+"%%a" c:\temp\code.r
)


:: merge with SIT.date
for %%a in (..\1gitblog\SIT.date\R\*.r) do (
	echo. >>c:\temp\code.r
	copy/b c:\temp\code.r+"%%a" c:\temp\code.r
)

remove_r_comments c:\temp\code.r c:\temp\code1.r
del c:\temp\code.r
copy/b Readme.txt+c:\temp\code1.r c:\temp\code.r


:: delete sit.zip if present
if exist sit.gz del sit.gz

:: create zip
c:\Library\exe\7z.exe a -tgzip sit.gz -mx9 c:\temp\code.r