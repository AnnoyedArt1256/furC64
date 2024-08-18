@echo off
if [%1]==[] goto usage
python3 convert_to_asm.py %1
echo converted .fur file to .asm!
cd asm
cl65 -d -vm -l furC64.lst -g -u __EXEHDR__ -t c64 -C .\c64-asm.cfg -m furC64.map -Ln furC64.lbl -o furC64-test.prg furC64.asm
@echo compiled .prg file at asm/furC64-test.prg
cd ..
goto :eof
:usage
@echo No arguments supplied
@echo Make sure to run this command with an argument
@echo example: convert.bat test_file.fur
exit /B 1
