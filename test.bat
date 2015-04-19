@echo off
cls & cargo test & gdb --quiet target\debug\rjs-196fcf447ab1a1c8.exe < gdbscript.txt
