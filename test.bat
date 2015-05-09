@echo off
cls & cargo build & gdb --quiet target\debug\rjs.exe < gdbscript.txt
