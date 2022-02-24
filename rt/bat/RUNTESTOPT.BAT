@echo off
setlocal
pushd %~dp0
cd..
cd..

rem
rem this one should assemble okay
call bat\asm   rt\mlc\testopt @rt\opt\testopt0.opt

rem
rem this one should assemble with error for invalid option
call bat\asm   rt\mlc\testopt @rt\opt\testopt1.opt

rem
rem this one should assemble okay with valid reference
call bat\asm   rt\mlc\testopt @rt\opt\testopt2.opt

rem
rem this one should assemble with error for incorrect reference
call bat\asm   rt\mlc\testopt @rt\opt\testopt3.opt

rem
rem this one should assemble with error for self-reference
call bat\asm   rt\mlc\testopt @rt\opt\testopt4.opt

rem
rem this one should assemble with error for circular reference
call bat\asm   rt\mlc\testopt @rt\opt\testopt5.opt

