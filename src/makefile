v.exe:	main.obj ex.obj vu.obj vl.obj vs.obj vm.obj vc.obj \
		va.obj vk.obj vv.obj sys3.obj
	tlink /c/x c:\tc\lib\c0s main ex vu vl vs vm vc va vk vv sys3 \
		c:\tc\lib\setargv, v,, c:\tc\lib\cs

v.map:	main.obj ex.obj vu.obj vl.obj vs.obj vm.obj vc.obj \
		va.obj vk.obj vv.obj sys3.obj
	tlink /c/m c:\tc\lib\c0s main ex vu vl vs vm vc va vk vv sys3 \
		c:\tc\lib\setargv, v, v, c:\tc\lib\cs

main.obj:	main.c vi.h 		; tcc -c main.c
ex.obj:		ex.c vi.h		; tcc -c ex.c
vu.obj:		vu.c vi.h		; tcc -c vu.c
vl.obj:		vl.c vi.h 		; tcc -c vl.c
vs.obj:		vs.c vi.h 		; tcc -c vs.c
vm.obj:		vm.c vi.h 		; tcc -c vm.c
vc.obj:		vc.c vi.h 		; tcc -c vc.c
va.obj:		va.asm 			; a86 +OXSc va.asm
vk.obj:		vk.asm 			; a86 +OXSc vk.asm
vv.obj:		vv.asm 			; a86 +OXSc vv.asm
sys3.obj:	sys3.asm 		; a86 +OXSc sys3.asm

install:	v.exe
	chmod -a *.obj
	bcopy /vi/v.exe /bin/vi.exe
	rm /vi/v.exe
