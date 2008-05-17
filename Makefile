# auto generated - do not edit

default: all

all:\
sdl-mixer.ali sdl-mixer.o

# -- SYSDEPS start
flags-sdl-ada:
	@echo SYSDEPS sdl-ada-flags run create flags-sdl-ada 
	@(cd SYSDEPS/modules/sdl-ada-flags && ./run)
libs-sdl-ada:
	@echo SYSDEPS sdl-ada-libs run create libs-sdl-ada 
	@(cd SYSDEPS/modules/sdl-ada-libs && ./run)


sdl-ada-flags_clean:
	@echo SYSDEPS sdl-ada-flags clean flags-sdl-ada 
	@(cd SYSDEPS/modules/sdl-ada-flags && ./clean)
sdl-ada-libs_clean:
	@echo SYSDEPS sdl-ada-libs clean libs-sdl-ada 
	@(cd SYSDEPS/modules/sdl-ada-libs && ./clean)


sysdeps_clean:\
sdl-ada-flags_clean \
sdl-ada-libs_clean \


# -- SYSDEPS end


ada-bind:\
conf-adabind conf-systype conf-adatype conf-adafflist flags-sdl-ada

ada-compile:\
conf-adacomp conf-adatype conf-systype conf-adafflist flags-sdl-ada

ada-link:\
conf-adalink conf-adatype conf-systype conf-aldfflist libs-sdl-ada

ada-srcmap:\
conf-adacomp conf-adatype conf-systype

ada-srcmap-all:\
ada-srcmap conf-adacomp conf-adatype conf-systype

cc-compile:\
conf-cc conf-cctype conf-systype

cc-link:\
conf-ld conf-ldtype conf-systype

cc-slib:\
conf-systype

conf-adatype:\
mk-adatype
	./mk-adatype > conf-adatype.tmp && mv conf-adatype.tmp conf-adatype

conf-cctype:\
conf-cc mk-cctype
	./mk-cctype > conf-cctype.tmp && mv conf-cctype.tmp conf-cctype

conf-ldtype:\
conf-ld mk-ldtype
	./mk-ldtype > conf-ldtype.tmp && mv conf-ldtype.tmp conf-ldtype

conf-systype:\
mk-systype
	./mk-systype > conf-systype.tmp && mv conf-systype.tmp conf-systype

mk-adatype:\
conf-adacomp conf-systype

mk-cctype:\
conf-cc conf-systype

mk-ctxt:\
mk-mk-ctxt
	./mk-mk-ctxt

mk-ldtype:\
conf-ld conf-systype conf-cctype

mk-mk-ctxt:\
conf-cc

mk-systype:\
conf-cc

sdl-mixer.ali:\
ada-compile sdl-mixer.ads sdl-mixer.ads
	./ada-compile sdl-mixer.ads

sdl-mixer.o:\
sdl-mixer.ali

clean-all: sysdeps_clean obj_clean ext_clean
clean: obj_clean
obj_clean:
	rm -f sdl-mixer.ali sdl-mixer.o
ext_clean:
	rm -f conf-adatype conf-cctype conf-ldtype conf-systype mk-ctxt

regen:\
ada-srcmap ada-srcmap-all
	./ada-srcmap-all
	cpj-genmk > Makefile.tmp && mv Makefile.tmp Makefile
