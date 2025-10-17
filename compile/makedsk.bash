# CREATE BLANK DSK WITH LOADER AND SCREEN ALREADY ON
rm build/HarrierAttackReloaded.dsk -f
rm build/HarrierAttackReloaded.dsk.zip -f
cp build/HarrierAttackReloaded_Blank.dsk build/HarrierAttackReloaded.dsk

# BUILD BINARIES

./rasm.exe -DHARRIERATTACK=1 -amper ../HarrierAttackSourceNew2_alt_CRTC_CART16.asm
#./rasm.exe -DHARRIERATTACK=1 -amper ../AMSTRADFONT3.asm HARRIER2
#./rasm.exe -amper ../HARR_SCR2.asm HARRSCR            # LOADING SCREEN
#./rasm.exe -amper ../loader3.asm LOADER               # DSK LOADER

# MAKE ARCHIVE

zip -j build/HarrierAttackReloaded.dsk.zip build/HarrierAttackReloaded.dsk
