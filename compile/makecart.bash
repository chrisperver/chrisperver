# BUILD BINARIES

./rasm.exe -DISCART=1 -amper ../HarrierAttackSourceNew2_alt_CRTC_CART16.asm HARRIER1
./rasm.exe -DHARRIERATTACK=1 -amper ../AMSTRADFONT3.asm HARRIER2
./rasm.exe -amper ../HARR_SCR2.asm HARRSCR            # LOADING SCREEN

rm ./boot.bin -f
rm ./HarrierAttackReloadedROM.bin -f

# CREATE ROM FILE

./rasm.exe -amper ../boot2.asm boot
./rominject -p 0 -o 0 boot.bin ./HarrierAttackReloadedROM.bin
./rominject -p 1 -o 0 HARRSCR.bin ./HarrierAttackReloadedROM.bin
./rominject -p 2 -o 0 HARRIER1.bin ./HarrierAttackReloadedROM.bin #0100-3FFF CODE
./rominject -p 3 -o 0 HARRIER2.bin ./HarrierAttackReloadedROM.bin #C000-FFFF CODE

rm ./build/HarrierAttackReloaded.cpr -f
rm ./build/HarrierAttackReloaded.cpr.zip -f
./buildcpr ./HarrierAttackReloadedROM.bin ./build/HarrierAttackReloaded.cpr

# MAKE ARCHIVE

zip -j ./build/HarrierAttackReloaded.cpr.zip ./build/HarrierAttackReloaded.cpr
zip -j ./build/HarrierAttackReloadedROM.zip ./HarrierAttackReloadedROM.bin
