# BUILD BINARIES
./rasm.exe -DHARRIERATTACK=1 -DISCASSETTE=1 -amper ../HarrierAttackSourceNew2_alt_CRTC_CART16.asm HARRIER1
#./rasm.exe  -amper ../AMSTRADFONT3.asm HARRIER2
./rasm.exe -amper ../HARR_SCR2.asm HARRSCR            # LOADING SCREEN
./rasm.exe -amper ../loadercasette.asm LOADERC        # CAS LOADER

# TURBO
# -----

rm build/HarrierAttackReloaded.cdt -f

# create new CDT and put binary loader

./2cdt -n -r harrier.bin ./LOADERC.bin build/HarrierAttackReloaded.cdt

# add screen to existing CDT data

./2cdt -r harrscr.bin ./HARRSCR.bin build/HarrierAttackReloaded.cdt

# add code to existing CDT data

./2cdt -r harrier1.bin ./HARRIER1.bin build/HarrierAttackReloaded.cdt
./2cdt -r harrier2.bin ./HARRIER2.bin build/HarrierAttackReloaded.cdt

# NORMAL BAUD - NEEDED FOR REAL HARDWARE
# --------------------------------------

# create new CDT and put binary loader

rm build/HarrierAttackReloadedN.cdt -f

./2cdt -t 0 -s 0 -n -r harrier.bin ./LOADERC.bin build/HarrierAttackReloadedN.cdt

# add screen to existing CDT data

./2cdt -t 0 -s 0 -r harrscr.bin ./HARRSCR.bin build/HarrierAttackReloadedN.cdt

# add code to existing CDT data

./2cdt -t 0 -s 0 -r harrier1.bin ./HARRIER1.bin build/HarrierAttackReloadedN.cdt
./2cdt -t 0 -s 0 -r harrier2.bin ./HARRIER2.bin build/HarrierAttackReloadedN.cdt

# ZIP ARCHIVE

rm build/HarrierAttackReloaded.cdt.zip -f
rm build/HarrierAttackReloadedN.cdt.zip -f
zip -j build/HarrierAttackReloaded.cdt.zip build/HarrierAttackReloaded.cdt
zip -j build/HarrierAttackReloadedN.cdt.zip build/HarrierAttackReloadedN.cdt
