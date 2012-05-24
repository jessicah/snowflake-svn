#!/bin/bash

rm -f snowflake.cap
rm -f snowflake.wav
#export QEMU_AUDIO_DRV=alsa
#export QEMU_WAV_PATH=snowflake.wav

#qemu -cdrom snowflake.iso -m 128 -curses -serial file:snowflake.txt -net user,vlan=5 -net dump,vlan=5,file=snowflake.cap -net nic,vlan=5,model=rtl8139
#qemu -cdrom snowflake.iso -m 128 -curses -serial file:snowflake.txt -net user,vlan=5 -net dump,vlan=5,file=snowflake.cap -net nic,vlan=5,model=ne2k_isa -soundhw ac97
#qemu -cdrom snowflake.iso -m 128 -curses -serial file:snowflake.txt -net user,vlan=5 -net dump,vlan=5,file=snowflake.cap -net nic,vlan=5,model=rtl8139 -soundhw ac97
qemu -vnc :1 -k en-us -cdrom snowflake.iso -hda archive.tar -m 512 -vga vmware -serial stdio -net user,vlan=5 -net dump,vlan=5,file=network.cap -net nic,vlan=5,model=rtl8139 -soundhw ac97
