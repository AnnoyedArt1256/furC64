# furC64
a C64/SID sound driver for Furnace

### **THIS SOUND DRIVER IS CURRENTLY A WIP**

A SID driver that's easy to make music with? It's more likely than you think.

* You have to have [Python](https://www.python.org/) and the [CC65 toolchain](https://cc65.github.io/) installed
* You **have** to set the pitch linearity option to "None". You can do this by going to `window -> song -> compatability flags -> Pitch/Playback -> Pitch linearity` and then setting the option to "None".

* The driver only supports **arpeggio, waveform, duty and cutoff** macros in each instrument and it DOESN'T support LFO and ADSR macros nor delay and step length, **although you can use LFO macros in the duty and cutoff macros (as in range-sweeping)**

* The furC64 driver only supports these effects:
  * 00xx: arpeggio
  * 01xx: pitch slide up
  * 02xx: pitch slide down
  * 03xx: portamento
  * 04xx: vibrato
  * 09xx: set speed 1
  * 0Bxx: jump to pattern
  * 0Dxx: jump to next pattern
  * 0Fxx: set speed 2
  * 1Axx: disable/enable envelope reset
  * 1Bxx: reset cutoff
  * 1Cxx: reset pulse-width
  * 4xxx: set filter cutoff
  * E1xx: note slide up
  * E2xx: note slide down
  * E5xx: note fine-pitch
  * EAxx: legato
  * ECxx: note cut

when you've finished / want to test out this driver:
* open the terminal/command prompt **to the furC64 directory**
* run `convert.sh your_fur_file.fur` or `convert.bat file.fur` (depending on your OS)
* in the `furC64/asm` directory you'll hopefully see a file called **`furC64-test.prg`**
  * that's your .prg file that you can run on hardware or on an emulator like VICE!

Hopefully you'll have fun with this driver alongside [furNES](https://github.com/AnnoyedArt1256/furNES) :D

Libraries used: chipchune

