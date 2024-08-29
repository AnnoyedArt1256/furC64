from chipchune.furnace.module import FurnaceModule
from chipchune.furnace.data_types import InsFeatureMacro, InsFeatureC64, InsFeatureAmiga
from chipchune.furnace.enums import MacroCode, MacroItem, MacroType
from chipchune.furnace.enums import InstrumentType
import sys

subsong = 0
note_transpose = 0
dups = {}

print(sys.argv)
module = FurnaceModule(sys.argv[1])
chnum = module.get_num_channels()

speed_type = len(module.subsongs[subsong].speed_pattern)

notes = ["C_","Cs","D_","Ds","E_","F_","Fs","G_","Gs","A_","As","B_"]

def comp(pat):
    i = 0
    o = []
    n = 0
    while i < len(pat):
        j = i
        k = 0
        if pat[i] >= 0x40 and pat[i] < 128: i += 1
        elif pat[i] == 0xFB: i += 2
        elif pat[i] == 0xFC: i += 2
        elif pat[i] == 0xE0: i += 2
        elif pat[i] == 0xE1: i += 2
        elif pat[i] == 0xE2: i += 2
        elif pat[i] == 0xE3: i += 2
        elif pat[i] == 0xE4: i += 2
        elif pat[i] == 0xE5: i += 3
        elif pat[i] == 0xE6: i += 2
        elif pat[i] == 0xE7: i += 2
        elif pat[i] == 0xE8: i += 2
        elif pat[i] == 0xE9: i += 3
        elif pat[i] == 0xEA: i += 3
        elif pat[i] == 0xEB: i += 2
        elif pat[i] == 0xEC: i += 2
        elif pat[i] == 0xED: i += 2
        elif pat[i] == 0xEE: i += 2
        elif pat[i] == 0xEF: i += 1
        elif pat[i] == 0xF0: i += 1
        elif pat[i] == 0xF1: i += 2
        elif pat[i] == 0xFF: i += 2
        elif pat[i] == 0xFD:
            i += 1
            n = 2
        elif pat[i] == 0xFE:
            i += 1
            n = 2
        elif pat[i] >= 128:
            i += 1
            n = 2
        else:
            k = 1
            if n == 0:
                o.append(pat[i])
            elif pat[i] > 1:
                o.append(pat[i]-1)
            #print(i,pat[i])
            i += 1
        n = max(n-1,0)
        if k == 0:
            o.extend(pat[j:i])
    #print(pat,"\n",o,"\n")
    return o

def conv_pattern(pattern):
    out = [0]
    oldtemp = [0,0]
    r = 0
    bitind = 0
    oldins = -1
    for row in pattern.data:
        has03xx = 0
        for l in row.effects:
            k = list(l)
            if k[0] == 0x03 and k[1] > 0:
                has03xx = 1
                break

        temp = []
        notnote = 0
        new_byte = 0
        if row.instrument != 65535 and oldins != row.instrument:
            new_byte = 1
            if row.instrument < 0x40:
                temp.append(row.instrument+0x40)
            else:
                temp.append(0xFB)
                temp.append(row.instrument)
            oldins = row.instrument
        if row.volume != 65535:
            new_byte = 1
            temp.append(0xFC)
            temp.append(row.volume)

        hasEffect = [-1,-1]
        has0Dxx = -1
        for l in row.effects:
            k = list(l)
            if k[1] == 65535:
                k[1] = 0

            if k[0] == 0xD:
                new_byte = 1
                has0Dxx = k[1]
                continue
            if k[0] == 0x0B:
                new_byte = 1
                temp.extend([0xED, k[1]])
                has0Dxx = 0
                continue
            if (k[0] == 0x09 or k[0] == 0x0F) and (speed_type == 1):
                new_byte = 1
                temp.extend([0xE1, k[1]])
                temp.extend([0xE0, k[1]])
                continue
            if k[0] == 0x0F and (speed_type == 2):
                new_byte = 1
                temp.extend([0xE1, k[1]])
                continue
            if k[0] == 0x09 and (speed_type == 2):
                new_byte = 1
                temp.extend([0xE0, k[1]])
                continue
            if k[0] == 0x00:
                new_byte = 1
                temp.extend([0xE2, k[1]])
                continue
            if k[0] == 0x01:
                new_byte = 1
                temp.extend([0xE3, k[1]])
                continue
            if k[0] == 0x02:
                new_byte = 1
                temp.extend([0xE4, k[1]])
                continue
            if k[0] == 0x03 and k[1] == 0:
                new_byte = 1
                temp.extend([0xE4, 0])
                continue
            if k[0] == 0x03 and k[1] > 0:
                new_byte = 1
                temp.extend([0xE5, k[1], max(min(notes.index(str(row.note))+(row.octave*12)+note_transpose,95),0)])
                continue
            if k[0] == 0x04:
                new_byte = 1
                temp.extend([0xE6, k[1]])
                continue
            if k[0] == 0x1B:
                new_byte = 1
                temp.extend([0xE7, k[1]])
                continue
            if k[0] == 0x1C:
                new_byte = 1
                temp.extend([0xE8, k[1]])
                continue
            if k[0] == 0xE1:
                new_byte = 1
                temp.extend([0xE9, k[1]>>4, k[1]&15])
                continue
            if k[0] == 0xE2:
                new_byte = 1
                temp.extend([0xEA, k[1]>>4, k[1]&15])
                continue
            if k[0] == 0xE5:
                new_byte = 1
                temp.extend([0xEB, k[1]])
                continue
            if k[0] == 0xEC:
                new_byte = 1
                temp.extend([0xEC, k[1]])
                continue
            if (k[0]>>4) == 4:
                new_byte = 1
                temp.extend([0xEE, (k[1]|(k[0]&0xf)<<8)>>3])
                continue
            if k[0] == 0xEA:
                new_byte = 1
                if k[1] == 0:
                    temp.extend([0xEF])
                else:
                    temp.extend([0xF0])
                continue
            if k[0] == 0x1A:
                new_byte = 1
                if k[1] > 0:
                    temp.extend([0xF1, 0x00])
                else:
                    temp.extend([0xF1, 0xFF])
                continue
        if str(row.note) == "OFF_REL":
            notnote = 1
            new_byte = 1
            temp.append(0xFD)
        elif str(row.note) == "REL":
            notnote = 1
            new_byte = 1
            temp.append(0xFD)
        elif str(row.note) == "OFF":
            notnote = 1
            new_byte = 1
            temp.append(0xFE)
        elif str(row.note) == "__" or (has03xx == 1):
            if has03xx == 0:
                notnote = 1
            #temp.append(0x80)
        else:
            new_byte = 1
            temp.append(max(min(notes.index(str(row.note))+(row.octave*12)+note_transpose,95),0)+0x80)

        if new_byte == 1:
            temp.append(0)
        out.extend(temp)
        durpass = False
        if out[-1] >= 63:
            out.append(0)
        if has0Dxx > -1:
            out[-1] += 1
            if out[0] == 0: out = out[1:]
            out.extend([0xFF, has0Dxx])
            return out
        out[-1] += 1
        r += 1
    out.extend([0xFF, 0])
    if out[0] == 0: out = out[1:]
    return out

f = open("asm/song.asm","w")

relW = []
relA = []
relD = []
relC = []

f.write("ticks_init:")
f.write(".byte ")
if speed_type == 1:
    f.write(str(module.subsongs[subsong].speed_pattern[0])+", ")
    f.write(str(module.subsongs[subsong].speed_pattern[0])+"\n")
elif speed_type == 2:
    f.write(str(module.subsongs[subsong].speed_pattern[0])+", ")
    f.write(str(module.subsongs[subsong].speed_pattern[1])+"\n")

f.write("insFL:\n")
f.write(".lobytes ")
for i in range(len(module.instruments)):
    f.write("ins"+str(i)+"F")
    if i == len(module.instruments)-1:
        f.write("\n")
    else:
        f.write(", ")
f.write("insFH:\n")
f.write(".hibytes ")
for i in range(len(module.instruments)):
    f.write("ins"+str(i)+"F")
    if i == len(module.instruments)-1:
        f.write("\n")
    else:
        f.write(", ")

f.write("insAL:\n")
f.write(".lobytes ")
for i in range(len(module.instruments)):
    f.write("ins"+str(i)+"A")
    if i == len(module.instruments)-1:
        f.write("\n")
    else:
        f.write(", ")
f.write("insAH:\n")
f.write(".hibytes ")
for i in range(len(module.instruments)):
    f.write("ins"+str(i)+"A")
    if i == len(module.instruments)-1:
        f.write("\n")
    else:
        f.write(", ")

f.write("insDL:\n")
f.write(".lobytes ")
for i in range(len(module.instruments)):
    f.write("ins"+str(i)+"D")
    if i == len(module.instruments)-1:
        f.write("\n")
    else:
        f.write(", ")
f.write("insDH:\n")
f.write(".hibytes ")
for i in range(len(module.instruments)):
    f.write("ins"+str(i)+"D")
    if i == len(module.instruments)-1:
        f.write("\n")
    else:
        f.write(", ")

f.write("insWL:\n")
f.write(".lobytes ")
for i in range(len(module.instruments)):
    f.write("ins"+str(i)+"W")
    if i == len(module.instruments)-1:
        f.write("\n")
    else:
        f.write(", ")
f.write("insWH:\n")
f.write(".hibytes ")
for i in range(len(module.instruments)):
    f.write("ins"+str(i)+"W")
    if i == len(module.instruments)-1:
        f.write("\n")
    else:
        f.write(", ")

f.write("insCL:\n")
f.write(".lobytes ")
for i in range(len(module.instruments)):
    f.write("ins"+str(i)+"C")
    if i == len(module.instruments)-1:
        f.write("\n")
    else:
        f.write(", ")
f.write("insCH:\n")
f.write(".hibytes ")
for i in range(len(module.instruments)):
    f.write("ins"+str(i)+"C")
    if i == len(module.instruments)-1:
        f.write("\n")
    else:
        f.write(", ")

for i in range(len(module.instruments)):
    features = module.instruments[i].features
    a = filter(
        lambda x: (
            type(x) == InsFeatureMacro
        ), features
    )
    macros = []
    for j in a:
        macros = j.macros
    hasWaveMacro = 0
    hasCutMacro = 0
    for j in macros:
        kind = j.kind
        if kind == MacroCode.WAVE:
            hasWaveMacro = 1
            continue
        if kind == MacroCode.ALG:
            hasCutMacro = 1
            continue

    hasAbsFilter = False
    hasAbsDuty = False
    written_ins = False

    a = filter(
        lambda x: (
            type(x) == InsFeatureMacro
        ), features
    )
    macros = []
    for j in a:
        macros = j.macros
    for j in macros:
        kind = j.kind
        if kind == MacroCode.DUTY and j.type == MacroType.LFO:
            hasAbsDuty = True

    a = filter(
        lambda x: (
            type(x) == InsFeatureC64
        ), features
    )
    for j in a:
        wave = j.tri_on
        wave |= j.saw_on<<1
        wave |= j.pulse_on<<2
        wave |= j.noise_on<<3
        wave <<= 4
        wave |= j.ring_mod<<2
        wave |= j.osc_sync<<1
        f.write("ins"+str(i)+"F:\n")
        f.write(".byte ")
        f.write(str(wave)+", ")
        ad = (j.envelope.a<<4)|j.envelope.d
        sr = (j.envelope.s<<4)|j.envelope.r
        f.write(str(ad)+", ")
        f.write(str(sr)+", ")
        f.write(str(j.duty&0xff)+", ")
        f.write(str(j.duty>>8)+", ")
        flags = 0
        if j.duty_is_abs:
            flags |= 1
        if hasWaveMacro:
            flags |= 2
        if j.to_filter:
            flags |= 4
        if j.init_filter:
            flags |= 8
        if j.filter_is_abs:
            flags |= 16
        if hasCutMacro:
            flags |= 32
        if not j.no_test:
            flags |= 64
        f.write(str(flags)+", ")
        fil = 0
        fil = j.lp<<4
        fil |= j.bp<<5
        fil |= j.hp<<6
        fil |= j.ch3_off<<7
        f.write(str(j.res<<4)+", ")
        f.write(str(fil)+", ")
        f.write(str(j.cut&0xff)+", ")
        f.write(str((j.cut>>8)&15)+"\n")
        f.write("\n")
        written_ins = True
        hasAbsDuty |= j.duty_is_abs
        hasAbsFilter = j.filter_is_abs
    if written_ins == False:
        f.write("ins"+str(i)+"F:\n")
        f.write(".byte 32, 8, 0, 255, 7, 0\n")
    a = filter(
        lambda x: (
            type(x) == InsFeatureMacro
        ), features
    )
    arp = [128,0xFF,0xFF]
    duty = [0xFF,0xFF]
    wave = [0xFF,0xFF]
    cutoff = [0xFF,0xFF]
    macros = []
    for j in a:
        macros = j.macros
    hasRelTotal = [0,0,0,0]
    for j in macros:
        kind = j.kind
        if kind == MacroCode.ARP:
            s = j.speed
            arp = []
            loop = 0xff
            hasRel = 0
            oldlen = 0
            if j.data[-1] == MacroItem.LOOP:
                arr = [MacroItem.LOOP, j.data[-2]]
                j.data = j.data[:-2] + arr
            for k in j.data:
                if k == MacroItem.LOOP:
                    loop = oldlen
                elif k == MacroItem.RELEASE:
                    arp.append(0xFF)
                    arp.append(loop)
                    relA.append(len(arp))
                    oldlen = max(len(arp),0)
                    hasRel = 1
                elif (k>>30) > 0:
                    arp.append(0xFE)
                    k = abs(k^(1<<30))
                    k = max(min(k,95),0)
                    arp.append(k%120)
                    oldlen = max(len(arp),0)
                else:
                    if k < 0:
                        arp.append((k%120)-120+128)
                    else:
                        arp.append((k%120)+128)
                    oldlen = max(len(arp),0)
            if hasRel == 0:
                relA.append(len(arp))
            hasRelTotal[1] = 1
            len_temp = len(arp)
            arp.append(0xFF)
            arp.append(loop if loop!=len_temp else 0xff)
        if kind == MacroCode.DUTY and j.type == MacroType.LFO:
            while type(j.data[0]) is not int:
                j.data = j.data[1:]

            s = j.speed
            duty = []
            hasRel = 0
            lfo = j.data[13]
            while lfo <= 1023:
                lfo += j.data[11]
                if lfo > 1023: break
                k = lfo
                if k & 512:
                    k = 1023-lfo
                k >>= 1
                k = j.data[0]+((k+(j.data[1]-j.data[0])*k)>>8)
                if (k&0xff) == 255:
                    duty.append(254)
                else:
                    duty.append(k&0xff)
                duty.append(k>>8)
            if hasRel == 0:
                relD.append(0)
            hasRelTotal[2] = 1
            duty.append(0xFF)
            duty.append(0)
        elif kind == MacroCode.DUTY:
            s = j.speed
            duty = []
            loop = 0xff
            loop2 = 0
            hasRel = 0
            for k in j.data:
                if k == MacroItem.LOOP:
                    loop = loop2
                elif k == MacroItem.RELEASE:
                    duty.append(0xFF)
                    duty.append(loop)
                    relD.append(len(duty))
                    hasRel = 1
                else:
                    loop2 = len(duty)+2
                    if hasAbsDuty:
                        if (k&0xff) == 255:
                            duty.append(254)
                        else:
                            duty.append(k&0xff)
                        duty.append(k>>8)
                    else:
                        k = 32768-(k*4) #4)
                        duty.append(k&0xff)
                        duty.append(k>>8)
            if hasRel == 0:
                relD.append(len(duty))
            hasRelTotal[2] = 1
            len_temp = len(duty)
            duty.append(0xFF)
            duty.append(loop if loop!=len_temp else 0xff)
        if kind == MacroCode.ALG and j.type == MacroType.LFO:
            while type(j.data[0]) is not int:
                j.data = j.data[1:]

            s = j.speed
            cutoff = []
            hasRel = 0
            lfo = j.data[13]
            while lfo <= 1023:
                lfo += j.data[11]
                k = lfo
                if k & 512:
                    k = 1023-lfo
                k >>= 1
                k = j.data[0]+((k+(j.data[1]-j.data[0])*k)>>8)
                if (k&0xff) == 255:
                    cutoff.append(254)
                else:
                    cutoff.append(k&0xff)
                cutoff.append(k>>8)
            if hasRel == 0:
                relC.append(len(cutoff))
            hasRelTotal[3] = 1
            cutoff.append(0xFF)
            cutoff.append(0)
        elif kind == MacroCode.ALG:
            s = j.speed
            cutoff = []
            loop = 0xff
            loop2 = 0
            hasRel = 0
            for k in j.data:
                if k == MacroItem.LOOP:
                    loop = loop2
                elif k == MacroItem.RELEASE:
                    cutoff.append(0xFF)
                    cutoff.append(loop)
                    relC.append(len(cutoff))
                    hasRel = 1
                else:
                    loop2 = len(cutoff)+1
                    if hasAbsFilter:
                        if (k&0xff) == 255:
                            cutoff.append(254)
                        else:
                            cutoff.append(k&0xff)
                        cutoff.append(k>>8)
                    else:
                        k = 32768+k*7
                        cutoff.append(k&0xff)
                        cutoff.append(k>>8)
            if hasRel == 0:
                relC.append(len(cutoff))
            hasRelTotal[3] = 1
            len_temp = len(cutoff)
            cutoff.append(0xFF)
            cutoff.append(loop if loop!=len_temp else 0xff)
        if kind == MacroCode.WAVE:
            s = j.speed
            wave = []
            loop = 0xff
            loop2 = 0
            hasRel = 0
            for k in j.data:
                if k == MacroItem.LOOP:
                    loop = len(wave)
                elif k == MacroItem.RELEASE:
                    wave.append(0xFF)
                    wave.append(loop)
                    relW.append(len(wave))
                    hasRel = 1
                else:
                    wave.append(k<<4)
            if hasRel == 0:
                relW.append(len(wave))
            hasRelTotal[0] = 1
            len_temp = len(wave)
            wave.append(0xFF)
            wave.append(loop if loop!=len_temp else 0xff)
    if hasRelTotal[0] == 0:
        relW.append(0)
    if hasRelTotal[1] == 0:
        relA.append(0)
    if hasRelTotal[2] == 0:
        relD.append(0)
    if hasRelTotal[3] == 0:
        relC.append(0)
    wave = str(wave)[1:-1]
    duty = str(duty)[1:-1]
    arp = str(arp)[1:-1]
    cutoff = str(cutoff)[1:-1]
    if arp in dups:
        f.write("ins"+str(i)+"A = "+dups[arp]+"\n")
    else:
        f.write("ins"+str(i)+"A:\n")
        f.write(".byte "+arp+"\n")
        dups[arp] = "ins"+str(i)+"A"

    if duty in dups:
        f.write("ins"+str(i)+"D = "+dups[duty]+"\n")
    else:
        f.write("ins"+str(i)+"D:\n")
        f.write(".byte "+duty+"\n")
        dups[duty] = "ins"+str(i)+"D"

    if wave in dups:
        f.write("ins"+str(i)+"W = "+dups[wave]+"\n")
    else:
        f.write("ins"+str(i)+"W:\n")
        f.write(".byte "+wave+"\n")
        dups[wave] = "ins"+str(i)+"W"

    if cutoff in dups:
        f.write("ins"+str(i)+"C = "+dups[cutoff]+"\n")
    else:
        f.write("ins"+str(i)+"C:\n")
        f.write(".byte "+cutoff+"\n")
        dups[cutoff] = "ins"+str(i)+"C"

relW = str(relW)[1:-1]
relD = str(relD)[1:-1]
relA = str(relA)[1:-1]
f.write("insArel:\n")
f.write(".byte "+relA+"\n")
f.write("insDrel:\n")
f.write(".byte "+relD+"\n")
f.write("insWrel:\n")
f.write(".byte "+relW+"\n")

for i in range(chnum):
    order = module.subsongs[subsong].order[i]
    f.write("order"+str(i)+"len = "+str(len(order))+"\n")
    f.write("order"+str(i)+"L:\n")
    f.write(".byte ")
    for o in range(len(order)):
        f.write("<(patCH"+str(i)+"N"+str(order[o])+"-1)")
        if o == len(order)-1:
            f.write("\n")
        else:
            f.write(", ")
    f.write("order"+str(i)+"H:\n")
    f.write(".byte ")
    for o in range(len(order)):
        f.write(">(patCH"+str(i)+"N"+str(order[o])+"-1)")
        if o == len(order)-1:
            f.write("\n")
        else:
            f.write(", ")

for i in range(chnum):
    order = module.subsongs[subsong].order[i]
    avail_patterns = filter(
        lambda x: (
            x.channel == i and
            x.subsong == subsong
        ),
        module.patterns
    )
    for p in avail_patterns:
        patnum = p.index
        #print(patnum,i)
        g = str(comp(conv_pattern(p)))[1:-1]
        f.write("patCH"+str(i)+"N"+str(patnum)+":\n")
        f.write(".byte "+g+"\n")

if chnum == 4:
    f.write("sampleHS:\n.hibytes ")
    for i in range(len(module.samples)):
        f.write("PCM"+str(i))
        if i == (len(module.samples)-1):
            f.write("\n")
        else:
            f.write(", ")

    f.write("sampleHE:\n.hibytes ")
    for i in range(len(module.samples)):
        f.write("PCMe"+str(i))
        if i == (len(module.samples)-1):
            f.write("\n")
        else:
            f.write(", ")

    total_maps = []
    f.write("insSI:\n.byte ")
    for i in range(len(module.instruments)):
        features = module.instruments[i].features
        a = filter(
            lambda x: (
                type(x) == InsFeatureAmiga
            ), features
        )
        init_sample = 0
        for j in a:
            if j.init_sample > 0 and j.init_sample != 65535:
                init_sample = j.init_sample
                break
        f.write(str(init_sample))
        if i == (len(module.instruments)-1):
            f.write("\n")
        else:
            f.write(", ")

    f.write(".res 256-(*&$ff), 0\n")
    for i in range(len(module.samples)):
        sample = []
        rate = max(min(module.samples[i].meta.sample_rate,384000),100)
        smp = list(module.samples[i].data)
        k = 0
        while k < len(smp):
            j = smp[int(k)]
            s = float(((int(j)+128)&0xff))
            s = ((s-128)/1.65)+128
            #s = ((s-128)/1.6)+128
            s = int(s/16)
            #s = (s>>1)+8
            sample.append(s)
            k += rate/7812
        s = sample[-1]
        if (len(sample)%256) == 0:
            sample.extend([s]*256)
        else:
            while (len(sample)%256) != 0:
                sample.append(s)
        f.write("PCM"+str(i)+":\n.byte "+str(sample)[1:-1]+"\n")
        f.write("PCMe"+str(i)+":\n")

f.close()

# frequency calculation code taken from
# https://codebase64.org/doku.php?id=base:how_to_calculate_your_own_sid_frequency_table

tuning = module.meta.tuning
f = open("asm/note_lo.bin","wb")
for i in range(96):
    hz = tuning * (2**(float(i-57)/12.0))
    cnst = (256**3)/985248.0 # PAL frequency
    freq = min(max(hz*cnst,0),0xffff)
    f.write(bytearray([int(freq)&0xff]))
f.close()
f = open("asm/note_hi.bin","wb")
for i in range(96):
    hz = tuning * (2**(float(i-57)/12.0))
    cnst = (256**3)/985248.0 # PAL frequency
    freq = min(max(hz*cnst,0),0xffff)
    f.write(bytearray([(int(freq)>>8)&0xff]))
f.close()
