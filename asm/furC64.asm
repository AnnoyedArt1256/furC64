.define chnum 3
.define use_zp 0
.define porta_once 0
.define compat_hr 1
.define CHIP_AMT 1
.define DIGI 0
.define HR_ADSR 1
TEST_AD := $00
TEST_SR := $00


;.define compat_hr 1
;TEST_AD := $0f
;TEST_SR := $0f


.if DIGI = 0
.define outch chnum
.else
.define outch 4
.endif

.macro variables
start_vars = *

mframeW: .res chnum
doMacroW: .res chnum

; why
env_reset: .res chnum
env_reset2: .res chnum
env_reset3: .res chnum
env_reset4: .res chnum

fil_lo_n2: .res CHIP_AMT
fil_hi_n2: .res CHIP_AMT
fil_st2: .res CHIP_AMT

legato: .res chnum

test_reset: .res chnum

note_tick: .res chnum

dur: .res chnum

vibrato_param: .res chnum
vibrato_phase: .res chnum

absfil: .res CHIP_AMT

doMacroC: .res CHIP_AMT
inst2: .res chnum
mframeC: .res CHIP_AMT

instF: .res CHIP_AMT
inst_prev_note: .res chnum

wav: .res chnum

hrframe: .res chnum

hrc: .res chnum

freq_lo1: .res chnum
freq_lo2: .res chnum

freq_hi1: .res chnum
freq_hi2: .res chnum

ad: .res chnum
sr: .res chnum

patseq: .res chnum*2

duty_lo: .res chnum
duty_hi: .res chnum

nextpat: .res 1
patind: .res 1
jumppat: .res 1
ch: .res 1
chm: .res 1
tick: .res 1
ins: .res chnum
inst1: .res chnum
inst_prev: .res chnum
mframeA: .res chnum
mframeD: .res chnum
doMacroA: .res chnum
doMacroD: .res chnum
arp: .res chnum
absarp: .res chnum
effects_temp: .res 2
tick_speeds: .res 2
tick_sel: .res 1
slide_amt: .res chnum
slide_amt_sign: .res chnum
slide_buffer_lo: .res chnum
slide_buffer_hi: .res chnum
note_pitch_lo: .res chnum
note_pitch_hi: .res chnum
note_n: .res chnum
note_dest: .res chnum
finepitch: .res chnum
cut_dur: .res chnum
pw_mod_lo: .res chnum
pw_mod_hi: .res chnum
abspw: .res chnum
retrigger_pw: .res chnum
retrigger_fil: .res CHIP_AMT
triggered_fil: .res CHIP_AMT
.if porta_once = 1
didporta: .res chnum
.endif
arpeff1: .res chnum
arpeff2: .res chnum
arpind: .res chnum
vol: .res CHIP_AMT
res: .res CHIP_AMT
fil_lo: .res CHIP_AMT
fil_hi: .res CHIP_AMT
fil_lo_n: .res CHIP_AMT
fil_hi_n: .res CHIP_AMT
base: .res 1
chipnum: .res 1
vars_len = *-start_vars

.endmacro

.if compat_hr = 0
inst_buffer = inst2
.else
inst_buffer = inst1
.endif

.if chnum > 4
.define actual_use_zp 0
.else
.define actual_use_zp use_zp
.endif

.ZEROPAGE
.if actual_use_zp = 1
.org $10
variables
.endif
patzp := $fe
macroIns := patzp
temp := patzp-4
flags_temp := temp+3
patloop = 0

.segment "CODE"

.org    $080D
jmp $900
.res $0900-*
main:
 sei
 lda #$35
 sta $01

 lda #127
 sta $dc0d

 and $d011
 sta $d011

 lda $dc0d
 lda $dd0d

 lda #<irq
 sta $fffe
 lda #>irq
 sta $ffff

.if DIGI <> 0
 lda #$0b
 sta $d011
 lda #$00
.else
 lda #$40
.endif
 sta $d012

 lda #1
 sta $d01a

.if DIGI <> 0
ldx #0
:
    lda real_addr, x
    sta $2, x
    inx
    cpx #nmi_end-nmi
    bne :-
.endif

 lda #0
 jsr $1000

.if DIGI <> 0
lda #<nmi
sta $fffa
lda #>nmi
sta $fffb

lda #$01
sta $dd0d
sta $dd0e

lda #$88  ; lo
sta $dd04
lda #0
sta $dd05 ; hi

lda $dd0d
lda #$81
sta $dd0d

lda #$40
sta $dd0c
.endif

    cli
	jmp *

irq:
	pha
    txa
    pha
    tya
    pha

    .if DIGI = 0
	  inc $d020
    .endif

    ;lda $d012
    ;sta rastertime
	jsr $1003

    .if DIGI = 0
	  dec $d020
    .endif

    ;lda $d012
    ;sec
    ;sbc rastertime
    ;sta rastertime

    asl $d019
    pla
    tay
    pla
    tax
    pla
    rti

;rastertime: .res 1

.if DIGI <> 0
real_addr = *
.org $2
nmi:
    sta z:ldat+1
ldn:
    lda $200
    sta $d418
    inc ldn+1
ldat:
    lda #0
    jmp $dd0c
NMI_BUFFER:
    ldy #0
NMI_LOOP:
    lda z:nmi_add+2
nmi_cmp:
    cmp #0
    bcc nmi_store
nmi_cmp2:
    lda #0
    sta z:nmi_add+2
nmi_none:
    lda vol
    and #$f0
    ora #10
    sta $200, y
    jmp NMI_LOOP_END

nmi_store:
    lda vol
    and #$f0
nmi_add:
    ora $1000
    sta $200, y
    clc
nmi_freq_lo2:
    lda #0
nmi_freq_lo:
    adc #0
    sta nmi_freq_lo2+1
    lda nmi_add+1
nmi_freq_hi:
    adc #1
    sta nmi_add+1
    bcc NMI_LOOP_END
    inc nmi_add+2
NMI_LOOP_END:
    iny
    cpy #144
    bne NMI_LOOP
    rts
nmi_end:
.org real_addr+(nmi_end-nmi)
.endif

.res $1000-*, 0
jmp initaddr
jmp playaddr
.byte " furC64 driver by AArt1256"

.if actual_use_zp = 0
variables
.endif
table_1_to_ff:
table_fil:
  .byte 0
  .res 15, $ff

.proc initaddr
.if actual_use_zp = 1
  ldx #vars_len
  lda #0
:
  sta start_vars-1, x
  dex
  bne :-
.endif


  ldx #(CHIP_AMT)-1
:
  lda #$7f
  sta vol, x
  lda #$00
  sta res, x
  sta retrigger_fil, x
  sta triggered_fil, x
  lda #$ff
  sta fil_lo, x
  sta fil_lo_n, x
  lda #$07
  sta fil_hi, x
  sta fil_hi_n, x
  lda #0
  sta doMacroC, x
  sta absfil, x
  sta fil_lo_n2, x
  sta fil_hi_n2, x
  sta fil_st2, x
  sta mframeC, x
  dex
  bpl :-

  lda #0
  sta tick_sel

  lda ticks_init
  sta tick_speeds
  lda ticks_init+1
  sta tick_speeds+1

  lda #0
  sta patind

  ldx #chnum-1
:
  lda #$80
  sta finepitch, x
  sta finepitch, x
  lda #1
  sta dur, x
  lda #$ff
  sta cut_dur, x
  lda #0
  sta pw_mod_lo, x
  sta pw_mod_hi, x
.if porta_once = 1
  sta didporta, x
.endif
  sta wav, x
  sta retrigger_pw, x
  sta abspw, x
  sta ins, x
  sta inst1, x
  sta inst_prev, x
  sta arp, x
  sta slide_amt, x
  sta slide_amt_sign, x
  sta slide_buffer_lo, x
  sta slide_buffer_hi, x
  sta vibrato_phase, x
  sta vibrato_param, x
  sta note_dest, x
  sta mframeA, x
  sta mframeW, x
  sta mframeD, x
  sta doMacroA, x
  sta doMacroW, x
  sta doMacroD, x
  sta env_reset, x
  lda #$ff
  sta env_reset2, x
  sta env_reset3, x
  lda #8
  sta test_reset, x
  lda #4
  sta hrframe, x
  sta note_tick, x
  dex
  bpl :-

  ldx #0
  jsr set_patseq_init
  lda #0
  sta tick
  sta jumppat

  rts
.endproc

.macro get_patzp
  .local skipW
  inc patzp
  bne skipW
  inc patzp+1
skipW:
  lda (patzp), y
.endmacro

.macro get_patzp_vol
  .local skipW
  inc patzp
  bne skipW
  inc patzp+1
skipW:
  lda vol
  and #$f0
  ora (patzp), y
.endmacro

.macro add_09xx
effectE0:
  get_patzp
  sta tick_speeds
  jmp begnote
.endmacro


.macro add_0Fxx
effectE1:
  get_patzp
  sta tick_speeds+1
  jmp begnote
.endmacro

.macro add_0Bxx
effectED:
  get_patzp
  sta patind
  lda #$ff
  sta jumppat
  jmp begnote
.endmacro


.macro add_00xx
effectE2:
  get_patzp
  sta effects_temp+1
  ldx ch
  lda #0
  sta arpind, x
  lda effects_temp+1
  and #$0f
  sta arpeff2, x
  lda effects_temp+1
  lsr
  lsr
  lsr
  lsr
  sta arpeff1, x
  jmp begnote
.endmacro


.macro add_01xx
effectE3:
  get_patzp
  ldx ch
  sta slide_amt, x
  lda #$ff
  sta slide_amt_sign, x
  ldx ch
  lda #88
  sta note_dest, x
.if porta_once = 1
  lda #$00
  sta didporta, x
.endif
  jmp begnote
.endmacro

.macro add_02xx
effectE4:
  get_patzp
  ldx ch
  sta slide_amt, x
  lda #$00
  sta slide_amt_sign, x
  ldx ch
  lda #0
  sta note_dest, x
.if porta_once = 1
  lda #$00
  sta didporta, x
.endif
  jmp begnote
.endmacro

.macro add_03xx
effectE5:
  get_patzp
  ldx ch
  sta slide_amt, x
  ldy #0
  get_patzp
  ldx ch
  sta note_dest, x

.if porta_once = 1
  lda #$ff
  sta didporta, x
.endif

  lda note_n, x
  cmp note_dest, x
  bne :+
  lda #0
  sta slide_amt, x
  sta slide_amt_sign, x
  jmp begnote
:
  bcc :+
  lda #$00
  sta slide_amt_sign, x
  jmp begnote
:
  lda #$ff
  sta slide_amt_sign, x
  jmp begnote
.endmacro

.macro add_04xx
  .local retskip
effectE6:
  get_patzp
  ldx ch
  sta vibrato_param, x
  beq retskip
  sta vibrato_phase, x
retskip:
  jmp begnote
.endmacro

.macro add_1Axx
effectF1:
  get_patzp
  ldx ch
  sta env_reset3, x
  jmp begnote
.endmacro


.macro add_1Bxx
  .local retskip
effectE7:
  get_patzp
  sta effects_temp+1
  and #$0f
  tax
  lda table_fil, x
  ldx ch
  beq :+
  ldx chipnum
  lda fil_lo_n, x
  sta fil_lo, x
  lda fil_hi_n, x
  sta fil_hi, x
  ldx ch
:
  lda effects_temp+1
  lsr
  lsr
  lsr
  lsr
  tax
  lda table_fil, x
  ldx chipnum
  sta retrigger_fil, x
  jmp begnote
.endmacro

.macro add_1Cxx
  .local retskip
effectE8:
  get_patzp
  sta effects_temp+1
  and #$0f
  tax
  lda table_fil, x
  ldx ch
  beq :+
  lda #0
  sta pw_mod_lo, x
  sta pw_mod_hi, x
:
  lda effects_temp+1
  lsr
  lsr
  lsr
  lsr
  tax
  lda table_fil, x
  ldx ch
  sta retrigger_pw, x
  jmp begnote
.endmacro

.macro add_E1xx
effectE9:
  get_patzp
  ldx ch
  asl
  asl
  sta slide_amt, x
  lda #$ff
  sta slide_amt_sign, x
  ldy #0
  get_patzp
  ldx ch
  ora #$80
  sta note_dest, x
.if porta_once = 1
  lda #$00
  sta didporta, x
.endif
  jmp begnote
.endmacro

.macro add_E2xx
effectEA:
  get_patzp
  ldx ch
  asl
  asl
  sta slide_amt, x
  lda #$00
  sta slide_amt_sign, x
  ldy #0
  get_patzp
  ldx ch
  ora #$80
  sta note_dest, x
.if porta_once = 1
  lda #$00
  sta didporta, x
.endif
  jmp begnote
.endmacro


.macro add_E5xx
effectEB:
  get_patzp
  ldx ch
  sta finepitch, x
  jmp begnote
.endmacro

.macro add_ECxx
effectEC:
  get_patzp
  ldx ch
  clc
.if compat_hr = 0
  adc #3
.else
  adc #2
.endif
  sta cut_dur, x
  jmp begnote
.endmacro

.macro add_4xxx
effectEE:
  ldx chipnum
  lda #0
  sta doMacroC, x
  sta fil_hi_n2, x
  lda #$ff
  sta fil_st2, x
  get_patzp
  ldx chipnum
  sta fil_lo_n2, x
  clc
  asl fil_lo_n2, x
  rol fil_hi_n2, x
  clc
  asl fil_lo_n2, x
  rol fil_hi_n2, x
  clc
  asl fil_lo_n2, x
  rol fil_hi_n2, x
  jmp begnote
.endmacro


.macro add_EAxx
effectEF:
  ldx ch
  lda #0
  sta legato, x
  jmp begnote

effectF0:
  ldx ch
  lda #$ff
  sta legato, x
  jmp begnote
.endmacro

add_09xx
add_0Fxx
add_00xx
add_01xx
add_02xx
add_03xx
add_04xx
add_0Bxx
add_1Axx
add_1Bxx
add_1Cxx
add_E1xx
add_E2xx
add_E5xx
add_EAxx
add_ECxx
add_4xxx

other_effects:
  lda effects_temp
  cmp #$FB
  bne :+
  ldx ch
  get_patzp
  sta ins, x
  jmp begnote
:
  cmp #$FC
  bne :+
  ldx chipnum
  get_patzp_vol
  sta vol, x
  jmp begnote
:

  cmp #$FD
  bne :+
  ldx ch
  ldy ins, x
  lda insArel, y
  sta mframeA, x
  lda insDrel, y
  sta mframeD, x
  ldx ch
  lda wav, x
  and #%11111110
  sta wav, x
  lda #1
  ldx ch
  sta dur, x
  ldy #0
  jmp end_advance
:

  cmp #$FE
  bne :+
  ldx ch
  lda wav, x
  and #%11111110
  sta wav, x
  lda #1
  ldx ch
  sta dur, x
  ldy #0
  jmp end_advance
:
 lda effects_temp
 cmp #224
 bcc cont_advance

 lda effects_temp
 and #$1f
 tax
 lda eff_lo, x
 sta effect_smc+1
 lda eff_hi, x
 sta effect_smc+2
 ldx ch
 lda effects_temp
effect_smc:
 jmp cont_advance

eff_lo:
.repeat $12, I
    .lobytes .ident(.concat ("effect", .sprintf("%02X",I+$e0)))
.endrepeat
eff_hi:
.repeat $12, I
    .hibytes .ident(.concat ("effect", .sprintf("%02X",I+$e0)))
.endrepeat

.macro add_advance_routine
advance:
  .local skipD, noIns, noVol, beg, blank2, blank3, hasNote, wait
beg:
  lda ch
  asl
  tax
  lda patseq, x
  sta patzp
  lda patseq+1, x
  sta patzp+1

  ldx ch
  dec dur, x
  lda dur, x
  beq begnote
  jmp end_advance
begnote:

  ldy #0
  get_patzp
  sta temp
  sta effects_temp
  and #$80
  beq wait
  lda temp
  cmp #$ff
  beq blank2
  jmp other_effects
cont_advance:
  lda temp
  and #$7f
  ldx ch
  sta note_n, x
  lda note_tick, x
  cmp #96
  bne :+
  lda #0
  sta slide_amt, x
  sta slide_amt_sign, x
:
  lda note_tick, x
  cmp #2
  bcc :+
  lda legato, x
  bne :+
  lda #0
  sta hrframe, x
  lda #$ff
  sta hrc, x
:
  lda #0
  sta slide_buffer_lo, x
  sta slide_buffer_hi, x
  sta note_tick, x
.if porta_once = 1
  cmp didporta, x
  beq :+
  sta didporta, x
  sta slide_amt, x
  sta slide_amt_sign, x
:
.endif
  ldx ch
  lda env_reset2, x
  sta env_reset, x
  lda env_reset3, x
  sta env_reset2, x
  lda #1
  sta dur, x
  ldy #0
  jmp end_advance

wait:
  lda temp
  and #$40
  beq :+
  ldx ch
  lda temp
  and #$3f
  sta ins, x
  jmp begnote
:
  lda temp
  ldx ch
  sta dur, x
blank2:
  lda temp
  cmp #$ff
  bne end_advance

  lda #$ff
  sta nextpat

end_advance:
  lda ch
  asl
  tax
  lda patzp
  sta patseq, x
  lda patzp+1
  sta patseq+1, x
  rts
.endmacro

add_advance_routine

.macro add_insarp
insarp:
  .local end, skip1, beg, skip2
beg:
  ldx ch
  lda doMacroA, x
  beq end

  ldx ch
  ldy inst_buffer, x
  lda insAL, y
  sta macroIns
  lda insAH, y
  sta macroIns+1
  ldx ch
  ldy mframeA, x
  lda (macroIns), y
  cmp #$fe
  beq skip2
  cmp #$ff
  bne skip1
  iny
  lda (macroIns), y
  cmp #$ff
  beq :+
  sta mframeA, x
  jmp beg
:
  lda #0
  sta doMacroA, x
  rts
skip1:
  sec
  sbc #128
  sta arp, x
  lda #0
  sta absarp, x
  inc mframeA, x
  rts
skip2:
  iny
  lda (macroIns), y
  sta arp, x
  lda #$ff
  sta absarp, x
  inc mframeA, x
  inc mframeA, x
end:
  rts
.endmacro

add_insarp

.macro add_insduty
insduty:
  .local end, skip1, beg, skip2
beg:
  ldx ch
  lda doMacroD, x
  beq end

  ldx ch
  ldy inst_buffer, x
  lda insDL, y
  sta macroIns
  lda insDH, y
  sta macroIns+1
  ldx ch
  ldy mframeD, x
  lda (macroIns), y
  sta temp
  cmp #$ff
  bne skip1
  iny
  lda (macroIns), y
  cmp #$ff
  beq :+
  sta mframeD, x
  jmp beg
:
  lda #0
  sta doMacroD, x
  rts
skip1:
  iny
  lda (macroIns), y
  sta temp+1

  lda abspw, x
  bne :+

  lda temp+1
  sec
  sbc #$80
  sta temp+1
  lda pw_mod_lo, x
  clc
  adc temp
  sta pw_mod_lo, x
  lda pw_mod_hi, x
  adc temp+1
  sta pw_mod_hi, x
  inc mframeD, x
  inc mframeD, x
  rts
:
  lda temp
  sta duty_lo, x
  lda temp+1
  sta duty_hi, x
  lda #0
  sta pw_mod_lo, x
  sta pw_mod_hi, x
  inc mframeD, x
  inc mframeD, x
end:
  rts
.endmacro

add_insduty

.macro add_inswave
inswave:
  .local end, skip1, beg, skip2
beg:
  ldx ch
  lda doMacroW, x
  beq end

  ldx ch
  ldy inst_buffer, x
  lda insWL, y
  sta macroIns
  lda insWH, y
  sta macroIns+1
  ldx ch
  ldy mframeW, x
  lda (macroIns), y
  cmp #$ff
  bne skip1
  iny
  lda (macroIns), y
  cmp #$ff
  beq :+
  sta mframeW, x
  jmp beg
:
  lda #0
  sta doMacroW, x
  rts
skip1:
  ldx ch
  pha
  lda wav, x
  and #$0f
  sta wav, x
  pla
  ora wav, x
  sta wav, x
  inc mframeW, x
end:
  rts
.endmacro

add_inswave

.macro add_inscut
inscut:
  .local end, skip1, beg, skip2
beg:
  ldx ch
  lda doMacroC, x
  beq end

  ldx ch
  ldy instF, x
  lda insCL, y
  sta macroIns
  lda insCH, y
  sta macroIns+1
  ldx ch
  ldy mframeC, x
  lda (macroIns), y
  sta temp
  cmp #$ff
  bne skip1
  iny
  lda (macroIns), y
  cmp #$ff
  beq :+
  sta mframeC, x
  jmp beg
:
  lda #0
  sta doMacroC, x
  rts
skip1:
  iny
  lda (macroIns), y
  sta temp+1

  lda absfil, x
  bne :+

  lda temp+1
  sec
  sbc #$80
  sta temp+1

  lda fil_lo, x
  clc
  adc temp
  sta fil_lo, x
  lda fil_hi, x
  adc temp+1
  sta fil_hi, x
  jmp :++
:
  lda temp
  sta fil_lo, x
  lda temp+1
  sta fil_hi, x
:
  inc mframeC, x
  inc mframeC, x
end:
  rts
.endmacro

add_inscut

.macro cmp16 val1, val2
    lda val1
    sec
    sbc val2
    php
    lda val1+1
    sbc val2+1
    php
    pla
    sta macroIns
    pla
    and #%00000010
    ora #%11111101
    and macroIns
    pha
    plp
.endmacro

doFinepitch:

  lda vibrato_param, x
  lsr
  lsr
  lsr
  lsr
  clc
  adc vibrato_phase, x
  and #63
  sta vibrato_phase, x
  tay
  lda vibrato_param, x
  and #$0f
  ora triangle_lookup, y
  tay

  clc
  lda note_pitch_lo, x
  ;adc #($80+$1f)
  ;adc #($80+$40)
  adc #($40)
  sta temp
  lda note_pitch_hi, x
  adc #1
  sta temp+1

  sec
  lda temp
  sbc finepitch, x
  sta temp
  bcs :+
  dec temp+1
:
;  bcs skip_pitch
;  lda #0
;  sta temp
;  sta temp+1
;skip_pitch:

  lda tri_vibrato_lookup, y
  lsr
  clc
  adc tri_vibrato_lookup, y
  sta temp+2
  lda #0
  adc #0
  sta temp+3

  sec
  lda temp
  sbc temp+2
  sta temp
  lda temp+1
  sbc temp+3
  sta temp+1

  rts

.macro add_do_ch
do_ch_jsr:
  .local skipHR1, skipHR2, end
  ldx ch
  lda env_reset4, x
  cmp #$ff
  beq :+
  dec env_reset4, x
:
  lda hrc, x
  beq skipHR1

  lda #0
  sta hrc, x
.if HR_ADSR <> 0
  lda env_reset, x
  beq :+
  lda #TEST_AD
  sta ad, x
  lda #TEST_SR
  sta sr, x
  lda wav, x
  and #255^1
  ora test_reset, x
  sta wav, x
  jmp :++
: 
  lda #1
  sta env_reset4, x
:
.else
  lda inst_buffer, x
  tax
  lda insFL, x
  sta patzp
  lda insFH, x
  sta patzp+1
  ldx ch
  ldy #1
  lda (patzp), y
  sta ad, x
  iny
  lda (patzp), y
  ldy base
  sta sr, x
  lda wav, x
  and #255^1
  ora test_reset, x
  sta wav, x
.endif
  lda #0
  sta doMacroA, x
  sta doMacroD, x
  sta doMacroW, x

skipHR1:

  lda hrframe, x
  cmp #4
  bne :+
  jmp skipHR2
:

  inc hrframe, x

.if compat_hr = 0
  cmp #2
.else
  cmp #1
.endif
  beq :+
  jmp skipHR2
:

  ldx ch
  lda #0
  sta mframeA, x
  sta mframeD, x
  sta mframeW, x
  lda #$ff
  sta doMacroA, x
  sta doMacroD, x
  sta doMacroW, x
 
  lda inst_buffer, x
  tax
  lda insFL, x
  sta patzp
  lda insFH, x
  sta patzp+1

  ldx ch
  ldy #1
  lda (patzp), y
  sta ad, x
  iny
  lda (patzp), y
  sta sr, x

  iny
  lda (patzp), y
  sta duty_lo, x
  iny
  lda (patzp), y
  sta duty_hi, x

  iny
  lda (patzp), y
  sta flags_temp
  and #1
  tay
  lda table_1_to_ff, y
  sta abspw, x

  lda flags_temp
  and #2
  beq :+
  lda #0
  sta pw_mod_lo, x
  sta pw_mod_hi, x
:

  lda flags_temp
  and #4
  tay
  ldx chipnum
  lda res, x
  ldx chm
  and ch_filter_enable_inv, x
  cpy #0
  beq :+
  ora ch_filter_enable, x
:
  ldx chipnum
  sta res, x

  lda flags_temp
  and #8
  beq :++
  ldx chipnum
  lda #0
  sta doMacroC, x
  lda res, x
  and #$0f
  ldy #6
  ora (patzp), y
  sta res, x
  iny
  lda vol, x
  and #$0f
  ora (patzp), y
  sta vol, x
  iny
  lda (patzp), y
  sta fil_lo_n, x
  iny
  lda (patzp), y
  sta fil_hi_n, x

  lda flags_temp
  and #32
  bne :++
  lda triggered_fil, x
  beq :+
  lda retrigger_fil, x
  beq :++
:
  lda #$ff
  sta triggered_fil, x
  lda fil_lo_n, x
  sta fil_lo, x
  lda fil_hi_n, x
  sta fil_hi, x
:

  ldx ch
  lda inst_buffer, x
  cmp inst_prev, x
  beq :+
  ldx chipnum
  lda fil_lo_n, x
  sta fil_lo, x
  lda fil_hi_n, x
  sta fil_hi, x
  ldx ch
  lda inst_buffer, x
  sta inst_prev, x
:

  lda flags_temp
  and #32
  beq :+
  ldx ch
  lda inst_buffer, x
  ldx chipnum
  sta instF, x
  lda #0
  sta mframeC, x
  lda #$ff
  sta doMacroC, x

  lda flags_temp
  and #16
  lsr
  tay
  lda table_1_to_ff, y
  ldy chipnum
  sta absfil, y

:

  lda flags_temp
  and #64
  lsr
  lsr
  lsr
  ldx ch
  sta test_reset, x

  lda retrigger_pw, x
  beq :+
  lda #0
  sta pw_mod_lo, x
  sta pw_mod_hi, x
:

  lda mframeW, x
  beq :+
  ldy #0
  lda (patzp), y
  and #$0f
  sta temp
  lda wav, x
  and #%11110000
  ora #1
  ora temp
  sta wav, x
  jmp skipHR2
:
  ldx ch
  ldy #0
  lda (patzp), y
  ora #1
  sta wav, x
skipHR2:
  rts

.endmacro

add_do_ch

.macro add_do_ch_DIGI
do_ch_DIGI:
  .local skipHR1, skipHR2, skipHR_ADSR, end

  ldx #3
  jsr doFinepitch

  ldx #3
  lda freq_lo1, x
  sta freq_lo2, x
  pha
  lda temp
  sta freq_lo1, x
  pla
  sta temp

  lda freq_hi1, x
  sta freq_hi2, x
  pha
  lda temp+1
  sta freq_hi1, x
  pla
  sta temp+1

.repeat 4
  clc
  lsr temp+1
  ror temp
.endrepeat

  lda temp
  sta nmi_freq_lo+1
  lda temp+1
  sta nmi_freq_hi+1

  ldx ch
  lda env_reset4, x
  cmp #$ff
  beq :+
  dec env_reset4, x
:

  ldx ch
  lda hrc, x
  beq skipHR1

  lda #0
  sta hrc, x
.if HR_ADSR <> 0
  lda env_reset, x
  bne :+
  lda #1
  sta env_reset4, x
:
.endif
  lda #0
  sta doMacroA, x
  sta doMacroD, x
  sta doMacroW, x

skipHR1:

  lda hrframe, x
  cmp #4
  bne :+
  jmp skipHR2
:

  inc hrframe, x

.if compat_hr = 0
  cmp #2
.else
  cmp #1
.endif
  beq :+
  jmp skipHR_ADSR
:

  ldx ch
  lda #0
  sta mframeA, x
  sta mframeD, x
  sta mframeW, x
  lda #$ff
  sta doMacroA, x
  sta doMacroD, x
  sta doMacroW, x

  lda inst_buffer, x
  tax
  lda insSI, x
  tax
  lda sampleHS, x
  sta nmi_add+2
  lda #0
  sta nmi_add+1
  lda sampleHE, x
  sta nmi_cmp+1
  sta nmi_cmp2+1

skipHR_ADSR:

skipHR2:
  rts

.endmacro

.if DIGI <> 0
  add_do_ch_DIGI
.endif

.proc playaddr
.if DIGI <> 0
  lda #0
  sta ldn+1
  jsr NMI_BUFFER
.endif

  ldx tick_sel
  inc tick
  lda tick
  cmp tick_speeds, x
  bcs :+
  jmp skipseq
:
  lda #0
  sta tick

advance_tick:

  lda tick_sel
  eor #1
  sta tick_sel
  ldx #(CHIP_AMT)-1
  lda #0
:
  sta fil_st2, x
  dex
  bpl :-


  .repeat chnum, I
    lda #I
    sta ch
    lda #I/3
    sta chipnum
    jsr advance
  .endrepeat

  lda nextpat
  beq skipnextpat
  lda #0
  sta nextpat
  lda jumppat
  beq :+
  lda #0
  sta jumppat
  jmp :++
:
  inc patind
  lda patind
  cmp #order0len
  bne :+
  lda #0 ; #patloop
  sta patind
:
  jsr set_patseq
  ldx #chnum-1
  lda #1
durloop:
    sta dur, x
    dex
    bpl durloop
  jmp advance_tick
skipnextpat:
  lda jumppat
  beq :+
  lda #$ff
  sta nextpat
:
skipseq:

.if DIGI = 0
.repeat chnum, I
  ldx #I
  lda inst1, x
  sta inst2, x
  lda ins, x
  sta inst1, x

  lda #I
  sta ch
  lda #I .mod 3
  sta chm
  lda #(I .mod 3)*7+(I/3)*$20
  sta base
  lda #I/3
  sta chipnum
  jsr do_ch_jsr
.endrepeat
.else
.repeat 4, I
  ldx #I
  lda inst1, x
  sta inst2, x
  lda ins, x
  sta inst1, x

  lda #I
  sta ch
  lda #I .mod 3
  sta chm
  lda #(I .mod 3)*7+(I/3)*$20
  sta base
  lda #I/3
  sta chipnum
  .if I = 3
    jsr do_ch_DIGI
  .else
    jsr do_ch_jsr
  .endif
.endrepeat
.endif


.repeat chnum, I
  lda #I
  sta ch
  jsr insarp
  jsr inswave
  jsr insduty
.endrepeat

.repeat CHIP_AMT, I
  lda #I
  sta ch
  jsr inscut
  ldx ch
  lda fil_st2, x
  beq :+
  lda fil_lo_n2, x
  sta fil_lo, x
  lda fil_hi_n2, x
  sta fil_hi, x
:

.endrepeat

  ldx #chnum-1
note_cut_loop:
  lda cut_dur, x
  cmp #$ff
  beq note_cut_loop_end
  dec cut_dur, x
  lda cut_dur, x
  beq :+
  jmp note_cut_loop_end
:
  lda #$ff
  sta cut_dur, x

  lda wav, x
  and #%11111110
  sta wav, x

note_cut_loop_end:
  dex
  bpl note_cut_loop
  
  ldx #chnum-1
relslide_loop:
  lda note_dest ,x
  and #$80
  beq slide_skip
  eor note_dest ,x
  sta macroIns
  lda slide_amt_sign, x
  beq positive_slide2
  lda note_n, x
  clc
  adc macroIns
  jsr clamp_note
  sta note_dest, x
  jmp slide_skip
positive_slide2:
  lda note_n, x
  sec
  sbc macroIns
  jsr clamp_note
  sta note_dest, x
slide_skip:
  dex
  bpl relslide_loop

  ldx #chnum-1
note_loop:
  lda absarp, x
  beq nrel
  lda arp, x
  and #127
  jmp nout
nrel:
  lda note_n, x
  clc
  ;adc arp, x
nout:
  clc
  jsr add_arpeff
  jsr clamp_note
  tay
  lda env_reset4, x
  .if compat_hr = 0
    cmp #$ff
  .else
    cmp #$00
  .endif
  bne :+
  clc
  lda note_table_lo, y
  adc slide_buffer_lo, x
  sta note_pitch_lo, x
  lda note_table_hi, y
  adc slide_buffer_hi, x
  sta note_pitch_hi, x
:
  dex
  bpl note_loop

  ldx #chnum-1
slide_loop:
  lda slide_amt, x
  bne :+
  jmp slide_loop2
:
  lda slide_amt_sign, x
  bne positive_slide
  sec
  lda slide_buffer_lo, x
  sbc slide_amt, x
  sta slide_buffer_lo, x
  lda slide_buffer_hi, x
  sbc #0
  sta slide_buffer_hi, x
;  bvc :+ ; i've never used this instruction before lmao
;  jmp finish_slide
;:
  ldy note_dest, x
  lda note_table_lo, y
  sta patzp
  lda note_table_hi, y
  sta patzp+1
  lda note_pitch_lo, x
  sta temp
  lda note_pitch_hi, x
  sta temp+1
  cmp16 patzp, temp
  bcc slide_loop2
  jmp finish_slide
positive_slide:
  clc
  lda slide_buffer_lo, x
  adc slide_amt, x
  sta slide_buffer_lo, x
  lda slide_buffer_hi, x
  adc #0
  sta slide_buffer_hi, x
  ;bcc :+
  jmp :+
finish_slide:
  lda note_dest, x
  sta note_n, x
  lda #0
  sta slide_buffer_lo, x
  sta slide_buffer_hi, x
  sta slide_amt, x
  sta slide_amt_sign, x
  jmp slide_loop2
:
  ldy note_dest, x
  lda note_table_lo, y
  sta patzp
  lda note_table_hi, y
  sta patzp+1
  lda note_pitch_lo, x
  sta temp
  lda note_pitch_hi, x
  sta temp+1
  cmp16 temp, patzp
  bcc slide_loop2
  jmp finish_slide
slide_loop2:
  dex
  bmi slide_loopt
  jmp slide_loop
slide_loopt:

  ldx #chnum-1
:
  lda note_tick, x
  cmp #96
  beq note_tick_loop
  inc note_tick, x
note_tick_loop:
  dex
  bpl :-

.if DIGI = 0
lda vol
sta $d418
.if chnum >= 6
lda vol+1
sta $d438
.endif
.if chnum >= 9
lda vol+2
sta $d458
.endif
.if chnum >= 12
lda vol+3
sta $d478
.endif
.endif

lda res
sta $d417
.if chnum >= 6
lda res+1
sta $d437
.endif
.if chnum >= 9
lda res+2
sta $d457
.endif
.if chnum >= 12
lda res+3
sta $d477
.endif

lda fil_lo
sta temp
lda fil_hi
sta temp+1

clc
lsr temp+1
ror temp
clc
lsr temp+1
ror temp
clc
lsr temp+1
ror temp

lda temp
sta $d416

.if chnum >= 6
lda fil_lo+1
sta temp
lda fil_hi+1
sta temp+1

clc
lsr temp+1
ror temp
clc
lsr temp+1
ror temp
clc
lsr temp+1
ror temp

lda temp
sta $d436
.endif

.if chnum >= 9
lda fil_lo+2
sta temp
lda fil_hi+2
sta temp+1

clc
lsr temp+1
ror temp
clc
lsr temp+1
ror temp
clc
lsr temp+1
ror temp

lda temp
sta $d456
.endif

.if chnum >= 12
lda fil_lo+2
sta temp
lda fil_hi+2
sta temp+1

clc
lsr temp+1
ror temp
clc
lsr temp+1
ror temp
clc
lsr temp+1
ror temp

lda temp
sta $d476
.endif

  ldx #chnum-1
note_loop3:
  lda absarp, x
  beq nrel3
  lda arp, x
  and #127
  jmp nout3
nrel3:
  lda note_n, x
  clc
  adc arp, x
nout3:
  clc
  jsr add_arpeff
  jsr clamp_note
  tay
  lda env_reset4, x
  cmp #$ff
  bne :+
  clc
  lda note_table_lo, y
  adc slide_buffer_lo, x
  sta note_pitch_lo, x
  lda note_table_hi, y
  adc slide_buffer_hi, x
  sta note_pitch_hi, x
:
  dex
  bpl note_loop3

.repeat CHIP_AMT, J
  ldx #2
:
  txa
  clc
  adc #J*3
  tax
  jsr doFinepitch
  txa
  sec
  sbc #J*3
  tax

  ldy ch_mul_tabl, x
  lda temp
  sta $d400+J*$20,y 
  lda temp+1
  sta $d401+J*$20, y

  txa
  clc
  adc #J*3
  tax
  lda freq_lo1, x
  sta freq_lo2, x
  lda temp
  sta freq_lo1, x

  lda freq_hi1, x
  sta freq_hi2, x
  lda temp+1
  sta freq_hi1, x
  txa
  sec
  sbc #J*3
  tax

  lda ad+J*3, x
  sta $d405+J*$20, y
  lda sr+J*3, x
  sta $d406+J*$20, y

  lda wav+J*3, x
  sta $d404+J*$20, y
  lda duty_lo, x
  clc
  adc pw_mod_lo+J*3, x
  sta $d402+J*$20, y
  lda duty_hi, x
  adc pw_mod_hi+J*3, x
  and #$0f
  sta $d403+J*$20, y

  dex
  bpl :-
.endrepeat


  rts
.endproc

set_patseq:
  stx temp+2

  ldx patind

  .repeat chnum, I
    lda .ident(.concat ("order", .sprintf("%d",I), "L")), x
    sta patseq+0+2*I
    lda .ident(.concat ("order", .sprintf("%d",I), "H")), x
    sta patseq+1+2*I
  .endrepeat

  ldx temp+2
  rts

set_patseq_init:
  .repeat chnum, I
    ldx patind
    lda .ident(.concat ("order", .sprintf("%d",I), "L")), x
    sta patseq+0+2*I
    lda .ident(.concat ("order", .sprintf("%d",I), "H")), x
    sta patseq+1+2*I
  .endrepeat
  rts

add_arpeff:
  pha
  inc arpind, x
  ldy arpind, x
  lda arp_mod, y
  sta arpind, x
  tay
  pla
  cpy #1
  beq arp1
  cpy #2
  beq arp2
  rts

arp1:
  clc
  adc arpeff1, x
  rts

arp2:
  clc
  adc arpeff2, x
  rts

arp_mod:
.byte 0,1,2,0

ch_mul_tabl:
.byte 0,7,14

clamp_note:
  bcc :+
  lda #95
:
  rts

note_table_lo:
    .incbin "note_lo.bin"
note_table_hi:
    .incbin "note_hi.bin"

ch_filter_enable:
.byte 1, 2, 4
ch_filter_enable_inv:
.byte 255^1, 255^2, 255^4

triangle_lookup:
  .repeat 64, I
    .if (I+0)&32
      .byte ((32-(((I+0)&63)-32)-1)>>1)<<4
    .else
      .byte (((I+0)&63)>>1)<<4
    .endif
  .endrepeat

tri_vibrato_lookup:
  .repeat 16, J
    .repeat 16, I
      ;.byte ((I*((J>>1)-15)*2)/15)+$1f
      .byte ((I*((J<<1)-15)*2)/4)+$80
    .endrepeat
  .endrepeat

.include "song.asm"

