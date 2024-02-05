_sysbase equ $4F2
.import tpa_size
.import tpa
.import main

	movea.l 4(a7), a6
        move.l  24(a6), d3
        add.l   28(a6), d3
        btst	#0, d3
        beq.b	l1
        addq	#1, d3
l1:     move.l  d3, tpa
        move.l	4(a6), a0
        sub.l	d3, a0
        move.l	a0, tpa_size
        move.l	#$bffff, -(sp)
        trap	#13
        addq	#4, sp
        and.b	#$f, d0
        cmpi.b	#$8, d0
        beq	pterm0
        bsr	appl_init
        move	global(pc), d0
        seq	in_auto
        bsr	main
        move	d0, d0
        beq.b	pterm0
        move.l  #$000E0001, -(sp)
        trap    #14
        move.l  d0, io
        moveq	#-1, d0
        move.l	d0, -(sp)
        move.l	d0, -(sp)
        move.l	d0, -(sp)
        move	#$10, -(sp)
        trap	#14
        move.l	d0, keytab
;        move.w  #$22, -(sp)
;        trap    #14
;        movea.l d0, a5
;        move.l  32(a5), old_kbs
;        move.l  #new_kbs, 32(a5)
        clr.l	(sp)
        move.w	#$20, -(sp)
        trap	#1
        movea.l _sysbase.w, a0
        cmpi    #$100, 2(a0)
        ble.b   l3
        move.l  $24(a0), kbshift
l3:	move.l	$118.w, a0
	move.l	-8(a0), d0
	cmp.l	#'CCal', d0
	bne.b	l4
	move.l	-4(a0), old_kbs
	bra.b	l5
l4:     move.l	a0, old_kbs
l5:     move.l	#new_kbs, $118.w
        move.l  tpa(pc), a3
        clr     (sp)
        suba.l  (a6), a3
        pea	(a3)
        move.w  #49, -(sp)
        trap    #1

pterm0: clr	-(sp)
	trap	#1

appl_init:
        move.w  #10, control
        move.w  #1, control+4
;;;;;   bra.b   gem             not needed

gregs   reg     a0-a2/d0-d2
gem: ;   movem.l #gregs, -(sp)
        lea     aespb(pc), a0
        move.l  a0, d1
        move.w  #$C8, d0
        trap    #2
     ;   movem.l (sp)+, #gregs
        rts

.import active_sh
.import active_ch

flock	equ $43e
longframe equ $59e

tk1:    move.l  old_kbs(pc), -(sp)
	rts

        dc.b    'XBRA'
        dc.b    'CCal'
old_kbs:ds.l    1
nkreg	reg a0-a1/d0-d1
new_kbs:
	tas	in_chk
	bne.b	tk1
	movem.l	#nkreg, -(sp)
	move	4*4(sp), d0	; get rte sr
	ori	#$2000, d0	; make sure S=1
	move.l	io(pc), a0
	move	8(a0), d1
	tst.w	longframe.w
	beq.b	tk3
        move    #$114, -(sp)	; exception frame format 0, vector 114
tk3:	pea	nks(pc)
        move	d0, -(sp)
        move.l  old_kbs(pc), a0
        jmp     (a0)
nks:	movea.l io(pc), a0
        move    8(a0), d0
        cmp     d1, d0
        bne.b   tk2
tk9:	movem.l	(sp)+, #nkreg
	sf	in_chk
        rte
tk2:	move.b	in_calc(pc), d1
	bne.b	tk9
        movea.l kbshift(pc), a1
        move.b  (a1), d1
        andi.b  #$f, d1
        sub.b   active_sh(pc), d1
        bne.b   tk9
        movea.l (a0), a1
        move    0(a1,d0), d1
        cmp.b	active_ch(pc), d1
        bne.b   tk9
        subq    #4, d0
        bcc.b   tk4
        add     4(a0), d0
tk4:    move    d0, 8(a0)
	move	flock.w, d0
	bne.b	tk9
	movem.l	(sp)+, #nkreg
	sf	in_chk
	btst	#2, (sp)
	bne.b	tk8		; get out if interrupt mask >= 4
	st	in_calc
	move	(sp), o_sr
	move.l	2(sp), o_addr
	move.l	#nkk, 2(sp)
	bset	#5, (sp)	; force RTE to super mode
tk8:	rte

.import	calc
nkkr	reg 	a0-a1/d0-d2
nkk:	move.l	a7, o_a7
	lea	stack(pc), a7
	movem.l	#nkkr, -(sp)
	bsr	calc
	movem.l	(sp)+, #nkkr
	move.l	o_a7(pc), a7
	move	o_sr(pc), sr
	move.l	o_addr(pc), -(sp)
	sf	in_calc
	rts

.export kbshift
.export io
o_a7:	ds.l	1
o_sr:	ds.w	1
o_addr:	ds.l	1
kbshift:dc.l    $e1b
aespb:  dc.l   control, global, intin, intin, io, io
control:ds.w    5
global: ds.w    15
intin:  ds.w    5
io: 	ds.l    5
in_auto:ds.b	1
in_calc:ds.b	1
in_chk:	ds.b	1
	ds.b	1
.export in_auto
.bss
	ds.l	128
stack:
.text
;
;   gtext( x, string ), fontptr, font_height, planes, next_pl, pl_mul )
;          8    10       14         18        20      22       24
;
          .export gtext
          .import scan_off
          .import scr_off
          .import fdat
          .import fht
          .import gt1
          .import gt2
          .import gt3

rest_CD:  lea       TSsave+4(pc), a0
          move.l    #$febf03ce, a1
          move.b    #5, (a1)+
          move.b    (a0), (a1)
          move.b    -(a0), -(a1)
          lea       $3c4-$3ce(a1), a1
          move.b    #4, (a1)+
          move.b    -(a0), (a1)
          move.b    #2, -(a1)
          move.b    -(a0), 1(a1)
          move.b    -(a0), (a1)
          rts

.import is_CD, falc_vid
CD:	  move      gt1(pc), d0
	  cmpi	    #8, d0
	  blt.b     tcd3
	  bgt.b     tcd4
	  lea	    Etext(pc), a0
	  bra.b	    tcd2
tcd4:  	  lea	    Tmod(pc), a5
	  lea	    Ttext(pc), a0
	  cmpi	    #16, d0
	  ble.b	    tcd8		; was blt
	  move      -2(a5), (a5)	; 24-bit
	  move	    d7, d5		; d5=d7*3
	  add	    d5, d5
	  add	    d7, d5
	  bra.b	    tcd3
tcd8:	  move      #$4e71, (a5)	; 16-bit
tcd2:	  move	    gt3(pc), d1
	  subq      #1, d1
	  asl	    d1, d5		; offset *= planes
tcd3:	  move.l    #$febf03c4, a2
	  lea       TSsave(pc), a1
	  move.b    (a2), (a1)+
	  move.b    #2, (a2)+
	  move.b    (a2), (a1)+
	  move.b    #$f, (a2)
	  move.b    #4, -1(a2)
	  move.b    (a2), (a1)+
	  cmpi	    #8, d0
	  bge.b     tcd7
	  andi.b    #$F7, (a2)
	  bra.b     tcd6
tcd7:	  ori.b     #8, (a2)
tcd6:	  lea       $3ce-$3c5(a2), a2
	  move.b    (a2), (a1)+
	  move.b    #5, (a2)+
	  move.b    (a2), (a1)
	  andi.b    #$fc, (a2)
	  lea       rest_CD(pc), a5
no_CD:	  rts

test_cd:
	  lea	    Ntext(pc), a0
          lea	    no_CD(pc), a5
	  move      gt1(pc), d0
          move.b    is_CD(pc), d1
	  bne       CD
	  move.b    falc_vid(pc), d1
	  beq.b	    no_CD
	  cmpi	    #15, d0
	  blt.b	    no_CD
	  move      #$4e71, Tmod	; 16-bit Falcon: nop
	  lea	    Ttext(pc), a0
	  asl	    #4, d5
tcd1:	  rts

logbase equ $44E

greg reg d2-d7/a2-a6

          .export gtext
          .import scan_off
          .import strlen
          
gtext:
          movem.l   #greg, -(sp)
          move.l    a0, a6
	  move      d0, d7           ; offset into the scan line (x)

          moveq     #0, d5              ; d5 designates the byte in the word
          btst      #3, d7           ; check the third bit of the x-coord
          beq.b     gt1l
          moveq.l   #1, d5              ; modify the LSB in the screen word

gt1l:	  bsr.b	    test_cd
          movea.l   logbase.w, a2       ; logbase in a2
          adda.l    scr_off(pc), a2
          cmpi	    #16, gt1
          bgt.b	    gt24
          lsr.w     #4, d7              ; figure out what byte starts the word
          move      gt3(pc), d0
          lsl.w     d0, d7              ; * planes * 2
          adda      d7, a2              ; first word to be modified
gt24:     adda      d5, a2              ; add the byte offset into the word
          move.w    fht(pc), d2          ; move char height-1 into d2
          subq      #1, d2
;          move	    gt1(pc), d7
;          subq      #1, d7              ; subtract 1 from the # of bitplanes
          movea.l   fdat(pc), a1          ; temp font pointer in a1
          move      gt2(pc), d7
          jsr	    (a0)
end:      jsr       (a5)
          movem.l   (a7)+, #greg        ; restore old regs
          rts

; Arbitrary number of bitplanes
Ntext:    move.w    d5, d3              ; temp byte parity in d3
          movea.l   a2, a3              ; temp screen pointer in a3
          moveq     #0, d0
          move.l    a6, a0
Nnext_chr:move.b    (a0)+, d0           ; next character to be printed
	  beq.b     N0c
          movea.l   a3, a4              ; another temp screen pointer
          move.w    gt1(pc), d1          ; number of bitplanes-1
          subq      #1, d1
Nloop:    move.b    $0(a1,d0), (a4)     ; move the font data onto the screen
          addq      #2, a4              ; next bitplane
          dbra      d1, Nloop           ; continue for all the bitplanes
          addq      #1, a3              ; increment temp screen pointer
          bchg      #0, d3              ; change the parity register
          beq.b     Ngt0                ; next byte is odd
          adda      d7, a3              ; skip over the other bitplanes
Ngt0:     bra.b     Nnext_chr       ; get the next character
N0c:      adda      #$100, a1           ; increment font pointer by 256
          adda.l    scan_off(pc), a2    ; add the screen width
          dbra      d2, Ntext           ; copy the whole char height
          rts
          
; 8 bitplanes
Etext:    movea.l   a2, a3              ; temp screen pointer in a3
          moveq     #0, d0
          move.l    a6, a0
Enext_chr:move.b    (a0)+, d0           ; next character to be printed
	  beq.b	    E0c
Eloop:    move.b    $0(a1,d0), d3       ; get font data
          moveq     #7, d1              ; number of bits
Ebit:	  add.b     d3, d3		; double it
	  scs	    (a3)+		; set color to 255 if bit was set
	  dbra      d1, Ebit		; repeat 8 times
Egt0:     bra.b     Enext_chr       ; get the next character
E0c:      adda      #$100, a1           ; increment font pointer by 256
          adda.l    scan_off(pc), a2    ; add the screen width
          dbra      d2, Etext           ; copy the whole char height
          rts
          
; 15/16/24 bit true color
Ttext:    movea.l   a2, a3              ; temp screen pointer in a3
          moveq     #0, d0
          move.l    a6, a0
Tnext_chr:move.b    (a0)+, d0           ; next character to be printed
	  beq.b     T0c
Tloop:    move.b    $0(a1,d0), d3       ; get font data
          moveq     #7, d1              ; number of bits
Tbit:	  add.b     d3, d3		; double it
	  scc	    (a3)+		; set color to black if bit was set
	  scc	    (a3)+		; set color to black if bit was set
Tmod:	  nop				; nop or scc (a3)+ for 24-bit
	  dbra      d1, Tbit		; repeat 8 times
Tgt0:     bra.b     Tnext_chr       ; get the next character
T0c:      adda      #$100, a1           ; increment font pointer by 256
          adda.l    scan_off(pc), a2    ; add the screen width
          dbra      d2, Ttext           ; copy the whole char height
          rts
          

/******logbase equ $44E

greg reg d3-d7/a2-a5

          .export gtext
          .import scan_off
          .import scr_off
          .import fdat
          .import fht
          .import gt1
          .import gt2
          .import gt3
          
gtext:    movem.l   #greg, -(a7)        ; save old regs
          
          moveq     #0, d5              ; d5 designates the byte in the word
          btst      #3, d0              ; check the third bit of the x-coord
          sne       d5                  ; modify the LSB in the screen word
          neg.b     d5
          
          movea.l   logbase.w, a2       ; logbase in a2
          adda.l    scr_off(pc), a2
          move.w    d0, d7              ; offset into the scan line (x)
          lsr.w     #4, d7              ; figure out what byte starts the word
          move      gt3(pc), d0
          lsl.w     d0, d7              ; * planes * 2
          adda      d7, a2              ; first word to be modified
          adda      d5, a2              ; add the byte offset into the word
          move.w    fht(pc), d2         ; move char height-1 into d2
          subq      #1, d2
          move	    gt1(pc), d6
          subq      #1, d6              ; subtract 1 from the # of bitplanes
          movea.l   fdat(pc), a1        ; temp font pointer in a1

	  move	    gt2(pc), d7
	  move.l    scan_off(pc), d4
          moveq     #0, d0
next_lin: move.l    a0, a5              ; string pointer into a5
          move.w    d5, d3              ; temp byte parity in d3
          movea.l   a2, a3              ; temp screen pointer in a3
next_chr: move.b    (a5)+, d0           ; next character to be printed
	  beq.b     gt0
          movea.l   a3, a4              ; another temp screen pointer
          move.w    d6, d1              ; number of bitplanes-1
loop:     move.b    $0(a1,d0), (a4)     ; move the font data onto the screen
          addq      #2, a4              ; next bitplane
          dbra      d1, loop            ; continue for all the bitplanes
          addq      #1, a3              ; increment temp screen pointer
          eori      #1, d3              ; change the parity register
          bne.b     next_chr            ; next byte is odd
          adda      d7, a3              ; skip over the other bitplanes
          bra.b     next_chr            ; get the next character
gt0:      lea       $100(a1), a1        ; increment font pointer by 256
          adda.l    d4, a2              ; add the screen width
          dbra      d2, next_lin        ; copy the whole char height
          
end:      movem.l   (a7)+, #greg        ; restore old regs
          rts
************/

.export la_init
.export linea0
  
la_init:        .ds.l   4

regs reg d0/a0-a2
linea0:         movem.l #regs, -(sp)
                .dc.w   $a000
                movem.l #regs, la_init
                movem.l (sp)+, #regs
                rts

.export longcpy
.export longcpyc
longcpy:
	movem.l	#greg, -(sp)
        move.l  a0, a6
        move.l  a1, a4
        move.l	d0, d7
	bsr	test_cd
        subq.l  #1, d7
        blt.b   lc1
lc2:    move.l  (a4)+, (a6)+
        dbra    d7, lc2
lc1:	jsr	(a5)
	movem.l	(sp)+, #greg
	rts

longcpyc:
	movem.l	#greg, -(sp)
        move.l  a0, a6
        move.l  a1, a4
        move.l	d0, d7
	bsr	test_cd
        subq.l  #1, d7
        blt.b   lcc1
        move	gt1(pc), d1
        cmpi	#8, d1
        bgt.b	lcc3
lcc2:   move.l  (a4), (a6)+
        clr.l   (a4)+
        dbra    d7, lcc2
lcc1:	jsr	(a5)
	movem.l	(sp)+, #greg
	rts

lcc3:	moveq	#-1, d1
lcc4:	move.l	(a4), (a6)+
	move.l	d1, (a4)+
	dbra	d7, lcc4
	bra.b	lcc1

.bss
.export keytab
keytab:	ds.l	1
TSsave:	ds.b	5
.text
