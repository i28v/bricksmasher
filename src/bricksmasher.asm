collisonram = $700

.segment "HEADER"

.byte "NES"
.byte $1A
.byte $02
.byte $01
.byte %00000000
.byte $00
.byte $00
.byte $00
.byte $00
.byte $00, $00, $00, $00, $00

.segment "ZEROPAGE"

northwest = $01
southwest = $02
northeast = $03
southeast = $04
collsionHandler:   .res 1
balldirection:     .res 1
paddlespeed:       .res 1 
ballspeed:         .res 1
paddlexpos:        .res 1
paddleypos:        .res 1
ballxpos:          .res 1
ballypos:          .res 1
paddlehitboxindex: .res 1

.segment "STARTUP"

Reset:
    sei 
    cld 
    ldx #$40
    stx $4017
    ldx #$FF
    txs 
    inx 
    stx $2000
    stx $2001 
    stx $4010
:
    bit $2002
    bpl :-
    txa 

clearmem:
    sta $0000, x
    sta $0100, x
    sta $0300, x
    sta $0400, x
    sta $0500, x 
    sta $0600, x
    sta $0700, x
    lda #$FF
    sta $0200, x
    lda #$00
    inx 
    bne clearmem 
:
    bit $2002
    bpl :-
    lda #$02
    sta $4014
    nop 
    lda #$3F
    sta $2006
    lda #$00
    sta $2006
    ldx #$00

loadpalettes:
    lda PaletteData, x
    sta $2007
    inx 
    cpx #$20
    bne loadpalettes
    ldx #$00   

enableNMI:
    cli 
    lda #%10010000
    sta $2000
    lda #%00011110
    sta $2001

init:
    ldx #$00
initCollisionRam:
    lda CollisionMap, x
    sta collisonram, x
    inx 
    cpx #$78
    bne initCollisionRam
    lda #$DE
    sta paddlexpos
    lda #$C8 
    sta paddleypos
    lda #130
    sta ballxpos
    lda #$64
    sta ballypos
    lda #southeast 
    sta balldirection
    lda #$02
    sta paddlespeed
    sta ballspeed

Forever:
    jmp Forever

CheckCollide:
    txa 
    lsr 
    lsr 
    lsr 
    lsr 
    lsr 
    lsr 
    sta collsionHandler
    tya 
    lsr 
    lsr 
    lsr 
    asl 
    asl 
    clc 
    adc collsionHandler
    tay 
    txa 
    lsr 
    lsr 
    lsr 
    and #%00000111
    tax 
    lda collisonram, y
    and BitMask, x
    rts 

update:
    lda #$01
    sta $4016
    lda #$00
    sta $4016
    lda $4016
    and #%00000001
    cmp #%00000001
    bne A_not_pressed
A_not_pressed:
    lda $4016
    and #%00000001
    cmp #%00000001
    bne B_not_pressed
B_not_pressed:
    lda $4016
    and #%00000001
    cmp #%00000001
    bne Select_not_pressed
Select_not_pressed:
    lda $4016
    and #%00000001
    cmp #%00000001
    bne Start_not_pressed
Start_not_pressed:
    lda $4016
    and #%00000001
    cmp #%00000001
    bne Up_not_pressed
Up_not_pressed:
    lda $4016
    and #%00000001
    cmp #%00000001
    bne Down_not_pressed
Down_not_pressed:
    lda $4016
    and #%00000001
    cmp #%00000001
    bne Left_not_pressed
    lda paddlexpos 
    sec 
    sbc paddlespeed 
    sta paddlexpos
    lda paddlexpos
    clc 
    adc #$10
    tax 
    ldy paddleypos 
    jsr CheckCollide
    beq :+
    lda paddlexpos 
    clc 
    adc paddlespeed 
    sta paddlexpos
:    
Left_not_pressed:
    lda $4016
    and #%00000001
    cmp #%00000001
    bne Right_not_pressed
    lda paddlexpos 
    clc 
    adc paddlespeed
    sta paddlexpos
    lda paddlexpos 
    clc 
    adc #$18
    tax 
    ldy paddleypos 
    jsr CheckCollide
    beq :+
    lda paddlexpos 
    sec 
    sbc paddlespeed 
    sta paddlexpos
:
Right_not_pressed:

updateBall:
    lda balldirection
    cmp #northwest 
    beq move_northwest 
    cmp #southwest 
    beq move_southwest
    cmp #northeast 
    beq move_northeast 
    cmp #southeast
    beq move_southeast 
    jmp finishupdateball
move_northwest:
    lda ballxpos 
    sec 
    sbc ballspeed 
    sta ballxpos  
    lda ballypos 
    sec 
    sbc ballspeed 
    sta ballypos
    lda ballypos 
    cmp #$00
    bne :+
    lda #southwest
    sta balldirection
:
    lda ballxpos 
    clc 
    adc #$08
    cmp #$00
    bne :+
    lda #northeast 
    sta balldirection
:
    jmp finishupdateball
move_southwest:
    lda ballxpos
    sec 
    sbc ballspeed 
    sta ballxpos  
    lda ballypos 
    clc 
    adc ballspeed 
    sta ballypos
    ldx ballxpos 
    ldy ballypos 
    jsr CheckCollide
    beq :+
    lda #northwest 
    sta balldirection
:
    lda ballxpos 
    clc 
    adc #$08
    cmp #$00
    bne finishupdateball
    lda #southeast
    sta balldirection
    jmp finishupdateball
move_northeast:
    lda ballxpos 
    clc 
    adc ballspeed 
    sta ballxpos
    lda ballypos 
    sec 
    sbc ballspeed 
    sta ballypos 
    lda ballypos 
    cmp #$00
    bne :+
    lda #southeast 
    sta balldirection
:
    lda ballxpos 
    sec 
    sbc #$1F
    cmp #$D1
    bne finishupdateball
    lda #northwest 
    sta balldirection
    jmp finishupdateball
move_southeast: 
    lda ballxpos 
    clc 
    adc ballspeed 
    sta ballxpos 
    lda ballypos 
    clc 
    adc ballspeed 
    sta ballypos
    ldx ballxpos 
    ldy ballypos 
    jsr CheckCollide
    beq :+
    lda #northeast
    sta balldirection
:
    lda ballxpos 
    sec 
    sbc #$1F
    cmp #$D1
    bne finishupdateball
    lda #southwest
    sta balldirection
finishupdateball:
        lda ballypos
    cmp paddleypos 
    bne skipCheckBall
    lda #$00
    sta paddlehitboxindex
    ldx #$00
checkBallPosLoop:
    lda paddlexpos
    clc
    adc paddlehitboxindex
    cmp ballxpos
    bne endCheckBallPosLoop
    lda balldirection
    cmp #southwest
    bne :+
    lda #northwest
    sta balldirection
    jmp endCheckBallPosLoop
:
    lda #northeast
    sta balldirection
endCheckBallPosLoop:
    inx
    stx paddlehitboxindex
    cpx #24
    bne checkBallPosLoop
skipCheckBall:    
    rts
    

clearPaddleCollisionBitmap:
    lda #$00
    sta collisonram+94
    sta collisonram+95
    sta collisonram+96
    sta collisonram+97
    sta collisonram+98
    sta collisonram+99
    rts


draw:
    lda #$08
    clc 
    adc ballypos
    sta $0200
    lda #$00
    sta $0201
    sta $0202
    lda #$08
    clc 
    adc ballxpos 
    sta $0203  
    lda #$08
    clc 
    adc paddleypos  
    sta $0204
    lda #$01
    sta $0205
    sta $0206
    lda #$08
    clc 
    adc paddlexpos 
    sta $0207  
    lda #$08
    clc 
    adc paddleypos 
    sta $0208
    lda #$02
    sta $0209
    lda #$01
    sta $020A
    lda #$08
    clc 
    adc paddlexpos 
    clc 
    adc #$08
    sta $020B
    lda #$08
    clc 
    adc paddleypos 
    sta $020C
    lda #$03
    sta $020D
    lda #$01
    sta $020E
    lda #$08
    clc 
    adc paddlexpos
    clc 
    adc #$10
    sta $020F
    rts 

NMI:
    lda #$00
    sta $2003
    lda #$02
    sta $4014
    jsr draw 
    jsr update
    rti 

PaletteData:
.byte $0C,$29,$1A,$0F,$22,$36,$17,$0F,$22,$30,$21,$0F,$22,$27,$17,$0F  
.byte $0C,$27,$28,$39,$0C,$16,$16,$17,$0C,$06,$0F,$12,$22,$0F,$36,$17  

CollisionMap:
    .byte %00000000, %00000000, %00000000, %00000000
    .byte %00000000, %00000000, %00000000, %00000000
    .byte %00000000, %00000000, %00000000, %00000000
    .byte %00000000, %00000000, %00000000, %00000000
    .byte %00000000, %00000000, %00000000, %00000000
    .byte %00000000, %00000000, %00000000, %00000000
    .byte %00000000, %00000000, %00000000, %00000000
    .byte %00000000, %00000000, %00000000, %00000000
    .byte %00000000, %00000000, %00000000, %00000000
    .byte %00000000, %00000000, %00000000, %00000000
    .byte %00000000, %00000000, %00000000, %00000000
    .byte %00000000, %00000000, %00000000, %00000000
    .byte %00000000, %00000000, %00000000, %00000000
    .byte %00000000, %00000000, %00000000, %00000000
    .byte %00000000, %00000000, %00000000, %00000000
    .byte %00000000, %00000000, %00000000, %00000000
    .byte %00000000, %00000000, %00000000, %00000000
    .byte %00000000, %00000000, %00000000, %00000000
    .byte %00000000, %00000000, %00000000, %00000000
    .byte %00000000, %00000000, %00000000, %00000000
    .byte %00000000, %00000000, %00000000, %00000000
    .byte %00000000, %00000000, %00000000, %00000000
    .byte %00000000, %00000000, %00000000, %00000000
    .byte %00000000, %00000000, %00000000, %00000000
    .byte %00000000, %00000000, %00000000, %00000000
    .byte %10000000, %00000000, %00000000, %00000001 
    .byte %00000000, %00000000, %00000000, %00000000
    .byte %00000000, %00000000, %00000000, %00000000
    .byte %00000000, %00000000, %00000000, %00000000
    .byte %00000000, %00000000, %00000000, %00000000
    
BitMask:
    .byte %10000000
    .byte %01000000
    .byte %00100000
    .byte %00010000
    .byte %00001000
    .byte %00000100
    .byte %00000010
    .byte %00000001
    
.segment "VECTORS"
    .word NMI
    .word Reset
.segment "CHARS"
    .incbin "../chrrom.chr"
