BasicUpstart2(raster)

// zeropage pointers
*=$F7 virtual
.zp {
    scrPrintPntLo: .byte 0
    scrPrintPntHi: .byte 0
}
*=$FA virtual
.zp {
    mapPntLo: .byte 0
    mapPntHi: .byte 0
    scrPntLo: .byte 0
    scrPntHi: .byte 0
    wrdPntLo: .byte 0
    wrdPntHi: .byte 0
}

*=$1001

map: .byte $FA, $FA, $FA, $FA, $FA
     .byte $FB, $FF, $FF, $FF, $FB
     .byte $FB, $FF, $FF, $FF, $FB
     .byte $FB, $FF, $FF, $FF, $FB
     .byte $FB, $FF, $FF, $FF, $FB
     .byte $FA, $FA, $FA, $FA, $FA
     .byte 0

introMap:
     .byte $FB, $FA, $FF, $FA, $FB
     .byte $FB, $FF, $FF, $FF, $FB
     .byte $FB, $FF, $FF, $FF, $FB
     .byte $FB, $FF, $FF, $FF, $FB
     .byte $FB, $FF, $FF, $FF, $FB
     .byte $FB, $FF, $FF, $FF, $FB
     .byte $FB, $FF, $FF, $FF, $FB
     .byte $FB, $FF, $FF, $FF, $FB
     .byte 0
temp: .byte 0, 0, 0, 0, 0 // intromap temp
screenPos: .byte 0
mapPos: .byte 0
introBool: .byte 0

.function toMs(X){
    .return round((50/1000)*X)  
}
msg2:
    .byte $fa
    .byte toMs(50)
    .text "testing!"
    .byte 0

exit:
    jmp exit

main: 
    jsr intro
    drawText(msg2,$0600)
    ldx #$0A
sleepLoop:
    cpx #$00
    beq breakLoop
    sleep(255)
    dex
    jmp sleepLoop
breakLoop:
    drawText(msg2,$0650)
    jmp exit

level1:
    lda #$00
    sta introBool
    lda #<map
    sta mapPntLo
    lda #>map
    sta mapPntHi
    jmp drawMap
intro:
    lda #$01
    sta introBool
    lda #<introMap
    sta mapPntLo
    lda #>introMap
    sta mapPntHi


// Drawing map
drawMap:
    ldy #$00
    ldx #$00
    lda #<$0400
    sta scrPntLo
    lda #>$0400
    sta scrPntHi
mapLoop:
    ldy mapPos
    lda playerPos
    clc
    adc #$30 
    sta $0410
    lda (mapPntLo),y
    cmp #$00
    beq mapDone
    cmp #$FA
    beq vertWall
    cmp #$FB
    beq horzWall
    tya
    cmp playerPos
    beq drawPlayer
    lda #$2E
    ldy screenPos
    sta (scrPntLo),y
resetML:
    inc screenPos
    inc mapPos
    jmp mapLoop

nextChar:
    inc screenPos
    inc mapPos   
    ldy screenPos         
    cpy #$05           
    bne mapLoop

    ldy #$00  
    sty screenPos       
    lda scrPntLo
    clc
    adc #$28           
    sta scrPntLo
    lda scrPntHi
    adc #$00           
    sta scrPntHi
    jmp mapLoop

drawPlayer:
    lda #$10
    ldy screenPos
    sta (scrPntLo),y
    jmp nextChar

vertWall:
    lda #$d9
    ldy screenPos
    sta (scrPntLo),y
    jmp nextChar
horzWall:
    lda #$c3
    ldy screenPos
    sta (scrPntLo),y
    jmp nextChar
mapDone:
    lda #$00
    sta mapPos
    sta screenPos
    rts
///////////////////

// Intro scroll map
scrollMap:
    ldx #$0
    jsr moveLastLine
    ldy #$27
    ldx #$22
loop:
    lda introMap,x
    sta introMap,y
    dey
    dex
    bpl loop
    ldx #$04
    ldy #$00
loop2:
    lda temp,x
    sta introMap,x
    dex
    bpl loop2
    rts
moveLastLine:
    ldy #$04
mLLLoop:
    lda introMap+35,y
    sta temp,y
    dey
    bpl mLLLoop
    rts
//////////////////

// Raster Controls
moveDown:
    lda playerPos
    clc
    adc #$05
    tay
    lda (mapPntLo),y
    cmp #$FA
    beq doneKey
    tya
    sta playerPos
    jmp doneKey
moveUp:
    lda playerPos
    sec
    sbc #$05
    tay
    lda (mapPntLo),y
    cmp #$FA
    beq doneKey
    lda introBool
    cmp #$01
    beq moveUpIntro
storePlayerUp:
    tya
    sta playerPos
moveUpCont:
    jsr scrollMap
    jmp doneKey
moveUpIntro:
    lda playerPos
    sec
    sbc #$12
    bcc moveUpCont
    tya
    sta playerPos
    jmp doneKey
moveLeft:
    ldy playerPos
    dey
    lda (mapPntLo),y
    cmp #$FB
    beq doneKey
    dec playerPos
    jmp doneKey
moveRight:
    ldy playerPos
    iny
    lda (mapPntLo),y
    cmp #$FB
    beq doneKey
    inc playerPos
    jmp doneKey
int:
    inc $d019
    inc counter
    jsr $ff9f
    jsr $ffe4
    cmp #$00
    beq doneKey
    cmp #$57
    beq moveUp
    cmp #$41
    beq moveLeft
    cmp #$53
    beq moveDown
    cmp #$44
    beq moveRight
doneKey:
    jsr drawMap
    jmp $ea81

// Raster interrupt
raster:
    ldx #0
    ldy #0
    sei
    lda #$7f
    sta $dc0d
    sta $dd0d
    and $d011
    sta $d011
    lda #100
    sta $d012
    lda #<int
    sta $0314
    lda #>int
    sta $0315
    lda #$01
    sta $d01a
    cli
initMsg: 
    lda #$00
    sta $d020 //background
    sta $d021 //border
    lda #$01
    sta $0286 //cursor
    jsr $e544 //CLS
    ldx #$00
    jsr main
/////////////////
// Sleep

ret:
    lda #$0
    sta screenPos
    sta msgPos
    rts

cont:
    jsr checkChar
    ldy msgPos
    lda (wrdPntLo),y
    cmp #$0
    beq ret
    iny
    sty msgPos
    ldy #$0
    sta (scrPrintPntLo),y   // we require a screenPosition and msgPosition, as they fall out of sync.
                        // see if End of Screen :)
    inc scrPrintPntLo
    bne nCarryContinue
    inc scrPrintPntHi
nCarryContinue:
    jsr chkScrEnd
    ldy msgPos
    lda (wrdPntLo),y
    cmp #$0
    beq ret
    ldy #$00
    sty counter
    sleep(sleepDelay)
    jmp cont
checkChar:      // checking our chars for sleep times and pauses
    ldy msgPos
    lda (wrdPntLo),y
    cmp #$fb
    beq pause3
    cmp #$fa
    beq sleepChange
    sty msgPos 
    rts
chkScrEnd:
    lda scrPrintPntLo // load lowbyte for screen
    cmp #$e7     // we check if we are at the end of the screen
    bne next
    lda scrPrintPntHi // load highbyte for screen
    cmp #$07
    bne next
    jsr $e8ea    // clear screen if we are at the end
    lda #<$0400  // reset to $0400
    sta scrPrintPntLo
    lda #>$0400
    sta scrPrintPntHi
next:  
    rts          // return to printing routine (pointer or text!)

sleepChange:     // changes our sleep delay based on finding #$01
    ldy msgPos   
    iny
    lda (wrdPntLo),y // grab our .byte containing our speed in interrupts 
    sta sleepDelay   // store for when sleep(sleepDelay) is called
    iny
    sty msgPos       // skip to next letter
    jmp checkChar

pause3: 
    jmp pause

slower:
    setDelay(50)
    ldy msgPos
    iny
    sty msgPos
    jmp checkChar

faster:
    setDelay(25)
    ldy msgPos
    iny
    sty msgPos
    jmp checkChar

pause:
    jsr $ff9f
    jsr $ffe4
    cmp #$0
    beq pause
    ldy msgPos
    iny
    sty msgPos
    jmp checkChar

.macro sleep(sleepTime) {
    lda #$00
    sta counter
sleep:
    lda sleepTime
    cmp counter
    bne sleep
}

.macro setDelay(delay) {
    lda delay
    sta sleepDelay
}

.macro drawText(msgInput, scr) {
    lda #<msgInput
    sta wrdPntLo
    lda #>msgInput
    sta wrdPntHi
    lda #<scr
    sta scrPrintPntLo
    lda #>scr
    sta scrPrintPntHi
    jsr cont
}

counter: .word 0
sleepDelay: .byte 01
addrOffset: .byte 0
offsetCount: .word 0
msgPos: .byte 0
retPntPos: .byte 0
wordLen: .byte 0

playerPos: .byte 12