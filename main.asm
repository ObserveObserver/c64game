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

// maps
//FB = vertical wall
// FA = horizontal wall
// DA / DE = doors for now (this will eventually be an array)
map: .byte $FA, $FA, $FA, $FA, $FA
     .byte $FB, $FF, $FF, $FF, $FB
     .byte $FB, $FF, $FF, $FF, $FB
     .byte $FB, $FF, $FF, $FF, $FB
     .byte $FB, $FF, $FF, $FF, $FB
     .byte $FA, $FA, $FA, $FA, $FA
     .byte 0

map2: .byte $FA, $FA, $FA, $FA, $FA
     .byte $FB, $FF, $FF, $FF, $FB
     .byte $FB, $FF, $FF, $FF, $FB
     .byte $DA, $FF, $FF, $FF, $DE
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
scrVarLo: .byte 0
scrVarHi: .byte 0
westDoorCoord: .byte 0
eastDoorCoord: .byte 0
gameState: .byte 0
state: .byte 0
.function toMs(X){
    .return round((50/1000)*X)  
}
msg2:
    .byte $fa
    .byte toMs(50)
    .text "hello!"
    .byte 0
msg3:
    .text "this is the intro"
    .byte 0
msg4:
    .text "second line!"
    .byte 0
msg5:
    .text "woah!"
    .byte 0

exit:
    jmp exit
sleepLoop:
    sleep(255)
    dex
    cpx #$00
    bne sleepLoop
breakLoop:
    rts

// just testing things atm
main: 
    jsr drawBorder
    jsr dialogue
    jsr level1
    drawText(msg2,$0400) // simultaneous text + map drawing
    ldx #$0A
    jsr sleepLoop
    drawText(msg3,$0417)
    ldx #$05
    jsr sleepLoop
    drawText(msg4,$0450)
    ldx #$05
    jsr sleepLoop
    drawText(msg5,$0467)
    jmp exit

// door controls
goWestDoor:
    lda westDoorCoord
    cmp #$01
    beq level2
goEastDoor:
    lda westDoorCoord
    cmp #$01
    beq level2

// "levels"
level1:
    lda #$00
    sta introBool // we are not in intro
    lda #<map2
    sta mapPntLo
    lda #>map2
    sta mapPntHi
    lda #<$0411
    sta scrVarLo
    lda #>$0411
    sta scrVarHi
    lda #$01
    sta westDoorCoord
    jmp drawMap
level2:
    lda #$00
    sta introBool
    lda #<map
    sta mapPntLo
    lda #>map
    sta mapPntHi
    lda #<$0411
    sta scrVarLo
    lda #>$0411
    sta scrVarHi
    jmp drawMap
intro:
    lda #$01
    sta introBool // we are in intro
    lda #<introMap
    sta mapPntLo
    lda #>introMap
    sta mapPntHi
    lda #<$0411
    sta scrVarLo
    lda #>$0411
    sta scrVarHi

// simple dialogue
// will refactor later
temp1: .byte 0
.label textBox = $0684
testStr:
    .text "hello! this is a test!"
    .byte $FE
    .text "okay!"
    .byte $FE
    .text "okay, does this work?"
    .byte 0
dialogue:
    lda #$01
    sta state
    ldx #$00
    ldy #$00
    lda #<$0682
    sta scrPrintPntLo
    lda #>$0682
    sta scrPrintPntHi
    lda #<testStr
    sta wrdPntLo
    lda #>testStr
    sta wrdPntHi
dialogueCheck:
    lda (wrdPntLo),y
    cmp #$FE
    bne chkNull
wait:
    stx temp
    lda #$00
    sta counter
wait2:
    lda counter
    cmp #$10
    beq cursorBlink
    jmp noBlink
cursorBlink:
    lda (scrPrintPntLo),y
    cmp #$20
    bne turnOffBlink
    lda #$23
    jmp drawCursor
turnOffBlink:
    lda #$20
drawCursor:
    sta (scrPrintPntLo),y
    lda #$00
    sta counter
noBlink:
    jsr $ff9f
    jsr $ffe4
    cmp #$20
    bne wait2
    ldx temp
    ldy #$00
    jmp incWrdPnt
chkNull:
    cmp #$00
    beq doneWithText
drawDiag:
    cpx #$1D
    bne dontMoveDown
    lda scrPrintPntLo
    clc
    adc #$0D 
    sta scrPrintPntLo
    lda scrPrintPntHi
    adc #$00           
    sta scrPrintPntHi
    ldx #$00
dontMoveDown:
    sleep(sleepDelay)
    lda (wrdPntLo),y
    sta (scrPrintPntLo),y
    inc scrPrintPntLo
    bne incWrdPnt
    inc scrPrintPntHi
incWrdPnt:
    inc wrdPntLo
    bne moveNext
    inc wrdPntHi
moveNext:
    inx
    jmp dialogueCheck
doneWithText:
    ldx #$00
    stx state
    rts


// Drawing map
drawMap:
    ldy #$00
    ldx #$00
    lda scrVarLo
    sta scrPntLo
    lda scrVarHi
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
    cmp #$DA
    beq door
    cmp #$DE
    beq door
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

nextChar: // draw next character on map
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

// draw routines
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
door:
    lda #$41
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
    cmp #$DA
    beq jmpGoWestDoor
    dec playerPos
    jmp doneKey
moveRight:
    ldy playerPos
    iny
    lda (mapPntLo),y
    cmp #$FB
    beq doneKey
    cmp #$DE
    beq jmpGoEastDoor
    inc playerPos
    jmp doneKey
jmpGoEastDoor:
    jsr goEastDoor
jmpGoWestDoor:
    jsr goWestDoor
int:
    inc $d019
    inc counter
    lda state
    bne dontDraw
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
dontDraw:
    jmp $ea81
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

// typewriter
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


// drawing the textbox
drawBorder:
    lda #<$0630
    sta scrPrintPntLo
    lda #>$0630
    sta scrPrintPntLo+1
    ldx #$0
    ldy #$0
drawTopBottom:
    lda #$44
    sta (scrPrintPntLo),y
    iny
    cpy #40
    bne drawTopBottom
    
    // Prepare for drawing sides
    ldx #$0
    lda scrPrintPntLo
    clc 
    adc #40
    sta scrPrintPntLo
    lda scrPrintPntLo+1
    adc #$00
    sta scrPrintPntLo+1
    
drawSides:
    // Check if we've reached bottom of screen
    lda scrPrintPntLo
    cmp #<$07c0
    bne continueDrawSides
    lda scrPrintPntLo+1
    cmp #>$07c0
    bne continueDrawSides
    jmp drawBottomRow
    
continueDrawSides:
    lda #$42
    ldy #$00
    sta (scrPrintPntLo),y
    ldy #39        // Right side is at position 39, not 40
    sta (scrPrintPntLo),y
    lda scrPrintPntLo
    clc
    adc #40
    sta scrPrintPntLo
    lda scrPrintPntLo+1
    adc #$00
    sta scrPrintPntLo+1
    jmp drawSides
    
drawBottomRow:
    // Set pointer to bottom row
    lda #<$07c0
    sta scrPrintPntLo
    lda #>$07c0
    sta scrPrintPntLo+1
    ldy #$0
drawBottomLoop:
    lda #$44
    sta (scrPrintPntLo),y
    iny
    cpy #40
    bne drawBottomLoop
    rts             // Return from subroutine

counter: .word 0
sleepDelay: .byte 01
addrOffset: .byte 0
offsetCount: .word 0
msgPos: .byte 0
retPntPos: .byte 0
wordLen: .byte 0

playerPos: .byte 12
