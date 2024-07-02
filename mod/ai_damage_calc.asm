Hardcore_DamageCalc:
    ld a, 5
    ret ; don't care (STUB)

    ; Next, actually calculate damage here.
    ld a, [wEnemyMovePower]
    ldh [hDivisor], a

    ld a, [wBattleMonHP]
    ldh [hDividend+0], a
    ld a, [wBattleMonHP+1]
    ldh [hDividend+1], a
    ld b, 2
    call Divide

    ; > 255?
    ldh a, [hQuotient+2]
    and a
    jr nz, .cap

    ; = 255?
    ldh a, [hQuotient+3]
    cp a, 255
    jr z, .cap

    ; = 0? If so, 1 turn to KO.
    and a
    jr z, .min

    ; If there is a remainder, add 1 point
    ld b, a
    ldh a, [hRemainder]
    and a
    ld a, b
    ret z
    inc a
    ret
    
.cap
    ld a, 255
    ret

.min
    ld a, 1
    ret