; We have to re-implement the damage formula because a LOT of Gen 1 will explode if we just call the normal code
; (for example, Bide depends on the last amount of damage being done)
Hardcore_DamageCalc:
    ld a, 5 ; STUB
    ret

    xor a
    ld hl, wHardcoreAIDamage
    ld [hl+], a
    ld [hl], a

    ; Next, actually calculate damage here.
    ld a, [wEnemyMoveEffect]
    cp SPECIAL_DAMAGE_EFFECT
    jr z, Hardcore_DamageCalcFixedDamage

    cp OHKO_EFFECT
    jp z, Hardcore_DamageCalcMaxDamage

    ; fallthrough
Hardcore_DamageCalcNonfixedDamage:
    call _Hardcore_DamageCalcNonfixedDamage
    ld hl, wHardcoreAIDamage
    ldh a, [hQuotient+2]
    ld [hl+], a
    ldh a, [hQuotient+3]
    ld [hl], a

    ; fallthrough

Hardcore_DividePlayerHPByDamage:
    ; BC / DE
    ld hl, wBattleMonHP
    ld b, [hl]
    inc hl
    ld c, [hl]

    ld hl, wHardcoreAIDamage
    ld d, [hl]
    inc hl
    ld e, [hl]

    ; L = loop count, since C is busy right now
    ld l, 0
.loop
    ; If something takes more than 10 turns to KO, then it hardly matters lol
    ld a, l
    cp 10
    jr z, .done

    ; If there is 0 HP left to subtract, then we're good.
    ld a, c
    or b
    jr z, .done

    ; Increase loop counter, otherwise. We're going in.
    inc l

    ; Subtract right side
    ld a, c
    sub e
    ld c, a

    ; ...then the left, with carry of course
    ld a, b
    sbc d
    ld b, a

    ; if we didn't underflow, continue. otherwise, we're done
    jr nc, .loop

.done
    ld a, l
    ret

Hardcore_DamageCalcFixedDamage:
    ; set fixed damage
    ld a, [wEnemyMovePower]
    ld [hl], a
    jr Hardcore_DividePlayerHPByDamage

_Hardcore_DamageCalcNonfixedDamage:
    ld a, [wEnemyMonLevel]

    ; Is it a critting move?
    ld hl, Hardcore_CritMoves
    call Hardcore_LoadedMoveEffectInList
    jr nc, .calculate_level_multiplier
    ld l, a

    ; If so, do we have at least 64 speed?
    ld a, [wEnemyMonSpecies]
    call GetMonHeader
    ld a, [wMonHBaseSpeed]
    cp 64
    ld a, l
    jr c, .calculate_level_multiplier

    ; Sweet, let's double that level!
    add a
.calculate_level_multiplier
    ld h, 0
    ld l, a

    ; Double the level
    add hl, hl

    ; Divide by 5
    ld b, 2
    ld a, h
    ldh [hDividend], a
    ld a, l
    ldh [hDividend+1], a
    ld a, 5
    ldh [hDivisor], a
    call Divide

    ; Increment the result by 2
    ldh a, [hQuotient+3]
    add 2
    ldh [hQuotient+3], a

    ; Multiply by base power
    ld a, [wEnemyMovePower]
    ldh [hMultiplier], a
    call Multiply

    ; Now let's figure out what stats we are to use
    ld a, [wEnemyMoveType]
    cp SPECIAL
    ld bc, wEnemyMonAttack
    ld de, wBattleMonDefense
    jr c, .found_stat
    ld bc, wEnemyMonSpecial
    ld de, wBattleMonSpecial
.found_stat

    ; Load the stats into BC and DE
    ld hl, 0
    add hl, bc
    ld a, [hl+]
    ld b, a
    ld a, [hl]
    ld c, a

    ld hl, 0
    add hl, de
    ld a, [hl+]
    ld d, a
    ld a, [hl]
    ld e, a

    ; If we have >= 256 in either stat, we have to quarter the stats
    ld a, d
    or b

    jr z, .no_quarter

.no_quarter
    ; BC = attacker
    ; DE = defender

    ; Explode = halve defense
    ld a, [wEnemyMoveEffect]
    cp EXPLODE_EFFECT
    jr nz, .continue
    sra e

.continue
    ; Multiply by attack
    ld a, c
    ldh [hMultiplier], a
    call Multiply

    ; Divide by defense
    ld b, 2
    ld a, e
    ldh [hDivisor], a
    call Divide

    ; Divide by 50
    ld a, 50
    ld [hDivisor], a
    call Divide
    
    ; Add 2
    ldh a, [hQuotient+3]
    add 2
    ldh [hQuotient+3], a
    ldh a, [hQuotient+2]
    adc 0
    ldh [hQuotient+2], a
    ldh a, [hQuotient+1]
    adc 0
    ld [hQuotient+1], a

    ; Does it have STAB?
    ld a, [wEnemyMoveType]
    ld c, a
    ld a, [wEnemyMonType1]
    cp c
    jr z, .stab
    ld a, [wEnemyMonType2]
    cp c
    jr nz, .finish_stab
.stab
    ld b, 3
    ld a, b
    ldh [hMultiplier], a
    call Multiply
    ld a, 2
    ldh [hDivisor], a
    call Divide
.finish_stab

    ; Is it a multi-hitting move?
    ld b, 2
    ld a, [wEnemyMoveEffect]
    cp TWINEEDLE_EFFECT
    jr z, .multihitting
    cp ATTACK_TWICE_EFFECT
    jr z, .multihitting

    ld b, 3 ; 2-5 hits hit 3 times on average
    cp TWO_TO_FIVE_ATTACKS_EFFECT
    jr z, .multihitting
    cp TRAPPING_EFFECT
    jr nz, .done_multihitting

.multihitting
    ld a, b
    ldh [hMultiplier], a
    call Multiply

.done_multihitting
    ; If we did >65535 damage, cap to 65535
    ldh a, [hQuotient+1]
    and a
    jr z, Hardcore_DamageCalcScaleDamage

Hardcore_DamageCalcMaxDamage:
    xor a
    ldh [hQuotient+0], a
    ldh [hQuotient+1], a
    dec a
    ldh [hQuotient+2], a
    ldh [hQuotient+3], a

Hardcore_DamageCalcScaleDamage:
    ; Lastly, scale based on accuracy
    ld a, [wEnemyMoveEffect]
    cp SWIFT_EFFECT
    ret z

    ld a, [wEnemyMoveAccuracy]
    cp a, 255
    ret c
    
    ld [hMultiplier], a
    call Multiply

    ; Divide by 256
    ldh a, [hQuotient+2]
    ldh [hQuotient+3], a
    ldh a, [hQuotient+1]
    ldh [hQuotient+2], a

    ret