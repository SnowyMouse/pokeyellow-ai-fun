AIMod_TypeMatchups:
    INCLUDE "data/types/type_matchups.asm"

; We have to re-implement the damage formula because a LOT of Gen 1 will explode if we just call the normal code
; (for example, Bide depends on the last amount of damage being done)
AIMod_DamageCalc:
    xor a
    ld hl, wAIModAIDamage
    ld [hl+], a
    ld [hl], a

    ; Next, actually calculate damage here.
    ld a, [wEnemyMoveEffect]
    cp SPECIAL_DAMAGE_EFFECT
    jr z, AIMod_DamageCalcFixedDamage

    ; OHKO = 65535 damage
    cp OHKO_EFFECT
    jp z, AIMod_DamageCalcMaxDamage

    ; 1 power, no side effect though. We probably ignored it earlier because it does nothing.
    ld a, [wEnemyMovePower]
    cp 1
    ld a, 255
    ret z

    ; fallthrough
AIMod_DamageCalcNonfixedDamage:
    call AIMod_DamageCalcNonfixedDamageCalculation
    call AIMod_CopyDamage
    ; fallthrough
AIMod_DividePlayerHPByDamage:
    ; BC / DE
    ld hl, wBattleMonHP
    ld b, [hl]
    inc hl
    ld c, [hl]

    ld hl, wAIModAIDamage
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

AIMod_DamageCalcFixedDamage:
    ; Is it based on level?
    ld hl, AIMod_LevelMoveEffects
    call AIMod_LoadedMoveEffectInList
    jr nc, .based_on_power
    ld a, [wEnemyMonLevel]
    jr .calculate
.based_on_power
    ; The power is the damage being done
    ld a, [wEnemyMovePower]
    ldh [hQuotient+3], a
    xor a
    ldh [hQuotient+0], a
    ldh [hQuotient+1], a
    ldh [hQuotient+2], a
.calculate
    call AIMod_DamageLuckScaling
    call AIMod_CopyDamage
    jr AIMod_DividePlayerHPByDamage

AIMod_DamageCalcNonfixedDamageCalculation:
    ld a, [wEnemyMonLevel]

    ; Is it a critting move?
    ld hl, AIMod_CritMoves
    call AIMod_LoadedMoveEffectInList
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

    srl b
    ld a, e
    rra
    srl b
    rra
    ld c, a

    srl d
    ld a, c
    rra
    srl d
    rra
    ld e, a

.no_quarter
    ; C = attacker
    ; E = defender

    ; Explode = halve defense
    ld a, [wEnemyMoveEffect]
    cp EXPLODE_EFFECT
    jr nz, .fix_attack
    sra e

    ; Set attack and/or defense stat to 1 if 0
.fix_attack
    ld a, c
    and a
    jr nz, .fix_defense
    ld c, 1
.fix_defense
    ld a, e
    and a
    jr nz, .finally_calculate_it
    ld e, 1

.finally_calculate_it
    ; Multiply by attack
    ld a, c
    ldh [hMultiplier], a
    call Multiply

    ; Divide by defense
    ld b, 4
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
    ; * 3/2
    ld b, 3
    ld a, b
    ldh [hMultiplier], a
    call Multiply
    ld a, 2
    ld b, 4
    ldh [hDivisor], a
    call Divide
.finish_stab
    ; * N/4
    call AIMod_FindOverallTypeEffectivenessOfLoadedMove
    ldh [hMultiplier], a
    call Multiply
    ld b, 4
    ld a, b
    ldh [hDivisor], a
    call Divide
.finish_type_effectiveness
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
    jr z, AIMod_DamageLuckScaling
    ; fallthrough
AIMod_DamageCalcMaxDamage:
    xor a
    ldh [hQuotient+0], a
    ldh [hQuotient+1], a
    dec a
    ldh [hQuotient+2], a
    ldh [hQuotient+3], a
    ; fallthrough
AIMod_DamageLuckScaling:
    ; Gamblers are here to win it big!
    ld a, [wTrainerClass]
    cp GAMBLER
    ret z

    ; Next, scale based on accuracy
    ld a, [wEnemyMoveEffect]
    cp SWIFT_EFFECT
    jr z, AIMod_DamageCalcDamageRange

    ; Don't scale 100% accurate moves. Yes, there are Gen 1 misses, but we shouldn't be too worried about that.
    ld a, [wEnemyMoveAccuracy]
    and a
    cp -1
    jr z, AIMod_DamageCalcDamageRange

    ; Multiply by accuracy
    ld [hMultiplier], a
    call Multiply

    ; Divide by 256
    ldh a, [hQuotient+2]
    ldh [hQuotient+3], a
    ldh a, [hQuotient+1]
    ldh [hQuotient+2], a

AIMod_DamageCalcDamageRange:
    ; Lastly, do damage ranges.

    ; Damage ranges don't apply to OHKO moves.
    ld a, [wEnemyMoveEffect]
    cp OHKO_EFFECT
    ret z

    ; Damage ranges don't apply to fixed damage moves.
    ld a, [wEnemyMoveEffect]
    cp SPECIAL_DAMAGE_EFFECT
    ret z

    ; Scientists assume worst luck. Everyone else assumes average luck, except Gamblers who have already checked out.
    ld a, [wTrainerClass]
    cp SCIENTIST
    ld a, 217
    jr z, .do_it
    ld a, (255+217)/2
.do_it
    ldh [hMultiplier], a
    call Multiply
    ld a, 255
    ld [hDivisor], a
    ld b, 4
    call Divide
    ret

; Maybe not 100% accurate to Gen 1, since neutral moves can technically do 1 less damage? Don't care.
;
; Returns N/4
AIMod_FindOverallTypeEffectivenessOfLoadedMove:
    ; Fixed damage moves ignore type effectiveness
    ld a, [wEnemyMoveEffect]
    cp SPECIAL_DAMAGE_EFFECT
    ld a, 4
    ret z

    push de
    push bc
    push hl

    ; Start is 4/4
    ld e, a

    ; Calculate first type
    ld a, [wEnemyMoveType]
    ld c, a
    ld a, [wBattleMonType1]
    ld b, a
    call AIMod_FindTypeEffectiveness
    call .handle
    
    ; Stop early if Type1 = Type2
    ld a, [wBattleMonType2]
    cp b
    jr z, .done

    ; Calculate second type
    ld b, a
    call AIMod_FindTypeEffectiveness
    call .handle

.done
    ld a, e
    pop hl
    pop bc
    pop de
    ret

.handle
    ; No effect? Okay...
    and a
    jr z, .no_effect

    ; Supereffective? Double it!
    cp a, SUPER_EFFECTIVE
    jr z, .double

    ; Supereffective? Double it!
    cp a, NOT_VERY_EFFECTIVE
    jr z, .halve

    ret

.double
    sla e
    ret

.halve
    srl e
    ret

.no_effect
    ld e, a
    ret

; B = defender type
; C = attacker type
; Returns A as type effectiveness, HL will be destroyed
AIMod_FindTypeEffectiveness:
    ld hl, AIMod_TypeMatchups

.loop_type_effectiveness
    ; Done?
    ld a, [hl+]
    cp -1
    jr z, .done

    ; Attacker type matches?
    cp c
    ld a, [hl+]
    jr nz, .next_type_effectiveness

    ; Defender type matches?
    cp b
    ld a, [hl]
    ret z

    ; fallthrough
.next_type_effectiveness
    inc hl
    jr .loop_type_effectiveness

.done
    ld a, EFFECTIVE
    ret

; Copy damage from hQuotient to wAIModAIDamge
AIMod_CopyDamage:
    ld hl, wAIModAIDamage
    ldh a, [hQuotient+2]
    ld [hl+], a
    ldh a, [hQuotient+3]
    ld [hl], a
    ret

DEF AIMod_NOT_BEST_DAMAGING_MOVE_DEPRIORITY EQU 5
DEF AIMod_OHKO_MOVE_BASE_PRIORITY EQU 50
DEF AIMod_OHKO_QUICK_ATTACK EQU 40
DEF AIMod_OHKO_SWIFT EQU 20
DEF AIMod_OHKO_ACCURATE EQU 10

AIMod_DamageTests:
    xor a
    ld hl, wAIModAITurnsToKill
    ld [hl+], a
    ld [hl+], a
    ld [hl+], a
    ld [hl], a
    ld [wAIModAIBuffer+1], a ; best "score" for a move with a secondary effect
    dec a
    ld [wAIModAIBuffer+0], a ; this will be used for storing the move with the least # of turns to kill

    ; Do damage tests
    ld b, 0
    ld hl, AIMod_DamageTestForMove
    call AIMod_CallHLForEachUnprioritizedMove

    ; If they all have 255+ turns to KO, don't continue
    ld a, [wAIModAIBuffer+0]
    cp 255
    ret z

    ; Cool, let's set the priority
    ld hl, AIMod_SetDamagingBaseMovePriority
    call AIMod_CallHLForEachUnprioritizedMove

    ret

AIMod_SetDamagingBaseMovePriority:
    ld c, a

    ; skip status moves
    ld a, [wEnemyMovePower]
    and a
    ret z

    ; skip bide
    ld a, [wEnemyMoveEffect]
    cp BIDE_EFFECT
    ret z

    ; See if it is the same number of turns to kill
    ld hl, wAIModAITurnsToKill
    add hl, bc
    ld a, [hl]
    ld b, a
    ld a, [wAIModAIBuffer+0]
    cp b
    jr nz, .deprioritize

    ; Is it one turn to KO? If so, side effects don't matter, so we score with different criteria.
    cp 1
    jr z, .one_turn_to_ko

    ; If not, does its score also match this?
    call AIMod_GetSameTurnToKOMoveScore
    ld b, a
    ld a, [wAIModAIBuffer+1]
    cp b
    jr nz, .deprioritize
    
    ret

.deprioritize
    ld b, 0
    ld hl, wAIModAIMovePriority
    add hl, bc
    ld a, [hl]
    sub a, AIMod_NOT_BEST_DAMAGING_MOVE_DEPRIORITY
    ld [hl], a
    ret 

.one_turn_to_ko
    ; Base amount
    ld d, AIMod_OHKO_MOVE_BASE_PRIORITY

    ; If Quick Attack KOs, prioritize the hell out of it!
    ld a, [wEnemyMoveNum]
    cp QUICK_ATTACK
    ld a, AIMod_OHKO_QUICK_ATTACK
    call z, .prioritize

    ; Accurate moves should be prioritized, if possible
    ld a, [wEnemyMoveAccuracy]
    cp 100 percent
    ld a, AIMod_OHKO_ACCURATE
    call z, .prioritize

    ; Swift should be prioritized to avoid a Gen 1 miss
    ld a, [wEnemyMoveEffect]
    cp SWIFT_EFFECT
    ld a, AIMod_OHKO_SWIFT
    call z, .prioritize

    ; Cool
    ld hl, wAIModAIMovePriority
    ld b, 0
    add hl, bc
    ld a, [hl]
    add a, d
    ld [hl], a

    ret

.prioritize
    add d
    ld d, a
    ret

AIMod_DamageTestForMove:
    ld c, a

    ; skip status moves
    ld a, [wEnemyMovePower]
    and a
    ret z

    ; skip bide
    ld a, [wEnemyMoveEffect]
    cp BIDE
    ret z

    ; Calculate damage and store the resulting number of turns to KO
    push bc
    call AIMod_DamageCalc
    pop bc
    ld hl, wAIModAITurnsToKill
    add hl, bc
    ld [hl], a

    ; Is it a new record?
    ld b, a
    ld a, [wAIModAIBuffer+0]
    cp b
    ld hl, wAIModAIBuffer+1
    jr z, .same ; nope, same number of turns to KO
    ret c       ; nope, more turns to KO

    ; Yes!
    ld a, b
    ld [wAIModAIBuffer+0], a

    ; Reset score
    xor a
    ld [hl], a

.same
    ; Now score it based on its effect. Is it a high score?
    call AIMod_GetSameTurnToKOMoveScore
    ld b, a
    ld a, [hl]
    cp b
    ret nc
    ld a, b
    ld [hl], a
    ret