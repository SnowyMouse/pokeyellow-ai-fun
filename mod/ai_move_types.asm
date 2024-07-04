AIMod_EffectsThatBoostAttack:
    db ATTACK_UP1_EFFECT
    db ATTACK_UP2_EFFECT
    db -1

AIMod_EffectsThatLowerDefense: ; ignoring moves that drop defense as a side effect since we don't want to deprioritize those...
    db DEFENSE_DOWN1_EFFECT
    db DEFENSE_DOWN2_EFFECT
    db -1

; Ignoring evasion/accuracy moves because those will have special handling
AIMod_EffectsThatBoostStats:
    db ATTACK_UP1_EFFECT, HIGH(wEnemyMonAttackMod), LOW(wEnemyMonAttackMod), 1
    db ATTACK_UP2_EFFECT, HIGH(wEnemyMonAttackMod), LOW(wEnemyMonAttackMod), 2
    db DEFENSE_UP1_EFFECT, HIGH(wEnemyMonDefenseMod), LOW(wEnemyMonDefenseMod), 1
    db DEFENSE_UP2_EFFECT, HIGH(wEnemyMonDefenseMod), LOW(wEnemyMonDefenseMod), 2
    db SPEED_UP1_EFFECT, HIGH(wEnemyMonSpeedMod), LOW(wEnemyMonSpeedMod), 1
    db SPEED_UP2_EFFECT, HIGH(wEnemyMonSpeedMod), LOW(wEnemyMonSpeedMod), 2
    db SPECIAL_UP1_EFFECT, HIGH(wEnemyMonSpecialMod), LOW(wEnemyMonSpecialMod), 1
    db SPECIAL_UP2_EFFECT, HIGH(wEnemyMonSpecialMod), LOW(wEnemyMonSpecialMod), 2
    db -1
AIMod_EffectsThatDropStats:
    db ATTACK_DOWN_SIDE_EFFECT, HIGH(wPlayerMonAttackMod), LOW(wPlayerMonAttackMod), 1
    db ATTACK_DOWN1_EFFECT, HIGH(wPlayerMonAttackMod), LOW(wPlayerMonAttackMod), 1
    db ATTACK_DOWN2_EFFECT, HIGH(wPlayerMonAttackMod), LOW(wPlayerMonAttackMod), 2
    db DEFENSE_DOWN_SIDE_EFFECT, HIGH(wPlayerMonDefenseMod), LOW(wPlayerMonDefenseMod), 1
    db DEFENSE_DOWN1_EFFECT, HIGH(wPlayerMonDefenseMod), LOW(wPlayerMonDefenseMod), 1
    db DEFENSE_DOWN2_EFFECT, HIGH(wPlayerMonDefenseMod), LOW(wPlayerMonDefenseMod), 2
    db SPEED_DOWN_SIDE_EFFECT, HIGH(wPlayerMonSpeedMod), LOW(wPlayerMonSpeedMod), 1
    db SPEED_DOWN1_EFFECT, HIGH(wPlayerMonSpeedMod), LOW(wPlayerMonSpeedMod), 1
    db SPEED_DOWN2_EFFECT, HIGH(wPlayerMonSpeedMod), LOW(wPlayerMonSpeedMod), 2
    db SPECIAL_DOWN_SIDE_EFFECT, HIGH(wPlayerMonSpecialMod), LOW(wPlayerMonSpecialMod), 1
    db SPECIAL_DOWN1_EFFECT, HIGH(wPlayerMonSpecialMod), LOW(wPlayerMonSpecialMod), 1
    db SPECIAL_DOWN2_EFFECT, HIGH(wPlayerMonSpecialMod), LOW(wPlayerMonSpecialMod), 2
    db -1

; Looks in HL for a stat raising/lowering move. Carry if found.
; bc -> address to stat being modified
; a  -> amount to modify
AIMod_FindStatChangingEffect:
    push de
    ld a, [wEnemyMoveEffect]
    ld d, a
.loop
    ld a, [hl+]
    cp a, -1
    jr z, .done
    cp d
    ld a, [hl+]
    ld b, a
    ld a, [hl+]
    ld c, a
    ld a, [hl+]
    jr nz, .loop
    scf
.done
    pop de
    ret

AIMod_CritMoves:
    INCLUDE "data/battle/critical_hit_moves.asm"

AIMod_EffectsThatDealStatusEffects:
    db POISON_EFFECT
    db SLEEP_EFFECT
    db PARALYZE_EFFECT
    db SLEEP_EFFECT
    ; fallthrough

AIMod_EffectsThatDealStatusSideEffects:
    db PARALYZE_SIDE_EFFECT2
    db POISON_SIDE_EFFECT2
    db BURN_SIDE_EFFECT2
    db TWINEEDLE_EFFECT
    db FREEZE_SIDE_EFFECT
    db -1

AIMod_TrashMoveEffects:
    db RAGE_EFFECT
    db SPLASH_EFFECT
    db FOCUS_ENERGY_EFFECT
    db SWITCH_AND_TELEPORT_EFFECT
    db TRANSFORM_EFFECT
    db -1

AIMod_LevelMoveEffects:
    db SEISMIC_TOSS
    db NIGHT_SHADE
    db -1

; Returns carry if in list HL
AIMod_LoadedMoveEffectInList:
    push af
    push bc
    ld a, [wEnemyMoveEffect]
    ld b, a
.loop
    ld a, [hl+]
    cp -1
    jr z, .nope
    cp b
    jr nz, .loop
.done
    pop bc
    pop af
    scf
    ret
.nope
    pop bc
    pop af
    and a
    ret

; If we have multiple moves with the same number of turns to KO, prioritize moves with good effects 
; higher score = better
AIMod_SameTurnToKOMoveScoring:
    db DEFENSE_DOWN_SIDE_EFFECT, 10
    db POISON_SIDE_EFFECT1,      20
    db POISON_SIDE_EFFECT2,      30
    db SPEED_DOWN_SIDE_EFFECT,   40
    db ATTACK_DOWN_SIDE_EFFECT,  50
    db SPECIAL_DOWN_SIDE_EFFECT, 60
    db SPECIAL_DAMAGE_EFFECT,    70
    db SWIFT_EFFECT,             80
    db CONFUSION_SIDE_EFFECT,    90
    db BURN_SIDE_EFFECT1,       100
    db BURN_SIDE_EFFECT2,       110
    db DRAIN_HP_EFFECT,         120
    db PARALYZE_SIDE_EFFECT1,   130
    db PARALYZE_SIDE_EFFECT2,   140
    db FREEZE_SIDE_EFFECT,      150
    db FLINCH_SIDE_EFFECT1,     200
    db FLINCH_SIDE_EFFECT2,     220
    db TRAPPING_EFFECT,         240
    db -1

; Get the score for the currently loaded move effect
AIMod_GetSameTurnToKOMoveScore:
    push hl
    push bc
    ld a, [wEnemyMoveEffect]
    ld b, a
    ld hl, AIMod_SameTurnToKOMoveScoring

.loop
    ld a, [hl+]
    cp -1
    jr z, .none_found
    cp b
    ld a, [hl+]
    jr z, .done
    jr .loop
.none_found
    inc a ; 255 -> 0 score
.done

    ; +5 score if the move is 100% accurate
    ld b, a
    ld a, [wEnemyMoveAccuracy]
    cp 100 percent
    jr nz, .clean_up
    ld a, 5
    add b
    ld b, a
    
.clean_up
    ld a, b
    pop bc
    pop hl
    ret