
Hardcore_EffectsThatDropAttack:
    db ATTACK_DOWN_SIDE_EFFECT
    db ATTACK_DOWN1_EFFECT
    db ATTACK_DOWN2_EFFECT
    db -1

Hardcore_EffectsThatDropDefense:
    db DEFENSE_DOWN_SIDE_EFFECT
    db DEFENSE_DOWN1_EFFECT
    db DEFENSE_DOWN2_EFFECT
    db -1

Hardcore_EffectsThatDropSpecial:
    db SPECIAL_DOWN_SIDE_EFFECT
    db SPECIAL_DOWN1_EFFECT
    db SPECIAL_DOWN2_EFFECT
    db -1

Hardcore_EffectsThatDropSpeed:
    db SPEED_DOWN_SIDE_EFFECT
    db SPEED_DOWN1_EFFECT
    db SPEED_DOWN2_EFFECT
    db -1

Hardcore_EffectsThatRaiseAttack:
    db ATTACK_UP1_EFFECT
    db ATTACK_UP2_EFFECT
    db -1

Hardcore_EffectsThatRaiseDefense:
    db DEFENSE_UP1_EFFECT
    db DEFENSE_UP2_EFFECT
    db -1

Hardcore_EffectsThatRaiseSpecial:
    db SPECIAL_UP1_EFFECT
    db SPECIAL_UP2_EFFECT
    db -1

Hardcore_EffectsThatRaiseSpeed:
    db SPEED_UP1_EFFECT
    db SPEED_UP2_EFFECT
    db -1

Hardcore_FavorableSideEffects:
    db -1 ; TEST: REMOVE THIS LATER

    db ATTACK_DOWN_SIDE_EFFECT
    db DEFENSE_DOWN_SIDE_EFFECT
    db SPECIAL_DOWN_SIDE_EFFECT
    db SPEED_DOWN_SIDE_EFFECT
    db TWINEEDLE_EFFECT
    db SWIFT_EFFECT
    db FLINCH_SIDE_EFFECT1
    db FLINCH_SIDE_EFFECT2
    db CONFUSION_SIDE_EFFECT
    ; fallthrough
Hardcore_EffectsThatDealStatus:
    ; fallthrough
Hardcore_EffectsThatDealStatusSideEffects:
    db PARALYZE_SIDE_EFFECT2
    db POISON_SIDE_EFFECT2
    db BURN_SIDE_EFFECT2
    ; db TWINEEDLE_EFFECT ; we can't ignore the whole effect of twineedle, so the AI won't care if this move poisons or not
    db FREEZE_SIDE_EFFECT
    db -1

Hardcore_TrashMoveEffects:
    db RAGE_EFFECT
    db SPLASH_EFFECT
    db FOCUS_ENERGY_EFFECT
    db SWITCH_AND_TELEPORT_EFFECT
    db TRANSFORM_EFFECT
    db -1

; Carry if in list
Hardcore_LoadedMoveEffectInList:
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
    scf
.nope
    pop bc
    ret

; If we have multiple moves with the same number of turns to KO, prioritize moves with good effects 
; higher score = better
Hardcore_SameTurnToKOMoveScoring:
    db DEFENSE_DOWN_SIDE_EFFECT, 40
    db POISON_SIDE_EFFECT1,      40
    db POISON_SIDE_EFFECT2,      40
    db SPECIAL_DAMAGE_EFFECT,    50
    db SWIFT_EFFECT,             50
    db CONFUSION_SIDE_EFFECT,    80
    db BURN_SIDE_EFFECT1,        90
    db BURN_SIDE_EFFECT2,        90
    db DRAIN_HP_EFFECT,          90
    db PARALYZE_SIDE_EFFECT2,   100
    db FREEZE_SIDE_EFFECT,      125
    db FLINCH_SIDE_EFFECT2,     250
    db FLINCH_SIDE_EFFECT1,     250
    db TRAPPING_EFFECT,         250
    db -1

; Get the score for the currently loaded move effect
Hardcore_GetSameTurnToKOMoveScore:
    ; return 0 if no effect
    ld a, [wEnemyMoveEffect]
    and a
    ret z

    push hl
    push bc
    ld b, a
    ld hl, Hardcore_SameTurnToKOMoveScoring

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