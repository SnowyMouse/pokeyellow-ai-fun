MACRO aimod_effect_jmp
	db \1, HIGH(\2), LOW(\2)
ENDM
    
AIMod_PatchRedundantEffect:
    ; Ignore stat raising/lowering moves if they wouldn't do anything
    ld hl, AIMod_EffectsThatBoostStats
    call AIMod_FindStatChangingEffect
    jr c, .stat_boost_effect
    ld hl, AIMod_EffectsThatDropStats
    call AIMod_FindStatChangingEffect
    jr c, .stat_drop_effect

    ; Check other things
    ld a, [wEnemyMoveEffect]

    ; Check if this is an attacking move that deals a status.
    ld hl, AIMod_EffectsThatDealStatusSideEffects
    call AIMod_LoadedMoveEffectInList
    jr c, .status_side_effect

    ; Or a status move that deals a status?
    ld hl, AIMod_EffectsThatDealStatusEffects
    call AIMod_LoadedMoveEffectInList
    jr c, .status_main_effect

    ; Go through the jump table
    ld hl, AIMod_RedundantSideEffectJumpTable
    ld b, a
.loop
    ; load the type
    ld a, [hl+]
    cp -1
    ret z

    ; check the type
    cp b

    ; load the address (advances hl even if we aren't going to do it)
    ld a, [hl+]
    ld c, a
    ld a, [hl+]

    ; now we check the result of that comparison!
    jr nz, .loop
    
    ; done
    ld h, c
    ld l, a
    jp hl

.status_side_effect
    ; If their typing matches the type's move, this move will not inflict a status.
    ld a, [wEnemyMoveType]
    ld b, a
    ld a, [wBattleMonType1]
    cp b
    jr z, .ignore
    ld a, [wBattleMonType2]
    cp b
    jr z, .ignore
    ; fallthrough
.status_main_effect
    ; Already statused?
    ld a, [wBattleMonStatus]
    and a
    jr nz, .ignore
    jp AIMod_IgnoreIfSubstituteUp

.stat_boost_effect
    ld a, [bc]
    cp 7 + 6
    ret nz
    jr .ignore

.stat_drop_effect
    call AIMod_IgnoreIfSubstituteUp
    ret c

    ; If the player has mist, this move will do nothing
    ld hl, wPlayerBattleStatus2
    bit PROTECTED_BY_MIST, [hl]
    jr nz, .ignore
    ld a, [bc]
    cp 7 - 6
    ret nz
.ignore
    jp AIMod_IgnoreEffect

AIMod_RedundantSideEffectJumpTable:
    aimod_effect_jmp HEAL_EFFECT, .heal_effect
    aimod_effect_jmp DRAIN_HP_EFFECT, .heal_effect
    aimod_effect_jmp OHKO_EFFECT, .ohko_effect
    aimod_effect_jmp MIST_EFFECT, .mist_effect
    aimod_effect_jmp LIGHT_SCREEN_EFFECT, .light_screen_effect
    aimod_effect_jmp REFLECT_EFFECT, .reflect_effect
    aimod_effect_jmp FOCUS_ENERGY_EFFECT, .focus_energy_effect
    aimod_effect_jmp FLINCH_SIDE_EFFECT1, .flinch_effect
    aimod_effect_jmp FLINCH_SIDE_EFFECT2, .flinch_effect
    aimod_effect_jmp CONFUSION_SIDE_EFFECT, .confusion_effect
    aimod_effect_jmp CONFUSION_EFFECT, .confusion_effect
    aimod_effect_jmp SUBSTITUTE_EFFECT, .substitute_effect
    aimod_effect_jmp EVASION_UP1_EFFECT, .evasion_up_effect
    aimod_effect_jmp EVASION_UP2_EFFECT, .evasion_up_effect
    aimod_effect_jmp ACCURACY_DOWN1_EFFECT, .accuracy_drop_effect
    aimod_effect_jmp ACCURACY_DOWN2_EFFECT, .accuracy_drop_effect
    db -1

; ignore if at full HP
.heal_effect
    ld de, wEnemyMonHP
    ld bc, wEnemyMonMaxHP
    call AIMod_CMP16
    ret nz
    jr AIMod_IgnoreEffect

; ignore if slower
.ohko_effect
    ld de, wBattleMonSpeed
    ld bc, wEnemyMonSpeed
    call AIMod_CMP16
    ret nc
    jr AIMod_IgnoreEffect

; ignore if the player's accuracy is -6 or they have X Accuracy or Mist
.accuracy_drop_effect
    call AIMod_IgnoreIfSubstituteUp
    ret c
    ld a, [wPlayerMonAccuracyMod]
    cp 7-6
    jr z, AIMod_IgnoreEffect
    jr .check_x_accuracy_mist

; ignore if the AI's accuracy is +6 or the player has X Accuracy or Mist
.evasion_up_effect
    ld a, [wEnemyMonEvasionMod]
    cp 7+6
    jr z, AIMod_IgnoreEffect
    ; fallthrough
.check_x_accuracy_mist
    ld hl, wPlayerBattleStatus2
    bit PROTECTED_BY_MIST, [hl]
    jr nz, AIMod_IgnoreEffect
    ld a, 1 << USING_X_ACCURACY
    jr .check_status_bit

; ignore if already confused
.confusion_effect
    ld hl, wEnemyBattleStatus1
    ld a, 1 << CONFUSED
    jr .check_status_bit

; ignore if mist already up
.mist_effect
    ld hl, wEnemyBattleStatus2
    ld a, 1 << PROTECTED_BY_MIST
    jr .check_status_bit

; ignore if reflect already up
.reflect_effect
    ld hl, wEnemyBattleStatus3
    ld a, 1 << HAS_REFLECT_UP
    jr .check_status_bit

; ignore if light screen already up
.light_screen_effect
    ld hl, wEnemyBattleStatus3
    ld a, 1 << HAS_LIGHT_SCREEN_UP
    jr .check_status_bit

; ignore if focus energy already up
.focus_energy_effect
    ld hl, wEnemyBattleStatus2
    ld a, 1 << GETTING_PUMPED
    jr .check_status_bit

; ignore if substitute already up or under 25% HP
.substitute_effect
    ld hl, wEnemyMonHP
    call AIMod_Double16
    call AIMod_Double16
    ld de, wEnemyMonHP
    ld bc, wEnemyMonMaxHP
    call AIMod_CMP16
    call AIMod_Halve16
    call AIMod_Halve16
    jr nc, AIMod_IgnoreEffect

    ld hl, wEnemyBattleStatus2
    ld a, 1 << HAS_SUBSTITUTE_UP
    jr .check_status_bit

; hl = pointer
; a = bit mask
.check_status_bit
    and [hl]
    ret z
    jr AIMod_IgnoreEffect

.flinch_effect
    call AIMod_IgnoreIfSubstituteUp
    ret c
    call AIMod_CheckIfOutspeed
    ret nc
    ; fallthrough

AIMod_IgnoreEffect:
    ld [wEnemyMoveEffect], a
    cp TWINEEDLE_EFFECT
    ld a, ATTACK_TWICE_EFFECT ; downgrade twinneedle into just an attack that hits twice as far as the AI is concerned
    jr z, .continue
    ld a, NO_ADDITIONAL_EFFECT
.continue
    ld [wEnemyMoveEffect], a
    ret

; Returns carry if substitute
AIMod_IgnoreIfSubstituteUp:
    and a
    ld hl, wPlayerBattleStatus2
    bit HAS_SUBSTITUTE_UP, [hl]
    ret z
    call nz, AIMod_IgnoreEffect
    scf
    ret