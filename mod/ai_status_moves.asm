DEF AIMod_USELESS_STAT_MOD_MOVE_PENALTY EQU 30
DEF AIMod_USEFUL_STAT_MOD_MOVE_BOOST EQU 5
DEF AIMod_VERY_USEFUL_STAT_MOD_MOVE_BOOST EQU 10
DEF AIMod_EXCESSIVE_STAT_MOD_MOVE_PENALTY EQU 5
DEF AIMod_STATUS_MOVE_BOOST EQU 7

AIMod_StatusMovesForEach:
    bigdw AIMod_PrioritizeBoostingMoves
    bigdw AIMod_PrioritizeDroppingMoves
    bigdw AIMod_PrioritizeStatusInflictingMoves
    bigdw AIMod_PrioritizeEvasionMoves
    bigdw AIMod_PrioritizeAccuracyLoweringMoves
    bigdw AIMod_PrioritizeAttackRaisingMovesIfBurned
    bigdw AIMod_DeprioritizeUselessSpeedLoweringMoves
    bigdw AIMod_DeprioritizeUselessSpeedRaisingMoves
    db -1

AIMod_PrioritizeStatusMoves:
    ; If there are no physical moves, there is no reason to buff attack/lower defense.
    call AIMod_HavePhysicalMoves
    call z, AIMod_DeprioritizeAttackRaisingMoves

    ; Try everything else!
    ld hl, AIMod_StatusMovesForEach
.loop
    ld a, [hl+]
    cp -1
    ret z
    ld b, a
    ld a, [hl+]
    ld c, a
    push hl
    ld h, b
    ld l, c
    call AIMod_CallHLForEachViableMove
    pop hl
    jr .loop

; Zero if none, non-zero if at least one
AIMod_HavePhysicalMoves:
    xor a
    ld [wAIModAIBuffer], a
    ld hl, .check_physical
    call AIMod_CallHLForEachViableMove
    ld a, [wAIModAIBuffer]
    and a
    ret

.check_physical
    ; Ignore all special 1 damage moves and status moves
    ld a, [wEnemyMovePower]
    cp 2
    ret c

    ; Ignore special moves
    ld a, [wEnemyMoveType]
    cp SPECIAL
    ret nc

    ; Sweet! We have a physical moves.
    ld a, 1
    ld [wAIModAIBuffer], a
    ret

AIMod_DeprioritizeAttackRaisingMoves:
    ld b, 0
    ld hl, .check_move
    call AIMod_CallHLForEachViableMove
    ret
.check_move
    ld c, a
    ld a, [wEnemyMovePower]
    and a
    ret nz
    
    ld hl, AIMod_EffectsThatBoostAttack
    call AIMod_LoadedMoveEffectInList
    jr c, .continue
    
    ld hl, AIMod_EffectsThatLowerDefense
    call AIMod_LoadedMoveEffectInList
    ret nc

.continue
    ld hl, wAIModAIMovePriority
    add hl, bc

    ; This move is now banned.
    ld a, 1
    ld [hl], a

    ret

AIMod_PrioritizeBoostingMoves:
    ld hl, wAIModAIMovePriority
    call AIMod_AddAToHL

    push hl
    ld hl, AIMod_EffectsThatBoostStats
    call AIMod_FindStatChangingEffect
    pop hl
    ret nc

    ; D = stat modifier
    ld d, a

    ; If neutral, we should boost it!
    ld a, [bc]
    cp 7
    jp z, AIMod_PrioritizeStatMod

    ; If above neutral, deprioritize it
    jp nc, AIMod_DeprioritizeExcessiveStatMod

    ; Below neutral? If this gets us up to +0 or higher, we should use it!
    add d
    cp 7
    jp nc, AIMod_PrioritizeUsefulStatMod

    ; Otherwise, don't.
    jp AIMod_DeprioritizeExcessiveStatMod

AIMod_PrioritizeDroppingMoves:
    ld hl, wAIModAIMovePriority
    call AIMod_AddAToHL

    push hl
    ld hl, AIMod_EffectsThatDropStats
    call AIMod_FindStatChangingEffect
    pop hl
    ret nc

    ; D = stat modifier
    ld d, a

    ; If neutral, we should drop it!
    ld a, [bc]
    cp 7
    jr z, AIMod_PrioritizeStatMod

    ; If below neutral, deprioritize it
    jr c, AIMod_DeprioritizeExcessiveStatMod

    ; Above neutral? If this gets us down to +0 or lower, we should use it!
    dec d
    cp 7
    jr c, AIMod_PrioritizeUsefulStatMod

    ; Otherwise, don't.
    jr AIMod_DeprioritizeExcessiveStatMod

AIMod_PrioritizeStatusInflictingMoves:
    ld hl, wAIModAIMovePriority
    call AIMod_AddAToHL

    ; Ignore non-status moves
    ld a, [wEnemyMovePower]
    and a
    ret nz

    ld hl, AIMod_EffectsThatDealStatusEffects
    call AIMod_LoadedMoveEffectInList
    ret nc

    ; Give it a whirl!
    ld a, [hl]
    add AIMod_STATUS_MOVE_BOOST
    ld [hl], a
    ret

AIMod_PrioritizeEvasionMoves:
    ld hl, wAIModAIMovePriority
    call AIMod_AddAToHL

    ld a, [wEnemyMoveEffect]
    cp EVASION_UP1_EFFECT
    jr z, .ok
    ld a, [wEnemyMoveEffect]
    cp EVASION_UP2_EFFECT
    ret nz

.ok
    ; If at +2, don't raise evasion further
    ld a, [wEnemyMonEvasionMod]
    cp 7+1
    jr nc, AIMod_DeprioritizeExcessiveStatMod

    ; If at +1, we're neutral about this
    ret z

    ; Raising evasion is fun
    jr AIMod_PrioritizeUsefulStatMod

AIMod_PrioritizeAccuracyLoweringMoves:
    ld hl, wAIModAIMovePriority
    call AIMod_AddAToHL

    ld a, [wEnemyMoveEffect]
    cp ACCURACY_DOWN1_EFFECT
    jr z, .ok
    ld a, [wEnemyMoveEffect]
    cp ACCURACY_DOWN1_EFFECT
    ret nz

.ok
    ; If at -2, don't drop accuracy further
    ld a, [wPlayerMonAccuracyMod]
    cp 7-1
    jr c, AIMod_DeprioritizeExcessiveStatMod

    ; If at -1, we're neutral about this
    ret z

    ; You love Sand Attack, don't you?
    jr AIMod_PrioritizeStatMod

AIMod_DeprioritizeExcessiveStatMod:
    ld a, [hl]
    sub a, AIMod_EXCESSIVE_STAT_MOD_MOVE_PENALTY
    ld [hl], a
    ret

AIMod_PrioritizeUsefulStatMod:
    ld a, [hl]
    add a, AIMod_VERY_USEFUL_STAT_MOD_MOVE_BOOST
    ld [hl], a
    ret

AIMod_PrioritizeStatMod:
    ld a, [hl]
    add a, AIMod_USEFUL_STAT_MOD_MOVE_BOOST
    ld [hl], a
    ret

; TODO: TEST UNCOMMITTED CHANGES!
; Prioritize glitchy behavior that raises attack when burned so that our attack is not actually cut.
; (TODO: consider adding one for lowering the enemy player's attack if they are burned by raising our own stats)
AIMod_PrioritizeAttackRaisingMovesIfBurned:
    ld hl, wAIModAIMovePriority
    call AIMod_AddAToHL

    ; No burn?
    ld a, [wEnemyMonStatus]
    bit BRN, a
    ret z

.burned
    ld a, [wEnemyMoveEffect]
    cp ATTACK_UP1_EFFECT
    jr z, .burned_continue
    cp ATTACK_UP2_EFFECT
    ret nz
.burned_continue
    ; if attack is not greater than unmodified attack, we need to boost it!
    ld de, wEnemyMonAttack
    ld bc, wEnemyMonUnmodifiedAttack
    call AIMod_CMP16
    ret nc

    ; Give it a double priority boost~!
    call AIMod_PrioritizeUsefulStatMod
    jr AIMod_PrioritizeUsefulStatMod

; TODO: TEST UNCOMMITTED CHANGES!
; Don't use speed lowering moves if +50% speed would not outspeed the opponent
AIMod_DeprioritizeUselessSpeedLoweringMoves:
    ld hl, wAIModAIMovePriority
    call AIMod_AddAToHL
    push hl

    ld hl, AIMod_EffectsThatLowerSpeed
    call AIMod_LoadedMoveEffectInList
    jr nc, .done

    ld a, [wPlayerMonSpeedMod]
    cp 7
    ld a, 1
    jr z, AIMod_DeprioritizeSpeedMoveWithStatStage
    
.done
    pop hl
    ret

; TODO: TEST UNCOMMITTED CHANGES!
; Don't use speed raising moves if +100% speed would not outspeed the opponent
AIMod_DeprioritizeUselessSpeedRaisingMoves:
    ld hl, wAIModAIMovePriority
    call AIMod_AddAToHL
    push hl

    ld hl, AIMod_EffectsThatBoostSpeed
    call AIMod_LoadedMoveEffectInList
    jr nc, .done

    ld a, [wEnemyMonSpeedMod]
    cp 7
    ld a, 2
    jr z, AIMod_DeprioritizeSpeedMoveWithStatStage
    
.done
    pop hl
    ret

AIMod_DeprioritizeSpeedMoveWithStatStage:
    push af

    ; First, do we even have a reason to boost speed? This is a waste of a turn if we don't.
    xor a
    ld [wAIModAIBuffer+2], a
    ld hl, .check_move
    call AIMod_DisableMovePatches
    call AIMod_CallHLForEachViableMove
    call AIMod_EnableMovePatches
    ld a, [wAIModAIBuffer+2]
    and a
    jr z, .nah

    ; Copy speed into wAIModAIBuffer+2
    ld bc, wAIModAIBuffer+2
    ld a, [wEnemyMonSpeed]
    ld [bc], a
    ld a, [wEnemyMonSpeed+1]
    inc bc
    ld [bc], a
    dec bc
    ld de, wBattleMonSpeed
    pop af
    
    ld hl, wAIModAIBuffer+2
    cp 1
    jr z, .fifty_percent
.one_hundred_percent
    call AIMod_Double16
    jr .continue
.fifty_percent
    call AIMod_AddHalf16
.continue
    call AIMod_CMP16
    jr nc, .fine
.deprioritize
    pop hl
    ; the move is now banned
    ld a, AIMod_MAX_DEPRIORITIZED_MOVE
    ld [hl], a
    ret
.fine
    pop hl
    ret
.nah
    pop af
    jr .deprioritize
.check_move
    ld hl, AIMod_EffectsThatBenefitFromBeingFaster
    call AIMod_LoadedMoveEffectInList
    ret nc
    ld hl, wAIModAIBuffer+2
    inc [hl]
    ret

; +50% to the [HL] = [HL] * 1.5
AIMod_AddHalf16:
    push af
    push bc

    ld a, [hl+]
    ld b, a
    ld a, [hl]
    ld c, a

    sra b
    rr c

    add c
    ld [hl-], a
    ld a, [hl]
    adc b
    ld [hl], a

    pop bc
    pop af
    ret