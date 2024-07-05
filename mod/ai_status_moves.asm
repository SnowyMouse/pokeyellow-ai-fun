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
    ld de, wAIModAIMovePriority
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

    ld a, [hl]
    sub AIMod_USELESS_STAT_MOD_MOVE_PENALTY
    ld [hl], a

    ret

AIMod_PrioritizeBoostingMoves:
    ld hl, wAIModAIMovePriority
    ld d, 0
    ld e, a
    add hl, de

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
    jp c, AIMod_DeprioritizeExcessiveStatMod

    ; Below neutral? If this gets us up to +0 or higher, we should use it!
    add d
    cp 7
    jp nc, AIMod_PrioritizeUsefulStatMod

    ; Otherwise, don't.
    jp AIMod_DeprioritizeExcessiveStatMod

AIMod_PrioritizeDroppingMoves:
    ld hl, wAIModAIMovePriority
    ld d, 0
    ld e, a
    add hl, de

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
    ld d, 0
    ld e, a
    add hl, de

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
    ld d, 0
    ld e, a
    add hl, de

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
    ld d, 0
    ld e, a
    add hl, de

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