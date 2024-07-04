DEF AIMod_USELESS_STAT_MOD_MOVE_PENALTY EQU 30
DEF AIMod_USEFUL_STAT_MOD_MOVE_BOOST EQU 5
DEF AIMod_VERY_USEFUL_STAT_MOD_MOVE_BOOST EQU 10
DEF AIMod_EXCESSIVE_STAT_MOD_MOVE_PENALTY EQU 5

AIMod_PrioritizeStatusMoves:
    ; If there are no physical moves, there is no reason to buff attack/lower defense.
    call AIMod_HavePhysicalMoves
    call z, AIMod_DeprioritizeAttackRaisingMoves

    ; Drop/boost stats
    ld hl, AIMod_PrioritizeBoostingMoves
    call AIMod_CallHLForEachUnprioritizedMove
    ld hl, AIMod_PrioritizeDroppingMoves
    call AIMod_CallHLForEachUnprioritizedMove

    ret

; Zero if none, non-zero if at least one
AIMod_HavePhysicalMoves:
    xor a
    ld [wAIModAIBuffer], a
    ld hl, .check_physical
    call AIMod_CallHLForEachUnprioritizedMove
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
    call AIMod_CallHLForEachUnprioritizedMove
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
    jr z, .prioritize_slightly

    ; If above neutral, deprioritize it
    jr nc, .deprioritize

    ; Below neutral? If this gets us up to +0 or higher, we should use it!
    add d
    cp 7
    jr nc, .prioritize_greatly

    ; Otherwise, don't.
    jr .deprioritize

.prioritize_slightly
    ld a, [hl]
    add AIMod_USEFUL_STAT_MOD_MOVE_BOOST
    ld [hl], a
    ret

.prioritize_greatly
    ld a, [hl]
    add AIMod_VERY_USEFUL_STAT_MOD_MOVE_BOOST
    ld [hl], a
    ret

.deprioritize
    ld a, [hl]
    sub AIMod_EXCESSIVE_STAT_MOD_MOVE_PENALTY
    ld [hl], a
    ret

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
    jr z, .prioritize_slightly

    ; If below neutral, deprioritize it
    jr c, .deprioritize

    ; Above neutral? If this gets us up to +0 or lower, we should use it!
    dec d
    cp 7
    jr c, .prioritize_greatly

    ; Otherwise, don't.
    jr .deprioritize

.prioritize_slightly
    ld a, [hl]
    add AIMod_USEFUL_STAT_MOD_MOVE_BOOST
    ld [hl], a
    ret

.prioritize_greatly
    ld a, [hl]
    add AIMod_VERY_USEFUL_STAT_MOD_MOVE_BOOST
    ld [hl], a
    ret

.deprioritize
    ld a, [hl]
    sub AIMod_EXCESSIVE_STAT_MOD_MOVE_PENALTY
    ld [hl], a
    ret