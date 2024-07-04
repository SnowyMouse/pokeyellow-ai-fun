; PRIORITY
;
; 0 = disabled
; 1 = enabled but using it would have no effect
; 2+ = priority (higher = better)
; Default = 127

DEF HARDCORE_DEFAULT_MOVE_PRIORITY EQU 127
DEF HARDCORE_MAX_MOVE_PRIORITY EQU 255
DEF HARDCORE_MAX_DEPRIORITIZED_MOVE EQU 1

INCLUDE "mod/ai_move_types.asm"

Hardcore_TypeMatchups:
INCLUDE "data/types/type_matchups.asm"

Hardcore_TestSetup:
    ld hl, wEnemyMonMoves
    ld a, SURF
    ld [hl+], a
    ld a, EXPLOSION
    ld [hl+], a
    ld a, NO_MOVE
    ld [hl+], a
    ld a, NO_MOVE
    ld [hl+], a
    ret

Hardcore_EnemyTrainerChooseMoves::
    call Hardcore_TestSetup
    call Hardcore_PrioritizeMetronome
    jr c, .find_best_move

    ; Damn! We don't have Metronome. Guess we have to do this the hard way.
    call Hardcore_InitializeMovePriorities
    call Hardcore_DamageTests

.find_best_move
    call Hardcore_HighestPriorityMove
    ld d, a
    ld b, 0

.loop_until_we_get_it
    ld hl, wHardcoreAIMovePriority
    call Random
    and $3
    ld c, a
    add hl, bc
    ld a, [hl]
    cp d
    jr nz, .loop_until_we_get_it

.got_it
    ld hl, wEnemyMonMoves
    add hl, bc
    ld a, [hl]

    ld [wEnemySelectedMove], a
    ret

Hardcore_PrioritizeMetronome:
    ; Metronome is disabled? :(
    ld a, [wEnemyDisabledMove]
    cp METRONOME
    ret z

    ; Okay, search for it then!
    ld c, NUM_MOVES
    ld hl, wEnemyMonMoves
.loop
    ld a, [hl+]
    cp METRONOME
    jr z, .done
    dec c
    jr nz, .loop

; no metronome...
    and a
    ret

.done
    xor a
    ld hl, wHardcoreAIMovePriority+3
    ld [hl-], a
    ld [hl-], a
    ld [hl-], a
    ld [hl], a

    call Hardcore_FlipCToA
    ld c, a
    ld b, 0
    add hl, bc
    ld [hl], 69

    scf
    ret

Hardcore_HighestPriorityMove:
    ld b, 0 ; the highest priority
    ld c, NUM_MOVES
    ld hl, wHardcoreAIMovePriority
.loop
    ld a, [hl+]
    cp b
    jr c, .next
    ld b, a
.next
    dec c
    jr nz, .loop
    ld a, b
    ret

Hardcore_InitializeMovePriorities:
    ld c, NUM_MOVES
    ld hl, wEnemyMonMoves
    ld de, wHardcoreAIMovePriority

.init_loop
    ld a, [hl+]
    and a
    jr z, .set_move_priority ; set to 0 if NO_MOVE

    ld b, a
    ld a, [wEnemyDisabledMove]
    sub b
    jr z, .set_move_priority ; set to 0 if disabled

    ld a, b
    call Hardcore_ReadMoveData

    push hl
    call Hardcore_CheckIfNoEffect
    pop hl

.set_move_priority
    ld [de], a
    inc de

    ; next move
    dec c
    jr nz, .init_loop
    ret


Hardcore_ReadMoveData:
    push af
    push bc
    push hl
    push de

    dec a
    ld hl, Moves
    ld bc, MOVE_LENGTH
    call AddNTimes
    ld a, BANK(Moves)
    ld de, wEnemyMoveNum

    call FarCopyData
    call Hardcore_IgnoreRedundantSideEffects

    pop de
    pop hl
    pop bc
    pop af
    ret

Hardcore_ReadMoveDataAtIndex:
    call Hardcore_MoveAtIndex
    call Hardcore_ReadMoveData
    ret

Hardcore_MoveAtIndex:
    push bc
    push hl
    ld b, 0
    ld c, a
    ld hl, wEnemyMonMoves
    add hl, bc
    ld a, [hl]
    pop hl
    pop bc
    ret

Hardcore_FlipCToA:
    ld a, NUM_MOVES
    sub c
    ret
    
Hardcore_IgnoreRedundantSideEffects:
    ld a, [wEnemyMoveEffect]

    ; Ignore healing moves if at 50% HP
    cp HEAL_EFFECT
    jr z, .heal_effect

    ; Ignore draining moves if at full HP
    cp DRAIN_HP_EFFECT
    jr z, .drain_effect

    ; Ignore OHKO moves if not faster
    cp OHKO_EFFECT
    jr z, .ohko_effect

    ; Ignore flinch as a side effect if slower
    cp FLINCH_SIDE_EFFECT1
    jr z, .flinch_side_effect
    cp FLINCH_SIDE_EFFECT2
    jr z, .flinch_side_effect

    ; Ignore confusion as a side effect if already confused
    cp CONFUSION_SIDE_EFFECT
    jr z, .confusion_side_effect

    ; Check if this is an attacking move that deals a status.
    ld hl, Hardcore_EffectsThatDealStatusSideEffects
    call Hardcore_LoadedMoveEffectInList
    ret nc

    ; If so, this status won't be dealt if the opponent has a status already
    ld a, [wBattleMonStatus]
    and a
    jr nz, Hardcore_IgnoreEffect

    ; ...or their typing matches the type's move
    ld a, [wEnemyMoveType]
    ld b, a
    ld a, [wBattleMonType1]
    cp b
    jr z, Hardcore_IgnoreEffect
    ld a, [wBattleMonType2]
    cp b
    ret nz

    ret

.confusion_side_effect
    ld a, [wPlayerBattleStatus1]
    bit CONFUSED, a
    ret nz
    jr Hardcore_IgnoreEffect

.ohko_effect
    ld de, wBattleMonSpeed
    ld bc, wEnemyMonSpeed
    call Hardcore_CMP16
    ret nc
    jr Hardcore_IgnoreEffect

.heal_effect
    ld hl, wEnemyMonHP
    call Hardcore_Double16
    ld de, wEnemyMonHP
    ld bc, wEnemyMonMaxHP
    call Hardcore_CMP16
    call Hardcore_Halve16
    ret nc
    jr Hardcore_IgnoreEffect

.drain_effect
    ld de, wEnemyMonHP
    ld bc, wEnemyMonMaxHP
    call Hardcore_CMP16
    ret nz
    jr Hardcore_IgnoreEffect

.flinch_side_effect
    call Hardcore_CheckIfOutspeed
    ret nc
    ; fallthrough
Hardcore_IgnoreEffect:
    ld a, NO_ADDITIONAL_EFFECT
    ld [wEnemyMoveEffect], a
    ret

; Compare DE to BC.
;
; If DE == BC, NC/ Z
; If DE >  BC,  C/NZ
; If DE <  BC, NC/NZ
Hardcore_CMP16:
    ; Check upper 8 bits
    push hl
    ld a, [de]
    ld l, a
    ld a, [bc]
    cp l
    pop hl
    ret nz
    push hl

    ; Now the lower 8 bits
    inc de
    ld a, [de]
    dec de
    ld l, a
    inc bc
    ld a, [bc]
    dec bc
    cp l

    pop hl
    ret

; Double the 16-bit value at hl
Hardcore_Double16:
    push af
    inc hl
    sla [hl]
    dec hl
    ld a, [hl]
    rla
    ld [hl], a
    pop af
    ret

; Halve the 16-bit value at hl
Hardcore_Halve16:
    push af
    srl [hl]
    inc hl
    ld a, [hl]
    rra
    ld [hl-], a
    pop af
    ret
    
Hardcore_CopyPlayerSpeedToDividend:
    xor a
    ldh [hDividend+0], a
    ldh [hDividend+1], a
    ld a, [wBattleMonSpeed]
    ldh [hDividend+2], a
    ld a, [wBattleMonSpeed+1]
    ldh [hDividend+3], a
    ret

; Returns with carry if we do NOT outspeed
Hardcore_CheckIfHalfOutspeed:
    call Hardcore_CopyPlayerSpeedToDividend
    ld a, 2
    ldh [hDivisor], a
    call Divide
    jr Hardcore_CheckDividendVersusOurSpeed

; Returns with carry if we do NOT outspeed
Hardcore_CheckIfOutspeed:
    call Hardcore_CopyPlayerSpeedToDividend
    ; fallthrough

; Returns with carry if we do NOT outspeed what is in hQuotient
Hardcore_CheckDividendVersusOurSpeed:
    ldh a, [hQuotient+2]
    ld b, a
    ld a, [wEnemyMonSpeed]
    cp b
    ret c ; no outspeed
    jr nz, .outspeed

    ldh a, [hQuotient+3]
    ld b, a
    ld a, [wEnemyMonSpeed+1]
    cp b
    ret c ; no outspeed

.outspeed
    and a
    ret

Hardcore_CheckIfNoEffect:
    ld hl, Hardcore_TrashMoveEffects
    call Hardcore_LoadedMoveEffectInList
    jr c, .no_effect

    ld a, [wEnemyMovePower]
    and a
    jr z, .check_status_move

    call Hardcore_FindOverallTypeEffectivenessOfLoadedMove
    and a
    jr z, .no_effect
    jr .done

.check_status_move
    ld a, [wEnemyMoveEffect]
    and a
    jr z, .no_effect

.done
    ld a, HARDCORE_DEFAULT_MOVE_PRIORITY  ; let's continue...
    ret
.no_effect
    ld a, HARDCORE_MAX_DEPRIORITIZED_MOVE ; usable, but no effect
    ret

DEF HARDCORE_NOT_BEST_DAMAGING_MOVE_DEPRIORITY EQU 5
DEF HARDCORE_OHKO_MOVE_BASE_PRIORITY EQU 50
DEF HARDCORE_OHKO_QUICK_ATTACK EQU 40
DEF HARDCORE_OHKO_SWIFT EQU 20
DEF HARDCORE_OHKO_ACCURATE EQU 10

Hardcore_DamageTests:
    xor a
    ld hl, wHardcoreAITurnsToKill
    ld [hl+], a
    ld [hl+], a
    ld [hl+], a
    ld [hl], a
    ld [wHardcoreAIBuffer+1], a ; best "score" for a move with a secondary effect
    dec a
    ld [wHardcoreAIBuffer+0], a ; this will be used for storing the move with the least # of turns to kill

    ; Do damage tests
    ld b, 0
    ld hl, Hardcore_DamageTestForMove
    call Hardcore_CallHLForEachUnprioritizedMove

    ; If they all have 255+ turns to KO, don't continue
    ld a, [wHardcoreAIBuffer+0]
    cp 255
    ret z

    ; Cool, let's set the priority
    ld hl, Hardcore_SetDamagingBaseMovePriority
    call Hardcore_CallHLForEachUnprioritizedMove

    ret

Hardcore_SetDamagingBaseMovePriority:
    ld c, a

    ; skip status moves
    ld a, [wEnemyMovePower]
    and a
    ret z

    ; See if it is the same number of turns to kill
    ld hl, wHardcoreAITurnsToKill
    add hl, bc
    ld a, [hl]
    ld b, a
    ld a, [wHardcoreAIBuffer+0]
    cp b
    jr nz, .deprioritize

    ; Is it one turn to KO? If so, side effects don't matter, so we score with different criteria.
    cp 1
    jr z, .one_turn_to_ko

    ; If not, does its score also match this?
    call Hardcore_GetSameTurnToKOMoveScore
    ld b, a
    ld a, [wHardcoreAIBuffer+1]
    cp b
    jr nz, .deprioritize
    
    ret

.deprioritize
    ld b, 0
    ld hl, wHardcoreAIMovePriority
    add hl, bc
    ld a, [hl]
    sub a, HARDCORE_NOT_BEST_DAMAGING_MOVE_DEPRIORITY
    ld [hl], a
    ret 

.one_turn_to_ko
    ; Base amount
    ld d, HARDCORE_OHKO_MOVE_BASE_PRIORITY

    ; If Quick Attack KOs, prioritize the hell out of it!
    ld a, [wEnemyMoveNum]
    cp QUICK_ATTACK
    ld a, HARDCORE_OHKO_QUICK_ATTACK
    call z, .prioritize

    ; Accurate moves should be prioritized, if possible
    ld a, [wEnemyMoveAccuracy]
    cp 100 percent
    ld a, HARDCORE_OHKO_ACCURATE
    call z, .prioritize

    ; Swift should be prioritized to avoid a Gen 1 miss
    ld a, [wEnemyMoveEffect]
    cp SWIFT_EFFECT
    ld a, HARDCORE_OHKO_SWIFT
    call z, .prioritize

    ; Cool
    ld hl, wHardcoreAIMovePriority
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

Hardcore_DamageTestForMove:
    ld c, a

    ; skip status moves
    ld a, [wEnemyMovePower]
    and a
    ret z

    ; Calculate damage and store the resulting number of turns to KO
    push bc
    call Hardcore_DamageCalc
    pop bc
    ld hl, wHardcoreAITurnsToKill
    add hl, bc
    ld [hl], a

    ; Is it a new record?
    ld b, a
    ld a, [wHardcoreAIBuffer+0]
    cp b
    ld hl, wHardcoreAIBuffer+1
    jr z, .same ; nope, same number of turns to KO
    ret c       ; nope, more turns to KO

    ; Yes!
    ld a, b
    ld [wHardcoreAIBuffer+0], a

    ; Reset score
    xor a
    ld [hl], a

.same
    ; Now score it based on its effect. Is it a high score?
    call Hardcore_GetSameTurnToKOMoveScore
    ld b, a
    ld a, [hl]
    cp b
    ret nc
    ld a, b
    ld [hl], a
    ret


INCLUDE "mod/ai_damage_calc.asm"

; Calls hl, loading each move and having the move slot in A.
;
; BC and DE are passed through into the function.
Hardcore_CallHLForEachUnprioritizedMove:
    push bc
    push de
    ld c, NUM_MOVES
    ld de, wHardcoreAIMovePriority
.loop
    ld a, [de]
    cp a, HARDCORE_MAX_DEPRIORITIZED_MOVE+1
    jr c, .next
    push bc
    push de
    push hl
    call Hardcore_FlipCToA
    push af
    call Hardcore_ReadMoveDataAtIndex
    pop af
    add sp, 6
    pop de
    pop bc
    add sp, -10
    call .call_hl
    pop hl
    pop de
    pop bc
.next
    dec c
    inc de
    jr nz, .loop
    pop de
    pop bc
    ret

.call_hl
    jp hl

Hardcore_CritMoves:
INCLUDE "data/battle/critical_hit_moves.asm"