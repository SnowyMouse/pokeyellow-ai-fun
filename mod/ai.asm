; PRIORITY
;
; 0 = disabled
; 1 = enabled but using it would have no effect
; 2+ = priority (higher = better)
; Default = 100

DEF AIMod_DEFAULT_MOVE_PRIORITY EQU 100
DEF AIMod_MAX_DEPRIORITIZED_MOVE EQU 1

INCLUDE "mod/ai_move_types.asm"
INCLUDE "mod/ai_damage_calc.asm"
INCLUDE "mod/ai_redundant_move_effects.asm"
INCLUDE "mod/ai_status_moves.asm"
INCLUDE "mod/ai_trainer_class_priorities.asm"

AIMod_EnemyTrainerChooseMoves::
    ; call AIMod_TestSetup
    call AIMod_PrioritizeMetronome
    jr c, .find_best_move

    ; Damn! We don't have Metronome. Guess we have to do this the hard way.
    call AIMod_PatchRedundantEffects
    call AIMod_DeprioritizeNoEffectMoves
    call AIMod_DamageTests
    call AIMod_PrioritizeStatusMoves
    call AIMod_PrioritizeBindingMoves
    call AIMod_TrainerClassPriorities

.find_best_move
    call AIMod_HighestPriorityMove
    ld d, a
    ld b, 0

.loop_until_we_get_it
    ld hl, wAIModAIMovePriority
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

AIMod_PrioritizeMetronome:
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
    ld hl, wAIModAIMovePriority+3
    ld [hl-], a
    ld [hl-], a
    ld [hl-], a
    ld [hl], a

    call AIMod_FlipCToA
    ld c, a
    ld b, 0
    add hl, bc
    ld [hl], 69

    scf
    ret

AIMod_PrioritizeBindingMoves:
    ld hl, .do_it
    jp AIMod_CallHLForEachViableMove
.do_it
    ld hl, wAIModAIMovePriority
    ld d, 0
    ld e, a
    add hl, de

    ld a, [wEnemyMoveEffect]
    cp TRAPPING_EFFECT
    ret nz

    ld a, [hl]
    add AIMod_NOT_BEST_DAMAGING_MOVE_DEPRIORITY
    ld [hl], a

    ret

AIMod_HighestPriorityMove:
    ld b, 0 ; the highest priority
    ld c, NUM_MOVES
    ld hl, wAIModAIMovePriority
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

AIMod_DeprioritizeNoEffectMoves:
    ld c, NUM_MOVES
    ld hl, wEnemyMonMoves
    ld de, wAIModAIMovePriority

.init_loop
    ld a, [hl+]
    and a
    jr z, .set_move_priority ; set to 0 if NO_MOVE

    ld b, a
    ld a, [wEnemyDisabledMove]
    sub b
    jr z, .set_move_priority ; set to 0 if disabled

    ld a, b
    call AIMod_ReadMoveData

    call AIMod_FlipCToA
    call AIMod_ApplyEffectPatchAtIndex

    push hl
    call AIMod_CheckIfNoEffect
    pop hl

.set_move_priority
    ld [de], a
    inc de

    ; next move
    dec c
    jr nz, .init_loop
    ret


AIMod_ReadMoveData:
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
    
    pop de
    pop hl
    pop bc
    pop af
    ret

AIMod_ReadMoveDataAtIndex:
    push af
    call AIMod_MoveAtIndex
    call AIMod_ReadMoveData
    pop af
    call AIMod_ApplyEffectPatchAtIndex
    ret

AIMod_ApplyEffectPatchAtIndex:
    push bc
    push hl
    ld hl, wAIModAIPatchedEffects
    ld b, 0
    ld c, a
    add hl, bc
    ld a, [hl]
    ld [wEnemyMoveEffect], a
    pop hl
    pop bc
    ret

AIMod_MoveAtIndex:
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

AIMod_FlipCToA:
    ld a, NUM_MOVES
    sub c
    ret

; Compare DE to BC.
;
; If DE == BC, NC/ Z
; If DE >  BC,  C/NZ
; If DE <  BC, NC/NZ
AIMod_CMP16:
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
AIMod_Double16:
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
AIMod_Halve16:
    push af
    srl [hl]
    inc hl
    ld a, [hl]
    rra
    ld [hl-], a
    pop af
    ret
    
AIMod_CopyPlayerSpeedToDividend:
    xor a
    ldh [hDividend+0], a
    ldh [hDividend+1], a
    ld a, [wBattleMonSpeed]
    ldh [hDividend+2], a
    ld a, [wBattleMonSpeed+1]
    ldh [hDividend+3], a
    ret

; Returns with carry if we do NOT outspeed
AIMod_CheckIfHalfOutspeed:
    call AIMod_CopyPlayerSpeedToDividend
    ld a, 2
    ldh [hDivisor], a
    call Divide
    jr AIMod_CheckDividendVersusOurSpeed

; Returns with carry if we do NOT outspeed
AIMod_CheckIfOutspeed:
    call AIMod_CopyPlayerSpeedToDividend
    ; fallthrough

; Returns with carry if we do NOT outspeed what is in hQuotient
AIMod_CheckDividendVersusOurSpeed:
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

AIMod_CheckIfNoEffect:
    ld hl, AIMod_TrashMoveEffects
    call AIMod_LoadedMoveEffectInList
    jr c, .no_effect

    ld a, [wEnemyMovePower]
    and a
    jr z, .check_status_move

    ; 1 base power but no effect? Seems it is a useless move.
    cp 1
    jr nz, .continue
    ld a, [wEnemyMoveEffect]
    and a
    jr z, .no_effect

.continue
    call AIMod_FindOverallTypeEffectivenessOfLoadedMove
    and a
    jr z, .no_effect
    jr .done

.check_status_move
    ld a, [wEnemyMoveEffect]
    and a
    jr z, .no_effect

.done
    ld a, AIMod_DEFAULT_MOVE_PRIORITY  ; let's continue...
    ret
.no_effect
    ld a, AIMod_MAX_DEPRIORITIZED_MOVE ; usable, but no effect
    ret


; Calls hl, loading each move and having the move slot in A.
;
; BC and DE are passed through into the function.
AIMod_CallHLForEachViableMove:
    push bc
    push de
    ld c, NUM_MOVES
    ld de, wAIModAIMovePriority
.loop
    ld a, [de]
    cp a, AIMod_MAX_DEPRIORITIZED_MOVE+1
    jr c, .next
    push bc
    push de
    push hl
    call AIMod_FlipCToA
    push af
    call AIMod_ReadMoveDataAtIndex
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

AIMod_TestSetup:
    ld hl, wEnemyMonMoves
    ld a, BIND
    ld [hl+], a
    ld a, SURF
    ld [hl+], a
    ld a, NO_MOVE
    ld [hl+], a
    ld a, NO_MOVE
    ld [hl+], a

    ret