MACRO aimod_trainer_jmp
	db \1, HIGH(\2), LOW(\2)
ENDM

AIMod_TrainerClassJump:
    aimod_trainer_jmp KOGA, AIMod_TrainerKoga
    aimod_trainer_jmp SCIENTIST, AIMod_TrainerScientist
    aimod_trainer_jmp SUPER_NERD, AIMod_TrainerScientist
    db -1

AIMod_TrainerClassPriorities:
    ld a, [wTrainerClass]
    ld d, a
    ld hl, AIMod_TrainerClassJump
.loop
    ld a, [hl+]
    cp -1
    ret z
    cp d
    ld a, [hl+]
    ld b, a
    ld a, [hl+]
    ld c, a
    jr nz, .loop
    ld h, b
    ld l, c
    jp AIMod_CallHLForEachViableMove

; Koga likes to get Toxic and Sleep up, and then spam Double Team if either are up.
AIMod_TrainerKoga:
    ld hl, wAIModAIMovePriority
    ld d, 0
    ld e, a
    add hl, de

    ; Koga wants to set up Toxic
    ld a, [wEnemyMoveNum]
    cp TOXIC
    jr z, AIMod_DrasticallyPrefer

    ; But failing that, sleep powder is OK
    ld a, [wEnemyMoveEffect]
    cp SLEEP_EFFECT
    jr z, AIMod_HeavilyPrefer

    ; Oh wow, heck yes.
    cp EVASION_UP1_EFFECT
    jr z, .spam_double_team
    cp EVASION_UP2_EFFECT
    jr z, .spam_double_team

    ret

.spam_double_team
    ; Badly poisoned? Spam Double Team!
    ld a, [wPlayerBattleStatus3]
    bit BADLY_POISONED, a
    jr nz, AIMod_DrasticallyPrefer

    ; Asleep? Good time to spam double team.
    ld a, [wBattleMonStatus]
    and SLP_MASK
    ret z

; Scientists have a bias towards supereffective moves
AIMod_TrainerScientist:
    ld hl, wAIModAIMovePriority
    ld d, 0
    ld e, a
    add hl, de

    ld a, [wEnemyMovePower]
    and a
    ret z

    call AIMod_FindOverallTypeEffectivenessOfLoadedMove
    cp 5
    jr nc, AIMod_SlightlyPrioritize

    ret

AIMod_DrasticallyPrefer:
    ld a, [hl]
    add 50
    ld [hl], a
    ret

AIMod_HeavilyPrefer:
    ld a, [hl]
    add 40
    ld [hl], a
    ret

AIMod_SlightlyPrioritize:
    inc [hl]
    ret