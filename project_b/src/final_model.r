modell_enkel_slump <- lmer(
    log_hsp70_norm ~ omr + behandl + (1|omr:lokal),
    data = raw_data
)
