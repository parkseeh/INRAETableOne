dfT0T4.analysis <- read.csv("dfTest.csv")

INRAETableOne(
    Period + Groupe_ttt ~
        Age + Sexe +
        Poids + BMI + Systole + Diastole + Freq_car +
        PAL_TEA +
        PAL_TEA5 +
        PAL_TEA6 +
        PAL_TEA8 +
        PAL_NPR +
        PAL_NPS +
        PAL_SC +
        PAL_SCFT +
        SSP_SL +
        SSP_TE +
        SSP_TU +
        SSPR_SL +
        SSPR_TE +
        SSPR_TUE +
        OTS_PSFC +
        OTS_FCE +
        OTS_MCC +
        fluenceV_Sem15 +
        fluenceV_Sem60 +
        fluenceV_lettre15 +
        fluenceV_lettre60 +
        MMSE +
        Wesch_im + Wesch_dif + Wesch_reco +
        MFI20_Fatig_G +
        SF36_general +
        # `16:08` +
        # `18:0` +
        # `18:1(n-9)` +
        # `18:2(n-6)` +
        # `18:3(n-6)` +
        # `18:3(n-3)` +
        # `20:2(n-6)` +
        # `20:3(n-6)` +
        # `20:4(n-6)` +
        # `20:5(n-3)` +
        # `22:4(n-6)` +
        # `22:5(n-6)` +
        # `22:5(n-3)` +
        # `22:6(n-3)` +
        # `26:0` +
        AGS +
        AGMI +
        # `AGPI (n-6)` +
        # `AGPI (n-3)` +
        # `Indice Harris` +
        Cortisol2_lever + Cortisol2_lever30,
    data = dfT0T4.analysis,
    max.x.level = 6,
    show.missing = F
)


INRAETableOne(
    Groupe_ttt ~
        Age + Sexe +
        Poids + BMI + Systole + Diastole + Freq_car +
        PAL_TEA +
        PAL_TEA5 +
        PAL_TEA6 +
        PAL_TEA8 +
        PAL_NPR +
        PAL_NPS +
        PAL_SC +
        PAL_SCFT +
        SSP_SL +
        SSP_TE +
        SSP_TU +
        SSPR_SL +
        SSPR_TE +
        SSPR_TUE +
        OTS_PSFC +
        OTS_FCE +
        OTS_MCC +
        fluenceV_Sem15 +
        fluenceV_Sem60 +
        fluenceV_lettre15 +
        fluenceV_lettre60 +
        MMSE +
        Wesch_im + Wesch_dif + Wesch_reco +
        MFI20_Fatig_G +
        SF36_general +
        # `16:08` +
        # `18:0` +
        # `18:1(n-9)` +
        # `18:2(n-6)` +
        # `18:3(n-6)` +
        # `18:3(n-3)` +
        # `20:2(n-6)` +
        # `20:3(n-6)` +
        # `20:4(n-6)` +
        # `20:5(n-3)` +
        # `22:4(n-6)` +
        # `22:5(n-6)` +
        # `22:5(n-3)` +
        # `22:6(n-3)` +
        # `26:0` +
        AGS +
        AGMI +
        # `AGPI (n-6)` +
        # `AGPI (n-3)` +
        # `Indice Harris` +
        Cortisol2_lever + Cortisol2_lever30,
    data = T4,
)
