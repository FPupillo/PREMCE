# block 1
source('scripts/pred_source.R')
str0 = 'wsls'
str1 = 'RL_rw'
str2 = 'RL_fict'
str3 = 'RL_rp'
str4 = 'RL_ph'
str5 = 'hmm0'
str6 = 'hmm0_rp'


f0_hc  = pred_run(str0, study=3,block=1, grp='hc', saveFit = T, test = F)
f1_hc  = pred_run(str1, study=3,block=1, grp='hc', saveFit = T, test = F)
f2_hc  = pred_run(str2, study=3,block=1, grp='hc', saveFit = T, test = F)
f3_hc  = pred_run(str3, study=3,block=1, grp='hc', saveFit = T, test = F)

f4_hc  = pred_run(str4, study=3,block=1, grp='hc', saveFit = T, test = F)
f5_hc  = pred_run(str5, study=3,block=1, grp='hc', saveFit = T, test = F)
f6_hc  = pred_run(str6, study=3,block=1, grp='hc', saveFit = T, test = F)


f0_sz  = pred_run(str0, study=3,block=1, grp='sz', saveFit = T, test = F)
f1_sz  = pred_run(str1, study=3,block=1, grp='sz', saveFit = T, test = F)
f2_sz  = pred_run(str2, study=3,block=1, grp='sz', saveFit = T, test = F)

f3_sz  = pred_run(str3, study=3,block=1, grp='sz', saveFit = T, test = F)
f4_sz  = pred_run(str4, study=3,block=1, grp='sz', saveFit = T, test = F)

f5_sz  = pred_run(str5, study=3,block=1, grp='sz', saveFit = T, test = F)
f6_sz  = pred_run(str6, study=3,block=1, grp='sz', saveFit = T, test = F)


# block 2

source('scripts/pred_source.R')
str0 = 'wsls'
str1 = 'RL_rw'
str2 = 'RL_rw_reset'  # old str 3
str3 = 'RL_fict'      # old str 2
str4 = 'RL_fict_reset'
str5 = 'RL_rp'
str6 = 'RL_rp_reset'
str7 = 'RL_ph'
str8 = 'RL_ph_reset'
str9 = 'hmm0'
str10 = 'hmm0_reset'
str11 = 'hmm0_rp'
str12 = 'hmm0_rp_reset'


f0_hc  = pred_run(str0, study=3,block=2, grp='hc', saveFit = T, test = F)
f1_hc  = pred_run(str1, study=3,block=2, grp='hc', saveFit = T, test = F)
f2_hc  = pred_run(str2, study=3,block=2, grp='hc', saveFit = T, test = F)
f3_hc  = pred_run(str3, study=3,block=2, grp='hc', saveFit = T, test = F)
f4_hc  = pred_run(str4, study=3,block=2, grp='hc', saveFit = T, test = F)
f5_hc  = pred_run(str5, study=3,block=2, grp='hc', saveFit = T, test = F)
f6_hc  = pred_run(str6, study=3,block=2, grp='hc', saveFit = T, test = F)
f7_hc  = pred_run(str7, study=3,block=2, grp='hc', saveFit = T, test = F)

f8_hc  = pred_run(str8, study=3,block=2, grp='hc', saveFit = T, test = F)
f9_hc  = pred_run(str9,  study=3,block=2, grp='hc', saveFit = T, test = F)
f10_hc = pred_run(str10, study=3,block=2, grp='hc', saveFit = T, test = F)
f11_hc = pred_run(str11, study=3,block=2, grp='hc', saveFit = T, test = F)
f12_hc = pred_run(str12, study=3,block=2, grp='hc', saveFit = T, test = F)



f0_sz  = pred_run(str0, study=3,block=2, grp='sz', saveFit = T, test = F)
f1_sz  = pred_run(str1, study=3,block=2, grp='sz', saveFit = T, test = F)
f2_sz  = pred_run(str2, study=3,block=2, grp='sz', saveFit = T, test = F)
f3_sz  = pred_run(str3, study=3,block=2, grp='sz', saveFit = T, test = F)
f4_sz  = pred_run(str4, study=3,block=2, grp='sz', saveFit = T, test = F)
f5_sz  = pred_run(str5, study=3,block=2, grp='sz', saveFit = T, test = F)
f6_sz  = pred_run(str6, study=3,block=2, grp='sz', saveFit = T, test = F)
f7_sz  = pred_run(str7, study=3,block=2, grp='sz', saveFit = T, test = F)

f8_sz  = pred_run(str8, study=3,block=2, grp='sz', saveFit = T, test = F)
f9_sz  = pred_run(str9,  study=3,block=2, grp='sz', saveFit = T, test = F)
f10_sz = pred_run(str10, study=3,block=2, grp='sz', saveFit = T, test = F)
f11_sz = pred_run(str11, study=3,block=2, grp='sz', saveFit = T, test = F)
f12_sz = pred_run(str12, study=3,block=2, grp='sz', saveFit = T, test = F)





