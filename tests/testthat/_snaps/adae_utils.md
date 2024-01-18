# build_adae_summary works

    Code
      build_table(lyt = lyt$lyt, df = lyt$df_out, alt_counts_df = adsl)
    Output
                                                                               A: Drug X    B: Placebo    C: Combination   All Patients
      Adverse Events                                                            (N=134)       (N=134)        (N=132)         (N=400)   
      ---------------------------------------------------------------------------------------------------------------------------------
        Serious AE                                                            104 (77.6%)   101 (75.4%)     99 (75.0%)     304 (76.0%) 
          SAEs with fatal outcome                                             76 (56.7%)    70 (52.2%)      75 (56.8%)     221 (55.2%) 
          Life-threatening SAEs                                                9 (6.7%)      13 (9.7%)      19 (14.4%)      41 (10.2%) 
          SAEs requiring hospitalization                                      24 (17.9%)    28 (20.9%)      30 (22.7%)      82 (20.5%) 
          SAEs resulting in substantial disruption of normal life functions   28 (20.9%)    21 (15.7%)      23 (17.4%)      72 (18.0%) 
          Congenital anomaly or birth defect                                  26 (19.4%)    27 (20.1%)      20 (15.2%)      73 (18.2%) 
          Other SAEs                                                          30 (22.4%)    37 (27.6%)      32 (24.2%)      99 (24.8%) 
        AE leading to permanent discontinuation of study drug                 27 (20.1%)    26 (19.4%)      30 (22.7%)      83 (20.8%) 
        AE leading to dose modification/interruption                          66 (49.3%)    76 (56.7%)      74 (56.1%)     216 (54.0%) 
          AE leading to interruption of study drug                             4 (3.0%)      4 (3.0%)        3 (2.3%)       11 (2.8%)  
          AE leading to reduction of study drug                               46 (34.3%)    46 (34.3%)      51 (38.6%)     143 (35.8%) 
          AE leading to dose delay of study drug                              20 (14.9%)    28 (20.9%)      30 (22.7%)      78 (19.5%) 
          Other AEs                                                           34 (25.4%)    48 (35.8%)      46 (34.8%)     128 (32.0%) 
        Any AE                                                                122 (91.0%)   123 (91.8%)    120 (90.9%)     365 (91.2%) 
          MILD                                                                 7 (5.2%)      9 (6.7%)        4 (3.0%)       20 (5.0%)  
          MODERATE                                                            24 (17.9%)    24 (17.9%)      23 (17.4%)      71 (17.8%) 
          SEVERE                                                              91 (67.9%)    90 (67.2%)      93 (70.5%)     274 (68.5%) 
          Grade 3-5 AE                                                        109 (81.3%)   104 (77.6%)    109 (82.6%)     322 (80.5%) 
          Grade 4/5 AE                                                        91 (67.9%)    90 (67.2%)      93 (70.5%)     274 (68.5%) 

# build_adae_summary works with filter condition

    Code
      build_table(lyt = lyt$lyt, df = lyt$df_out, alt_counts_df = adsl)
    Output
                                                                              A: Drug X    B: Placebo   C: Combination   All Patients
      Adverse Events                                                           (N=134)      (N=134)        (N=132)         (N=400)   
      -------------------------------------------------------------------------------------------------------------------------------
        Serious AE                                                            61 (45.5%)   64 (47.8%)     50 (37.9%)     175 (43.8%) 
          SAEs with fatal outcome                                             41 (30.6%)   45 (33.6%)     42 (31.8%)     128 (32.0%) 
          Life-threatening SAEs                                                5 (3.7%)     7 (5.2%)       9 (6.8%)       21 (5.2%)  
          SAEs requiring hospitalization                                      19 (14.2%)   21 (15.7%)     14 (10.6%)      54 (13.5%) 
          SAEs resulting in substantial disruption of normal life functions   18 (13.4%)   16 (11.9%)     11 (8.3%)       45 (11.2%) 
          Congenital anomaly or birth defect                                  16 (11.9%)   18 (13.4%)     14 (10.6%)      48 (12.0%) 
          Other SAEs                                                          18 (13.4%)   21 (15.7%)     19 (14.4%)      58 (14.5%) 
        AE leading to permanent discontinuation of study drug                     0            0              0               0      
        AE leading to dose modification/interruption                              0            0              0               0      
          AE leading to interruption of study drug                                0            0              0               0      
          AE leading to reduction of study drug                                   0            0              0               0      
          AE leading to dose delay of study drug                                  0            0              0               0      
          Other AEs                                                               0            0              0               0      
        Any AE                                                                72 (53.7%)   77 (57.5%)     65 (49.2%)     214 (53.5%) 
          Grade 3-5 AE                                                        62 (46.3%)   65 (48.5%)     61 (46.2%)     188 (47.0%) 
          Grade 4/5 AE                                                        53 (39.6%)   60 (44.8%)     54 (40.9%)     167 (41.8%) 

# build_adae_by_sev_tox works

    Code
      out_df
    Output
      Primary System Organ Class                                  A: Drug X    B: Placebo    C: Combination   All Patients
        Dictionary-Derived Term                                    (N=134)       (N=134)        (N=132)         (N=400)   
      --------------------------------------------------------------------------------------------------------------------
      Total number of patients with at least one adverse event   122 (91.0%)   123 (91.8%)    120 (90.9%)     365 (91.2%) 
      Total number of adverse events                                 609           622            703             1934    
          cl A                                                                                                            
            dcd A.1.1.1.1                                                                                                 
              MILD                                               50 (37.3%)    45 (33.6%)      63 (47.7%)     158 (39.5%) 
              MODERATE                                                0             0              0               0      
              SEVERE                                                  0             0              0               0      
            dcd A.1.1.1.2                                                                                                 
              MILD                                                    0             0              0               0      
              MODERATE                                           48 (35.8%)    48 (35.8%)      50 (37.9%)     146 (36.5%) 
              SEVERE                                                  0             0              0               0      
          cl B                                                                                                            
            dcd B.1.1.1.1                                                                                                 
              MILD                                                    0             0              0               0      
              MODERATE                                                0             0              0               0      
              SEVERE                                             47 (35.1%)    49 (36.6%)      43 (32.6%)     139 (34.8%) 
            dcd B.2.1.2.1                                                                                                 
              MILD                                                    0             0              0               0      
              MODERATE                                           49 (36.6%)    44 (32.8%)      52 (39.4%)     145 (36.2%) 
              SEVERE                                                  0             0              0               0      
            dcd B.2.2.3.1                                                                                                 
              MILD                                               48 (35.8%)    54 (40.3%)      51 (38.6%)     153 (38.2%) 
              MODERATE                                                0             0              0               0      
              SEVERE                                                  0             0              0               0      
          cl C                                                                                                            
            dcd C.1.1.1.3                                                                                                 
              MILD                                                    0             0              0               0      
              MODERATE                                                0             0              0               0      
              SEVERE                                             43 (32.1%)    46 (34.3%)      43 (32.6%)     132 (33.0%) 
            dcd C.2.1.2.1                                                                                                 
              MILD                                                    0             0              0               0      
              MODERATE                                           35 (26.1%)    48 (35.8%)      55 (41.7%)     138 (34.5%) 
              SEVERE                                                  0             0              0               0      
          cl D                                                                                                            
            dcd D.1.1.1.1                                                                                                 
              MILD                                                    0             0              0               0      
              MODERATE                                                0             0              0               0      
              SEVERE                                             50 (37.3%)    42 (31.3%)      51 (38.6%)     143 (35.8%) 
            dcd D.1.1.4.2                                                                                                 
              MILD                                                    0             0              0               0      
              MODERATE                                           48 (35.8%)    42 (31.3%)      50 (37.9%)     140 (35.0%) 
              SEVERE                                                  0             0              0               0      
            dcd D.2.1.5.3                                                                                                 
              MILD                                               47 (35.1%)    58 (43.3%)      57 (43.2%)     162 (40.5%) 
              MODERATE                                                0             0              0               0      
              SEVERE                                                  0             0              0               0      

# build_adae_by_sev_tox works with AETOXGR

    Code
      out_df
    Output
      Primary System Organ Class                                  A: Drug X    B: Placebo    C: Combination   All Patients
        Dictionary-Derived Term                                    (N=134)       (N=134)        (N=132)         (N=400)   
      --------------------------------------------------------------------------------------------------------------------
      Total number of patients with at least one adverse event   122 (91.0%)   123 (91.8%)    120 (90.9%)     365 (91.2%) 
      Total number of adverse events                                 609           622            703             1934    
          cl A                                                                                                            
            dcd A.1.1.1.1                                                                                                 
              1                                                  50 (37.3%)    45 (33.6%)      63 (47.7%)     158 (39.5%) 
              2                                                       0             0              0               0      
              3                                                       0             0              0               0      
              4                                                       0             0              0               0      
              5                                                       0             0              0               0      
            dcd A.1.1.1.2                                                                                                 
              1                                                       0             0              0               0      
              2                                                  48 (35.8%)    48 (35.8%)      50 (37.9%)     146 (36.5%) 
              3                                                       0             0              0               0      
              4                                                       0             0              0               0      
              5                                                       0             0              0               0      
          cl B                                                                                                            
            dcd B.1.1.1.1                                                                                                 
              1                                                       0             0              0               0      
              2                                                       0             0              0               0      
              3                                                       0             0              0               0      
              4                                                       0             0              0               0      
              5                                                  47 (35.1%)    49 (36.6%)      43 (32.6%)     139 (34.8%) 
            dcd B.2.1.2.1                                                                                                 
              1                                                       0             0              0               0      
              2                                                       0             0              0               0      
              3                                                  49 (36.6%)    44 (32.8%)      52 (39.4%)     145 (36.2%) 
              4                                                       0             0              0               0      
              5                                                       0             0              0               0      
            dcd B.2.2.3.1                                                                                                 
              1                                                  48 (35.8%)    54 (40.3%)      51 (38.6%)     153 (38.2%) 
              2                                                       0             0              0               0      
              3                                                       0             0              0               0      
              4                                                       0             0              0               0      
              5                                                       0             0              0               0      
          cl C                                                                                                            
            dcd C.1.1.1.3                                                                                                 
              1                                                       0             0              0               0      
              2                                                       0             0              0               0      
              3                                                       0             0              0               0      
              4                                                  43 (32.1%)    46 (34.3%)      43 (32.6%)     132 (33.0%) 
              5                                                       0             0              0               0      
            dcd C.2.1.2.1                                                                                                 
              1                                                       0             0              0               0      
              2                                                  35 (26.1%)    48 (35.8%)      55 (41.7%)     138 (34.5%) 
              3                                                       0             0              0               0      
              4                                                       0             0              0               0      
              5                                                       0             0              0               0      
          cl D                                                                                                            
            dcd D.1.1.1.1                                                                                                 
              1                                                       0             0              0               0      
              2                                                       0             0              0               0      
              3                                                       0             0              0               0      
              4                                                       0             0              0               0      
              5                                                  50 (37.3%)    42 (31.3%)      51 (38.6%)     143 (35.8%) 
            dcd D.1.1.4.2                                                                                                 
              1                                                       0             0              0               0      
              2                                                       0             0              0               0      
              3                                                  48 (35.8%)    42 (31.3%)      50 (37.9%)     140 (35.0%) 
              4                                                       0             0              0               0      
              5                                                       0             0              0               0      
            dcd D.2.1.5.3                                                                                                 
              1                                                  47 (35.1%)    58 (43.3%)      57 (43.2%)     162 (40.5%) 
              2                                                       0             0              0               0      
              3                                                       0             0              0               0      
              4                                                       0             0              0               0      
              5                                                       0             0              0               0      

---

    Code
      out_df1
    Output
      Primary System Organ Class                                 A: Drug X    B: Placebo   C: Combination   All Patients
        Dictionary-Derived Term                                   (N=134)      (N=134)        (N=132)         (N=400)   
      ------------------------------------------------------------------------------------------------------------------
      Total number of patients with at least one adverse event   72 (53.7%)   77 (57.5%)     65 (49.2%)     214 (53.5%) 
      Total number of adverse events                                377          419            378             1174    
          cl A                                                                                                          
            dcd A.1.1.1.1                                                                                               
              1                                                  34 (25.4%)   31 (23.1%)     33 (25.0%)      98 (24.5%) 
              2                                                      0            0              0               0      
              3                                                      0            0              0               0      
              4                                                      0            0              0               0      
              5                                                      0            0              0               0      
            dcd A.1.1.1.2                                                                                               
              1                                                      0            0              0               0      
              2                                                  32 (23.9%)   33 (24.6%)     24 (18.2%)      89 (22.2%) 
              3                                                      0            0              0               0      
              4                                                      0            0              0               0      
              5                                                      0            0              0               0      
          cl B                                                                                                          
            dcd B.1.1.1.1                                                                                               
              1                                                      0            0              0               0      
              2                                                      0            0              0               0      
              3                                                      0            0              0               0      
              4                                                      0            0              0               0      
              5                                                  28 (20.9%)   33 (24.6%)     24 (18.2%)      85 (21.2%) 
            dcd B.2.1.2.1                                                                                               
              1                                                      0            0              0               0      
              2                                                      0            0              0               0      
              3                                                  29 (21.6%)   30 (22.4%)     22 (16.7%)      81 (20.2%) 
              4                                                      0            0              0               0      
              5                                                      0            0              0               0      
            dcd B.2.2.3.1                                                                                               
              1                                                  30 (22.4%)   32 (23.9%)     26 (19.7%)      88 (22.0%) 
              2                                                      0            0              0               0      
              3                                                      0            0              0               0      
              4                                                      0            0              0               0      
              5                                                      0            0              0               0      
          cl C                                                                                                          
            dcd C.1.1.1.3                                                                                               
              1                                                      0            0              0               0      
              2                                                      0            0              0               0      
              3                                                      0            0              0               0      
              4                                                  30 (22.4%)   36 (26.9%)     27 (20.5%)      93 (23.2%) 
              5                                                      0            0              0               0      
            dcd C.2.1.2.1                                                                                               
              1                                                      0            0              0               0      
              2                                                  23 (17.2%)   36 (26.9%)     30 (22.7%)      89 (22.2%) 
              3                                                      0            0              0               0      
              4                                                      0            0              0               0      
              5                                                      0            0              0               0      
          cl D                                                                                                          
            dcd D.1.1.1.1                                                                                               
              1                                                      0            0              0               0      
              2                                                      0            0              0               0      
              3                                                      0            0              0               0      
              4                                                      0            0              0               0      
              5                                                  25 (18.7%)   29 (21.6%)     27 (20.5%)      81 (20.2%) 
            dcd D.1.1.4.2                                                                                               
              1                                                      0            0              0               0      
              2                                                      0            0              0               0      
              3                                                  30 (22.4%)   22 (16.4%)     27 (20.5%)      79 (19.8%) 
              4                                                      0            0              0               0      
              5                                                      0            0              0               0      
            dcd D.2.1.5.3                                                                                               
              1                                                  26 (19.4%)   40 (29.9%)     34 (25.8%)     100 (25.0%) 
              2                                                      0            0              0               0      
              3                                                      0            0              0               0      
              4                                                      0            0              0               0      
              5                                                      0            0              0               0      

