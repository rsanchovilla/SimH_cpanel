* $$JOB POWER07,,,F2
// JOB POWER7A  LINK LIBRARY UTILITIES FOR F2
// OPTION CATAL
   INCLUDE IJBSL3           RSERV
// EXEC LNKEDT
   INCLUDE IJBSL4           SSERV
// EXEC LNKEDT
   INCLUDE IJBSL5           CORGZ
// EXEC LNKEDT
/*
/&
// JOB POWER7A  LINK EREP                                               LINK0069
// OPTION CATAL                                                         LINK0071
  INCLUDE IJBECALL                                                      LINK0072
// EXEC LNKEDT                                                          LINK0073
/&                                                                      LINK0074
// JOB POWER7B LINK GROUPI UTILITY TO ASSIGN ALTERNATE TRK 360N-UT-461  LINK0242
// OPTION CATAL                                                         LINK0244
   INCLUDE IJWAD                                                        LINK0245
// EXEC LNKEDT                                                          LINK0246
/&                                                                      LINK0247
// JOB POWER7C  LINK GROUP I UTILITY CARD TO DISK          360N-UT-461  LINK0248
// OPTION CATAL                                                         LINK0250
   INCLUDE IJWCD                                                        LINK0251
   PHASE CDDK5,IJWCDCS2,NOAUTO                                          LINK0252
   INCLUDE IJWLAB                                                       LINK0253
// EXEC LNKEDT                                                          LINK0254
/&                                                                      LINK0255
// JOB POWER7D  LINK GROUP I UTILITY CARD TO PRINTER/PUNCH 360N-UT-461  LINK0256
// OPTION CATAL                                                         LINK0258
   INCLUDE IJWCP                                                        LINK0259
   PHASE CDPP5,IJWCPCS2,NOAUTO                                          LINK0260
   INCLUDE IJWLAB                                                       LINK0261
// EXEC LNKEDT                                                          LINK0262
/&                                                                      LINK0263
// JOB POWER7E  LINK GROUP I UTILITY CLEAR DISK            360N-UT-461  LINK0264
// OPTION CATAL                                                         LINK0266
   INCLUDE IJWCLD                                                       LINK0267
// EXEC LNKEDT                                                          LINK0268
/&                                                                      LINK0269
// JOB POWER7F  LINK GROUP I UTILITY DISK TO CARD          360N-UT-461  LINK0270
// OPTION CATAL                                                         LINK0272
   INCLUDE IJWDC                                                        LINK0273
   PHASE DKCD5,IJWDCCS2,NOAUTO                                          LINK0274
   INCLUDE IJWLAB                                                       LINK0275
// EXEC LNKEDT                                                          LINK0276
/&                                                                      LINK0277
// JOB POWER7G  LINK GROUP I UTILITY DISK TO DISK          360N-UT-461  LINK0278
// OPTION CATAL                                                         LINK0280
   INCLUDE IJWDD                                                        LINK0281
   PHASE DKDK5,IJWDDCS2,NOAUTO                                          LINK0282
   INCLUDE IJWLAB                                                       LINK0283
// EXEC LNKEDT                                                          LINK0284
/&                                                                      LINK0285
// JOB POWER7H  LINK GROUP I UTILITY DISK TO PRINTER       360N-UT-461  LINK0286
// OPTION CATAL                                                         LINK0288
   INCLUDE IJWDP                                                        LINK0289
   PHASE DKPR5,IJWDPCS2,NOAUTO                                          LINK0290
   INCLUDE IJWLAB                                                       LINK0291
// EXEC LNKEDT                                                          LINK0292
/&                                                                      LINK0293
// JOB POWER7I  LINK GROUP I UTILITY INITIALIZE DISK       360N-UT-461  LINK0294
// OPTION CATAL                                                         LINK0296
 INCLUDE IJWID                                                          LINK0297
// EXEC LNKEDT                                                          LINK0298
/&                                                                      LINK0299
// JOB POWER7J  LINK GROUP I UTILITY COPY DISK TO CARD     360N-UT-461  LINK0300
// OPTION CATAL                                                         LINK0302
   INCLUDE IJWKC                                                        LINK0303
// LBLTYP NSD(10)                                                       LINK0304
// EXEC LNKEDT                                                          LINK0305
/&                                                                      LINK0306
// JOB POWER7K  LINK GROUP I UTILITY RESTORE CARD TO DISK  360N-UT-461  LINK0320
// OPTION CATAL                                                         LINK0322
   INCLUDE IJWRC                                                        LINK0323
// LBLTYP NSD(10)                                                       LINK0324
// EXEC LNKEDT                                                          LINK0325
/&                                                                      LINK0326
// JOB POWER7L  LINK GROUP I UTILITY COPY DISK TO DISK     360N-UT-461  LINK0327
// OPTION CATAL                                                         LINK0329
   INCLUDE IJWRD                                                        LINK0330
// LBLTYP NSD(10)                                                       LINK0331
// EXEC LNKEDT                                                          LINK0332
/&                                                                      LINK0333
// JOB POWER7M  LINK GROUP II UTILITY INITIALIZE TAPE       360N-UT-462 LINK0334
// OPTION CATAL                                                         LINK0336
   PHASE INTT,*,NOAUTO                                                  LINK0337
   INCLUDE IJWIT                                                        LINK0338
// EXEC LNKEDT                                                          LINK0339
/&                                                                      LINK0340
// JOB POWER7N  LINK GROUP II UTILITY COPY DISK/DATA CELL TO TAPE       LINK0341
// OPTION CATAL                                                         LINK0343
   INCLUDE IJWKT                                                        LINK0344
// LBLTYP NSD(10)                                                       LINK0345
// EXEC LNKEDT                                                          LINK0346
/&                                                                      LINK0347
// JOB POWER7O  LINK GROUP II UTILITY RESTORE TAPE TO DISK/DATA CELL    LINK0348
// OPTION CATAL                                                         LINK0350
   INCLUDE IJWRT                                                        LINK0351
// LBLTYP NSD(10)                                                       LINK0352
// EXEC LNKEDT                                                          LINK0353
/&                                                                      LINK0354
// JOB POWER7P  LINK GROUP II UTILITY TAPE TO TAPE         360N-UT-462  LINK0355
// OPTION CATAL                                                         LINK0357
   INCLUDE IJWTT                                                        LINK0358
   PHASE TPTP5,IJWTTCS2,NOAUTO                                          LINK0359
   INCLUDE IJWLAB                                                       LINK0360
// LBLTYP TAPE                                                          LINK0361
// EXEC LNKEDT                                                          LINK0362
/&                                                                      LINK0363
// JOB POWER7Q  LINK GROUP II UTILITY TAPE TO PRINTER      360N-UT-462  LINK0364
// OPTION CATAL                                                         LINK0366
   INCLUDE IJWTP                                                        LINK0367
   PHASE TPPR5,IJWTPCS2,NOAUTO                                          LINK0368
   INCLUDE IJWLAB                                                       LINK0369
// LBLTYP TAPE                                                          LINK0370
// EXEC LNKEDT                                                          LINK0371
/&                                                                      LINK0372
// JOB POWER7R  LINK GROUP II UTILITY TAPE TO DISK         360N-UT-462  LINK0373
// OPTION CATAL                                                         LINK0375
   INCLUDE IJWTD                                                        LINK0376
   PHASE TPDK5,IJWTDCS2,NOAUTO                                          LINK0377
   INCLUDE IJWLAB                                                       LINK0378
// LBLTYP TAPE                                                          LINK0379
// EXEC LNKEDT                                                          LINK0380
/&                                                                      LINK0381
// JOB POWER7S  LINK GROUP II UTILITY TAPE COMPARE         360N-UT-462  LINK0391
// OPTION CATAL                                                         LINK0393
   PHASE TPCP,*,NOAUTO                                                  LINK0394
   INCLUDE IJWTCP                                                       LINK0395
   INCLUDE IJJCPD0                                                      LINK0396
   INCLUDE IJWXIT                                                       LINK0397
   INCLUDE IJWTPCP                                                      LINK0398
// LBLTYP TAPE                                                          LINK0399
// EXEC LNKEDT                                                          LINK0400
/&                                                                      LINK0401
// JOB POWER7T  LINK GROUP II UTILITY TAPE TO CARD         360N-UT-462  LINK0402
// OPTION CATAL                                                         LINK0404
   INCLUDE IJWTC                                                        LINK0405
   PHASE TPCD5,IJWTCCS2,NOAUTO                                          LINK0406
   INCLUDE IJWLAB                                                       LINK0407
// LBLTYP TAPE                                                          LINK0408
// EXEC LNKEDT                                                          LINK0409
/&                                                                      LINK0410
// JOB POWER7U  LINK GROUP II UTILITY DISK TO TAPE         360N-UT-462  LINK0411
// OPTION CATAL                                                         LINK0413
   INCLUDE IJWDT                                                        LINK0414
   PHASE DKTP5,IJWDTCS2,NOAUTO                                          LINK0415
   INCLUDE IJWLAB                                                       LINK0416
// LBLTYP TAPE                                                          LINK0417
// EXEC LNKEDT                                                          LINK0418
/&                                                                      LINK0419
// JOB POWER7V  LINK GROUP II UTILITY CARD TO TAPE         360N-UT-462  LINK0429
// OPTION CATAL                                                         LINK0431
   INCLUDE IJWCT                                                        LINK0432
   PHASE CDTP5,IJWCTCS2,NOAUTO                                          LINK0433
   INCLUDE IJWLAB                                                       LINK0434
// LBLTYP TAPE                                                          LINK0435
// EXEC LNKEDT                                                          LINK0436
/&                                                                      LINK0437
// JOB POWER7W  LINK VOCABULARY FILE UTILITY PROGRAM       360N-UT-472  LINK0546
// OPTION CATAL                                                         LINK0548
   INCLUDE IJNVOC                                                       LINK0549
// LBLTYP NSD(01)                                                       LINK0550
// EXEC LNKEDT                                                          LINK0551
/&                                                                      LINK0552
// JOB POWER7X  LINK OLTEP                                 360N-DN-481  LINK0559
// OPTION CATAL                                                         LINK0561
   INCLUDE IJZABOOK                                                     LINK0562
// EXEC LNKEDT                                                          LINK0563
/&                                                                      LINK0564
// JOB POWER7Y  LINK LANGUAGE CONVERSION PROGRAM           360N-CV-489  LINK0627
// OPTION CATAL                                                         LINK0629
  INCLUDE IKLLCP                                                        LINK0630
// EXEC LNKEDT                                                          LINK0631
/&                                                                      LINK0632
* $$EOJ