         DEFINE CATALOG,NAME=Z390CAT2
*
* FIXED ESDS
*
         DEFINE CLUSTER,                                               X
               NAME=ESDS_F,                                            X
               RECORDSIZE=50,                                          X
               INDEX=NONINDEXED,                                       X
               DATABLOCKSIZE=4K
*
* VARIABLE ESDS
*
         DEFINE CLUSTER,                                               X
               NAME=ESDS_V,                                            X
               RECORDSIZE=(1,26),                                      X
               INDEX=NONINDEXED,                                       X
               DATABLOCKSIZE=4K,                                       X
               DATAFREESPACE=(00,20)
*
         DEFINE END
         END
