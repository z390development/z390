IEFBR14  TITLE 'Dummy program'
***********************************************************************
* Start create : 17-09-2015
* 1st delivery :
* Designer     : AF Kornelis
* Programmer   : AF Kornelis
* Purpose      : For testing
***********************************************************************

         START X'1234'
IEFBR14  RSECT ,                       *
IEFBR14  AMODE 24
IEFBR14  RMODE 24
***********************************************************************
         XR    15,15                   * Set RC=0
         BR    14                      * Return
         END
