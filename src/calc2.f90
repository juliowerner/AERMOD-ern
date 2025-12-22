SUBROUTINE EMFACT (QARG)
!***********************************************************************
!                 EMFACT Module of the AMS/EPA Regulatory Model - AERMOD
!
!        PURPOSE: Applies Variable Emission Rate and
!                 Unit Conversion Factors
!
!        PROGRAMMER: Roger Brode, Jeff Wang
!        MODIFIED  : for handling OpenPit Source Type - PES, 7/26/94
!
!        DATE:    March 2, 1992
!
!        MODIFIED:   To include options to vary emissions by
!                    hour-of-day and day-of-week (HRDOW and HRDOW7).
!                    R.W. Brode, U.S. EPA/OAQPS/AQMG,  10/19/2009
!
!        MODIFIED:   To include options to vary emissions by month,
!                    hour-of-day, and day-of-week (MHRDOW and MHRDOW7).
!                    R.W. Brode, MACTEC (f/k/a PES), Inc., 06/22/05
!
!        MODIFIED:   To include an option to vary emissions by season,
!                    hour-of-day, and day-of-week (SHRDOW).
!                    R.W. Brode, PES, 4/10/2000
!
!        INPUTS:  Arrays of Source Parameters
!                 Date and Hour
!                 Meteorological Variables for One Hour
!                 Variable Emission Rate Flags and Factors
!                 Unit Conversion Rate Factors
!
!        OUTPUTS: Adjusted Emission Rate, QTK
!
!        CALLED FROM:   PCALC
!                       VCALC
!                       ACALC
!***********************************************************************

!     Variable Declarations
   USE MAIN1
   IMPLICIT NONE
   CHARACTER MODNAM*12

   DOUBLE PRECISION :: QARG

!     Variable Initializations
   MODNAM = 'EMFACT'

! --- Apply Variable Emission Rate Factor, Based on Value of QFLAG
!     Emission unit factor is applied later since it varies by
!     output type
   IF (QFLAG(ISRC) .EQ. ' ') THEN
      QTK = QARG

!*----   ISCSTM Modification: To handle hourly emissions - jah 11/4/94
   ELSE IF (QFLAG(ISRC) .EQ. 'HOURLY') THEN
      QTK = QARG
!*----
!*#

   ELSE IF (QFLAG(ISRC) .EQ. 'MONTH') THEN
      QTK = QARG * QFACT(IMONTH,ISRC)

   ELSE IF (QFLAG(ISRC) .EQ. 'HROFDY') THEN
      QTK = QARG * QFACT(IHOUR,ISRC)

   ELSE IF (QFLAG(ISRC) .EQ. 'WSPEED') THEN
      QTK = QARG * QFACT(IUCAT,ISRC)

   ELSE IF (QFLAG(ISRC) .EQ. 'SEASON') THEN
      QTK = QARG * QFACT(ISEAS,ISRC)

   ELSE IF (QFLAG(ISRC) .EQ. 'SEASHR') THEN
      QTK = QARG * QFACT((IHOUR+(ISEAS-1)*24),ISRC)

   ELSE IF (QFLAG(ISRC) .EQ. 'HRDOW') THEN
      QTK = QARG * QFACT((IHOUR +&
      &(IDAY_OF_WEEK-1)*24),ISRC)

   ELSE IF (QFLAG(ISRC) .EQ. 'HRDOW7') THEN
      QTK = QARG * QFACT((IHOUR +&
      &(IDAY_OF_WEEK7-1)*24),ISRC)

   ELSE IF (QFLAG(ISRC) .EQ. 'SHRDOW') THEN
      QTK = QARG * QFACT((IHOUR+(ISEAS-1)*24+&
      &(IDAY_OF_WEEK-1)*96),ISRC)

   ELSE IF (QFLAG(ISRC) .EQ. 'SHRDOW7') THEN
      QTK = QARG * QFACT((IHOUR+(ISEAS-1)*24+&
      &(IDAY_OF_WEEK7-1)*96),ISRC)

   ELSE IF (QFLAG(ISRC) .EQ. 'MHRDOW') THEN
      QTK = QARG * QFACT((IHOUR+(IMONTH-1)*24+&
      &(IDAY_OF_WEEK-1)*288),ISRC)

   ELSE IF (QFLAG(ISRC) .EQ. 'MHRDOW7') THEN
      QTK = QARG * QFACT((IHOUR+(IMONTH-1)*24+&
      &(IDAY_OF_WEEK7-1)*288),ISRC)

   END IF

   RETURN
END

SUBROUTINE BGVAL (ISECT,BARG)
!***********************************************************************
!                 BGVAL Module of the AMS/EPA Regulatory Model - AERMOD
!
!        PURPOSE: Processes temporally-varying BACKGRND concentrations
!
!        PROGRAMMER: Roger Brode
!
!        DATE:    February 28, 2011
!
!        INPUTS:  Arrays of Source Parameters
!                 Date and Hour
!                 Meteorological Variables for One Hour
!                 Variable Emission Rate Flags and Factors
!                 Unit Conversion Rate Factors
!
!        OUTPUTS: Adjusted Emission Rate, QTK
!
!        CALLED FROM:   PCALC
!                       VCALC
!                       ACALC
!***********************************************************************

!     Variable Declarations
   USE MAIN1
   IMPLICIT NONE
   CHARACTER MODNAM*12

   INTEGER          :: ISECT

   DOUBLE PRECISION :: BARG

!     Variable Initializations
   MODNAM = 'BGVAL'

! --- Apply temporally-varying background concentration value
!     to BARG variable
   IF (BFLAG(ISECT) .EQ. 'ANNUAL') THEN
      BARG = BACKGRND(1,ISECT)

   ELSE IF (BFLAG(ISECT) .EQ. 'MONTH') THEN
      BARG = BACKGRND(IMONTH,ISECT)

   ELSE IF (BFLAG(ISECT) .EQ. 'HROFDY') THEN
      BARG = BACKGRND(IHOUR,ISECT)

   ELSE IF (BFLAG(ISECT) .EQ. 'WSPEED') THEN
      BARG = BACKGRND(IUCAT,ISECT)

   ELSE IF (BFLAG(ISECT) .EQ. 'SEASON') THEN
      BARG = BACKGRND(ISEAS,ISECT)

   ELSE IF (BFLAG(ISECT) .EQ. 'SEASHR') THEN
      BARG = BACKGRND(IHOUR+(ISEAS-1)*24,ISECT)

   ELSE IF (BFLAG(ISECT) .EQ. 'HRDOW') THEN
      BARG = BACKGRND(IHOUR +&
      &(IDAY_OF_WEEK-1)*24,ISECT)

   ELSE IF (BFLAG(ISECT) .EQ. 'HRDOW7') THEN
      BARG = BACKGRND(IHOUR +&
      &(IDAY_OF_WEEK7-1)*24,ISECT)

   ELSE IF (BFLAG(ISECT) .EQ. 'SHRDOW') THEN
      BARG = BACKGRND(IHOUR+(ISEAS-1)*24+&
      &(IDAY_OF_WEEK-1)*96,ISECT)

   ELSE IF (BFLAG(ISECT) .EQ. 'SHRDOW7') THEN
      BARG = BACKGRND(IHOUR+(ISEAS-1)*24+&
      &(IDAY_OF_WEEK7-1)*96,ISECT)

   ELSE IF (BFLAG(ISECT) .EQ. 'MHRDOW') THEN
      BARG = BACKGRND(IHOUR+(IMONTH-1)*24+&
      &(IDAY_OF_WEEK-1)*288,ISECT)

   ELSE IF (BFLAG(ISECT) .EQ. 'MHRDOW7') THEN
      BARG = BACKGRND(IHOUR+(IMONTH-1)*24+&
      &(IDAY_OF_WEEK7-1)*288,ISECT)

   END IF

! --- Adjust background concentration units to UG/M3 if needed;
!     conversion is based on reference temperature (25C) and
!     pressure (1013.25 mb)
   IF (POLLUT .EQ. 'NO2') THEN
      IF (BackUnits .EQ. 'PPB') THEN
         BARG = BARG / NO2_PPB
      ELSE IF (BackUnits .EQ. 'PPM') THEN
         BARG = BARG / NO2_PPM
      END IF
   ELSE IF (POLLUT .EQ. 'SO2') THEN
      IF (BackUnits .EQ. 'PPB') THEN
         BARG = BARG / SO2_PPB
      ELSE IF (BackUnits .EQ. 'PPM') THEN
         BARG = BARG / SO2_PPM
      END IF
   ELSE IF (POLLUT .EQ. 'CO') THEN
      IF (BackUnits .EQ. 'PPB') THEN
         BARG = BARG * CO_PPB
      ELSE IF (BackUnits .EQ. 'PPM') THEN
         BARG = BARG * CO_PPM
      END IF
   END IF

   RETURN
END

SUBROUTINE OZONVALS (ISECT,O3ARG)
!***********************************************************************
!                 OZONVALS Module of the AMS/EPA Regulatory Model - AERMOD
!
!        PURPOSE: Applies Variable Ozone Concentrations
!
!        PROGRAMMER: Roger Brode
!
!        DATE:    February 28, 2011
!
!        MODIFIED:  Modified to use ISECT instead of IO3SECT for
!                   HRDOW option.
!
!                   Roger Brode, EPA
!                   January XX, 2015
!
!        INPUTS:  Arrays of Ozone Values
!                 Date and Hour
!                 Meteorological Variables for One Hour
!                 Variable Emission Rate Flags and Factors
!                 Unit Conversion Rate Factors
!
!        OUTPUTS: Ozone value in ug/m3
!
!        CALLED FROM:   PCALC
!                       VCALC
!                       ACALC
!***********************************************************************

!     Variable Declarations
   USE MAIN1
   IMPLICIT NONE
   CHARACTER MODNAM*12

   INTEGER          :: ISECT

   DOUBLE PRECISION :: O3ARG

!     Variable Initializations
   MODNAM = 'OZONVALS'

! --- Apply Variable Background O3 values Based on Value of O3FLAG
   IF (O3FLAG(ISECT) .EQ. 'ANNUAL') THEN
      O3ARG = O3VARY(1,ISECT)

   ELSE IF (O3FLAG(ISECT) .EQ. 'MONTH') THEN
      O3ARG = O3VARY(IMONTH,ISECT)

   ELSE IF (O3FLAG(ISECT) .EQ. 'HROFDY') THEN
      O3ARG = O3VARY(IHOUR,ISECT)

   ELSE IF (O3FLAG(ISECT) .EQ. 'WSPEED') THEN
      O3ARG = O3VARY(IUCAT,ISECT)

   ELSE IF (O3FLAG(ISECT) .EQ. 'SEASON') THEN
      O3ARG = O3VARY(ISEAS,ISECT)

   ELSE IF (O3FLAG(ISECT) .EQ. 'SEASHR') THEN
      O3ARG = O3VARY(IHOUR+(ISEAS-1)*24,ISECT)

   ELSE IF (O3FLAG(ISECT) .EQ. 'HRDOW') THEN
      O3ARG = O3VARY(IHOUR +&
      &(IDAY_OF_WEEK-1)*24,ISECT)

   ELSE IF (O3FLAG(ISECT) .EQ. 'HRDOW7') THEN
      O3ARG = O3VARY(IHOUR +&
      &(IDAY_OF_WEEK7-1)*24,ISECT)

   ELSE IF (O3FLAG(ISECT) .EQ. 'SHRDOW') THEN
      O3ARG = O3VARY(IHOUR+(ISEAS-1)*24+&
      &(IDAY_OF_WEEK-1)*96,ISECT)

   ELSE IF (O3FLAG(ISECT) .EQ. 'SHRDOW7') THEN
      O3ARG = O3VARY(IHOUR+(ISEAS-1)*24+&
      &(IDAY_OF_WEEK7-1)*96,ISECT)

   ELSE IF (O3FLAG(ISECT) .EQ. 'MHRDOW') THEN
      O3ARG = O3VARY(IHOUR+(IMONTH-1)*24+&
      &(IDAY_OF_WEEK-1)*288,ISECT)

   ELSE IF (O3FLAG(ISECT) .EQ. 'MHRDOW7') THEN
      O3ARG = O3VARY(IHOUR+(IMONTH-1)*24+&
      &(IDAY_OF_WEEK7-1)*288,ISECT)

   END IF

! --- Convert O3ARG from PPB or PPM to UG/M3, based on reference
!     temperature (25C) and pressure (1013.25 mb)
   IF (OzoneUnits .EQ. 'PPB') THEN
      O3ARG = O3ARG * O3_PPB
   ELSE IF (OzoneUnits .EQ. 'PPM') THEN
      O3ARG = O3ARG * O3_PPM
   ELSE IF (OzoneUnits .EQ. 'UG/M3') THEN
      O3ARG = O3ARG
   ELSE
! ---    Default units are PPB
      O3ARG = O3ARG * O3_PPB
   END IF

   RETURN
END

SUBROUTINE VARYNOXVALS (ISECT,NOXARG)
!***********************************************************************
!                 VARYNOXVALS Module of the AMS/EPA Regulatory Model - AERMOD
!
!        PURPOSE: Applies Variable NOx Concentrations
!
!        PROGRAMMER: CERC
!
!        DATE:     November 2020
!
!        INPUTS:  Arrays of NOx Values
!                 Date and Hour
!                 Meteorological Variables for One Hour
!                 Variable Emission Rate Flags and Factors
!                 Unit Conversion Rate Factors
!
!        OUTPUTS: NOx value in ug/m3
!
!        CALLED FROM:   PCALC
!                       VCALC
!                       ACALC
!***********************************************************************

!     Variable Declarations
   USE MAIN1
   IMPLICIT NONE
   CHARACTER MODNAM*12

   INTEGER          :: ISECT

   DOUBLE PRECISION :: NOXARG

!     Variable Initializations
   MODNAM = 'VARYNOXVALS'

! --- Apply Variable Background NOX values Based on Value of NOXFLAG
   IF (NOXFLAG(ISECT) .EQ. 'ANNUAL') THEN
      NOXARG = NOXVARY(1,ISECT)

   ELSE IF (NOXFLAG(ISECT) .EQ. 'MONTH') THEN
      NOXARG = NOXVARY(IMONTH,ISECT)

   ELSE IF (NOXFLAG(ISECT) .EQ. 'HROFDY') THEN
      NOXARG = NOXVARY(IHOUR,ISECT)

   ELSE IF (NOXFLAG(ISECT) .EQ. 'WSPEED') THEN
      NOXARG = NOXVARY(IUCAT,ISECT)

   ELSE IF (NOXFLAG(ISECT) .EQ. 'SEASON') THEN
      NOXARG = NOXVARY(ISEAS,ISECT)

   ELSE IF (NOXFLAG(ISECT) .EQ. 'SEASHR') THEN
      NOXARG = NOXVARY(IHOUR+(ISEAS-1)*24,ISECT)

   ELSE IF (NOXFLAG(ISECT) .EQ. 'HRDOW') THEN
      NOXARG = NOXVARY(IHOUR +&
      &(IDAY_OF_WEEK-1)*24,ISECT)

   ELSE IF (NOXFLAG(ISECT) .EQ. 'HRDOW7') THEN
      NOXARG = NOXVARY(IHOUR +&
      &(IDAY_OF_WEEK7-1)*24,ISECT)

   ELSE IF (NOXFLAG(ISECT) .EQ. 'SHRDOW') THEN
      NOXARG = NOXVARY(IHOUR+(ISEAS-1)*24+&
      &(IDAY_OF_WEEK-1)*96,ISECT)

   ELSE IF (NOXFLAG(ISECT) .EQ. 'SHRDOW7') THEN
      NOXARG = NOXVARY(IHOUR+(ISEAS-1)*24+&
      &(IDAY_OF_WEEK7-1)*96,ISECT)

   ELSE IF (NOXFLAG(ISECT) .EQ. 'MHRDOW') THEN
      NOXARG = NOXVARY(IHOUR+(IMONTH-1)*24+&
      &(IDAY_OF_WEEK-1)*288,ISECT)

   ELSE IF (NOXFLAG(ISECT) .EQ. 'MHRDOW7') THEN
      NOXARG = NOXVARY(IHOUR+(IMONTH-1)*24+&
      &(IDAY_OF_WEEK7-1)*288,ISECT)

   END IF

! --- Convert NOXARG from PPB or PPM to UG/M3, based on reference
!     temperature (25C) and pressure (1013.25 mb)
! --- using NO2 factors (NOx expressed as 'NOx as NO2')
   IF (NOxUnits .EQ. 'PPB') THEN
      NOXARG = NOXARG / NO2_PPB
   ELSE IF (NOxUnits .EQ. 'PPM') THEN
      NOXARG = NOXARG / NO2_PPM
   ELSE IF (NOxUnits .EQ. 'UG/M3') THEN
      NOXARG = NOXARG
   ELSE
! ---    Default units are PPB
      NOXARG = NOXARG / NO2_PPB
   END IF

   RETURN
END

SUBROUTINE DISTF
!***********************************************************************
!                 DISTF Module of the AMS/EPA Regulatory Model - AERMOD
!
!        PURPOSE: Calculates Distance to Final Plume Rise
!
!        PROGRAMMER: Roger Brode, Jeff Wang
!
!        DATE:    March 2, 1992
!
!        INPUTS:  Arrays of Source Parameters
!                 Buoyancy and Momentum Fluxes
!                 Meteorological Variables for One Hour
!                 Wind Speed Adjusted to Stack Height
!
!        OUTPUTS: Distance to Final Plume Rise, XMAX (m), and Final
!                 Rise (DHFAER)
!
!        CALLED FROM:   PCALC
!***********************************************************************

!     Variable Declarations
   USE MAIN1
   IMPLICIT NONE
   CHARACTER MODNAM*12
   DOUBLE PRECISION :: XLN, DELHNN, XMAXN
   DOUBLE PRECISION :: DHFSAV       ! save original DHFAER for URBSTAB cases
   DOUBLE PRECISION :: BVZI2

!     Variable Initializations
   MODNAM = 'DISTF'

   IF( STABLE  .or.  (UNSTAB .and. (HS .GE. ZI) ) )THEN

!        Compute the distance to final rise, XMAX;
!        The negative sign appears on the FB term to insure that the
!           resulting angle is between 0 and PI (i.e., positive)
      XMAX   = UP * DATAN2( FM*BVPRIM, -FB ) / BVPRIM

!        Compute the final stable plume rise, DHF, from Eqn. 3-113 of MFD
      DHFAER = 2.66D0 * (FB/(BVF*BVF*UP))**THIRD
      XLN = FB/(UP*USTAR*USTAR)
      DELHNN = 1.2D0*XLN**0.6D0 * (HSP + 1.2D0*XLN)**0.4D0
      DHFAER = MIN( DHFAER, DELHNN )

!        Compute Neutral/Unstable Final Rise
      IF(FB .LE. 0.0D0) THEN
         XMAXN = 4.D0*DS*(VS+3.D0*UP)*(VS+3.D0*UP)/(VS*UP)
         DHFAER   = MIN( DHFAER, 3.0D0 * DS * VS / UP )
      ELSE
         IF(FB .GE. 55.0D0) THEN
            XMAXN = 119.0D0 * FB**0.4D0
         ELSE
            XMAXN = 49.0D0 * FB**0.625D0
         END IF
         CALL CBLPRD(XMAXN)
         DHFAER = MIN( DHFAER, DHP1 )
      END IF

!        Apply calm, stable rise limit
      DHFAER = MIN( DHFAER, 4.0D0 * FB**0.25D0 / (BVF*BVF)**0.375D0 )

! ---    Save "original" DHFAER for URBSTAB cases
      DHFSAV = DHFAER

!        For urban stable boundary layers, limit plume rise to 1.25*ZI - HSP
      IF (URBSTAB) THEN
! ---       "New fomulation" for v15181 to account for "partial penetration" of plume
!           above the urban "stable" mixing height, similar to approach used for
!           convective conditions

! JAT 3/8/22: D009_TALL_STACK_HT_EQUAL_MIX_HT_JAT :
! Modify logic to only perform the calculations below when the stack
! is below the mixing height.  For cases where stack height is at or above mixing
! height, use the DHFAER as calculated above.  This is done to avoid NaN's for PPF
! and HEDHH. Also, HEDHH calculations are not appropriate for stacks at or above mixing
! height.  HEDHH is not calculated in the subroutine PENFCT and not used in DELTAH
! for stacks at or above mixing height
!            IF( HSP+DHFAER .GE. ZI )THEN
         IF( HSP+DHFAER .GE. ZI .and. HSP .LT. ZI )THEN
! ---          Stack height + plume rise is .GE. ZI; use pseudo-penetrated plume
!              approach for URBAN SBL cases

!              Compute the square of the Brunt-Vaisala frequency at ZI, BVZI2

            BVZI2 = (G / PTATZI) * 0.005D0

!              Compute the value of PsubS, Eq. 26b in the 2nd reference
            PSUBS = FB / ( UP * BVZI2 * (ZI-HSP)*(ZI-HSP)*(ZI-HSP) )

!              Compute the ratio of delta(Hsub_e)/delta(Hsub_h), HEDHH
!              (Eq. 25 in the 2nd ref.
!              NOTE: 17.576 = (2.6)**3 and 0.296296 is (2/3)**3
            HEDHH = (17.576D0 * PSUBS + 0.296296D0) ** THIRD

!              Check the value of HEDHH and compute the plume penetration, P
            IF( HEDHH .LT. (2.0D0*THIRD) )THEN
               PPF = 0.0D0

            ELSE IF( HEDHH .GT. 2.0D0 )THEN
               PPF = 1.0D0

            ELSE
               PPF = 1.5D0 - (1.0D0 / HEDHH)

            END IF

! ---          Include calculation of penetrated plume rise and height
            IF( PPF .GT. 0.0D0 )THEN

!                 Compute the plume height for the penetrated source
!                 (See Eq. 8 in the reference for Source 3)
               IF (PPF .EQ. 1.0D0) THEN
                  DHFAER = HEDHH * (ZI-HSP)
               ELSE
                  DHFAER = 0.75D0 * (ZI-HSP) * HEDHH + 0.5D0*(ZI-HSP)
               END IF

            ELSE
! ---             Use "original" DHFAER value
               DHFAER = DHFSAV

            END IF

         END IF
      END IF

   ELSE
!        Unstable plume

      IF( FB .LE. 0.0D0 )THEN
         XMAX = 4.D0*DS*(VS+3.0D0*UP)*(VS+3.0D0*UP)/(VS*UP)
         DHFAER = 3.0D0 * DS * VS / UP
      ELSE
         IF (FB .GE. 55.0D0) THEN
            XMAX = 119.D0 * FB**0.4D0
         ELSE
            XMAX = 49.D0 * FB**0.625D0
         END IF
         CALL CBLPRD(XMAX)
         DHFAER = DHP1
      END IF

   END IF

   RETURN
END

SUBROUTINE WAKFLG
!***********************************************************************
!                 WAKFLG Module of the AMS/EPA Regulatory Model - AERMOD
! ----------------------------------------------------------------------
! ---    ISC-PRIME     Version 1.0    Level 970812              Modified
! ---        D. Strimaitis
! ---        Earth Tech, Inc.
!            Prepared for EPRI under contract WO3527-01
! ----------------------------------------------------------------------
!
!        PURPOSE: To Set Wake Flags for Building Downwash Algorithms
!
!        PROGRAMMER: Roger Brode, Jeff Wang
!
!        DATE:    March 2, 1992
!
!        MODIFIED:   To remove check on stack height >/= EPA formula
!                    height as a criterion for ignoring building
!                    downwash effects.  A one-time warning is issued
!                    for each source with HS >/= EPA formula height.
!                    R.W. Brode, U.S. EPA/OAQPS/AQMG, 02/28/2011
!
!        INPUTS:  Building Dimensions
!                 Source Parameters
!                 Meteorological Variables for One Hour
!
!        OUTPUTS: Logical Flags for Wake Switch, WAKE;
!                 And Building
!
!        CALLED FROM:   PCALC
!***********************************************************************

!     Variable Declarations
   USE MAIN1
   IMPLICIT NONE
   CHARACTER MODNAM*12

!     Variable Initializations
   MODNAM = 'WAKFLG'

!     Select Building Dimensions for This Sector
   IF (IFVSEC .LE. NSEC) THEN
      DSBH = ADSBH(IFVSEC,ISRC)
      DSBW = ADSBW(IFVSEC,ISRC)

! --- PRIME ---------------------------------
      DSBL = ADSBL(IFVSEC,ISRC)
      XADJ = ADSXADJ(IFVSEC,ISRC)
      YADJ = ADSYADJ(IFVSEC,ISRC)
      B_SUBS = MIN( DSBH, DSBW )
      B_SUBL = MAX( DSBH, DSBW )
      B_SUBL = MIN( B_SUBL, 8.0D0*B_SUBS )
      RSCALE = B_SUBS**(2.0D0*THIRD) * B_SUBL**THIRD
! -------------------------------------------

   END IF

!     Set Initial Wake Switches Based on Building Dimensions
   IF (DSBH .LE. 1.0D-5 .or. DSBW .LE. 1.0D-5) THEN
! ---    No building inputs defined for this source, set WAKE = .F.
      WAKE   = .FALSE.
   ELSE
! ---    Building inputs defined for this source, set WAKE = .T.;
!        PRIME downwash algorithm will determine whether plume is
!        subject to downwash influences based on source/building
!        characteristics and meteorology
      WAKE   = .TRUE.
! ---    Check for stack height greater the EPA formula height;
!        issue warning message once for each source that meets
!        this criterion

!CRT 3/5/2021 D067: Delete GEP stack height warning - causes confusion
!CRT This warning was added in v.11059 when WAKEFLG was disabled to
!CRT inform user that downwash would be applied even though stack height
!CRT was at or above GEP calculated for the wind direction of the current
!CRT hour based on sector specific building dimensions.  However, message
!CRT is confusing as it implies stack height is >= EPA formula GEP based
!CRT on building ht and max projected width, independent of wind direction.

!         IF (HS .GE. (DSBH + 1.5D0*MIN(DSBH,DSBW))) THEN
!            IF (.NOT. L_WakeMessage(ISRC)) THEN
!C              Write Warning Message:  Stack height > EPA formula ht
!               CALL ERRHDL(PATH,MODNAM,'W','305',SRCID(ISRC))
!               L_WakeMessage(ISRC) = .TRUE.
!            END IF
!         END IF
   END IF

! --- PRIME ----------------------------------------------------

   RETURN
END

SUBROUTINE XYDIST(INDX)
!***********************************************************************
!                 XYDIST Module of the AMS/EPA Regulatory Model - AERMOD
!
!        PURPOSE: Sets Receptor Variables and Calculates Downwind (X)
!                 and Crosswind (Y) Distances,
!                 and Radial Distance from Source to Receptor (DISTR)
!
!        PROGRAMMER: Roger Brode, Jeff Wang
!
!        DATE:    March 2, 1992
!
!        MODIFIED BY R.W. Brode, PES, Inc. to use calling argument to
!                 specify array index, so that routine can be used by
!                 both the regular ISCST3 routines and the routines of
!                 the EVENT processor (ISCEV3). - 12/29/97
!
!        INPUTS:  Source Location
!                 Arrays of Receptor Locations
!                 SIN and COS of Wind Direction FROM Which Wind
!                 is Blowing, WDSIN and WDCOS
!
!        OUTPUTS: Values of X, Y, and DISTR (m)
!
!        CALLED FROM:   PCALC
!                       VCALC
!***********************************************************************

!     Variable Declarations
   USE MAIN1
   IMPLICIT NONE
   CHARACTER MODNAM*12

   INTEGER :: INDX

!     Variable Initializations
   MODNAM = 'XYDIST'

!     Set Receptor Coordinates, Terrain Elevation and Flagpole Heights
   XR = AXR(INDX)
   YR = AYR(INDX)
   ZELEV = AZELEV(INDX)
   ZHILL = AZHILL(INDX)
   ZFLAG = AZFLAG(INDX)

!     Calculate Downwind (X) and Crosswind (Y) Distances
   X = -((XR-XS)*WDSIN + (YR-YS)*WDCOS)
   Y =   (XR-XS)*WDCOS - (YR-YS)*WDSIN

!     Calculate Source-Receptor (Radial) Distance, DISTR
   DISTR = DSQRT(X*X + Y*Y)

!     Calculate height of receptor above stack base, ZRT
   IF (L_FLATSRC(ISRC)) THEN
      ZRT = ZFLAG
   ELSE
      ZRT = ZELEV - ZS + ZFLAG
   END IF

!     Check for SCREENing Mode and Set X,Y to Force Centerline Calc.
   IF (SCREEN) THEN
      X = DISTR
      Y = 0.0D0
   END IF

   RETURN
END

SUBROUTINE FTERM
!***********************************************************************
!             FTERM Module of the AMS/EPA Regulatory Model - AERMOD
!
!        PURPOSE: To Calculate the Value of 'F' Which is Related to the
!                 Fraction of Plume Material Below HCrit
!
!        PROGRAMMER: Roger Brode, Jayant Hardikar
!
!        DATE:    September 30, 1993
!
!        INPUTS:  PHEE - Fraction of Plume Material Below HCrit
!
!        OUTPUTS: FOPT  - The 'F' Term
!
!        CALLED FROM:   PCALC
!***********************************************************************

!     Variable Declarations
   USE MAIN1
   IMPLICIT NONE
   CHARACTER MODNAM*12

!     Variable Initializations
   MODNAM = 'FTERM'

   FOPT = 0.5D0 * (1.0D0 + PHEE)

   RETURN
END


SUBROUTINE FYPLM(SYARG,FYOUT)
!***********************************************************************
!             FYPLM Module of the AMS/EPA Regulatory Model - AERMOD
!
!        PURPOSE: To Calculate the Value of the Horizontal Gaussian
!                 Distribution Function for the Coherent Plume
!
!        PROGRAMMER: Roger Brode
!
!        DATE:    September 30, 1993
!
!        INPUTS:
!                 SY   - Sigma-Y
!                 Y    - The Crosswind Distance of the Receptor from
!                        the Plume
!
!        OUTPUTS: 'FSUBY' Term
!
!        CALLED FROM:   AERCALC, PRMCALC, VOLCALC, ACALC
!***********************************************************************

!     Variable Declarations
   USE MAIN1
   IMPLICIT NONE
   CHARACTER MODNAM*12
   DOUBLE PRECISION :: SYARG, EXPARG, FYOUT

!     Variable Initializations
   MODNAM = 'FYPLM'

   EXPARG = -(Y*Y / (2.0D0*SYARG*SYARG))
!
!     Add meander component
!
   IF (EXPARG .GT. EXPLIM) THEN
!        Calculate lateral term for Gaussian plume
      FYOUT  = DEXP(EXPARG)/(SRT2PI*SYARG)
   ELSE
      FYOUT  = 0.0D0
   END IF

   RETURN
END


SUBROUTINE FYPAN(FYOUT)
!***********************************************************************
!             FYPAN Module of the AMS/EPA Regulatory Model - AERMOD
!
!        PURPOSE: To Calculate the Value of the Horizontal Gaussian
!                 Distribution Function for the Random ("Pancake")
!                 Component
!
!        PROGRAMMER: Roger Brode
!
!        DATE:    September 30, 1993
!
!        INPUTS:
!                 DISTR - Real - Radial distance of receptor from the
!                                source (m)
!
!        OUTPUTS: 'FSUBY' Term
!
!        CALLED FROM:   PCALC
!***********************************************************************

!     Variable Declarations
   USE MAIN1
   IMPLICIT NONE
   CHARACTER MODNAM*12
   DOUBLE PRECISION :: FYOUT

!     Variable Initializations
   MODNAM = 'FYPAN'

   FYOUT = 1.0D0/(TWOPI * DISTR)

   RETURN
END


SUBROUTINE MEANDR( UEF, SVEF, FRAN )
!***********************************************************************
!             MEANDR Module of the AMS/EPA Regulatory Model - AERMOD
!
!        PURPOSE: Calculates fraction of random plume for lateral
!                 meander
!
!        PROGRAMMER: Roger Brode, PES, Inc.
!
!        DATE:       June 26, 2001
!
!        MODIFICATIONS:
!
!                    To use UEF instead of UMEAN in denominator of TTRAV
!                    term in calculation of SIGRAN.
!                    R.W. Brode, PES, Inc.  8/28/01
!
!                    To use radial distance (DISTR) in calculation of
!                    TTRAV instead of downwind distance (X).
!                    R.W. Brode, PES, Inc.  6/19/01
!
!        INPUTS:  Effective wind speed, UEF, in m/s
!                 Effective wind sigma_V, SVEF, in m/s
!
!        OUTPUTS: Fraction of plume in random lateral distribution, FRAN
!
!        CALLED FROM:   AERCALC
!                       PRMCALC
!                       VCALC
!
!        CALLS:         None
!***********************************************************************

!     Variable Declarations
   USE MAIN1
   IMPLICIT NONE
   CHARACTER MODNAM*12
!RCO 9/28/2020 D061 User BIGT
!RCO BIGT is now defined as user input for LOW_WIND
!RCO default is set in coset. (CRT: Now declared in modules.f)
!      DOUBLE PRECISION, PARAMETER :: BIGT = 24.0D0
   DOUBLE PRECISION :: UEF, SVEF, FRAN, UMEAN, TOTKIN, TRAN, TTRAV,&
   &SIGRAN
   DOUBLE PRECISION :: SQRTARG

!     Variable Initializations
   MODNAM = 'MEANDR'

!     Compute meander fraction of horizontal distribution function
!     from Venky's memo of 6/24/98.

! --- Calculate time scale (s) for random dispersion,
!     based value of BIGT (default = 24.0 hrs)
   TRAN = BIGT * 3600.0D0

!     Remove the SVeff component from UEF
!
   SQRTARG = UEF*UEF - 2.0D0*SVEF*SVEF
   IF (SQRTARG .GE. 0.01D0) THEN
      UMEAN = DSQRT( SQRTARG )
   ELSE
      UMEAN = 0.1D0
   END IF
   TOTKIN = UEF * UEF
   TTRAV  = DISTR/UEF
   SIGRAN = 2.0D0*SVEF*SVEF + UMEAN*UMEAN*(1.0D0-DEXP(-TTRAV/TRAN))
   FRAN   = SIGRAN/TOTKIN

!CRT  4/12/2022 D131 FRAN Alpha PBAL
   IF (L_PBal) FRAN = DSQRT(FRAN)

! --- Issue informational messages for FRAN > FRANMAX
!CRT  5/1/2018: Add logical variable to condition so message is written
!CRT  only if FRANMAX is specified by user under LOW_WIND option
   IF( L_UserFRANmax .and. (FRAN .GT. FRANMAX) )THEN
      WRITE( DUMMY,'(I2.2,1X,I2.2,1X,I2.2,1X,I3)') IMONTH, IDAY,&
      &IHOUR,&
      &MIN(ISRC,999)
      CALL ERRHDL(PATH,MODNAM,'I','494',DUMMY)
   ENDIF

! --- Wood 3/18/2022 D127 Issue informational messages for FRAN < FRANMIN
   IF( L_UserFRANmin .and. (FRAN .LT. FRANMIN) )THEN
      WRITE( DUMMY,'(I2.2,1X,I2.2,1X,I2.2,1X,I3)') IMONTH, IDAY,&
      &IHOUR,&
      &MIN(ISRC,999)
      CALL ERRHDL(PATH,MODNAM,'I','424',DUMMY)
   ENDIF

!     Reset FRAN to min. of computed FRAN and FRANMAX (could be default
!     value or user-defined value)
!CRT  5/1/2018: moved outside of conditional statement above
   FRAN = MIN( FRAN, FRANMAX )

!     3/18/22 Wood D127 - added FRANMIN keyword to LOW_WIND option
   FRAN = MAX( FRAN, FRANMIN )

   IF (DEBUG) THEN
      WRITE(DBGUNT,*)
      WRITE(DBGUNT,*) 'SVEF, UEF, UMEAN:'
      WRITE(DBGUNT,*)  SVEF, UEF, UMEAN
      WRITE(DBGUNT,*)
      WRITE(DBGUNT,*) 'DISTR, TTRAV, TRAN:'
      WRITE(DBGUNT,*)  DISTR, TTRAV, TRAN
      WRITE(DBGUNT,*)
      WRITE(DBGUNT,*) 'TOTKIN, SIGRAN, FRAN:'
      WRITE(DBGUNT,*)  TOTKIN, SIGRAN, FRAN
      WRITE(DBGUNT,*) ' '
   END IF

   RETURN
END

SUBROUTINE CRITDS (HEARG)
!***********************************************************************
!             CRITDS Module of the AMS/EPA Regulatory Model - AERMOD
!
!        PURPOSE: Compute the critical dividing streamline for each receptor
!                 (this routine is source dependent)
!
!        PROGRAMMER: Jim Paumier and Roger Brode, PES, Inc.
!
!        DATE:    September 30, 1993
!
!        MODIFICATIONS:
!
!        MODIFIED:    Allowed RLINE/RLINEXT sources to use the RL_GRIDWS
!                     Wood, 7/5/22
!
!                    To redefine the upper limit on the integration for
!                    HCRIT as the minimum of the plume height above
!                    the receptor height, and the height scale input
!                    from AERMAP.
!                    R.W. Brode, PES, Inc.  9/4/01
!
!        INPUTS:  Plume Height, HEARG
!                 Gridded profile heights, GRIDHT
!                                 wind speed, GRIDWS
!                                 potential temperature, GRIDPT
!                                 potential temperature gradient, GRIDTG
!                 Hill height scale, ZHILL, input from AERMAP
!
!        OUTPUTS: Critical dividing streamline for the receptor
!
!        Assumptions:
!
!        References:  "User's Guide to the Complex Terrain Dispersion
!                      Model Plus ..."
!                     "Approach for Determining Hill Heights for AERMOD",
!                      A. Cimorelli, 6/25/93
!
!        CALLED FROM:
!***********************************************************************

!     Variable Declarations
   USE MAIN1
   USE RLINE_DATA
   IMPLICIT NONE
   CHARACTER MODNAM*12
   INTEGER :: NDX4HC
   DOUBLE PRECISION :: HHILL, UATHH, PTATHH, TGATHH, PTHC, TGHC,&
   &TOPHT, WSTOP
   DOUBLE PRECISION :: HEARG

!     DEFINE LOCAL VARIABLES
   DOUBLE PRECISION :: A, AC4, B, B2, C, DETER, HBOT, HTOP,&
   &LS(MXGLVL), RS(MXGLVL), XN2(MXGLVL), N2, DWS,&
   &DWS2, ZMID
   INTEGER IB, IT, NL, NLEV

!     Variable Initializations
   MODNAM = 'CRITDS'

!CRT  Initialize ZMID, 12/27/2017
   ZMID =  0.0D0
   TOPHT = 0.0D0
   TGHC =  0.0D0
   PTHC =  0.0D0


!     Compute the upper limit on the integration for HCRIT, called HHILL.
!     Set as minimum of plume height above receptor height, and height
!     scale above the receptor height (ZHILL) input from AERMAP.

   IF (L_FLATSRC(ISRC)) THEN
! ---    This source is being modeled with FLAT terrain
!        Set HHILL and HCRIT to 0.0 and return
      HHILL = 0.0D0
      HCRIT = 0.0D0
      RETURN
   ELSE
! ---    Calculate 'effective' hill height for HCRIT calculation
      HHILL = MIN( ZHILL - ZS, ZELEV - ZS + HEARG )
   END IF

   IF ( STABLE .and. ELEV .and. HHILL .GT. 0.0D0 ) THEN
!        The hill elevation is above the source elevation and we are
!        using elevated terrain;

!        Determine the index of the gridded height immediately below
!        the hill height and determine the number of levels to use

      CALL LOCATE ( GRIDHT, 1, MXGLVL, HHILL, NDX4HC )
      NLEV = NDX4HC + 1

!        Compute values at hill height

!........Added IF Statments for RLINE and RLINEXT source to use RL_GRIDWS - WOOD 6-28-2022
!         CALL GINTRP ( GRIDHT(NDX4HC), GRIDWS(NDX4HC),
!     &                  GRIDHT(NDX4HC+1), GRIDWS(NDX4HC+1),
!     &                  HHILL, UATHH )

      IF ((SRCTYP(ISRC) .EQ. 'RLINE') .or.&
      &(SRCTYP(ISRC) .EQ. 'RLINEXT')) THEN
         CALL GINTRP ( GRIDHT(NDX4HC), RL_GRIDWS(NDX4HC,I_ALPHA),&
         &GRIDHT(NDX4HC+1), RL_GRIDWS(NDX4HC+1,I_ALPHA),&
         &HHILL, UATHH )
      ELSE
         CALL GINTRP ( GRIDHT(NDX4HC), GRIDWS(NDX4HC),&
         &GRIDHT(NDX4HC+1), GRIDWS(NDX4HC+1),&
         &HHILL, UATHH )
      END IF !End IF for RL_GRIDWS windspeed - Wood 7/5/2022

      CALL GINTRP ( GRIDHT(NDX4HC), GRIDPT(NDX4HC),&
      &GRIDHT(NDX4HC+1), GRIDPT(NDX4HC+1),&
      &HHILL, PTATHH )
      CALL GINTRP ( GRIDHT(NDX4HC), GRIDTG(NDX4HC),&
      &GRIDHT(NDX4HC+1), GRIDTG(NDX4HC+1),&
      &HHILL, TGATHH )

!        Compute the left side of Eq. 32 in the CTDMPLUS User's Guide
!        for all gridded levels; the actual number of levels to use is
!        determined later in the routine

      DO NL = 1, NLEV - 1
! - Wood 7/5/2022       LS(NL) = 0.5D0 * GRIDWS(NL) * GRIDWS(NL)

! begin - Use RLINE windspeeds if RLINE sourcetype - Wood 7/5/2022
         IF ((SRCTYP(ISRC) .EQ. 'RLINE') .or.&
         &(SRCTYP(ISRC) .EQ. 'RLINEXT')) THEN
            LS(NL) = 0.5D0 * RL_GRIDWS(NL,I_ALPHA) *&
            &RL_GRIDWS(NL,I_ALPHA)
         ELSE
            LS(NL) = 0.5D0 * GRIDWS(NL) * GRIDWS(NL)
         END IF
! end -  Use RLINE windspeeds if RLINE sourcetype  - Wood 7/5/2022
      END DO

!        Define LS at the hill top
      LS(NLEV) = 0.5D0 * UATHH * UATHH

!        Compute the right-hand side (RHS) of Eq. 32 in the CTDMPLUS
!        User's Guide using the midpoint of each layer

      RS(NLEV) = 0.0D0
      DO NL = NLEV-1, 1, -1

         IF( NL .LT. NLEV-1 )THEN
            ZMID = 0.5D0 * ( GRIDHT(NL+1) + GRIDHT(NL) )
            PTHC = 0.5D0 * ( GRIDPT(NL+1) + GRIDPT(NL) )
            TGHC = 0.5D0 * ( GRIDTG(NL+1) + GRIDTG(NL) )
            TOPHT = GRIDHT(NL+1)

         ELSE IF( NL .EQ. NLEV-1 )THEN
            ZMID = 0.5D0 * ( HHILL + GRIDHT(NL) )
            PTHC = 0.5D0 * ( PTATHH + GRIDPT(NL) )
            TGHC = 0.5D0 * ( TGATHH + GRIDTG(NL) )
            TOPHT = HHILL

         END IF

!           Compute the Brunt-Vaisala frequency and then the RHS of Eq. 32

         XN2(NL) = (G / PTHC) * TGHC
         RS(NL)  = RS(NL+1) + XN2(NL) * ( (HHILL - ZMID) *&
         &( TOPHT - GRIDHT(NL) ) )

      END DO

!        Find the layer(s) where Eq. 32 is satisfied; the lowest layer
!        is saved for the computation

      IT = 1
      DO NL = NLEV, 1, -1
         IF( LS(NL) .GE. RS(NL) )THEN
            IT = NL
         END IF
      END DO

!        Interpolate to get the critical dividing streamline, HC,
!        assuming a linear change of variables within a layer;
!        the result is a quadratic equation for HC
!
!        DWS is wind speed shear; N2 is the Brunt-Vaisala frequency.
!
      IF( IT .GT. 1 )THEN

         IF( IT .EQ. NLEV )THEN
            WSTOP = UATHH
            HTOP  = HHILL
         ELSE
! Wood 7/5/2022 WSTOP = GRIDWS(IT)
! Wood 7/5/2022 HTOP  = GRIDHT(IT)

! begin - Use RLINE wind speed if RLINE or RLINEXT source
            IF ((SRCTYP(ISRC) .EQ. 'RLINE') .or.&
            &(SRCTYP(ISRC) .EQ. 'RLINEXT')) THEN
               WSTOP = RL_GRIDWS(IT,I_ALPHA)
               HTOP  = GRIDHT(IT)
            ELSE
               WSTOP = GRIDWS(IT)
               HTOP  = GRIDHT(IT)
            END IF
! end - Use RLINE wind speed if RLINE or RLINEXT source
         END IF

         IB = IT - 1
         HBOT = GRIDHT(IB)
! Wood 7/5/2022 DWS = (WSTOP - GRIDWS(IB)) / (HTOP - HBOT)

! begin - Use RLINE wind speed if RLINE or RLINEXT source
         IF ((SRCTYP(ISRC) .EQ. 'RLINE') .or.&
         &(SRCTYP(ISRC) .EQ. 'RLINEXT')) THEN
            DWS = (WSTOP - RL_GRIDWS(IB, I_ALPHA)) / (HTOP - HBOT)
         ELSE
            DWS = (WSTOP - GRIDWS(IB)) / (HTOP - HBOT)
         END IF
! end - Use RLINE wind speed if RLINE or RLINEXT source

         DWS2 = DWS * DWS
         N2 = XN2(IB)
!
!           Solve the quadratic eqn
!
         A = 0.5D0 * (N2 - DWS2)
         B = (HTOP * DWS2 - WSTOP * DWS - N2 * HHILL)
         C = (N2 * HHILL * HTOP) - 0.5D0 * (N2 * HTOP * HTOP) -&
         &0.5D0 * (DWS2 * HTOP * HTOP) + WSTOP * DWS * HTOP -&
         &(LS(IT) - RS(IT))
         B2 = B * B
         AC4 = 4.0D0 * A * C
!crfl 6/19/96 Avoid sqrt (neg #) when near zero.
         IF ((B2-AC4)/B2.LT.0.0D0 .and. (B2-AC4)/B2.GT.-0.001D0) THEN
            AC4 = B2
         END IF
!crflendtest
         DETER = DSQRT( B2 - AC4 )
         HCRIT = (-B - DETER) / (2.0D0 * A)
      ELSE
         HCRIT = 0.0D0
      END IF

   ELSE
!        The hill height is less than zero (i.e., the hill elevation is
!        less than stack base); set HCRIT = 0.0 for this receptor
      HCRIT = 0.0D0

   END IF

   RETURN
END

SUBROUTINE PDF
!=======================================================================
!             PDF Module of the AMS/EPA Regulatory Model - AERMOD
!
!   Purpose:     To calculate the parameters required by the CBL
!                probability density function
!
!   Input:
!
!
!   Output:
!
!   Assumptions:
!
!   Called by:   PCALC
!
!   Programmer:  Jim Paumier, PES, Inc.
!   Date:        September 30, 1993
!
!   Revision history:
!                <none>
!
!   References:  "Summary of Expressions for the CBL", J. Weil, 4/12/93
!
!-----------------------------------------------------------------------
!---- Variable declarations
!
   USE MAIN1
   IMPLICIT NONE
   CHARACTER MODNAM*12

!---- Data initializations

   MODNAM = 'PDF'

!---- Calculate the skewness, SKEW                         --- CALL SKCALC
   CALL SKCALC

!---- Calculate the Lagrangian correlation function, R     --- CALL CRCALC
   CALL CRCALC

!---- Calculate the parameter ALPHPD                       --- CALL ALCALC
   CALL ALCALC

!---- Calculate the parameter BETAPD                       --- CALL BECALC
   CALL BECALC

!---- Calculate the ratio of the mean updraft and downdraft velocities
!     to the standard deviation of the vertical velocity, ASUB1 and ASUB2,
!     respectively                                         --- CALL AACALC
   CALL AACALC

!---- Calculate the ratio of the turbulent energy in the updrafts
!     and downdrafts to the standard deviation of the total vertical
!     velocity, BSUB1 and BSUB2, respectively              --- CALL BBCALC
   CALL BBCALC

!---- Calculate the relative frequencies of updrafts and
!     downdrafts, LAMDA1 and LAMDA2, respectively          --- CALL LLCALC
   CALL LLCALC

   RETURN
END

SUBROUTINE SKCALC
!=======================================================================
!             SKCALC Module of the AMS/EPA Regulatory Model - AERMOD
!
!   Purpose:     To calculate the skewness of the vertical velocity
!
!   Input:       Height at which computation is made, HEIGHT
!                Convective scaling velocity, WSTAR
!RJP
!RJP             Change SWEFF to SWEFFD throughout
!RJP
!RJP             Effective sigma_W, SWEFF
!                Effective sigma_W, SWEFFD
!
!   Output:      Skewness, SKEW
!
!   Assumptions:
!
!   Called by:   PDF
!
!   Programmer(s):  Jim Paumier, PES, Inc.
!   Date:           September 30, 1993
!
!   Revision history:
!                <none>
!
!   References:  "Summary of Expressions for the CBL", J. Weil, 4/12/93
!
!-----------------------------------------------------------------------
!
!---- Variable declarations
!
   USE MAIN1
   IMPLICIT NONE
   CHARACTER MODNAM*12

   DOUBLE PRECISION  WBAR3

!---- Data initializations
!
   MODNAM = 'SKCALC'

!---- Define the mean of the third moment of vertical velocity
   IF( SURFAC )THEN
!        This is a surface layer release
      WBAR3 = 1.25D0 * (WSTAR**3) * (CENTER/ZI)

   ELSE
!        The release is above the surface layer
      WBAR3 = 0.125D0 * (WSTAR**3)

   END IF

!---- Calculate the skewness
   SKEW = WBAR3 / (SWEFFD*SWEFFD*SWEFFD)

   RETURN
END

SUBROUTINE CRCALC ( )
!=======================================================================
!             CRCALC Module of the AMS/EPA Regulatory Model - AERMOD
!
!   Purpose:     To calculate the lagrangian correlation coefficient
!
!   Input:       Convective scaling velocity, WSTAR
!                Surface friction velocity, USTAR
!
!   Output:      Correlation coefficient, R
!
!   Assumptions:
!
!   Called by:   PDF
!
!   Programmer:  Jim Paumier, PES, Inc.
!   Date:        September 30, 1993
!
!   Revision history:
!                <none>
!
!   References:  "Summary of Expressions for the CBL", J. Weil, 4/12/93
!
!-----------------------------------------------------------------------
!
!---- Variable declarations
!
   USE MAIN1
   IMPLICIT NONE
   CHARACTER MODNAM*12

!---- Data initializations
!
   MODNAM = 'CRCALC'

!---- Set value of R to 2.0

   R = 2.0D0

   RETURN
END

SUBROUTINE ALCALC ( )
!=======================================================================
!             ALCALC Module of the AMS/EPA Regulatory Model - AERMOD
!
!   Purpose:     To calculate the coefficient ALPHPD for the CBL PDF
!
!   Input:       Lagrangian correlation coefficient, R
!
!   Output:      Coefficient, ALPHPD
!
!   Assumptions:
!
!   Called by:   PDF
!
!   Programmer:  Jim Paumier, PES, Inc.
!   Date:        September 30, 1993
!
!   Revision history:
!                <none>
!
!   References:  "Summary of Expressions for the CBL", J. Weil, 4/12/93
!
!-----------------------------------------------------------------------
!
!---- Variable declarations
!
   USE MAIN1
   IMPLICIT NONE
   CHARACTER MODNAM*12

!---- Data initializations
!
   MODNAM = 'ALCALC'

!---- Calculate the coefficient ALPHPD

   ALPHPD = ( 1.0D0 + R*R ) / (1.0D0 + 3.0D0*R*R)

   RETURN
END

SUBROUTINE BECALC ( )
!=======================================================================
!             BECALC Module of the AMS/EPA Regulatory Model - AERMOD
!
!   Purpose:     To calculate the coefficient BETAPD for the CBL PDF
!
!   Input:       Lagrangian correlation coefficient, R
!
!   Output:      Coefficient, BETAPD
!
!   Assumptions:
!
!   Called by:   PDF
!
!   Programmer:  Jim Paumier, PES, Inc.
!   Date:        September 30, 1993
!
!   Revision history:
!                <none>
!
!   References:  "Summary of Expressions for the CBL", J. Weil, 4/12/93
!
!-----------------------------------------------------------------------
!
!---- Variable declarations
!
   USE MAIN1
   IMPLICIT NONE
   CHARACTER MODNAM*12

!---- Data initializations
!
   MODNAM = 'BECALC'

!---- Calculate the coefficient BETAPD

   BETAPD = 1.0D0 + R*R

   RETURN
END

SUBROUTINE AACALC ( )
!=======================================================================
!             AACALC Module of the AMS/EPA Regulatory Model - AERMOD
!
!   Purpose:     Calculate the ratio of the mean updraft and downdraft
!                velocities to the standard deviation of the vertical
!                velocity
!
!   Input:       Skewness, SKEW
!                The coefficients ALPHPD and BETAPD
!
!   Output:      ASUB1 and ASUB2
!
!   Assumptions:
!
!   Called by:   PDF
!
!   Programmer:  Jim Paumier, PES, Inc.
!   Date:        September 30, 1993
!
!   Revision history:
!                <none>
!
!   References:  "Summary of Expressions for the CBL", J. Weil, 4/12/93
!
!-----------------------------------------------------------------------
!
!---- Variable declarations
!
   USE MAIN1
   IMPLICIT NONE
   CHARACTER MODNAM*12
   DOUBLE PRECISION :: DETERM, SWRATIO

!---- Data initializations
!
   MODNAM = 'AACALC'

!---- These two coefficients appear to be the solutions to a quadratic
!     equation.  Therefore, first compute the value of the determinant.

   DETERM = (ALPHPD*ALPHPD) * (SKEW*SKEW) + (4.0D0 / BETAPD)

!---- Compute square root of sigma-wc^2/wstar^2
   SWRATIO = SWEFFD/WSTAR

!---- Calculate the coefficients ASUB1 and ASUB2

   ASUB1 = SWRATIO * (0.5D0 * ALPHPD * SKEW + 0.5D0 * DSQRT(DETERM))
   ASUB2 = SWRATIO * (0.5D0 * ALPHPD * SKEW - 0.5D0 * DSQRT(DETERM))

   RETURN
END

SUBROUTINE BBCALC ( )
!=======================================================================
!             BBCALC Module of the AMS/EPA Regulatory Model - AERMOD
!
!   Purpose:     Calculate the ratio of the turbulent energy in the
!                updrafts and downdrafts to the standard deviation of
!                the vertical velocity
!
!   Input:       The Lagrangian correlation, R
!                The coefficients ASUB1 and ASUB2
!
!   Output:      BSUB1 and BSUB2
!
!   Assumptions:
!
!   Called by:   PDF
!
!   Programmer:  Jim Paumier, PES, Inc.
!   Date:        September 30, 1993
!
!   Revision history:
!                <none>
!
!   References:  "Summary of Expressions for the CBL", J. Weil, 4/12/93
!
!-----------------------------------------------------------------------
!
!---- Variable declarations
!
   USE MAIN1
   IMPLICIT NONE
   CHARACTER MODNAM*12

!---- Data initializations
!
   MODNAM = 'BBCALC'

!---- Calculate the coefficients BSUB1 and BSUB2

   BSUB1 =  R * ASUB1
   BSUB2 = -R * ASUB2

   RETURN
END

SUBROUTINE LLCALC ( )
!=======================================================================
!             LLCALC Module of the AMS/EPA Regulatory Model - AERMOD
!
!   Purpose:     Calculate the relative frequencies of updrafts and
!                downdrafts
!
!   Input:       The coefficients ASUB1 and ASUB2
!
!   Output:      LAMDA1 and LAMDA2
!
!   Assumptions:
!
!   Called by:   PDF
!
!   Programmer:  Jim Paumier, PES, Inc.
!   Date:        September 30, 1993
!
!   Revision history:
!                <none>
!
!   References:  "Summary of Expressions for the CBL", J. Weil, 4/12/93
!
!-----------------------------------------------------------------------
!
!---- Variable declarations
!
   USE MAIN1
   IMPLICIT NONE
   CHARACTER MODNAM*12

!---- Data initializations
!
   MODNAM = 'LLCALC'

!---- Calculate the coefficients LAMDA1 and LAMDA2

   LAMDA1 = ASUB2 / (ASUB2 - ASUB1)
   LAMDA2 = 1.0D0 - LAMDA1

   RETURN
END

SUBROUTINE DECAY (XARG)
!***********************************************************************
!                 DECAY Module of the AMS/EPA Regulatory Model - AERMOD
!
!        PURPOSE: Calculates Decay Term for Use in Gaussian Plume Equation
!
!        PROGRAMMER: Roger Brode, Jeff Wang
!
!        DATE:    March 2, 1992
!
!        INPUTS:  Downwind Distance, XARG (m)
!                 Stack Top Wind Speed, US (m/s)
!                 Decay Coefficient, DECOEFF (1/s)
!
!        OUTPUTS: Decay Term, D
!
!        CALLED FROM:   CHI
!                       DEP
!***********************************************************************

!     Variable Declarations
   USE MAIN1
   IMPLICIT NONE
   CHARACTER MODNAM*12

!     JAT DC1 is a temporary DCOEF variable
   DOUBLE PRECISION :: XARG,DC1

!     Variable Initializations
   MODNAM = 'DECAY'

   D = 1.0D0
   DC1=DECOEF !SET TEMPORARY DECAY HOLDER JAT

!      IF (DFAULT .and. URBAN .and. POLLUT.EQ.'SO2' .and.
!     &    URBSRC(ISRC).EQ.'Y') THEN !commented out 9/12/17 JAT
!     modified 9/29/17 JAT, use half-life for SO2 URBAN even without DFAULT
!     if HALFLIFE or DCAYCOEF used, use that value, not 4-hours
   IF (URBAN .and. POLLUT.EQ.'SO2' .and. URBSRC(ISRC).EQ.'Y' .and.&
   &((ICSTAT(7) .EQ. 0 .and. ICSTAT(8) .EQ. 0) .or. DFAULT)) THEN !urban SO2 source
      DECOEF = 4.81D-5
   ELSE IF ((POLLUT.EQ.'SO2' .and. URBSRC(ISRC).EQ.'N') .or.&
   &DFAULT) THEN  !rural source for SO2 or default modified 10/12/17
      DECOEF = 0.0D0
!      ELSE IF (DFAULT) THEN !removed and moved to else if above JAT 10/12/17
!         DECOEF = 0.0D0
   END IF

   IF (DECOEF .GT. 0.0D0) THEN
      IF (STABLE .or. (UNSTAB .and. HS.GE.ZI)) THEN
         D = DEXP (MAX (EXPLIM, -DECOEF*XARG/UEFF))
      ELSE
         D = DEXP (MAX (EXPLIM, -DECOEF*XARG/UEFFD))
      END IF
   END IF
   DECOEF=DC1 !RESET DECOEF TO ORIGINAL VALUE JAT

   RETURN
END

SUBROUTINE VRTSBL (SZARG, HEARG, ZIARG)
!***********************************************************************
!        VRTSBL Module of the AMS/EPA Regulatory Model - AERMOD
!
!        PURPOSE: Calculates Vertical Term for Use in Gaussian Plume
!                 Equation for Stable Conditions.
!
!        PROGRAMMER: Roger Brode
!
!        DATE:    September 30, 1993
!
!        MODIFIED BY R.W. Brode, PES, Inc. to adjust HE and ZI for cases
!                 with receptors below stack base (ZR < 0) - 12/26/00
!
!        INPUTS:  Plume Height, HE
!                 Vertical Dispersion Parameter, SZ
!                 Mixing/Reflection Height, HSBL (= max(zi,he))
!                 Receptor Height, ZR
!
!        OUTPUTS: Vertical Term, FSUBZ
!
!        ASSUMPTIONS:   Vertical term for STABLE plumes includes
!                       multiple reflection terms.
!
!        REVISIONS:  Concentrations for receptors above HSBL forced
!                    to zero.  Change made 8/31/94 by R.F. Lee.
!
!        CALLED FROM:   WRAP, LIFT
!***********************************************************************

!     Variable Declarations
   USE MAIN1
   IMPLICIT NONE
   CHARACTER MODNAM*12
   INTEGER :: I
   DOUBLE PRECISION :: SZARG, HEARG, ZIARG, A1, A2, A3, A4, A5, A6,&
   &TWOIZI, SUM, T, V
   DOUBLE PRECISION :: HETMP, ZITMP

!     Variable Initializations
   MODNAM = 'VRTSBL'
   V = 0.0D0

   IF (ZR .EQ. 0.0D0) THEN
!        Vertical Term for Case With FLAT Terrain and No Flagpole
!        Receptor (ZR = 0.0D0)
      A1 = (-0.5D0/(SZARG*SZARG)) * HEARG * HEARG
      IF (A1 .GT. EXPLIM)  V = DEXP(A1)
      SUM = 0.0D0
      DO I = 1, 100
         T  = 0.0D0
!           Use ZIARG (set in PCALC = max(HE,ZI)) instead of ZI.
         TWOIZI = 2.0D0*DBLE(I)*ZIARG
         A2 = (-0.5D0/(SZARG*SZARG)) * (TWOIZI-HEARG) *&
         &(TWOIZI-HEARG)
         A3 = (-0.5D0/(SZARG*SZARG)) * (TWOIZI+HEARG) *&
         &(TWOIZI+HEARG)
         IF (A2 .GT. EXPLIM)  T = DEXP(A2)
         IF (A3 .GT. EXPLIM)  T = T + DEXP(A3)
         SUM = SUM + T

!RWB        Modify convergence criterion to use relative value of T
         IF (DABS(T) .LE. 5.0D-7*DABS(SUM)) THEN
!              Exit Loop
            EXIT
         END IF
      END DO
!        Calculate Total Vert. Term - (2.*) was Removed for Optimization
      V  = 2.0D0*(V + SUM)

   ELSE IF (ZR .LE. ZIARG) THEN
!        Vertical Term for Case of ZR .NE. 0.0
!        First adjust for terrain below stack base with ZR < 0,
!        by keeping HE and ZI horizontal.
      HETMP = MAX( HEARG, HEARG - ZR )
      ZITMP = MAX( ZIARG, ZIARG - ZR )

      A1 = (-0.5D0/(SZARG*SZARG)) * (ZR-HETMP) * (ZR-HETMP)
      A2 = (-0.5D0/(SZARG*SZARG)) * (ZR+HETMP) * (ZR+HETMP)
      IF (A1 .GT. EXPLIM)  V = DEXP(A1)
      IF (A2 .GT. EXPLIM)  V = V + DEXP(A2)
      SUM = 0.0D0
      DO I = 1, 100
         T  = 0.0D0
         TWOIZI = 2.0D0*DBLE(I)*ZITMP
         A3 = (-0.5D0/(SZARG*SZARG)) * (ZR-(TWOIZI-HETMP)) *&
         &(ZR-(TWOIZI-HETMP))
         A4 = (-0.5D0/(SZARG*SZARG)) * (ZR+(TWOIZI-HETMP)) *&
         &(ZR+(TWOIZI-HETMP))
         A5 = (-0.5D0/(SZARG*SZARG)) * (ZR-(TWOIZI+HETMP)) *&
         &(ZR-(TWOIZI+HETMP))
         A6 = (-0.5D0/(SZARG*SZARG)) * (ZR+(TWOIZI+HETMP)) *&
         &(ZR+(TWOIZI+HETMP))
         IF (A3 .GT. EXPLIM)  T = T + DEXP(A3)
         IF (A4 .GT. EXPLIM)  T = T + DEXP(A4)
         IF (A5 .GT. EXPLIM)  T = T + DEXP(A5)
         IF (A6 .GT. EXPLIM)  T = T + DEXP(A6)
         SUM = SUM + T

!RWB        Modify convergence criterion to use relative value of T
         IF (DABS(T) .LE. 1.0D-6*DABS(SUM)) THEN
!              Exit Loop
            EXIT
         END IF
      END DO
      V  = V + SUM
!CRFL
!CRFL  Add 'ELSE' to cover case where receptor is above HSBL, and
!CRFL  set V = 0 for that case.
   ELSE
      V = 0.0D0
   END IF

!     Calculate FSUBZ from V;  FSUBZ = V / (SQRT(2*PI) * SZARG)
   FSUBZ = V / (SRT2PI*SZARG)

   RETURN
END

SUBROUTINE VRTSBN (SZARG, HEARG)
!***********************************************************************
!        VRTSBN Module of the AMS/EPA Regulatory Model - AERMOD
!
!        PURPOSE: Calculates Vertical Term for Use in Gaussian Plume
!                 Equation for Stable Conditions WITHOUT mixing lid.
!                 This subroutine is used for plumes above the CBL.
!
!        PROGRAMMER: Russ Lee, adapted from SUBROUTINE VRTSBL written
!                 by Roger Brode
!
!        DATE:    August 31, 1994
!
!        MODIFIED BY R.W. Brode, PES, Inc. to adjust HE for cases
!                 with receptors below stack base (ZR < 0) - 12/26/00
!
!        INPUTS:  Plume Height, HE
!                 Vertical Dispersion Parameter, SZ
!                 Receptor Height, ZR
!
!        OUTPUTS: Vertical Term, FSUBZ
!
!        ASSUMPTIONS:   This routine for Vertical term for STABLE
!                       plumes does not include multiple reflection
!                       terms (used in stable layer above CBL).
!
!        CALLED FROM:   WRAP, LIFT
!***********************************************************************

!     Variable Declarations
   USE MAIN1
   IMPLICIT NONE
   CHARACTER MODNAM*12
   DOUBLE PRECISION :: SZARG, HEARG, A1, A2, V
   DOUBLE PRECISION :: HETMP

!     Variable Initializations
   MODNAM = 'VRTSBN'
   V = 0.0D0

   IF (ZR .EQ. 0.0D0) THEN
!        Vertical Term for Case With FLAT Terrain and No Flagpole
!        Receptor (ZR = 0.0)
      A1 = (-0.5D0/(SZARG*SZARG)) * HEARG * HEARG
      IF (A1 .GT. EXPLIM)  V = DEXP(A1)
      V  = 2.D0 * V
   ELSE
!        Vertical Term for Case of ZR .NE. 0.0
!        First adjust for terrain below stack base with ZR < 0,
!        by keeping HE and ZI horizontal.
      HETMP = MAX( HEARG, HEARG - ZR )

      A1 = (-0.5D0/(SZARG*SZARG)) * (ZR-HETMP) * (ZR-HETMP)
      A2 = (-0.5D0/(SZARG*SZARG)) * (ZR+HETMP) * (ZR+HETMP)
      IF (A1 .GT. EXPLIM)  V = DEXP(A1)
      IF (A2 .GT. EXPLIM)  V = V + DEXP(A2)
   END IF

!     Calculate FSUBZ from V;  FSUBZ = V / (SQRT(2*PI) * SZ)
   FSUBZ = V / (SRT2PI*SZARG)

   RETURN
END


SUBROUTINE VRTCBL (HE1, HE2, SZ1, SZ2, FACT)
!***********************************************************************
!        VRTCBL Module of the AMS/EPA Regulatory Model - AERMOD
!
!        PURPOSE: Calculates Vertical Term for Use in Bi-Gaussian Plume
!                 Equation for Unstable (Convective) Conditions.
!                 Skewness of the plume is treated
!                 using two Gaussian plumes.  Revised from VRTCBL as
!                 programmed by Roger Brode, September 30, 1993.
!
!        PROGRAMMERS: Roger Brode, Russ Lee
!
!        DATE:    July 20, 1994
!
!        MODIFIED BY R.W. Brode, PES, Inc. to adjust HE and ZI for cases
!                 with receptors below stack base (ZR < 0) - 12/26/00
!
!        MODIFIED BY R.W. Brode, PES, Inc. to set vertical term to 0.0
!                 for cases when receptor is above mixing height - 1/22/98
!
!        INPUTS:  Plume 1 Height (arg), HE1
!                 Plume 2 Height (arg), HE2
!                 Vertical Dispersion Parameter (Plume 1), SZ1
!                 Vertical Dispersion Parameter (Plume 2), SZ2
!                 Factor to distinguish between direct
!                    and indirect plumes, FACT =  1.0 for Direct Plume
!                                         FACT = -1.0 for Indirect Plume
!                 Mixing Height, ZI
!                 Receptor Height, ZR
!
!        OUTPUTS: Vertical Term, FSUBZ
!
!        ASSUMPTIONS:   Vertical term for UNSTAB plumes includes
!                       one-half of reflection terms corresponding
!                       to the updraft portion of the plume.  Plume
!                       heights and sigma-z's are passed as arguments.
!
!        CALLED FROM:   WRAP, LIFT
!***********************************************************************

!     Variable Declarations
   USE MAIN1
   IMPLICIT NONE
   CHARACTER MODNAM*12
   INTEGER :: I
   DOUBLE PRECISION :: HE1, HE2, SZ1, SZ2, FACT, HEARG1,&
   &HEARG2, A1, A2, A3, A4, TWOIZI, SUM, T1, T2, TERM, V
   DOUBLE PRECISION :: HE1TMP, HE2TMP, ZITMP

!     Variable Initializations
   MODNAM = 'VRTCBL'
   V = 0.0D0

   IF (DABS(ZR-0.0D0) .LT. 1.0D-10) THEN
!        Vertical Term for Case With FLAT Terrain and No Flagpole
!        Receptor (ZR = 0.0)
      SUM = 0.0D0

      DO I = 0, 1000
         T1 = 0.0D0
         T2 = 0.0D0
         TWOIZI = 2.0D0*DBLE(I)*ZI * FACT
!           Check for FACT < 0 and skip first term.
         IF (FACT .LT. 0.0D0 .and. I .EQ. 0) CYCLE

         HEARG1 = TWOIZI+HE1
         HEARG2 = TWOIZI+HE2
         A1 = (-0.5D0/(SZ1*SZ1)) * (HEARG1) * (HEARG1)
         IF (A1 .GT. EXPLIM)  T1 = DEXP(A1)
         A2 = (-0.5D0/(SZ2*SZ2)) * (HEARG2) * (HEARG2)
         IF (A2 .GT. EXPLIM)  T2 = DEXP(A2)

!           Sum the Plume 1 and Plume 2 Portions
         TERM = (LAMDA1/SZ1)*T1 + (LAMDA2/SZ2)*T2
         SUM = SUM + TERM

!           Check for Convergence of Summation Term
         IF (DABS(TERM) .LE. 5.0D-7*DABS(SUM)) THEN
!              Exit Loop
            EXIT
         END IF

      END DO

!        Calculate Total Vert. Term - (2.*) was Removed for Optimization
      V  = 2.0D0* SUM

   ELSE IF (ZR .LE. ZI) THEN
!        Vertical Term for Case of ZR .NE. 0.0
!        First adjust for terrain below stack base with ZR < 0,
!        by keeping HE and ZI horizontal.
      HE1TMP = MAX( HE1, HE1 - ZR )
      HE2TMP = MAX( HE2, HE2 - ZR )
      ZITMP  = MAX( ZI, ZI - ZR )

      SUM = 0.0D0

      DO I = 0, 1000
         T1 = 0.0D0
         T2 = 0.0D0
         TWOIZI = 2.0D0*DBLE(I)*ZITMP * FACT
!           Check for FACT < 0 and skip first term.
         IF (FACT .LT. 0.0D0 .and. I .EQ. 0) CYCLE
!
!      Note:  The following code can be used for the indirect plume
!      as well as the direct plume, since HEn, for the indirect plume,
!      already contains ZI, and thus represents the first "reflection"
!      off the top of the mixed layer.
!
         HEARG1 = TWOIZI+HE1TMP
         HEARG2 = TWOIZI+HE2TMP
         A1 = (-0.5D0/(SZ1*SZ1)) * (ZR-(HEARG1)) *&
         &(ZR-(HEARG1))
         A2 = (-0.5D0/(SZ1*SZ1)) * (ZR+(HEARG1)) *&
         &(ZR+(HEARG1))
         IF (A1 .GT. EXPLIM)  T1 = DEXP(A1)
         IF (A2 .GT. EXPLIM)  T1 = T1 + DEXP(A2)
         A3 = (-0.5D0/(SZ2*SZ2)) * (ZR-(HEARG2)) *&
         &(ZR-(HEARG2))
         A4 = (-0.5D0/(SZ2*SZ2)) * (ZR+(HEARG2)) *&
         &(ZR+(HEARG2))
         IF (A3 .GT. EXPLIM)  T2 = DEXP(A3)
         IF (A4 .GT. EXPLIM)  T2 = T2 + DEXP(A4)

!           Sum the Plume 1 and Plume 2 Portions
         TERM = (LAMDA1/SZ1)*T1 + (LAMDA2/SZ2)*T2
         SUM = SUM + TERM

!           Check for Convergence of Summation Term
         IF (DABS(TERM) .LE. 1.0D-6*DABS(SUM)) THEN
!              Exit Loop
            EXIT
         END IF

      END DO

      V  = SUM

   ELSE
!        Receptor is above mixing height, set V=0.
      V = 0.0D0

   END IF

!     Calculate FSUBZ from V;  FSUBZ = V / SQRT(2*PI)
!     (Note that 1/SZ term is included in V)
   FSUBZ = V / SRT2PI

   RETURN
END


SUBROUTINE PFRACT (HEARG)
!***********************************************************************
!        PFRACT Module of the AMS/EPA Regulatory Model - AERMOD
!
!        PURPOSE: Calculates Fraction of Plume Material Below HCRIT
!
!        PROGRAMMER: Roger Brode
!
!        DATE:    September 30, 1993
!
!        INPUTS:  Plume Height, HEARG
!                 Vertical Dispersion Parameter, SZEFF
!                 Mixing/Reflection Height, HSBL (= max(zi,he))
!                 Critical Dividing Streamline Height, HCRIT
!
!        OUTPUTS: Fraction of plume below HCRIT, PHEE
!
!        CALLED FROM:   PCALC
!***********************************************************************

!     Variable Declarations
   USE MAIN1
   IMPLICIT NONE
   CHARACTER MODNAM*12
   INTEGER :: I
   DOUBLE PRECISION :: TWOIZI, HCINT, HEARG

   DOUBLE PRECISION A1, A2, A3, A4, A5, A6, B1, B2, B3, B4, B5, B6,&
   &T, SUM, ERFX

!     Variable Initializations
   MODNAM = 'PFRACT'
   PHEE = 0.0D0

   IF (STABLE .and. (HCRIT .GT. 0.0D0)) THEN

!        Define HCINT = MIN( HSBL, HCRIT) as the limit of the integral,
!        where HSBL = MAX( HE, ZI).
      HCINT = MIN( HSBL, HCRIT)

!        Calculate Terms Corresponding to n=0.
      A1 = (HCINT-HEARG)/(RTOF2 * SZ)
      A2 = (HCINT+HEARG)/(RTOF2 * SZ)
      B1 = ERFX (A1)
      B2 = ERFX (A2)

!        Calculate Summation Term.
      SUM = 0.0D0
      DO I = 1, 100
         T  = 0.0D0
!           Use HSBL (set in PCALC = max(HE,ZI)) instead of ZI.
         TWOIZI = 2.0D0*DBLE(I)*HSBL
         A3 = (HCINT-HEARG+TWOIZI)/(RTOF2 * SZ)
         A4 = (HCINT+HEARG+TWOIZI)/(RTOF2 * SZ)
         A5 = (HCINT-HEARG-TWOIZI)/(RTOF2 * SZ)
         A6 = (HCINT+HEARG-TWOIZI)/(RTOF2 * SZ)
         B3 = ERFX (A3)
         B4 = ERFX (A4)
         B5 = ERFX (A5)
         B6 = ERFX (A6)

         T = B3 + B4 + B5 + B6
         SUM = SUM + T

!           Check for convergence of summation term
         IF (DABS(T) .LE. 1.0D-6*DABS(SUM)) THEN
! ---          Set lower limit of 5 on number of iterations
            IF( I .GE. 5 )THEN
!                 Exit Loop
               EXIT
            ENDIF
         END IF

      END DO

      PHEE = 0.5D0 * (B1 + B2 + SUM)

!        Check for PHEE > 1.01 and Set = 1.0
!        (this patch may need to be changed).
      IF (PHEE .GT. 1.01D0 .and. .NOT. L_SkipMessages)  THEN
         WRITE(DUMMY,'(I8.8)') KURDAT
         CALL ERRHDL(PATH,MODNAM,'I','405',DUMMY)
         PHEE = 1.0D0
      END IF

   END IF

   RETURN
END

FUNCTION ERFX(ARG)
!***********************************************************************
!        ERFX Module of the AMS/EPA Regulatory Model - AERMOD
!
!        PURPOSE: Calculates Error Function, Using Method Documented
!                 on Page 187 of "Approximations for Digital Computers"
!                 by Cecil Hastings, Princeton University Press, 1955
!
!        PROGRAMMER: Roger Brode, Jeff Wang
!
!        DATE:    March 2, 1992
!
!        INPUTS:  Error Function Argument, ARG
!
!        OUTPUTS: Error Function Value, ERFX
!
!        CALLED FROM:   PFRACT
!***********************************************************************

!     Variable Declarations
   IMPLICIT NONE
   CHARACTER MODNAM*12

   DOUBLE PRECISION ARG, X, ERFX

!     Variable Initializations
   MODNAM = 'ERFX'

   IF (ARG .GT. 4.0D0) THEN
      ERFX = 1.0D0
   ELSE IF (ARG .LT. -4.0D0) THEN
      ERFX = -1.0D0
   ELSE IF (DABS(ARG) .LT. 1.0D-10) THEN
      ERFX = 0.0D0
   ELSE
      X = DABS(ARG)
      ERFX = 1.D0 - 1.D0/(1.D0+X*(0.705230784D-1+X*(0.422820123D-1+X*&
      &(0.92705272D-2+X*(0.1520143D-3+X*(0.2765672D-3+X*&
      &0.430638D-4))))))**16.0D0
      IF (ARG .LT. 0.0D0)  ERFX = -ERFX
   END IF

   RETURN
END

SUBROUTINE SUMVAL
!***********************************************************************
!                 SUMVAL Module of the AMS/EPA Regulatory Model - AERMOD
!
!        PURPOSE: Sums HRVAL to AVEVAL and ANNVAL Arrays
!
!        PROGRAMMER: Roger Brode
!
!        DATE:    March 2, 1992
!
!
!        INPUTS:  HRVAL - Hourly Value for (IREC,ISRC) Combination
!                 Averaging Period Options
!                 Source Groupings
!
!        OUTPUTS: Updated Sums of AVEVAL and ANNVAL Arrays
!
!        CALLED FROM:   PCALC
!                       VCALC
!                       ACALC
!                       RLCALC
!
!***********************************************************************

!     Variable Declarations
   USE MAIN1
   IMPLICIT NONE
   CHARACTER MODNAM*12

!     Variable Initializations
   MODNAM = 'SUMVAL'

!     Begin LOOP Over Output Types
   DO ITYP = 1, NUMTYP
      IF (HRVAL(ITYP) .NE. 0.0D0) THEN
!           Check for Source Belonging to Group
         IF (IGROUP(ISRC,IGRP) .EQ. 1) THEN
!              Begin Averaging Period LOOP
            DO IAVE = 1, NUMAVE
               AVEVAL(IREC,IGRP,IAVE,ITYP) = HRVAL(ITYP) +&
               &AVEVAL(IREC,IGRP,IAVE,ITYP)
            END DO
!              End Averaging Period LOOP
            IF (PERIOD .or. ANNUAL) THEN
               ANNVAL(IREC,IGRP,ITYP) = HRVAL(ITYP) +&
               &ANNVAL(IREC,IGRP,ITYP)
            END IF
            IF (ISEAHR(IGRP) .EQ. 1) THEN
               SHVALS(IREC,IGRP,ISEAS,IHOUR,ITYP) = HRVAL(ITYP) +&
               &SHVALS(IREC,IGRP,ISEAS,IHOUR,ITYP)
            END IF
         END IF
      END IF
   END DO
!     End LOOP Over Output Types

   RETURN
END

SUBROUTINE SUMBACK
!***********************************************************************
!                 SUMBACK Module of the AMS/EPA Regulatory Model - AERMOD
!
!        PURPOSE: Sums Background Values to AVEVAL and ANNVAL Arrays
!
!        PROGRAMMER: Roger Brode
!
!        DATE:       February 28, 2011
!
!        INPUTS:
!
!        OUTPUTS: Updated Sums of AVEVAL and ANNVAL Arrays
!
!        CALLED FROM:   PCALC
!                       VCALC
!                       ACALC
!***********************************************************************

!     Variable Declarations
   USE MAIN1
   IMPLICIT NONE
   DOUBLE PRECISION :: BCKGRD
   CHARACTER MODNAM*12

!     Variable Initializations
   MODNAM = 'SUMBACK'
   BCKGRD = 0.0D0

!     Begin LOOP Over Output Types
!     JAT 6/25/19 ADDED FROM 18081
!     TWO MODIFICATIONS
!     1.  Multiply BGCONC by ratio of EMIFAC/1.0E6 to account for the
!         that background concentrations are in
!         micrograms/m^3 for internal calculations
!         but modeled output may be in other units
!         such as ppb.  This division puts the
!         background in the same units as the modeled
!         concentrations before adding them.
!     2.  Only add background to the concentration output
!         type, i.e. ityp=1.  Background concentration (ug/^3)
!         should not be added to deposition (g/m^2)
   ITYP=1
!      DO ITYP = 1, NUMTYP  !JAT comment out do loop since ITYP only = 1
   IF (GRP_BACK(IGRP)) THEN
! ---       Include background for this source group
      BCKGRD = BGCONC*EMIFAC(ITYP)/1.0D6
      BACKAVE(IGRP) = BACKAVE(IGRP) + BGCONC*EMIFAC(ITYP)/1.0D6
      IF (PERIOD .or. ANNUAL) THEN
         BACKANN(IGRP) = BACKANN(IGRP) + BGCONC*EMIFAC(ITYP)/1.0D6
      END IF
      IF (ISEAHR(IGRP) .EQ. 1) THEN
         BACKSEASHR(IGRP,ISEAS,IHOUR) =&
         &BACKSEASHR(IGRP,ISEAS,IHOUR) + BGCONC*EMIFAC(ITYP)/1.0D6
      END IF
   ELSE
! ---       Do not include background for this source group
      BCKGRD = 0.0D0
   END IF
!        Begin Averaging Period LOOP
   DO IAVE = 1, NUMAVE
      AVEVAL(1:NUMREC,IGRP,IAVE,ITYP) = BCKGRD +&
      &AVEVAL(1:NUMREC,IGRP,IAVE,ITYP)
   END DO
!        End Averaging Period LOOP
   IF (PERIOD .or. ANNUAL) THEN
      ANNVAL(1:NUMREC,IGRP,ITYP) = BCKGRD +&
      &ANNVAL(1:NUMREC,IGRP,ITYP)
   END IF
   IF (ISEAHR(IGRP) .EQ. 1) THEN
      SHVALS(1:NUMREC,IGRP,ISEAS,IHOUR,ITYP) = BCKGRD +&
      &SHVALS(1:NUMREC,IGRP,ISEAS,IHOUR,ITYP)
   END IF
!      END DO !JAT comment out do loop since ITYP only = 1
!     End LOOP Over Output Types

   RETURN
END

SUBROUTINE SUMBACK_NO2
!***********************************************************************
!                 SUMBACK Module of the AMS/EPA Regulatory Model - AERMOD
!
!        PURPOSE: Sums Background Values to BACKAVE, BACKANN, and
!                 BACKSEASHR arrays, and to AVEVAL, ANNVAL, and SHVALS
!                 Arrays for the current receptor for the ARM2,
!                 OLM, PVMRM and GRSM options for modeling NO2
!
!        PROGRAMMER: Roger Brode
!
!        DATE:    May 14, 2014
!
!        INPUTS:
!
!        OUTPUTS: Updated Sums of BACKAVE, BACKANN, BACKSEASHR, AVEVAL,
!                 ANNVAL, and SHVALS Arrays, as applicable
!
!        CALLED FROM:   ARM2_CALC, OLM_CALC, PVMRM_CALC and GRSM_CALC
!
!***********************************************************************

!     Variable Declarations
   USE MAIN1
   IMPLICIT NONE
   CHARACTER MODNAM*12

!     Variable Initializations
   MODNAM = 'SUMBACK_NO2'

!     Begin LOOP Over Output Types
!     RCO 7/27/20 ADDED FROM 19191
!     TWO MODIFICATIONS
!     1.  Multiply BGCONC by ratio of EMIFAC/1.0E6 to account for the
!         that background concentrations are in
!         micrograms/m^3 for internal calculations
!         but modeled output may be in other units
!         such as ppb.  This division puts the
!         background in the same units as the modeled
!         concentrations before adding them.
!     2.  Only add background to the concentration output
!         type, i.e. ityp=1.  Background concentration (ug/^3)
!         should not be added to deposition (g/m^2)
   ITYP=1
!      DO ITYP = 1, NUMTYP  !RCO comment out do loop since ITYP only = 1
   IF (GRP_BACK(IGRP) .and. BGCONC .GT. 0.0D0) THEN
! ---       Include background for this source group
      BACKAVE(IGRP) = BACKAVE(IGRP) + BGCONC*EMIFAC(ITYP)/1.0D6
      IF (PERIOD .or. ANNUAL) THEN
         BACKANN(IGRP) = BACKANN(IGRP) +&
         &BGCONC*EMIFAC(ITYP)/1.0D6
      END IF
      IF (ISEAHR(IGRP) .EQ. 1) THEN
         BACKSEASHR(IGRP,ISEAS,IHOUR) =&
         &BACKSEASHR(IGRP,ISEAS,IHOUR) +&
         &BGCONC*EMIFAC(ITYP)/1.0D6
      END IF

!           Begin Averaging Period LOOP
      DO IAVE = 1, NUMAVE
         AVEVAL(IREC,IGRP,IAVE,ITYP) =&
         &BGCONC*EMIFAC(ITYP)/1.0D6 +&
         &AVEVAL(IREC,IGRP,IAVE,ITYP)
      END DO
!           End Averaging Period LOOP
      IF (PERIOD .or. ANNUAL) THEN
         ANNVAL(IREC,IGRP,ITYP) = BGCONC*EMIFAC(ITYP)/1.0D6 +&
         &ANNVAL(IREC,IGRP,ITYP)
      END IF
      IF (ISEAHR(IGRP) .EQ. 1) THEN
         SHVALS(IREC,IGRP,ISEAS,IHOUR,ITYP) =&
         &BGCONC*EMIFAC(ITYP)/1.0D6 +&
         &SHVALS(IREC,IGRP,ISEAS,IHOUR,ITYP)
      END IF
   END IF
!      END DO !JAT comment out do loop since ITYP only = 1
!     End LOOP Over Output Types

   RETURN
END

SUBROUTINE SUMVALPSD(SRCS2USE)
!***********************************************************************
!                 SUMVAL Module of the AMS/EPA Regulatory Model - AERMOD
!
!        PURPOSE: Sums HRVAL to ANNVAL Arrays for PSD credit and
!                 increment consumption
!
!                 There are only two source groups to consider:
!                 NAAQS and increment expanding
!
!        PROGRAMMER: J Paumier
!
!        DATE:    September 30, 2006
!
!        INPUTS:  HRVAL - Hourly Value for (IREC,ISRC) Combination
!                 Averaging Period Options
!
!        OUTPUTS: Updated Sums of ANNVAL Arrays
!
!        CALLED FROM:   PVMRM_CALC
!
!***********************************************************************

!     Variable Declarations
   USE MAIN1
   IMPLICIT NONE
   CHARACTER MODNAM*12, SRCS2USE*7

!     Variable Initializations
   MODNAM = 'SUMVALPSD'

!     Begin LOOP Over Output Types
   DO ITYP = 1, NUMTYP

!        Begin Averaging Period LOOP
      DO IAVE = 1, NUMAVE

         IF( TRIM(SRCS2USE) .EQ. 'NAAQSRC' )THEN
!              NAAQS group: assign to (A+B) group 1
            AVEVAL(IREC,1,IAVE,ITYP) = ABVAL(IREC,ITYP) +&
            &AVEVAL(IREC,1,IAVE,ITYP)

         ELSE IF( TRIM(SRCS2USE) .EQ. 'ALLBASE' )THEN
!              PSDINC group: assign (A+B)-(B+C) to group 2
            AVEVAL(IREC,2,IAVE,ITYP) = (ABVAL(IREC,ITYP) -&
            &BCVAL(IREC,ITYP)) +&
            &AVEVAL(IREC,2,IAVE,ITYP)
         END IF

      END DO

!        Check for ANNUAL or PERIOD Averaging
      IF (PERIOD .or. ANNUAL) THEN

         IF( TRIM(SRCS2USE) .EQ. 'NAAQSRC' )THEN
!              NAAQS group: assign to (A+B) group 1
            ANNVAL(IREC,1,ITYP) = ABVAL(IREC,ITYP) +&
            &ANNVAL(IREC,1,ITYP)

         ELSE IF( TRIM(SRCS2USE) .EQ. 'ALLBASE' )THEN
!              PSDINC group: assign (A+B)-(B+C) to group 2
            ANNVAL(IREC,2,ITYP) = (ABVAL(IREC,ITYP) -&
            &BCVAL(IREC,ITYP)) +&
            &ANNVAL(IREC,2,ITYP)
         END IF

      END IF

   END DO
!     End LOOP Over Output Types

   RETURN
END

SUBROUTINE AVER
!***********************************************************************
!                 AVER Module of the AMS/EPA Regulatory Model - AERMOD
!
!        PURPOSE: Calculates Short Term (<=24 hr) Average Concentrations
!
!        PROGRAMMER: Roger Brode, Jeff Wang
!
!        DATE:    March 2, 1992
!
!        INPUTS:  Averaging Time Option Switches
!                 Updated Array of Cumulative Values, AVEVAL
!
!        OUTPUTS: Updated Array of Averages, AVEVAL
!
!        CALLED FROM: HRLOOP
!***********************************************************************

!     Variable Declarations
   USE MAIN1
   IMPLICIT NONE
   CHARACTER MODNAM*12

   DOUBLE PRECISION :: SNUM

!     Variable Initializations
   MODNAM = 'AVER'
   PATH = 'CN'  !D081 CRT 5/13/2024

   IF (KAVE(IAVE) .NE. 1) THEN
!        Calculate Denominator Considering Calms and Missing,
!        Skipping Averaging if Averaging Period is 1-Hour
      SNUM = MAX(DBLE(NUMHRS(IAVE)-NUMCLM(IAVE)-NUMMSG(IAVE)),&
      &DNINT(DBLE(NUMHRS(IAVE))*0.75D0+0.4D0))
!        D081 - Add warning message - less than 18 hours used for 24-hr avg (WSP, 4/2023))
      IF(DBLE(NUMHRS(IAVE)-NUMCLM(IAVE)-NUMMSG(IAVE)) .LT. 18&
      &.and. KAVE(IAVE) .EQ. 24) THEN
         WRITE(DUMMY,'(I10)') FULLDATE
         CALL ERRHDL(PATH,MODNAM,'W','732',DUMMY)
!        D081 - Add warning message - less than 6 hours used for 8-hr avg (CRT, 5/1/2023)
      ELSEIF (DBLE(NUMHRS(IAVE)-NUMCLM(IAVE)-NUMMSG(IAVE)) .LT. 6&
      &.and. KAVE(IAVE) .EQ. 8) THEN
         WRITE(DUMMY,'(I10)') FULLDATE
         CALL ERRHDL(PATH,MODNAM,'W','733',DUMMY)
!        D081 - Add warning message - less than 3 hours used for 3-hr avg (CRT, 5/1/2023)
      ELSEIF (DBLE(NUMHRS(IAVE)-NUMCLM(IAVE)-NUMMSG(IAVE)) .LT. 3&
      &.and. KAVE(IAVE) .EQ. 3) THEN
         WRITE(DUMMY,'(I10)') FULLDATE
         CALL ERRHDL(PATH,MODNAM,'W','734',DUMMY)
      END IF
      AVEVAL(1:NUMREC,1:NUMGRP,IAVE,1) =&
      &AVEVAL(1:NUMREC,1:NUMGRP,IAVE,1) / SNUM
   END IF

   RETURN
END

SUBROUTINE HIVALS
!***********************************************************************
!                 HIVALS Module of the AMS/EPA Regulatory Model - AERMOD
!
!        PURPOSE: Updates High Value Tables
!
!        PROGRAMMER: Roger Brode, Jeff Wang
!
!        DATE:    March 2, 1992
!
!        MODIFIED:   To change subroutine name MAXVAL to MAXVALUE to
!                    avoid conflicts with intrinsic function MAXVAL under
!                    Fortran 90.  R. Brode, PES, 12/29/97
!
!        INPUTS:  High Value Option Switches
!                 Array of CONC or DEPOS Averages
!
!        OUTPUTS: Updated High Value Arrays
!
!        CALLED FROM:   MAIN
!***********************************************************************

!     Variable Declarations
   USE MAIN1
   IMPLICIT NONE
   CHARACTER MODNAM*12

!     Variable Initializations
   MODNAM = 'HIVALS'

!     Check for High/Max Value Options - Skip Update If KAVE=1,
!     And No CALCS Were Made for the Current Hour
   IF (CALCS .or. KAVE(IAVE).NE.1) THEN
      IF (INHI(IAVE) .EQ. 1) THEN
         DO ITYP = 1, NUMTYP
!              Update High Values for Each Receptor            ---   CALL NHIGH
            CALL NHIGH
         END DO
      END IF
      IF (MAXAVE(IAVE) .EQ. 1) THEN
         DO ITYP = 1, NUMTYP
!              Update Maximum Value Table for KAVE             ---   CALL MAXVALUE
            CALL MAXVALUE
         END DO
      END IF
   END IF
!     Reset Counters for This Averaging Period
   NUMHRS(IAVE) = 0
   NUMCLM(IAVE) = 0
   NUMMSG(IAVE) = 0

   RETURN
END

SUBROUTINE NHIGH
!***********************************************************************
!                 NHIGH Module of the AMS/EPA Regulatory Model - AERMOD
!
!        PURPOSE: Update Highest Value by Receptor Arrays
!                 Note: For duplicate values, the earlier occurrence keeps its
!                       rank within the array
!
!        PROGRAMMER: Roger Brode, Jeff Wang
!
!        DATE:    March 2, 1992
!
!        INPUTS:  High Value Options
!                 Array of CONC or DEPOS Averages
!                 Averaging Period
!
!        OUTPUTS: Updated Highest Value Array
!                 Updated Highest Date Array
!
!        CALLED FROM:   HIVALS
!***********************************************************************

!     Variable Declarations
   USE MAIN1
   IMPLICIT NONE
   CHARACTER MODNAM*12

   INTEGER :: J

!     Variable Initializations
   MODNAM = 'NHIGH'

!     Begin Source Group LOOP
   DO IGRP = 1, NUMGRP
!        Begin Receptor LOOP
      RECEPTOR_LOOP: DO IREC = 1, NUMREC
         IF (NHIVAL .GT. 1) THEN
            IF (AVEVAL(IREC,IGRP,IAVE,ITYP) .GT.&
            &HIVALU(IREC,NHIVAL,IGRP,IAVE,ITYP)) THEN
               DO J = NHIVAL-1, 1, -1
                  IF (AVEVAL(IREC,IGRP,IAVE,ITYP) .LE.&
                  &HIVALU(IREC,J,IGRP,IAVE,ITYP)) THEN
                     HIVALU(IREC,J+1,IGRP,IAVE,ITYP) =&
                     &AVEVAL(IREC,IGRP,IAVE,ITYP)
                     IF (NUMCLM(IAVE).EQ.0 .and.&
                     &NUMMSG(IAVE).EQ.0) THEN
                        HCLMSG(IREC,J+1,IGRP,IAVE,ITYP) = ' '
                     ELSE
!                          Set Indicator Of Calm and Msg    ---   CALL HSETFG
                        CALL HSETFG(0,J)
                     END IF
                     NHIDAT(IREC,J+1,IGRP,IAVE,ITYP) = KURDAT
!                       Exit Block
                     CYCLE RECEPTOR_LOOP
                  ELSE
                     HIVALU(IREC,J+1,IGRP,IAVE,ITYP) =&
                     &HIVALU(IREC,J,IGRP,IAVE,ITYP)
                     HCLMSG(IREC,J+1,IGRP,IAVE,ITYP) =&
                     &HCLMSG(IREC,J,IGRP,IAVE,ITYP)
                     NHIDAT(IREC,J+1,IGRP,IAVE,ITYP) =&
                     &NHIDAT(IREC,J,IGRP,IAVE,ITYP)
                     IF (J .EQ. 1) THEN
                        HIVALU(IREC,1,IGRP,IAVE,ITYP) =&
                        &AVEVAL(IREC,IGRP,IAVE,ITYP)
                        IF (NUMCLM(IAVE).EQ.0 .and.&
                        &NUMMSG(IAVE).EQ.0) THEN
                           HCLMSG(IREC,1,IGRP,IAVE,ITYP) = ' '
                        ELSE
!                             Set Indicator Of Calm and Msg ---   CALL HSETFG
                           CALL HSETFG(1,1)
                        END IF
                        NHIDAT(IREC,1,IGRP,IAVE,ITYP) = KURDAT
                     END IF
                  END IF
               END DO
            END IF
         ELSE IF (NHIVAL .EQ. 1) THEN
            IF (AVEVAL(IREC,IGRP,IAVE,ITYP) .GT.&
            &HIVALU(IREC,1,IGRP,IAVE,ITYP)) THEN
               HIVALU(IREC,1,IGRP,IAVE,ITYP) = AVEVAL(IREC,IGRP,IAVE,ITYP)
               IF (NUMCLM(IAVE).EQ.0 .and.&
               &NUMMSG(IAVE).EQ.0) THEN
                  HCLMSG(IREC,1,IGRP,IAVE,ITYP) = ' '
               ELSE
!                    Set Indicator Of Calm and Missing      ---   CALL HSETFG
                  CALL HSETFG(1,1)
               END IF
               NHIDAT(IREC,1,IGRP,IAVE,ITYP) = KURDAT
            END IF
         END IF
      END DO RECEPTOR_LOOP
!        End Receptor LOOP
   END DO
!     End Source Group LOOP

   RETURN
END

SUBROUTINE HSETFG(INDT,J)
!***********************************************************************
!                 HSETFG Module of the AMS/EPA Regulatory Model - AERMOD
!
!        PURPOSE: Set Calm and Missing Flag Of the Result
!
!        PROGRAMMER: Jeff Wang
!
!        DATE:    March 2, 1992
!
!        MODIFIED:   To correct error in order of indices for array
!                    HCLMSG on first assignment to 'b' - 9/29/92
!
!        INPUTS:  High Value Options
!                 Array of CONC or DEPOS Averages
!                 Averaging Period
!
!        OUTPUTS: Updated Highest Value Flag Array
!
!        CALLED FROM:   NHIGH
!***********************************************************************

!     Variable Declarations
   USE MAIN1
   IMPLICIT NONE
   CHARACTER MODNAM*12

   INTEGER :: J, INDT

!     Variable Initializations
   MODNAM = 'HSETFG'

   IF (INDT .EQ. 0) THEN
!        Set Indicator Of Calm and Missing
      IF (NUMCLM(IAVE).NE.0 .and.&
      &NUMMSG(IAVE).EQ.0) THEN
         HCLMSG(IREC,J+1,IGRP,IAVE,ITYP) = 'c'
      ELSE IF (NUMCLM(IAVE).EQ.0 .and.&
      &NUMMSG(IAVE).NE.0) THEN
         HCLMSG(IREC,J+1,IGRP,IAVE,ITYP) = 'm'
      ELSE IF (NUMCLM(IAVE).NE.0 .and.&
      &NUMMSG(IAVE).NE.0) THEN
         HCLMSG(IREC,J+1,IGRP,IAVE,ITYP) = 'b'
      END IF
   ELSE IF (INDT .EQ. 1) THEN
!        Set Indicator Of Calm and Missing
      IF (NUMCLM(IAVE).NE.0 .and.&
      &NUMMSG(IAVE).EQ.0) THEN
         HCLMSG(IREC,1,IGRP,IAVE,ITYP) = 'c'
      ELSE IF (NUMCLM(IAVE).EQ.0 .and.&
      &NUMMSG(IAVE).NE.0) THEN
         HCLMSG(IREC,1,IGRP,IAVE,ITYP) = 'm'
      ELSE IF (NUMCLM(IAVE).NE.0 .and.&
      &NUMMSG(IAVE).NE.0) THEN
         HCLMSG(IREC,1,IGRP,IAVE,ITYP) = 'b'
      END IF
   END IF

   RETURN
END

SUBROUTINE MAXVALUE
!***********************************************************************
!                 MAXVALUE Module of the AMS/EPA Regulatory Model - AERMOD
!
!        PURPOSE: Update Overall Maximum Value Arrays
!                 NMAX = 50 Assigned in PARAMETER Statement
!                 Note: For duplicate values, the earlier occurrence keeps
!                       its rank within the array
!
!        PROGRAMMER: Roger Brode, Jeff Wang
!
!        DATE:    March 2, 1992
!
!        INPUTS:  Maximum Value Table Options
!                 Array of CONC or DEPOS Averages
!                 Averaging Period
!
!        OUTPUTS: Updated Maximum Value Array
!                 Updated Maximum Date Array
!                 Updated Maximum Receptor Array
!
!        CALLED FROM:   HIVALS
!***********************************************************************

!     Variable Declarations
   USE MAIN1
   IMPLICIT NONE
   CHARACTER MODNAM*12

   INTEGER :: J

!     Variable Initializations
   MODNAM = 'MAXVALUE'

!     Begin Source Group LOOP
   DO IGRP = 1, NUMGRP
!        Begin Receptor LOOP
      RECEPTOR_LOOP: DO IREC = 1, NUMREC
         IF (NMXVAL .GT. 1) THEN
            IF (AVEVAL(IREC,IGRP,IAVE,ITYP) .GT.&
            &RMXVAL(NMXVAL,IGRP,IAVE,ITYP)) THEN
               DO J = NMXVAL-1, 1, -1
                  IF(AVEVAL(IREC,IGRP,IAVE,ITYP) .LE.&
                  &RMXVAL(J,IGRP,IAVE,ITYP)) THEN
                     RMXVAL(J+1,IGRP,IAVE,ITYP) = AVEVAL(IREC,IGRP,IAVE,ITYP)
                     IF (NUMCLM(IAVE).EQ.0 .and.&
                     &NUMMSG(IAVE).EQ.0) THEN
                        MCLMSG(J+1,IGRP,IAVE,ITYP) = ' '
                     ELSE
!                          Set Indicator Of Calm and Msg    ---   CALL MSETFG
                        CALL MSETFG(0,J)
                     END IF
                     MXDATE(J+1,IGRP,IAVE,ITYP) = KURDAT
                     MXLOCA(J+1,IGRP,IAVE,ITYP) = IREC
!                       Exit Block
                     CYCLE RECEPTOR_LOOP
                  ELSE
                     RMXVAL(J+1,IGRP,IAVE,ITYP) = RMXVAL(J,IGRP,IAVE,ITYP)
                     MXDATE(J+1,IGRP,IAVE,ITYP) = MXDATE(J,IGRP,IAVE,ITYP)
                     MCLMSG(J+1,IGRP,IAVE,ITYP) = MCLMSG(J,IGRP,IAVE,ITYP)
                     MXLOCA(J+1,IGRP,IAVE,ITYP) = MXLOCA(J,IGRP,IAVE,ITYP)
                     IF (J .EQ. 1) THEN
                        RMXVAL(1,IGRP,IAVE,ITYP) = AVEVAL(IREC,IGRP,IAVE,ITYP)
                        IF (NUMCLM(IAVE).EQ.0 .and.&
                        &NUMMSG(IAVE).EQ.0) THEN
                           MCLMSG(1,IGRP,IAVE,ITYP) = ' '
                        ELSE
!                             Set Indicator Of Calm and Msg ---   CALL MSETFG
                           CALL MSETFG(1,1)
                        END IF
                        MXDATE(1,IGRP,IAVE,ITYP) = KURDAT
                        MXLOCA(1,IGRP,IAVE,ITYP) = IREC
                     END IF
                  END IF
               END DO
            END IF
         ELSE IF (NMXVAL .EQ. 1) THEN
            IF (AVEVAL(IREC,IGRP,IAVE,ITYP) .GT.&
            &RMXVAL(1,IGRP,IAVE,ITYP)) THEN
               RMXVAL(1,IGRP,IAVE,ITYP) = AVEVAL(IREC,IGRP,IAVE,ITYP)
               IF (NUMCLM(IAVE).EQ.0 .and.&
               &NUMMSG(IAVE).EQ.0) THEN
                  MCLMSG(1,IGRP,IAVE,ITYP) = ' '
               ELSE
!                    Set Indicator Of Calm and Missing      ---   CALL MSETFG
                  CALL MSETFG(1,1)
               END IF
               MXDATE(1,IGRP,IAVE,ITYP) = KURDAT
               MXLOCA(1,IGRP,IAVE,ITYP) = IREC
            END IF
         END IF
      END DO RECEPTOR_LOOP
!        End Receptor LOOP
   END DO
!     End Source Group LOOP

   RETURN
END

SUBROUTINE MSETFG(INDT,J)
!***********************************************************************
!                 MSETFG Module of the AMS/EPA Regulatory Model - AERMOD
!
!        PURPOSE: Set Calm and Missing Flag Of the Max Result
!
!        PROGRAMMER: Jeff Wang
!
!        DATE:    March 2, 1992
!
!        INPUTS:  Maximum Value Table Options
!                 Array of CONC or DEPOS Averages
!                 Averaging Period
!
!        OUTPUTS: Updated Maximum Value Flag Array
!
!        CALLED FROM:   MAXVALUE
!***********************************************************************

!     Variable Declarations
   USE MAIN1
   IMPLICIT NONE
   CHARACTER MODNAM*12

   INTEGER :: J, INDT

!     Variable Initializations
   MODNAM = 'MSETFG'

   IF (INDT .EQ. 0) THEN
!        Set Indicator Of Calm and Missing
      IF (NUMCLM(IAVE).NE.0 .and.&
      &NUMMSG(IAVE).EQ.0) THEN
         MCLMSG(J+1,IGRP,IAVE,ITYP) = 'c'
      ELSE IF (NUMCLM(IAVE).EQ.0 .and.&
      &NUMMSG(IAVE).NE.0) THEN
         MCLMSG(J+1,IGRP,IAVE,ITYP) = 'm'
      ELSE IF (NUMCLM(IAVE).NE.0 .and.&
      &NUMMSG(IAVE).NE.0) THEN
         MCLMSG(J+1,IGRP,IAVE,ITYP) = 'b'
      END IF
   ELSE IF (INDT .EQ. 1) THEN
!        Set Indicator Of Calm and Missing
      IF (NUMCLM(IAVE).NE.0 .and.&
      &NUMMSG(IAVE).EQ.0) THEN
         MCLMSG(1,IGRP,IAVE,ITYP) = 'c'
      ELSE IF (NUMCLM(IAVE).EQ.0 .and.&
      &NUMMSG(IAVE).NE.0) THEN
         MCLMSG(1,IGRP,IAVE,ITYP) = 'm'
      ELSE IF (NUMCLM(IAVE).NE.0 .and.&
      &NUMMSG(IAVE).NE.0) THEN
         MCLMSG(1,IGRP,IAVE,ITYP) = 'b'
      END IF
   END IF

   RETURN
END

SUBROUTINE MAXFIL
!***********************************************************************
!                 MAXFIL Module of the AMS/EPA Regulatory Model - AERMOD
!
!        PURPOSE: Update Maximum Value File (>Threshold)
!
!        PROGRAMMER: Roger Brode, Jeff Wang
!
!        DATE:    March 2, 1992
!
!        MODIFIED:   Moved check for RSTSAV (SAVEFILE option) outside
!                    the receptor loop, and replaced 'read to end' loop
!                    with POSITION='APPEND' in OPEN statement for
!                    Fortran 90 version.
!                    R.W. Brode, PES, Inc.,  6/23/98
!
!        INPUTS:  Maximum File Options
!                 Array of CONC or DEPOS Averages
!                 Averaging Period
!
!        OUTPUTS: Updated Maximum Value File
!
!        CALLED FROM:   HRLOOP
!***********************************************************************

!     Variable Declarations
   USE MAIN1
   IMPLICIT NONE
   CHARACTER MODNAM*12

!     Variable Initializations
   MODNAM = 'MAXFIL'

!     Check for High/Max Value Options - Skip Update If KAVE=1,
!     And No CALCS Were Made for the Current Hour
   IF (CALCS .or. KAVE(IAVE).NE.1) THEN
!        Begin Source Group LOOP
      DO IGRP = 1, NUMGRP
!           Check for MAXIFILE Option for This IGRP,IAVE Combination
         IF (MAXFLE(IGRP,IAVE) .EQ. 1) THEN
!              Begin Receptor LOOP
            DO IREC = 1, NUMREC
!                 For the Values Over Threshold
               IF (AVEVAL(IREC,IGRP,IAVE,1) .GE.&
               &THRESH(IGRP,IAVE)) THEN
                  WRITE(IMXUNT(IGRP,IAVE),THRFRM,ERR=99) KAVE(IAVE),&
                  &GRPID(IGRP), KURDAT, AXR(IREC), AYR(IREC),&
                  &AZELEV(IREC), AZHILL(IREC), AZFLAG(IREC),&
                  &AVEVAL(IREC,IGRP,IAVE,1)
               END IF
            END DO
!              End Receptor LOOP
            IF (RSTSAV) THEN
!                 Saving Intermediate Results to File for Later Re-start
!                 Close MAXIFILE and Reposition to End
               CLOSE (IMXUNT(IGRP,IAVE))
               OPEN(IMXUNT(IGRP,IAVE),FILE=THRFIL(IGRP,IAVE),&
               &POSITION='APPEND')
            END IF
         END IF
      END DO
!        End Source Group LOOP
   END IF

   GO TO 999

!     WRITE Error Message for Problem Writing to Maximum Value File
99 WRITE(DUMMY,'("MAXFL",I3.3)') IMXUNT(IGRP,IAVE)
   CALL ERRHDL(PATH,MODNAM,'E','520',DUMMY)
   RUNERR = .TRUE.

999 RETURN
END

SUBROUTINE POSTFL
!***********************************************************************
!                 POSTFL Module of the AMS/EPA Regulatory Model - AERMOD
!
!        PURPOSE: Write Concurrent Values to File for Postprocessing
!
!        PROGRAMMER: Roger Brode, Jeff Wang
!
!        DATE:    March 2, 1992
!
!        MODIFIED:   Replaced 'read to end' loop with POSITION='APPEND'
!                    in OPEN statements for Fortran 90 version with
!                    RSTSAV (SAVEFILE option).
!                    R.W. Brode, PES, Inc.,  6/23/98
!
!        INPUTS:  Postprocessing File Options
!                 Array of CONC or DEPOS Averages
!
!        OUTPUTS: Postprocessor Files
!
!        CALLED FROM:   HRLOOP
!***********************************************************************

!     Variable Declarations
   USE MAIN1
   IMPLICIT NONE
   CHARACTER MODNAM*12

!     Variable Initializations
   MODNAM = 'POSTFL'

!     Begin Source Group LOOP
   DO IGRP = 1, NUMGRP
!        Check for POSTFILE Option for This IGRP,IAVE Combination
      IF (IPSTFL(IGRP,IAVE) .EQ. 1) THEN
         IF (IPSFRM(IGRP,IAVE) .EQ. 0) THEN
!              WRITE Results to Unformatted POSTFILE
            WRITE(IPSUNT(IGRP,IAVE),ERR=99) KURDAT, KAVE(IAVE),&
            &GRPID(IGRP), ((AVEVAL(IREC,IGRP,IAVE,ITYP),&
            &IREC=1,NUMREC),ITYP=1,NUMTYP)
            IF (RSTSAV) THEN
!                 Saving Intermediate Results to File for Later Re-start
!                 Close POSTFILE and Reposition to End
               CLOSE (IPSUNT(IGRP,IAVE))
               OPEN(IPSUNT(IGRP,IAVE),FILE=PSTFIL(IGRP,IAVE),&
               &FORM='UNFORMATTED',POSITION='APPEND')
            END IF
         ELSE
!              WRITE Results to Formatted Plot File
!              Begin Receptor LOOP
            DO IREC = 1, NUMREC
               WRITE(IPSUNT(IGRP,IAVE),PSTFRM,ERR=99)&
               &AXR(IREC), AYR(IREC), (AVEVAL(IREC,IGRP,IAVE,ITYP),&
               &ITYP=1,NUMTYP),&
               &AZELEV(IREC), AZHILL(IREC), AZFLAG(IREC),&
               &CHRAVE(IAVE), GRPID(IGRP), KURDAT, NETID(IREC)
            END DO
!              End Receptor LOOP
            IF (RSTSAV) THEN
!                 Saving Intermediate Results to File for Later Re-start
!                 Close POSTFILE and Reposition to End
               CLOSE (IPSUNT(IGRP,IAVE))
               OPEN(IPSUNT(IGRP,IAVE),FILE=PSTFIL(IGRP,IAVE),&
               &FORM='FORMATTED',POSITION='APPEND')
            END IF
         END IF
      END IF
   END DO
!     End Source Group LOOP

   GO TO 999

!     WRITE Error Message for Problem Writing to Postprocessor File
99 WRITE(DUMMY,'("PSTFL",I3.3)') IPSUNT(IGRP,IAVE)
   CALL ERRHDL(PATH,MODNAM,'E','520',DUMMY)
   RUNERR = .TRUE.

999 RETURN
END

SUBROUTINE MXDLYFL
!***********************************************************************
!                 MXDLYFL Module of the AMS/EPA Regulatory Model - AERMOD
!
!        PURPOSE:    Update daily maximum value arrays, and write values to
!                    file for the MAXDAILY output option if applicable.
!
!        PROGRAMMER: Roger Brode
!
!        DATE:       February 28, 2011
!
!        MODIFIED:   Included IF-THEN block to account for cases with
!                    NHIMXDLY = 1, i.e., 1st-highest rank only.
!                    Previous version resulted in all values being 0.0
!                    if only the 1st-highest rank was selected for
!                    applications involving the special processing
!                    for daily maximum values (24hr PM25, 1hr NO2 and
!                    1hr SO2).
!                    R.W. Brode, U.S. EPA/OAQPS/AQMG, 04/13/2011
!
!        INPUTS:     MAXDAILY file options
!                    Array of maximum daily 1-hour values
!
!        OUTPUTS:
!
!        CALLED FROM:   HRLOOP
!***********************************************************************

!     Variable Declarations
   USE MAIN1
   IMPLICIT NONE

   INTEGER J
   CHARACTER MODNAM*12

!     Variable Initializations
   MODNAM = 'MXDLYFL'

!     Begin Source Group LOOP
   DO IGRP = 1, NUMGRP
      RECEPTOR_LOOP: DO IREC = 1, NUMREC
         ITYP = 1
!           Check for MAXDAILY Option for This IGRP
         IF (IMXDLY(IGRP) .EQ. 1) THEN
!              WRITE Results to MAXDAILY file for this day
            WRITE(IMDUNT(IGRP),MXDFRM,ERR=99) AXR(IREC),&
            &AYR(IREC), MXDVAL(IREC,IGRP), AZELEV(IREC),&
            &AZHILL(IREC), AZFLAG(IREC),'  1-HR',&
            &GRPID(IGRP), JDAY, IMXDHR(IREC,IGRP),&
            &(KURDAT/100) * 100 + IMXDHR(IREC,IGRP),&
            &NETID(IREC)
         END IF
! ---       Update arrays of highest MAXDAILY values
         IF (NHIMXDLY .GT. 1) THEN
            IF (MXDVAL(IREC,IGRP) .GT.&
            &HIMXDLY(IREC,IGRP,NHIMXDLY)) THEN
               DO J = NHIMXDLY-1, 1, -1
                  IF (MXDVAL(IREC,IGRP) .LE.&
                  &HIMXDLY(IREC,IGRP,J)) THEN
                     HIMXDLY(IREC,IGRP,J+1) =&
                     &MXDVAL(IREC,IGRP)
                     NHIDATMXD(IREC,IGRP,J+1) =&
                     &(KURDAT/100) * 100 + IMXDHR(IREC,IGRP)
!                       Exit Block
                     CYCLE RECEPTOR_LOOP
                  ELSE
                     HIMXDLY(IREC,IGRP,J+1) =&
                     &HIMXDLY(IREC,IGRP,J)
                     NHIDATMXD(IREC,IGRP,J+1) =&
                     &NHIDATMXD(IREC,IGRP,J)
                     IF (J .EQ. 1) THEN
                        HIMXDLY(IREC,IGRP,1) =&
                        &MXDVAL(IREC,IGRP)
                        NHIDATMXD(IREC,IGRP,1) =&
                        &(KURDAT/100) * 100 + IMXDHR(IREC,IGRP)
                     END IF
                  END IF
               END DO
            END IF
         ELSE IF (NHIMXDLY .EQ. 1) THEN
            IF (MXDVAL(IREC,IGRP) .GT.&
            &HIMXDLY(IREC,IGRP,1)) THEN
               HIMXDLY(IREC,IGRP,1) = MXDVAL(IREC,IGRP)
               NHIDATMXD(IREC,IGRP,1) = (KURDAT/100) * 100 +&
               &IMXDHR(IREC,IGRP)
            END IF
         END IF
      END DO RECEPTOR_LOOP
!        End Receptor LOOP
      IF (RSTSAV .and. IMXDLY(IGRP) .EQ. 1) THEN
!           Saving Intermediate Results to File for Later Re-start
!           Close MAXDAILY file and Reposition to End
         CLOSE (IMDUNT(IGRP))
         OPEN(IMDUNT(IGRP),FILE=MAXDLY(IGRP),&
         &FORM='FORMATTED',POSITION='APPEND')
      END IF
   END DO
!     End Source Group LOOP

! --- Reinitialize MXDVAL array
   MXDVAL = 0.0D0
   IMXDHR = 0

   GO TO 999

!     WRITE Error Message for Problem Writing to Postprocessor File
99 WRITE(DUMMY,'("MXDLY",I3.3)') IMDUNT(IGRP)
   CALL ERRHDL(PATH,MODNAM,'E','520',DUMMY)
   RUNERR = .TRUE.

999 RETURN
END

SUBROUTINE TOXXFL
!***********************************************************************
!                 TOXXFL Module of the AMS/EPA Regulatory Model - AERMOD
!
!        PURPOSE: Update TOXXFILE Buffers, and Write Out if Full
!
!        PROGRAMMER: Roger Brode
!
!        DATE:    September 29, 1992
!
!        INPUTS:  TOXXFILE Options
!                 Array of CONC or DEPOS Averages
!                 Averaging Period
!
!        OUTPUTS: Updated TOXXFILE Buffers and File
!
!        CALLED FROM:   HRLOOP
!***********************************************************************

!     Variable Declarations
   USE MAIN1
   IMPLICIT NONE
   CHARACTER MODNAM*12

   INTEGER :: I, IG, ICODE
   DOUBLE PRECISION :: CUTOFF

!     Variable Initializations
   MODNAM = 'TOXXFL'

!     Check for TOXXFILE Option - Skip Update If KAVE=1,
!     And No CALCS Were Made for the Current Hour
   IF (ITOXFL(IAVE).EQ.1 .and. (CALCS .or. KAVE(IAVE).NE.1)) THEN
!        Convert TOXXFILE Threshold to User Units
      CUTOFF = TOXTHR(IAVE) * EMIFAC(1)

!        Begin Receptor LOOP
      DO IREC = 1, NUMREC

!           Begin Source Group LOOP
         DO IGRP = 1, NUMGRP

!              For the Values Over Threshold (in user units), Fill Buffers
            IF (AVEVAL(IREC,IGRP,IAVE,1) .GE. CUTOFF) THEN
               DO IG = 1, NUMGRP
!                    Loop Through Groups and Write Values to Buffer
                  IPAIR = IPAIR + 1
                  ICODE = 100000*ILINE + 1000*IG + IREC
                  IDCONC(IAVE,IPAIR) = ICODE
!                    Convert CONC Values Back to Units of g/s
                  TXCONC(IAVE,IPAIR)=AVEVAL(IREC,IG,IAVE,1)/EMIFAC(1)
                  IF (IPAIR .EQ. NPAIR) THEN
!                       Write Out Full Buffers and Reset Counter
                     WRITE(ITXUNT(IAVE),ERR=99) (IDCONC(IAVE,I),&
                     &I=1,NPAIR)
                     WRITE(ITXUNT(IAVE),ERR=99) (TXCONC(IAVE,I),&
                     &I=1,NPAIR)
                     IPAIR = 0
                  END IF
               END DO
!                 Exit Source Group LOOP
               EXIT
            END IF

         END DO
!           End Source Group LOOP

      END DO
!        End Receptor LOOP
   END IF

   GO TO 999

!     WRITE Error Message for Problem Writing to TOXXFILE
99 WRITE(DUMMY,'("TOXFL",I3.3)') ITXUNT(IAVE)
   CALL ERRHDL(PATH,MODNAM,'E','520',DUMMY)
   RUNERR = .TRUE.

999 RETURN
END

SUBROUTINE PRTDAY
!***********************************************************************
!                 PRTDAY Module of the AMS/EPA Regulatory Model - AERMOD
!
!        PURPOSE: Write Concurrent Values to Printed Output File
!
!        PROGRAMMER: Roger Brode, Jeff Wang
!
!        DATE:    March 2, 1992
!
!        MODIFIED:   To remove obsolete reference to BOUNDARY receptors.
!                    R.W. Brode, U.S. EPA/OAQPS/AQMG, 10/19/2009
!
!        MODIFIED:   To adjust format statement 9082 for BOUNDARY receptors
!                    to better accommodate UTM coordinates - 9/29/92
!
!        INPUTS:  Postprocessing File Options
!                 Array of CONC or DEPOS Averages
!
!        OUTPUTS: Postprocessor Files
!
!        CALLED FROM:   HRLOOP
!***********************************************************************

!     Variable Declarations
   USE MAIN1
   IMPLICIT NONE
   CHARACTER MODNAM*12

   INTEGER :: I, J, K, II, NX, NY, INDZ, INDC, INDEXW
   DOUBLE PRECISION :: YCOVAL, XRMS, YRMS, DIST, DIR
   CHARACTER BUF132*132

!     Variable Initializations
   MODNAM = 'PRTDAY'
   BUF132 = ' '
   INDZ   = 0

!     Begin Source Group LOOP
   DO IGRP = 1, NUMGRP

!        Fill Work Array With SRCIDs For This Group
      IF (.NOT. PSDCREDIT) THEN
!           Fill Work Array With SRCIDs For This Group
         INDGRP = 0
         DO ISRC = 1, NUMSRC
            IF (IGROUP(ISRC,IGRP) .EQ. 1) THEN
               INDGRP = INDGRP + 1
               WORKID(INDGRP) = SRCID(ISRC)
            END IF
         END DO
      ELSE
!           Assign 'N/A' for source IDs for PSDCREDIT option
         INDGRP = 1
         WORKID(INDGRP) = 'N/A'
      END IF

! ---    Check for BACKGRND "source" being included
!        in source group
      IF (GRP_BACK(IGRP)) THEN
         INDGRP = INDGRP + 1
         WORKID(INDGRP) = 'BACKGROUND'
!           Check for More Than 29 Sources Per Group
         INDEXW = MIN(29,NSRC+1)
      ELSE
         INDEXW = MIN(29,NSRC)
      END IF
!        Check for More Than 29 Sources Per Group
      IF (INDGRP .GT. INDEXW) THEN
         WORKID(INDEXW) = ' . . . '
         INDGRP = INDEXW
      END IF

!        Print Results for Receptor Networks
!        Set Number of Columns Per Page, NCPP
      NCPP = 9
!        Set Number of Rows Per Page, NRPP
      NRPP = 40
!        Begin LOOP Through Networks
      DO I = 1, INNET
!           Calculate Number of Pages Per X-Group, NPPX, & Per Y-Group, NPPY
         NPPX = 1 + INT((NUMXPT(I)-1)/NCPP)
         NPPY = 1 + INT((NUMYPT(I)-1)/NRPP)
         DO NX = 1, NPPX
            DO NY = 1, NPPY
               CALL HEADER(IOUNIT)
               WRITE(IOUNIT,9032) CHRAVE(IAVE), (CHIDEP(II,ITYP),&
               &II=1,6),&
               &IHOUR,JDAY,IYR,GRPID(IGRP),(WORKID(K),K = 1,INDGRP)
               WRITE(IOUNIT,9037) NTID(I), NTTYP(I)
!                 Print The Value By Groups
               WRITE(IOUNIT,9011) CHIDEP(3,ITYP), POLLUT,OUTLBL(ITYP)
               IF (NX .EQ. NPPX) THEN
                  IF (NTTYP(I) .EQ. 'GRIDCART') THEN
                     WRITE(IOUNIT,9016)
                     WRITE(IOUNIT,9017) (XCOORD(J,I),J=1+NCPP*(NX-1),&
                     &NUMXPT(I))
                  ELSE IF (NTTYP(I) .EQ. 'GRIDPOLR') THEN
                     WRITE(IOUNIT,9018)
                     WRITE(IOUNIT,9019) (XCOORD(J,I),J=1+NCPP*(NX-1),&
                     &NUMXPT(I))
                  END IF
               ELSE
                  IF (NTTYP(I) .EQ. 'GRIDCART') THEN
                     WRITE(IOUNIT,9016)
                     WRITE(IOUNIT,9017) (XCOORD(J,I),J=1+NCPP*(NX-1),&
                     &NCPP*NX)
                  ELSE IF (NTTYP(I) .EQ. 'GRIDPOLR') THEN
                     WRITE(IOUNIT,9018)
                     WRITE(IOUNIT,9019) (XCOORD(J,I),J=1+NCPP*(NX-1),&
                     &NCPP*NX)
                  END IF
               END IF
               WRITE(IOUNIT,9010)
               IF (NY .EQ. NPPY) THEN
                  DO K = 1+NRPP*(NY-1), NUMYPT(I)
                     IF (NTTYP(I) .EQ. 'GRIDCART') THEN
                        INDZ = NETEND(I) - K*NUMXPT(I) + 1
                        YCOVAL = YCOORD(NUMYPT(I)-K+1,I)
                     ELSE IF (NTTYP(I) .EQ. 'GRIDPOLR') THEN
                        INDZ = NETSTA(I) + (K-1)*NUMXPT(I)
                        YCOVAL = YCOORD(K,I)
                     END IF
                     IF (NX .EQ. NPPX) THEN
                        WRITE(IOUNIT,9013) YCOVAL,&
                        &(AVEVAL(INDZ+J-1,IGRP,IAVE,ITYP),J=1+NCPP*(NX-1),&
                        &NUMXPT(I))
                     ELSE
                        WRITE(IOUNIT,9013) YCOVAL,&
                        &(AVEVAL(INDZ+J-1,IGRP,IAVE,ITYP),J=1+NCPP*(NX-1),&
                        &NCPP*NX)
                     END IF
                  END DO
               ELSE
                  DO K = 1+NRPP*(NY-1), NRPP*NY
                     IF (NTTYP(I) .EQ. 'GRIDCART') THEN
                        INDZ = NETEND(I) - K*NUMXPT(I) + 1
                        YCOVAL = YCOORD(NUMYPT(I)-K+1,I)
                     ELSE IF (NTTYP(I) .EQ. 'GRIDPOLR') THEN
                        INDZ = NETSTA(I) + (K-1)*NUMXPT(I)
                        YCOVAL = YCOORD(K,I)
                     END IF
                     IF (NX .EQ. NPPX) THEN
                        WRITE(IOUNIT,9013) YCOVAL,&
                        &(AVEVAL(INDZ+J-1,IGRP,IAVE,ITYP),J=1+NCPP*(NX-1),&
                        &NUMXPT(I))
                     ELSE
                        WRITE(IOUNIT,9013) YCOVAL,&
                        &(AVEVAL(INDZ+J-1,IGRP,IAVE,ITYP),J=1+NCPP*(NX-1),&
                        &NCPP*NX)
                     END IF
                  END DO
               END IF
            END DO
         END DO
      END DO
!        End LOOP Through Networks

      IF (IRSTAT(4).NE.0 .or. IRSTAT(8).NE.0) THEN
!RWB        Include EVALCART receptors with DISCCART receptors.  2/14/95
!           Print Out The Coord. & Concentrations For Discrete Cart Receptors
         INDC = 0
         DO IREC = 1, NUMREC
            IF (RECTYP(IREC) .EQ. 'DC') THEN
               INDC = INDC + 1
               IF (MOD(INDC-1,80) .EQ. 0) THEN
                  CALL HEADER(IOUNIT)
                  WRITE(IOUNIT,9032) CHRAVE(IAVE),(CHIDEP(II,ITYP),&
                  &II=1,6),IHOUR,JDAY,IYR,GRPID(IGRP),(WORKID(K),K=1,INDGRP)
                  WRITE(IOUNIT,9043)
                  WRITE(IOUNIT,9011) CHIDEP(3,ITYP), POLLUT,&
                  &OUTLBL(ITYP)
                  WRITE(IOUNIT,9048) CHIDEP(3,ITYP), CHIDEP(3,ITYP)
               END IF
               IF (MOD(INDC,2) .NE. 0) THEN
                  WRITE(BUF132(1:60),9045) AXR(IREC),AYR(IREC),&
                  &AVEVAL(IREC,IGRP,IAVE,ITYP)
               ELSE
                  WRITE(BUF132(61:120),9045) AXR(IREC),&
                  &AYR(IREC), AVEVAL(IREC,IGRP,IAVE,ITYP)
                  WRITE(IOUNIT,9090) BUF132
                  WRITE(BUF132,9095)
               END IF
            END IF
         END DO
         IF (MOD(INDC,2) .NE. 0) THEN
            WRITE(IOUNIT,9090) BUF132
            WRITE(BUF132,9095)
         END IF
      END IF

      IF (IRSTAT(5) .NE. 0) THEN
!           Print Out The Coord. & Concentrations For Discrete Polar Receptors
         INDC = 0
         DO IREC = 1, NUMREC
            IF (RECTYP(IREC) .EQ. 'DP') THEN
               INDC = INDC + 1
               XRMS = AXR(IREC) - AXS(IREF(IREC))
               YRMS = AYR(IREC) - AYS(IREF(IREC))
               DIST = DSQRT(XRMS*XRMS + YRMS*YRMS)
               DIR  = DATAN2(XRMS, YRMS) * RTODEG
               IF (DIR .LE. 0.0D0) DIR = DIR + 360.0D0
               IF (MOD(INDC-1,80) .EQ. 0) THEN
                  CALL HEADER(IOUNIT)
                  WRITE(IOUNIT,9032) CHRAVE(IAVE), (CHIDEP(II,ITYP),&
                  &II=1,6),IHOUR,JDAY,IYR,GRPID(IGRP),(WORKID(K),K=1,INDGRP)
                  WRITE(IOUNIT,9044)
                  WRITE(IOUNIT,9011) CHIDEP(3,ITYP), POLLUT,&
                  &OUTLBL(ITYP)
                  WRITE(IOUNIT,9049) CHIDEP(3,ITYP), CHIDEP(3,ITYP)
               END IF
               IF (MOD(INDC,2) .NE. 0) THEN
                  WRITE(BUF132(1:65),9047) SRCID(IREF(IREC)),&
                  &DIST, DIR, AVEVAL(IREC,IGRP,IAVE,ITYP)
               ELSE
                  WRITE(BUF132(66:130),9047) SRCID(IREF(IREC)),&
                  &DIST, DIR, AVEVAL(IREC,IGRP,IAVE,ITYP)
                  WRITE(IOUNIT,9090) BUF132
                  WRITE(BUF132,9095)
               END IF
            END IF
         END DO
         IF (MOD(INDC,2) .NE. 0) THEN
            WRITE(IOUNIT,9090) BUF132
            WRITE(BUF132,9095)
         END IF
      END IF

   END DO
!     End Source Group LOOP

9011 FORMAT(/40X,'** ',A4,' OF ',A8,' IN ',A40,' **'/)
9010 FORMAT(66(' -')/)
9013 FORMAT(2X,F10.2,1X,'|',1X,9(F13.5))
9016 FORMAT(3X,' Y-COORD  |',48X,'X-COORD (METERS)')
9017 FORMAT(3X,' (METERS) |',1X,9(1X,F12.2,:))
9018 FORMAT(3X,'DIRECTION |',48X,'DISTANCE (METERS)')
9019 FORMAT(3X,'(DEGREES) |',1X,9(1X,F12.2,:))
9032 FORMAT(20X,'*** CONCURRENT ',A5,1X,6A4,'VALUES',&
   &' ENDING WITH HOUR ',I2,' FOR DAY ',I3,' OF ',I4,' ***'&
   &/24X,'FOR SOURCE GROUP:',1X,A8,&
   &/24X,'INCLUDING SOURCE(S):     ',5(A12,', ',:),&
   &/17X,8(A12,', ',:)/17X,8(A12,', ',:)/17X,8(A12,', ',:))
9037 FORMAT(/35X,'*** NETWORK ID: ',A8,' ;  NETWORK TYPE: ',&
   &A8,' ***')
9043 FORMAT(/45X,'*** DISCRETE CARTESIAN RECEPTOR POINTS ***')
9044 FORMAT(/47X,'*** DISCRETE POLAR RECEPTOR POINTS ***')
9045 FORMAT(6X,2(F12.2,2X),F13.5)
9047 FORMAT(2X,A12,': ',F12.2,2X,F10.2,2X,F13.5)
9048 FORMAT(6X,' X-COORD (M)   Y-COORD (M)        ',A4,&
   &22X,' X-COORD (M)   Y-COORD (M)        ',A4,/65(' -'))
9049 FORMAT(5X,'ORIGIN',59X,'ORIGIN',&
   &/5X,' SRCID         DIST (M)   DIR (DEG)        ',A4,&
   &18X,' SRCID         DIST (M)   DIR (DEG)        ',A4,&
   &/65(' -'))
9090 FORMAT(A132)
9095 FORMAT(132(' '))

   RETURN
END

SUBROUTINE RSDUMP
!***********************************************************************
!                 RSDUMP Module of the AMS/EPA Regulatory Model - AERMOD
!
!        PURPOSE: To Save Intermediate Results Arrays for Later Restart
!
!        PROGRAMMER: Roger Brode, Jeff Wang
!
!        DATE:    March 2, 1992
!
!        MODIFIED:   To incorporate modifications to date processing
!                    for Y2K compliance.  Specifically, to output the
!                    10-digit date variable (FULLDATE) with 4-digit
!                    year for date comparisons.
!                    Also modified to output arrays associated with
!                    post-1997 PM10 processing.
!                    R.W. Brode, PES, Inc., 5/12/99
!
!        MODIFIED:   Changed parameter for specifying the number of
!                    high annual/period averages from NVAL to NHIANN.
!                    R.W. Brode, PES, Inc.,  4/3/98
!
!        MODIFIED:   Changed parameter for specifying the number of
!                    high annual/period averages from NVAL to NHIANN.
!                    R.W. Brode, PES, Inc.,  4/3/98
!
!        INPUTS:  Current Date Variable
!                 Array Limits
!                 Results Arrays
!
!        OUTPUTS: Unformatted File of Intermediate Results
!
!        CALLED FROM:   HRLOOP
!***********************************************************************

!     Variable Declarations
   USE MAIN1
   IMPLICIT NONE
   CHARACTER MODNAM*12

   INTEGER :: I, J, K, L, M

!     Variable Initializations
   MODNAM = 'RSDUMP'
   NDUMP = NDUMP + 1

!     Check for Monthly Averages and Only Dump at End of Month
   IF (MONTH .and. .NOT.ENDMON)  GO TO 1000

   IF (SAVFIL .EQ. SAVFL2 .or. MOD(NDUMP,2) .NE. 0) THEN
      OPEN(UNIT=IDPUNT,ERR=99,FILE=SAVFIL,FORM='UNFORMATTED',&
      &IOSTAT=IOERRN,STATUS='REPLACE')
      WRITE(IDPUNT) FULLDATE, NTOTHRS
      WRITE(IDPUNT) NHIVAL, NMXVAL, NUMREC, NUMGRP, NUMAVE, NUMTYP

      IF (NHIVAL .GT. 0) THEN
         WRITE(IDPUNT) (((((HIVALU(I,J,K,L,M),I=1,NUMREC),J=1,NHIVAL),&
         &K=1,NUMGRP),L=1,NUMAVE),M=1,NUMTYP)
         WRITE(IDPUNT) (((((NHIDAT(I,J,K,L,M),I=1,NUMREC),J=1,NHIVAL),&
         &K=1,NUMGRP),L=1,NUMAVE),M=1,NUMTYP)
         WRITE(IDPUNT) (((((HCLMSG(I,J,K,L,M),I=1,NUMREC),J=1,NHIVAL),&
         &K=1,NUMGRP),L=1,NUMAVE),M=1,NUMTYP)

! ---       Include arrays associated with multi-year processing of high
!           ranked values for 24-hr PM2.5, 1-hr NO2, and 1-hr SO2 NAAQS
         IF (PM25AVE .or. NO2AVE .or. SO2AVE) THEN
            WRITE(IDPUNT) NUMYRS
            WRITE(IDPUNT) (((SUMHNH(I,J,K),I=1,NUMREC),J=1,NUMGRP),&
            &K=1,NHIVAL)
            WRITE(IDPUNT) (((HIMXDLY(I,J,K),I=1,NUMREC),J=1,NUMGRP),&
            &K=1,NHIVAL)
            WRITE(IDPUNT)(((NHIDATMXD(I,J,K),I=1,NUMREC),J=1,NUMGRP),&
            &K=1,NHIVAL)
            WRITE(IDPUNT) ((((HIMXDLY_BYYR(I,J,K,L),I=1,NUMREC),&
            &J=1,NUMGRP),K=1,NHIVAL),L=1,NUMYRS)
            WRITE(IDPUNT) ((((NHIDATMXD_BYYR(I,J,K,L),I=1,NUMREC),&
            &J=1,NUMGRP),K=1,NHIVAL),L=1,NUMYRS)
         END IF

      END IF

      IF (NMXVAL .GT. 0) THEN
         WRITE(IDPUNT) ((((RMXVAL(I,J,K,L),I=1,NMXVAL),J=1,NUMGRP),&
         &K=1,NUMAVE),L=1,NUMTYP)
         WRITE(IDPUNT) ((((MXDATE(I,J,K,L),I=1,NMXVAL),J=1,NUMGRP),&
         &K=1,NUMAVE),L=1,NUMTYP)
         WRITE(IDPUNT) ((((MXLOCA(I,J,K,L),I=1,NMXVAL),J=1,NUMGRP),&
         &K=1,NUMAVE),L=1,NUMTYP)
         WRITE(IDPUNT) ((((MCLMSG(I,J,K,L),I=1,NMXVAL),J=1,NUMGRP),&
         &K=1,NUMAVE),L=1,NUMTYP)
      END IF

      IF (SEASONHR) THEN
         WRITE(IDPUNT) (((((SHVALS(I,J,K,L,M),I=1,NUMREC),&
         &J=1,NUMGRP),K=1,4),L=1,24),M=1,NUMTYP)
         WRITE(IDPUNT) ((NSEAHR(I,J),I=1,4),J=1,24)
         WRITE(IDPUNT) ((NSEACM(I,J),I=1,4),J=1,24)
      END IF

      IF (PERIOD) THEN
         WRITE(IDPUNT) IANHRS, IANCLM, IANMSG, NUMYRS
         WRITE(IDPUNT) (((ANNVAL(I,J,K),I=1,NUMREC),J=1,NUMGRP),&
         &K=1,NUMTYP)
         IF (MULTYR) THEN
            WRITE(IDPUNT) (((AMXVAL(I,J,K),I=1,NHIANN),J=1,NUMGRP),&
            &K=1,NUMTYP)
            WRITE(IDPUNT) (((IMXLOC(I,J,K),I=1,NHIANN),J=1,NUMGRP),&
            &K=1,NUMTYP)
         END IF
      ELSE IF (ANNUAL) THEN
         WRITE(IDPUNT) IANHRS, IANCLM, IANMSG, NUMYRS
         WRITE(IDPUNT) (((ANNVAL(I,J,K),I=1,NUMREC),J=1,NUMGRP),&
         &K=1,NUMTYP)
         WRITE(IDPUNT) (((SUMANN(I,J,K),I=1,NUMREC),J=1,NUMGRP),&
         &K=1,NUMTYP)
      END IF

      CLOSE (IDPUNT)

   ELSE
      OPEN(UNIT=IDPUN2,ERR=99,FILE=SAVFL2,FORM='UNFORMATTED',&
      &IOSTAT=IOERRN,STATUS='REPLACE')
      WRITE(IDPUN2) FULLDATE, NTOTHRS
      WRITE(IDPUN2) NHIVAL, NMXVAL, NUMREC, NUMGRP, NUMAVE, NUMTYP

      IF (NHIVAL .GT. 0) THEN
         WRITE(IDPUN2) (((((HIVALU(I,J,K,L,M),I=1,NUMREC),J=1,NHIVAL),&
         &K=1,NUMGRP),L=1,NUMAVE),M=1,NUMTYP)
         WRITE(IDPUN2) (((((NHIDAT(I,J,K,L,M),I=1,NUMREC),J=1,NHIVAL),&
         &K=1,NUMGRP),L=1,NUMAVE),M=1,NUMTYP)
         WRITE(IDPUN2) (((((HCLMSG(I,J,K,L,M),I=1,NUMREC),J=1,NHIVAL),&
         &K=1,NUMGRP),L=1,NUMAVE),M=1,NUMTYP)

! ---       Include arrays associated with multi-year processing of high
!           ranked values for 24-hr PM2.5, 1-hr NO2, and 1-hr SO2 NAAQS
         IF (PM25AVE .or. NO2AVE .or. SO2AVE) THEN
            WRITE(IDPUN2) NUMYRS
            WRITE(IDPUN2) (((SUMHNH(I,J,K),I=1,NUMREC),J=1,NUMGRP),&
            &K=1,NHIVAL)
            WRITE(IDPUN2) (((HIMXDLY(I,J,K),I=1,NUMREC),J=1,NUMGRP),&
            &K=1,NHIVAL)
            WRITE(IDPUN2)(((NHIDATMXD(I,J,K),I=1,NUMREC),J=1,NUMGRP),&
            &K=1,NHIVAL)
            WRITE(IDPUN2) ((((HIMXDLY_BYYR(I,J,K,L),I=1,NUMREC),&
            &J=1,NUMGRP),K=1,NHIVAL),L=1,NUMYRS)
            WRITE(IDPUN2) ((((NHIDATMXD_BYYR(I,J,K,L),I=1,NUMREC),&
            &J=1,NUMGRP),K=1,NHIVAL),L=1,NUMYRS)
         END IF

      END IF

      IF (NMXVAL .GT. 0) THEN
         WRITE(IDPUN2) ((((RMXVAL(I,J,K,L),I=1,NMXVAL),J=1,NUMGRP),&
         &K=1,NUMAVE),L=1,NUMTYP)
         WRITE(IDPUN2) ((((MXDATE(I,J,K,L),I=1,NMXVAL),J=1,NUMGRP),&
         &K=1,NUMAVE),L=1,NUMTYP)
         WRITE(IDPUN2) ((((MXLOCA(I,J,K,L),I=1,NMXVAL),J=1,NUMGRP),&
         &K=1,NUMAVE),L=1,NUMTYP)
         WRITE(IDPUN2) ((((MCLMSG(I,J,K,L),I=1,NMXVAL),J=1,NUMGRP),&
         &K=1,NUMAVE),L=1,NUMTYP)
      END IF

      IF (SEASONHR) THEN
         WRITE(IDPUN2) (((((SHVALS(I,J,K,L,M),I=1,NUMREC),&
         &J=1,NUMGRP),K=1,4),L=1,24),M=1,NUMTYP)
         WRITE(IDPUN2) ((NSEAHR(I,J),I=1,4),J=1,24)
         WRITE(IDPUN2) ((NSEACM(I,J),I=1,4),J=1,24)
      END IF

      IF (PERIOD) THEN
         WRITE(IDPUN2) IANHRS, IANCLM, IANMSG, NUMYRS
         WRITE(IDPUN2) (((ANNVAL(I,J,K),I=1,NUMREC),J=1,NUMGRP),&
         &K=1,NUMTYP)
         IF (MULTYR) THEN
            WRITE(IDPUN2) (((AMXVAL(I,J,K),I=1,NHIANN),J=1,NUMGRP),&
            &K=1,NUMTYP)
            WRITE(IDPUN2) (((IMXLOC(I,J,K),I=1,NHIANN),J=1,NUMGRP),&
            &K=1,NUMTYP)
         END IF
      ELSE IF (ANNUAL) THEN
         WRITE(IDPUN2) IANHRS, IANCLM, IANMSG, NUMYRS
         WRITE(IDPUN2) (((ANNVAL(I,J,K),I=1,NUMREC),J=1,NUMGRP),&
         &K=1,NUMTYP)
         WRITE(IDPUN2) (((SUMANN(I,J,K),I=1,NUMREC),J=1,NUMGRP),&
         &K=1,NUMTYP)
      END IF

      CLOSE (IDPUN2)

   END IF

   GO TO 1000

99 CALL ERRHDL(PATH,MODNAM,'E','500','SAVEFILE')
   RUNERR = .TRUE.

1000 RETURN
END


SUBROUTINE EVLINI
!***********************************************************************
!                 EVLINI Module of AERMOD
!
!        PURPOSE: Initialize ARC Values for EVALFILE Option
!
!        PROGRAMMER: Roger Brode
!
!        DATE:    November 29, 1993
!
!        MODIFIED:   To include initializations for SZMAX(:) and
!                    HSBLMX(:) arrays.
!                    R.W. Brode, U.S. EPA/OAQPS/AQMG, 02/28/2011
!
!        REVISIONS:  Added ARCCL() variable for true centerline
!                    calculations.  Changed 7/25/94, R.F. Lee.
!
!        INPUTS:
!
!        OUTPUTS:
!
!        CALLED FROM:   PCALC
!***********************************************************************

!     Variable Declarations
   USE MAIN1
   IMPLICIT NONE
   CHARACTER MODNAM*12

!     Variable Initializations
   MODNAM = 'EVLINI'

   ARCMAX(:) = 0.0D0
!CRFL
!CRFL  Add true centerline calculations:  add ARCCL(I)
!CRFL  Changed 7/25/94, R.F. Lee
!CRFL
   ARCCL(:)  = 0.0D0
   QMAX(:)   = 0.0D0
   DXMAX(:)  = 0.0D0
   UMAX(:)   = 0.0D0
   U3MAX(:)  = 0.0D0
   SVMAX(:)  = 0.0D0
   SWMAX(:)  = 0.0D0
   SYMAX(:)  = 0.0D0
   SY3MX(:)  = 0.0D0
   SZMAX(:)  = 0.0D0
   HEMAX(:)  = 0.0D0
   CHIDMW(:) = 0.0D0
   CHINMW(:) = 0.0D0
   CHI3MW(:) = 0.0D0
   CHIDML(:) = 0.0D0
   CHINML(:) = 0.0D0
   CHI3ML(:) = 0.0D0
   HSBLMX(:) = 0.0D0

   RETURN
END

SUBROUTINE EVALCK
!***********************************************************************
!                 EVALCK Module of AERMOD
!
!        PURPOSE: Check ARC Values for EVALFILE Option
!
!        PROGRAMMER: Roger Brode
!
!        DATE:    November 29, 1993
!
!        REVISIONS:  Added true centerline calculations.
!                    Changed 7/25/94, R.F. Lee.
!
!        INPUTS:
!
!        OUTPUTS:
!
!        CALLED FROM:   PCALC
!***********************************************************************

!     Variable Declarations
   USE MAIN1
   IMPLICIT NONE
   CHARACTER MODNAM*12

   INTEGER :: INDEX
   DOUBLE PRECISION :: CHIOQ

!     Variable Initializations
   MODNAM = 'EVALCK'

!     Set ARC Index
   INDEX = NDXARC(IREC)

!     Check for INDEX = 0, i.e., this receptor is not an EVALCART receptor.
!     Skip to RETURN for INDEX = 0
   IF (INDEX .EQ. 0) GO TO 99

!CRFL
!CRFL  Add true centerline calculations:  add CHIOQC
!CRFL  Change made 7/25/94, R.F. Lee.
!CRFL
!     Calculate Normalized Concentration, CHI/Q
   CHIOQ = HRVAL(1)/(QTK*EMIFAC(1))
!     Check ARCMAX Array
   IF (CHIOQ .GT. ARCMAX(INDEX)) THEN
      ARCMAX(INDEX) = CHIOQ
!CRFL
!CRFL  Add true centerline calculations:  add arc centerline
!CRFL  calculation ARCCL(INDEX).  Note that, although ARCCL is
!CRFL  is calculated redundantly for all receptors in the arc,
!CRFL  the value calculated at the receptor showing the max is
!CRFL  used.  This assures that the most reasonable downwind
!CRFL  distance will be used in the calculation.
!CRFL  Changed 7/25/94, R.F. Lee.
!CRFL
      ARCCL(INDEX) = CHIOQ
      QMAX(INDEX)  = QTK * EMIFAC(1)
      DXMAX(INDEX) = DISTR
!RJP
!RJP     Use appropriate effective parameters
!RJP
      IF( STABLE .or. (UNSTAB .and. (HS .GE. ZI) ) )  THEN
         UMAX(INDEX)   = UEFF
         SVMAX(INDEX)  = SVEFF
         SWMAX(INDEX)  = SWEFF
         SYMAX(INDEX)  = SY
         HEMAX(INDEX)  = HE
!crfl 5/19/95 Grab SZ at maximum receptor in arc
         SZMAX(INDEX)  = SZ
         CHIDMW(INDEX) = 0.0D0
         CHINMW(INDEX) = 0.0D0
         CHI3MW(INDEX) = 0.0D0
         CHIDML(INDEX) = 0.0D0
         CHINML(INDEX) = 0.0D0
         CHI3ML(INDEX) = 0.0D0
         HSBLMX(INDEX) = HSBL
      ELSE IF (PPF .GT. 0.999D0) THEN
         UMAX(INDEX)   = UEFFD
         U3MAX(INDEX)  = UEFF3
         SVMAX(INDEX)  = SVEFF3
         SWMAX(INDEX)  = SWEFF3
         SYMAX(INDEX)  = SY
         SY3MX(INDEX)  = SY3
         HEMAX(INDEX)  = HSP + DHP3
         CHIDMW(INDEX) = CHIDW/QMAX(INDEX)
         CHINMW(INDEX) = CHINW/QMAX(INDEX)
         CHI3MW(INDEX) = CHI3W/QMAX(INDEX)
         CHIDML(INDEX) = CHIDL/QMAX(INDEX)
         CHINML(INDEX) = CHINL/QMAX(INDEX)
         CHI3ML(INDEX) = CHI3L/QMAX(INDEX)
         HSBLMX(INDEX) = HPEN
      ELSE
         UMAX(INDEX)   = UEFFD
         U3MAX(INDEX)  = UEFF3
         SVMAX(INDEX)  = SVEFFD
         SWMAX(INDEX)  = SWEFFD
         SYMAX(INDEX)  = SY
         SY3MX(INDEX)  = SY3
         HEMAX(INDEX)  = HSP + DHP1
         CHIDMW(INDEX) = CHIDW/QMAX(INDEX)
         CHINMW(INDEX) = CHINW/QMAX(INDEX)
         CHI3MW(INDEX) = CHI3W/QMAX(INDEX)
         CHIDML(INDEX) = CHIDL/QMAX(INDEX)
         CHINML(INDEX) = CHINL/QMAX(INDEX)
         CHI3ML(INDEX) = CHI3L/QMAX(INDEX)
         HSBLMX(INDEX) = HPEN
      END IF
   END IF

99 RETURN
END

SUBROUTINE EVALFL
!***********************************************************************
!                 EVALFL Module of AERMOD
!
!        PURPOSE: Output ARC Values for EVALFILE Option
!
!        PROGRAMMER: Roger Brode
!
!        DATE:    November 29, 1993
!
!        REVISIONS:  Added true centerline calculations.
!                    Changed 7/25/94, R.F. Lee.
!
!        INPUTS:
!
!        OUTPUTS:
!
!        CALLED FROM:   PCALC
!***********************************************************************

!     Variable Declarations
   USE MAIN1
   IMPLICIT NONE
   CHARACTER MODNAM*12
!     JAT 7/22/21 D065 UOUST AND SVOU NOT USED
!      DOUBLE PRECISION :: CWIC, CWICN, CWICW, CWICL, UOUST, SVOU, HEOZI,
   DOUBLE PRECISION :: CWIC, CWICN, CWICW, CWICL, HEOZI,&
!     JAT 7/22/21 D065 ZIOL AND UOWST NOT USED
!     &        ZIOL, FSTAR, UOWST, XNDIM, PWSTAR, UOUT, SYOUT, OBUOUT
   &FSTAR, XNDIM, PWSTAR, UOUT, SYOUT, OBUOUT
   INTEGER :: I

!     Variable Initializations
   MODNAM = 'EVALFL'

!     LOOP Through ARCs
   DO I = 1, NUMARC
!C
!C   Changes dated 2/25/94, 3/2/94, 3/4/94, 3/8/94, 3/9/94, 3/14/94,
!C     and 4/20/94
!C     by Russ Lee, to add Bowen Ratio and additional parameters.
!C
!        Calculate Crosswind Integrated Concentration, CWIC
!CRFL
!CRFL  "ARCMAX" was changed to ARCCL in the following statement to
!CRFL  give a "true" CWIC.  Changed 7/25/94, R.F. Lee.
!CRFL
!RWB         CWIC = SRT2PI * SYMAX(I) * ARCCL(I)
!RWB     Modify CWIC to be sum of CWIC's of individual "plumes".  2/13/95
!RWB     Note that WRAP and LIFT components are included in ARCCL.
      IF( STABLE .or. (UNSTAB .and. HS.GE.ZI) )THEN
         CWIC  = SRT2PI * SYMAX(I) * ARCCL(I)
!           Now calculate CWIC with U*ZI normalization,
!           using maximum of HE & ZI, instead of ZI.
         CWICN = CWIC * UMAX(I) * MAX( HEMAX(I), ZI)

      ELSE
!           Calculate WRAP and LIFT components of CWIC
!           First calculate CWIC without U*ZI normalization.
!           Note that the CHIDM_, CHINM_ and CHI3M_ terms have already been
!           normalized by QTK.
         CWICW = SRT2PI * SYMAX(I) * (CHIDMW(I)+CHINMW(I)) +&
         &SRT2PI * SY3MX(I) *  CHI3MW(I)

         CWICL = SRT2PI * SYMAX(I) * (CHIDML(I)+CHINML(I)) +&
         &SRT2PI * SY3MX(I) *  CHI3ML(I)
!           Combine WRAP and LIFT components. Include decay and normalization.
         CWIC  = (FOPT * CWICW + (1.0D0-FOPT) * CWICL) * D

!           Calculate WRAP and LIFT components of CWIC
!           Now calculate CWIC with U*ZI normalization.
!           Use HPEN (=MAX(HE3,ZI)) for penetrated source instead of ZI.
         CWICW = SRT2PI * SYMAX(I)*UMAX(I)*ZI*(CHIDMW(I)+CHINMW(I)) +&
         &SRT2PI * SY3MX(I)*U3MAX(I)*HPEN*CHI3MW(I)

         CWICL = SRT2PI * SYMAX(I)*UMAX(I)*ZI*(CHIDML(I)+CHINML(I)) +&
         &SRT2PI * SY3MX(I)*U3MAX(I)*HPEN*CHI3ML(I)
!           Combine WRAP and LIFT components. Include decay and normalization.
         CWICN = (FOPT * CWICW + (1.0D0-FOPT) * CWICL) * D

      END IF

!        Calculate U/Ustar
!     JAT 7/22/21 D065 UOUST AND SVOU NOT USED
!         IF (USTAR .GE. 1.0D-10) THEN
!            UOUST = UMAX(I) / USTAR
!         ELSE
!            UOUST = -999.0D0
!         END IF

!        Calculate sigma-v / U
!         IF (UMAX(I) .GE. 1.0D-10) THEN
!            SVOU = SVMAX(I)/UMAX(I)
!         ELSE
!            SVOU = -999.0D0
!         END IF

!        Calculate He / Zi
      IF (ZI .GE. 1.0D-10) THEN
         HEOZI = HEMAX(I) / ZI
      ELSE
         HEOZI = -999.0D0
      END IF

!        Calculate Zi / L
!     JAT 7/22/21 D065 ZIOL NOT USED
!         IF (DABS(OBULEN) .GE. 1.0D-10) THEN
!            ZIOL = ZI / OBULEN
!         ELSE
!            ZIOL = 999.0D0
!         END IF

!RWBC      Calculate total F
!RWB       FTOT = FB + FM
!RWB     Replace FTOT with FSTAR (non-dimensional buoyancy flux).  2/13/95
!RWB     Note that UP is the latest value for plume rise wind speed
!RWB     from the iterative stable plume rise.
      IF (WSTAR .GE. 1.0D-10) THEN
         FSTAR = FB / (UP * WSTAR * WSTAR * ZI)
      ELSE
         FSTAR = -999.0D0
      END IF

      IF (OBULEN .LT. 0.0D0) THEN

!           Calculate U / WSTAR when L < 0
!         JAT 7/22/21 D065 UOWST NOT USED
!            IF (WSTAR .GE. 1.0D-10) THEN
!               UOWST = UMAX(I) / WSTAR
!            ELSE
!               UOWST = -999.0D0
!            END IF

!           Calculate nondimensional distance when L < 0
         IF (UMAX(I) .GE. 1.0D-10 .and. ZI .GE. 1.0D-10) THEN
            XNDIM = DXMAX(I) * WSTAR / (UMAX(I) * ZI)
         ELSE
            XNDIM = -999.0D0
         END IF
!crfl 5/18/95 When unstable, put WSTAR into PWSTAR variable to be printed.
         PWSTAR = WSTAR

      ELSE

!           Set UOWST and XNDIM to -999 when L >= 0
!         JAT 7/22/21 D065 UOWST NOT USED
!            UOWST = -999.0D0
         XNDIM = -999.0D0
!crfl 5/18/95 When stable, put Sigma-Z into PWSTAR variable to be printed.
         PWSTAR = SZMAX(I)
      END IF

!CRFL
!CRFL  Added ARCCL(I), arc true centerline concentration for the arc.
!CRFL  Change made 7/25/94, R.F. Lee.
!CRFL
!RWB           WRITE(IELUNT(ISRC),9000) SRCID(ISRC), KURDAT, ARCID(I),
!RWB     &                      ARCMAX(I), QMAX(I), CWIC,
!RWB     &                      DXMAX(I), UMAX(I), SVMAX(I),
!RWB     &                      SWMAX(I), SYMAX(I), HEMAX(I),
!RWB     &                      OBULEN, ZI, USTAR, WSTAR, FB, FM,
!RWB     &                      BOWEN, UOUST, SVOU, ZIOL, UOWST, XNDIM,
!RWB     &                      HEOZI, FTOT, AHS(ISRC), ARCCL(I), DOPTS
!RWBCRWB                        Added DOPTS, Developmental Options (C*10)

!RWB     Modified to output CHI's for individual "plumes".  2/13/95
!RWB     First select appropriate sigma-y to print out. Use SY3 for mostly
!RWB     penetrated plumes.
      IF (UNSTAB .and. HS.LT.ZI .and. PPF.GT.0.999D0) THEN
         UOUT  = U3MAX(I)
         SYOUT = SY3MX(I)
      ELSE
         UOUT  = UMAX(I)
         SYOUT = SYMAX(I)
      END IF

      IF (URBSTAB) THEN
         OBUOUT = URBOBULEN(IURB)
      ELSE
         OBUOUT = OBULEN
      END IF


!crfl 5/18/95 Changed WSTAR to PWSTAR so I could output another variable
!crfl         (Sigma-Z) in stable conditions without upsetting WSTAR.
      WRITE(IELUNT(ISRC),9000) SRCID(ISRC), KURDAT, ARCID(I),&
      &ARCMAX(I), QMAX(I), CWIC, CWICN,&
      &DXMAX(I), UOUT, SVMAX(I),&
      &SWMAX(I), SYOUT, HEMAX(I),&
      &OBUOUT, ZI, USTAR, PWSTAR, FB, FM,&
      &BOWEN, PPF, CHIDML(I), CHINML(I), CHI3ML(I),&
      &XNDIM, HEOZI, FSTAR, AHS(ISRC), ARCCL(I),&
      &AFV, HSBLMX(I)
!RWB                      Added Flow Vector, AFV
!RWB                      Added height of effective reflecting surface, HSBLMX


   END DO
!RCO remove line breaks 9/2/21
9000 FORMAT(1X,A12,1X,I8.8,1X,A8,4(1X,G12.6),&
   &9X,6(1X,G12.4),9X,6(1X,G12.4),&
   &9X,6(1X,G12.4),9X,4(1X,G12.4),1X,'0000000000',&
   &1X,G12.4,1X,G12.4)
!C   End of changes dated 2/25/94 through 3/14/94 by Russ Lee
!C

   RETURN
END

SUBROUTINE MXDYBYYR(N)
!***********************************************************************
!                 MXDYBYYR Module of the AMS/EPA Regulatory Model - AERMOD
!
!        PURPOSE: Write Maximum Daily 1-hour Values for the Year to
!                 a file by rank
!
!        PROGRAMMER: Roger Brode
!
!        DATE:       February 28, 2011
!
!        INPUTS:  MAXDAILY file options
!                 Array of maximum daily 1-hour values
!
!        OUTPUTS:
!
!        CALLED FROM:   HRLOOP
!***********************************************************************

!     Variable Declarations
   USE MAIN1
   IMPLICIT NONE

   INTEGER :: I, N
   INTEGER :: IDEC, IMOD
!     JAT 7/22/21 D065 ICHR NOT USED
!      INTEGER :: ICYR, ICMN, ICDY, ICHR, ICDAT, ICDAT8, ICJDY
   INTEGER :: ICYR, ICMN, ICDY, ICDAT, ICDAT8, ICJDY
   CHARACTER RANK(10)*5, CHRVAL*5
   CHARACTER MODNAM*12
! Unused:       INTEGER :: J
! Unused:       CHARACTER PERCHR*6

!     Variable Initializations
   DATA (RANK(I),I=1,10) /'  1ST','  2ND','  3RD','  4TH','  5TH',&
   &'  6TH','  7TH','  8TH','  9TH',' 10TH'/
   MODNAM = 'MXDYBYYR'
   ICDAT  = 0

! --- Assign character label for rank
   IF (N .LE. 10) THEN
      CHRVAL = RANK(N)
   ELSE IF (MOD(N,100) .GT. 10 .and.&
   &MOD(N,100) .LT. 20) THEN
      IDEC = INT(N/10)
      IMOD = MOD(N,10)
      WRITE(CHRVAL,'(I2,I1,"TH")') IDEC, IMOD
   ELSE IF (N .LE. 999) THEN
      IDEC = INT(N/10)
      IMOD = MOD(N,10)
      IF (IMOD .EQ. 0) IMOD = 10
      WRITE(CHRVAL,'(I2,A3)') IDEC, RANK(IMOD)(3:5)
   END IF

!     Begin Source Group LOOP
   DO IGRP = 1, NUMGRP
!        Check for MXDYBYYR Option for This IGRP
      IF (IMXDLY_BYYR(IGRP) .EQ. 1) THEN

         RECEPTOR_LOOP: DO IREC = 1, NUMREC
            ITYP = 1
!              WRITE Results to Formatted Plot File
!              Begin Receptor LOOP

! ---          Extract date information
            ICDAT8 = NHIDATMXD(IREC,IGRP,N)
            ICYR   = ICDAT8/1000000

!            D001 Call LONG_DATE and determine the longform date Wood 9/15/22
            CALL LONG_DATE(ICDAT8,ICDAT,ICYR,ICYR)
! ---  D001 remove original calculation of year Wood 9/15/22
!               IF (ICYR .GE. ISTRT_WIND .and. ICYR .LE. 99) THEN
!                  ICYR  = ISTRT_CENT*100 + ICYR
!                  ICDAT = ISTRT_CENT*100000000 + ICDAT8
!               ELSE IF (ICYR .LT. ISTRT_WIND) THEN
!                  ICYR  = (ISTRT_CENT+1)*100 + ICYR
!                  ICDAT = (ISTRT_CENT+1)*100000000 + ICDAT8
!               END IF

            ICMN = (ICDAT/10000) - (ICDAT/1000000)*100
            ICDY = (ICDAT/100) - (ICDAT/10000)*100
!             JAT 7/22/21 D065 ICHR NOT USED
!               ICHR =  ICDAT - (ICDAT/100)*100
!              Calculate JULIAN Day for Start and End Dates
!              but first make sure date variables are not 0
            IF (ICMN.GT.0 .and. ICDY.GT.0) THEN
               CALL JULIAN (ICYR,ICMN,ICDY,ICJDY)
            ELSE
               ICJDY = 0
            END IF

! ---          Write MXDYBYYR values to file
            WRITE(IMDUNT_BYYR(IGRP),MXDFRM,ERR=99)&
            &AXR(IREC), AYR(IREC), HIMXDLY(IREC,IGRP,N),&
            &AZELEV(IREC), AZHILL(IREC), AZFLAG(IREC),CHRVAL,&
            &GRPID(IGRP), ICJDY,&
            &NHIDATMXD(IREC,IGRP,N)-(NHIDATMXD(IREC,IGRP,N)/100)*100,&
            &NHIDATMXD(IREC,IGRP,N), NETID(IREC)

         END DO RECEPTOR_LOOP
      END IF
   END DO
!     End Source Group LOOP

   GO TO 999

!     WRITE Error Message for Problem Writing to MXDYBYYR File
99 WRITE(DUMMY,'("MXDLY",I3.3)') IMDUNT_BYYR(IGRP)
   CALL ERRHDL(PATH,MODNAM,'E','520',DUMMY)
   RUNERR = .TRUE.

999 RETURN
END

