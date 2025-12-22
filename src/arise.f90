SUBROUTINE AFLUXES
!***********************************************************************
!      AFLUXES Module of the AMS/EPA Regulatory Model - AERMOD
!
!        PURPOSE:    Compute Buoyancy Flux for Aircraft Sources
!                    (Area/Volume Sources)
!
!        PROGRAMMER: Gavendra Pandey, UNC-IE, Chapel Hill, NC, USA &
!                    Akula Venkatram, UC-Riverside, CA, USA &
!                    Sarav Arunachalam, UNC-IE, Chapel Hill, NC, USA.
!
!        DATE:    April 01, 2023
!
!        INPUTS:  Met Variables:
!                    Ambient temperature (K), TA
!                 Engine Parameters:
!                    Fuel Burn Rate (kg/s), MFUEL
!                    Thrust (N), THRUST
!                    Aircraft Velocity (m/s), VAA
!                    Air Fuel Ratio, AFR
!                    By-Pass Ratio, BYPR
!                    Rated Power (kilo watt), RPWR
!                 Constants:
!                    Acceleration due to Gravity (m/s), G = 9.81
!                    Ambient Pressure (N/m2), PAA = 1.013e05
!                    Gas Constant of Air (J/kgK), RAA = 287
!                    Specific Heat of Exhaust Gases (J/kgK), CPA = 1003
!                    Heating Value of Fuel (J/kg), HFUEL = 43e06
!                    Power Setting, PWST = 7% (Idle), 30% (Approach),
!                                   PWST = 85% (Climb-out), 100% (Take-off)
!                    Power setting is based on the Wayson et al. (2009)
!
!        OUTPUTS: Buoyancy Flux, FB (m4/s3)
!
!        CALLED FROM:   ARISE
!
!        REFERENCE: Equations are from Pandey et al., (2023) (In Review),
!                   Accounting for Plume Rise of Aircraft Emissions in AERMOD
!***********************************************************************

!     Variable Declarations
   USE MAIN1
   IMPLICIT NONE
   CHARACTER MODNAM*12
!     Local Variables:

!      DOUBLE PRECISION  :: MFUEL, VAA, THRUST, MAIR, AFR, BYPR, RPWR
   DOUBLE PRECISION  :: MAIR, VE, VAVG
   DOUBLE PRECISION  :: VEMAX, THRSTMX
   DOUBLE PRECISION  :: QEE, TE, RHOE, ETAT, PWST, RPWRW

!     Fixed VALUES
!      DOUBLE PRECISION, PARAMETER  ::
!     &                     PAA = 1.013D+5, RAA = 287.058D0,
!     &                     HFUEL = 4.3D+7, CPA = 1003.0D0

!     Variable Initializations
   MODNAM = 'AFLUXES'

   PI = 4.0D0 * DATAN(1.0D0)

!     For Turbofan and Turbojet Engines
   IF(BYPR .GT. 0.0D0) THEN
!     Check for Positive Fuel burn rate and Thrust
      IF (MFUEL .GT. 0.0D0 .AND. THRUST .GT. 0.0D0) THEN
         MAIR = MFUEL * AFR * (1.D0 + BYPR)                             ! Total Mass Flow (Equation 22)
         VE   = VAA + (THRUST / MAIR)                                   ! Exhaust Velocity (Equation 24)
         TE   = (HFUEL*MFUEL/MAIR-((VE*VE)-(VAA*VAA))/2.0D0)/CPA + TA   ! Exhaust Temperature (By substituting eqns 22 and 27 in eqn 21)
         QEE  = MAIR * CPA * (TE - TA)                                  ! Thermal Power/Heat Rejection (Equation 27)
      ELSE
         TE   = 1.0D-10
         QEE  = 1.0D-10
      END IF
!     For Non-Turbofan/Turbojet Engines or Shaft-based Engines,
!     Turboprop, Turboshaft, Piston, and Helicopter
!     PWST is based on Table 4 of Wayson et al.(2009)
   ELSE IF (BYPR .EQ. -999.D0) THEN
      IF (AFR .EQ. 106.D0) THEN
         PWST = 0.07D0                                               ! Idle/Taxi
      ELSE IF (AFR .EQ. 83.D0) THEN
         PWST = 0.30D0                                               ! Approach/Landing
      ELSE IF (AFR .EQ. 51.D0) THEN
         PWST = 0.85D0                                               ! Climb-out
      ELSE
         PWST = 1.0D0                                                ! Take-Off
      END IF
!     kilo-Watt to Watt Conversion for Rated Power
      RPWRW = 1000.0D0*RPWR

      ETAT = ( PWST * RPWRW ) / ( MFUEL * HFUEL )                   ! Thermal Efficiency (Equation 30)
      QEE =  MFUEL * HFUEL * (1.D0 - ETAT)                          ! Thermal Power (Equation 31)
      TE = TA + (((1.D0 - ETAT) * HFUEL) / (AFR * CPA))             ! Exhaust Temperature (Equation 32)
   ELSE
      TE  = 1.0D-10
      QEE  = 1.0D-10
   END IF

   IF (TE .EQ. 1.0D-10) THEN
      RHOE = 1.0D-10
   ELSE
      RHOE = PAA / (RAA * TE)                                        ! Exhaust Density (Equation 26 or 33)
   END IF

   IF (QEE .LT. 0.0D0 .OR. QEE .EQ. 1.0D-10) THEN
      FB = 1.0D-10
   ELSE
      FB = G * QEE / (PI * RHOE * CPA * TA)                          ! Buoyancy Flux (Equation 5 or 25)
   END IF

   RETURN
END


SUBROUTINE MOMENTUM_PLUMERISE (XARG)
!***********************************************************************
!      AIRCRAFT_MOMENTUM Module of the AMS/EPA Regulatory Model - AERMOD
!
!        PURPOSE: Calculate Momentum Plume Rise for Aircraft Sources.
!
!        PROGRAMMER: Gavendra Pandey, UNC-IE, Chapel Hill, NC, USA &
!                    Akula Venkatram, UC-Riverside, CA, USA &
!                    Sarav Arunachalam, UNC-IE, Chapel Hill, NC, USA.
!
!        DATE:    April 01, 2023
!
!        INPUTS:  Met Variables:
!                    Ambient temperature (K), TA
!                    Wind Speed (m/s), UEFFA
!                    Lateral Wind Velocity Fluctuation (m/s), SVEFFA
!                 Engine Parameters:
!                    Thrust (N), THRUST
!                 Constants:
!                    Ambient Pressure (N/m2), PAA
!                    Gas Constant of Air (J/kgK), RAA = 287.058
!                    Momentum Entrainment Constant, ALPHAM = 0.10
!                    Initial Plume Radius (m), R00 = 2
!
!        OUTPUTS: Momentum Plume Rise, HPM;
!                 Momentum Plume Displacement for Airborne Sources, HDISP;
!                 Final Plume Radius, RP0;
!                 Maximum Distance, XMAXX
!
!        CALLED FROM:   ADELTAH
!
!        REFERENCE: Equations are from Pandey et al., (2023) (In Review),
!                   Accounting for Plume Rise of Aircraft Emissions in AERMOD
!***********************************************************************

!     Variable Declarations
   USE MAIN1
   IMPLICIT NONE
   CHARACTER MODNAM*20
!     Local Variables:

   DOUBLE PRECISION  :: RHOA, RMAX, XARG, XPMAX, XMAXX
   DOUBLE PRECISION  :: RP01, RP02, XP, SRCANGLEU
   DOUBLE PRECISION  :: VEMAX, THRSTMX, RATT,UEFFA,SVEFFA

!     Fixed VALUES
!      DOUBLE PRECISION, PARAMETER  ::
!     &                     PAA = 1.013D+5, RAA = 287.058D0,
!     &                     R00 = 2.0D0, ALPHAM = 0.10D0,
!     &                     HFUEL = 4.3D+7


!     Variable Initializations
   MODNAM = 'MOMENTUM_PLUMERISE'

!     Set initial effective parameters
   UEFFA  = UP
   SVEFFA = SVP

   RP0 = 0.0D0                                                       ! Initialize the Maximum Plume Radius
   HPM = 0.0D0                                                       ! Initialize the Momentum Plume Rise
   HDISP = 0.0D0                                                     ! Initialize the Momentum Plume Rise for Airborne Sources

!     For turbofan engines
   IF (BYPR .GT. 0.0D0) THEN
!      Check for positive thrust
      IF (THRUST .GT. 0.0D0) THEN
!      Calculation of maximum values of exhaust velocity and thrust to
!      avoid inconsistency between thrust and fuel burn rate
         VEMAX   = SQRT(2.0D0*HFUEL/(AFR*(1.D0+BYPR))+(VAA*VAA))        ! Maximum Exhaust Velocity
         THRSTMX = MFUEL*AFR*(1.D0+BYPR)*(VEMAX-VAA)                    ! Maximum Thrust

         RATT = THRUST/THRSTMX                                          ! Thrust Ratio

!     For inconsistent thrust and fuel burn values
         IF (RATT .GT. 1.0D0 ) THEN
            THRUST = THRSTMX
         ELSE
            THRUST = THRUST
         END IF

         RHOA  = PAA / (RAA * TA)                                       ! Air Density
         RMAX  = SQRT(THRUST/(PI*RHOA*((VAA+UEFFA)+SVEFFA)*SVEFFA))     ! Maximum Plume Radius (Equation 11)
         XMAXX = (ABS(RMAX - R00)) / ALPHAM                             ! Distance for RMAX (Equation 13)

         IF ( XARG .LE. XMAXX) THEN
            RP0 = R00 + (ALPHAM * XARG/2.0D0)                          ! Final Plume Radius (Equation 15)
            HPM = R00 + (ALPHAM * XARG)                                ! Momentum Plume Rise (Equation 12)
         ELSE
            RP01 = (XMAXX / XARG) * (R00 + (ALPHAM * XMAXX/2.0D0))
            RP02 = RMAX * (1.0D0 - (XMAXX/XARG))
            RP0 = RP01 + RP02                                          ! Final Plume Radius (Equation 15)
            HPM = RMAX                                                 ! Momentum Plume Rise (Equation 12)
         END IF

!        For Airborne Sources, calculate plume displacement using
!        source angle and xpmax
         IF (HS .GT. 12.0D0) THEN
            XPMAX = (RP0 - R00) / ALPHAM
!           Set the minimum value for the source angle
            IF (SRCANGLE .EQ. 0.0D0) THEN
               SRCANGLEU = 0.01
            ELSE
               SRCANGLEU = SRCANGLE
            END IF
            HDISP = XPMAX * SIN(SRCANGLEU*PI/180.0D0)
         END IF

!      For zero thrust values in Turbofan Engines
      ELSE

         RP0 = R00
         HPM = 1.0D-10

      END IF

!     For Shaft-based/non-turbofan engines, we assume that momentum
!     induced by Shaft-based/non-turbofan engines is almost negligible
   ELSE

      RP0 = R00
      HPM   = 1.0D-10

   END IF


   RETURN
END


SUBROUTINE ADISTF
!***********************************************************************
!                 ADISTF Module of the AMS/EPA Regulatory Model - AERMOD
!
!        PURPOSE: Calculates Distance to Final Plume Rise for Aircraft
!                 Sources.
!
!        PROGRAMMER: Gavendra Pandey, UNC-IE, Chapel Hill, NC, USA &
!                    Akula Venkatram, UC-Riverside, CA, USA.
!
!        DATE:    April 01, 2023
!
!        INPUTS:  Arrays of Source Parameters
!                 Buoyancy Flux, Momentum plumse rise call from arise
!                 Meteorological Variables for One Hour
!                 Wind Speed
!
!        OUTPUTS: Distance to Final Plume Rise, XMAX (m), and Final
!                 Rise (DHFAER)
!
!        CALLED FROM:  VCALC,ACALC
!***********************************************************************

!     Variable Declarations
   USE MAIN1
   IMPLICIT NONE
   CHARACTER MODNAM*12

   DOUBLE PRECISION :: XLN, DELHNN, XMAXN
   DOUBLE PRECISION :: DHFSAV       ! save original DHFAER for URBSTAB cases
   DOUBLE PRECISION :: BVZI2
   DOUBLE PRECISION :: XMAXI, ATMAX

!     External Functions:
   DOUBLE PRECISION, EXTERNAL  :: BISEC_TMAX

!     Variable Initializations
   MODNAM = 'ADISTF'


!     For turbofan and non-turbofan/shaft-based engines having greater
!     than 1.0D-10 buoyancy flux
   IF ( FB .GT. 1.0D-10 ) THEN

      IF( STABLE  .OR.  (UNSTAB .AND. (HS .GE. ZI) ) )THEN
!        Compute the distance to final rise, XMAX;
!        The negative sign appears on the FB term to ensure that the
!        resulting angle is between 0 and PI (i.e., positive)

         XMAX = UP * 4.0D0 * DATAN(1.0D0)/ BVPRIM
!        Call the momentum plume rise code for RP0             --- CALL MOMENTUM_PLUMERISE
         CALL MOMENTUM_PLUMERISE (XMAX)

!        Compute the final stable plume rise, DHF, from Eqn. 3-113 of MFD
         DHFAER = ((RP0/BETA1)**3.0D0 + (6.0D0/(BETA1**(2.0D0)))*&
         &FB*((4.0D0*DATAN(1.0D0)/BVF)**2.0D0)/(UP+VAA))**(THIRD)-&
         &(RP0/BETA1)

         XLN = FB/((UP+VAA)*USTAR*USTAR)
         DELHNN = 1.2D0*XLN**0.6D0 * (HSP + 1.2D0*XLN)**0.4D0
         DHFAER = MIN( DHFAER, DELHNN )

!        Compute Neutral/Unstable Final Rise

         XMAXN = ((1.0D0/BETA1)**2.0D0)*&
         &((FB*UP/(UP+VAA))/(SWP**3.0D0))

         CALL CBLPRD(XMAXN)
         DHFAER = MIN( DHFAER, DHP1 )

!        Apply calm, stable rise limit
         DHFAER = MIN( DHFAER, 4.0D0 * FB**0.25D0 / (BVF*BVF)**0.375D0 )

! ---    Save "original" DHFAER for URBSTAB cases
         DHFSAV = DHFAER

!        For urban stable boundary layers, limit plume rise to 1.25*ZI - HSP
         IF (URBSTAB) THEN
! ---       "New fomulation" for v15181 to account for "partial penetration" of plume
!           above the urban "stable" mixing height, similar to approach used for
!           convective conditions
            IF( HSP+DHFAER .GE. ZI )THEN
! ---          Stack height + plume rise is .GE. ZI; use pseudo-penetrated plume
!              approach for URBAN SBL cases

!              Compute the square of the Brunt-Vaisala frequency at ZI, BVZI2

               BVZI2 = (G / PTATZI) * 0.005D0

!              Compute the value of PsubS, Eq. 26b in the 2nd reference
               PSUBS = FB /((UP+VAA)*BVZI2*(ZI-HSP)*(ZI-HSP)*(ZI-HSP))

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

         XMAXI = ((1.0D0/BETA1)**2.0D0)*&
         &((FB*UP/(UP+VAA))/(SWP**3.0D0))

!        Call the momentum plume rise code for RP0             --- CALL MOMENTUM_PLUMERISE
         CALL MOMENTUM_PLUMERISE (XMAXI)

!        Calculation of ATMAX using the BISECTION_TMAX function
         ATMAX = BISEC_TMAX(FB,VAA,RP0,UP,SWP)

         XMAX  = UP * ATMAX

!        Call the momentum plume rise code for RP0             --- CALL MOMENTUM_PLUMERISE
         CALL MOMENTUM_PLUMERISE (XMAX)

         IF (HS .LE. 12.0D0) THEN

!       Calculation of DHP is based on the equation 6 of Pandey et al. (2023)
            DHP1 = ((RP0/BETA1)**3.0D0 + (1.5D0/BETA1**(2.0D0))*&
            &FB*((ATMAX)**2.0D0)/(UP+VAA))**(THIRD)- RP0/BETA1

            DHFAER = DHP1
         ELSE
            DHFAER = 1.85D0 * (FB/(SWP*SWP*(UP+VAA)))
         END IF

      END IF
!     Turbofan Engines (having zero thrust) and Shaft-based Engines
!     (having negligible or zero buoyancy flux)
   ELSE
      XMAX  = 0.0D0
      DHFAER = 1.0D-10
   END IF

!     Check the Aircraft Debug Option to print the debug file
   IF(ARCFTDEBUG) THEN

      WRITE(ARCFTDBG,*) '===========================================',&
      &'=============================================================',&
      &'========================'
      WRITE (ARCFTDBG,*)'SOURCE TYPE:  ',SRCTYP(ISRC),&
      &'    SOURCE ID:  ',SRCID(ISRC)  ,'      SOURCE NO.:  ',ISRC
      WRITE (ARCFTDBG,7130) KURDAT

      WRITE (ARCFTDBG,7135) MFUEL, THRUST, VAA, AFR, BYPR,&
      &RPWR, SRCANGLE, FB, XMAX, DHFAER, RP0, HPM,&
      &HDISP

7130  FORMAT(' YR/MN/DY/HR:  ', I8,&
      &//,2X,'<----------------------------------- SOURCE INFORMATION',&
      &' ----------------------------------->',/,&
      &'  MFUEL    THRUST     VAA     AFR     BYPR',&
      &'      RPWR      SRCANGLE    FB      XMAX     DHFAER    RP0',&
      &'     HPM      HDISP',/,&
      &' (KG/S)     (N)      (M/S)    (#)     (#)',&
      &'      (kW)      (degree)   (M4/S3)    (M)      (M)',&
      &'      (M)     (M)       (M)',/)
7135  FORMAT(1X,F5.2,2X,F10.1,1X,F7.2,3X,F5.1,2X,F7.2,2X,F10.1,3X,F6.2,&
      &2X,F9.1,1X,F8.2,1X,F8.1,2X,F5.1,4X,F5.1,4X,F5.1)


   END IF


   RETURN
END

DOUBLE PRECISION FUNCTION BISEC_TMAX(FBB,VAB,RP00,AUEFFFF,SWEFFAA)
!***********************************************************************
!        COMPUTE_TMAX Function of the AMS/EPA Regulatory Model - AERMOD
!
!        PURPOSE: Calculate MaxRMAimum time when the rate of plume rise equal
!                 to SIGMA-W.
!
!        PROGRAMMER: Gavendra Pandey, UNC-IE, Chapel Hill, NC, USA &
!                    Akula Venkatram, UC-Riverside, CA, USA.
!
!        DATE:    April 01, 2023
!
!        INPUTS:  Variables:
!                  Buoyancy Flux (m4/s3), FBB
!                  Aircraft Speed (m/s), VAB
!                  Wind Speed (m/s), AUEFFF
!                  Plume Radius (m), RP00
!                  Vertical Wind Velocity Fluctuation (m/s), SWEFFAA
!                 Constants:
!                  Buoyancy Entrainment Constant, BETA1 = 0.6
!        OUTPUTS:  Time (s), ATMAX
!
!        CALLED FROM: MAXIMUM_PLUMERISE
!***********************************************************************
!     Variable Declarations
!      USE MAIN1
   IMPLICIT NONE

   DOUBLE PRECISION  :: ATMAX, RP00
   DOUBLE PRECISION  :: A, B, C, FBB, VAB
   DOUBLE PRECISION  :: XM, X1, X2, FRIGHT,&
   &XRIGHT, XLEFT, DELX, XMID,&
   &FMID, AUEFFFF, SWEFFAA

   INTEGER :: I, AAAITER, NSTEPS
!     Fixed Values
   DOUBLE PRECISION, PARAMETER  :: ERRLIMIT = 1.0E-04, BETA1 = 0.60D0

   NSTEPS = ANINT(DLOG(1.0D0/ERRLIMIT))

   XM = ((2.0D0/3.0D0/BETA1)**2.0D0)*&
   &((FBB/(AUEFFFF+VAB))/(SWEFFAA**3.0D0))        ! Equation 17

   X1 = XM/2.0

   X2 = 2.0*XM

   A = (FBB/(AUEFFFF+VAB)/BETA1)**(2.0D0)

   B = (RP00/BETA1)**3.0D0

   C = (B + A*1.5D0*X2**2.0D0)

   FRIGHT = A*X2 - SWEFFAA*C**(2.0D0/3.0D0)

   IF (FRIGHT .GT. 0.0D0) THEN

      XRIGHT = X2
      XLEFT  = X1

   ELSE

      XRIGHT = X1

      XLEFT  = X2

   END IF

   DO I = 1, NSTEPS

      DELX = XRIGHT - XLEFT

      XMID = XLEFT + 0.5D0*DELX

      C = (B + A*1.5D0*XMID**2.0D0)

      FMID = A*XMID - SWEFFAA*C**(2.0D0/3.0D0)

      IF (FMID .GT. 0.0D0) THEN

         XRIGHT = XMID

      ELSE

         XLEFT = XMID

      END IF

   END DO

   ATMAX = XMID

END FUNCTION


SUBROUTINE ADELTAH ( XARG )
!***********************************************************************
!             ADELTAH Module of the AMS/EPA Regulatory Model - AERMOD
!
!   PURPOSE: To calculate plume rise for Aircraft Sources
!
!   PROGRAMMER: Gavendra Pandey, UNC-IE, Chapel Hill, NC, USA &
!               Akula Venkatram, UC-Riverside, CA, USA &
!               Sarav Arunachalam, UNC-IE, Chapel Hill, NC, USA.
!
!   DATE:    April 01, 2023
!
!   INPUTS:  The distance at which to make the computation, XARG
!
!   OUTPUTS: Distance-Dependent Plume Rise, DHP (m)
!
!   CALLED FROM:  VCALC, ACALC
!
!   Assumptions:  All plume rise calculations are for gradual rise,
!                 except in stable conditions when the downwind distance
!                 exceeds XMAX
!
!   References:   "Accounting for Plume Rise of Aircraft Emissions in AERMOD"
!                 G.Pandey, A. Venkatram, & S. Arunachalam; In Review 2023
!                 "Dispersion in the Stable Boundary Layer",
!                 A. Venkatram, 2/12/93
!                 "A Dispersion Model for the Convective Boundary Layer",
!                 J. Weil, 8/17/93
!                 "Plume Penetration of the CBL and Source 3: Source
!                 Strength and Plume Rise", J. Weil, 9/1/93
!
!CC The framework of the plume rise calculations is adopted from PRISE.f CC
!***********************************************************************

!     Variable Declarations
   USE MAIN1
   IMPLICIT NONE

   CHARACTER MODNAM*12
   INTEGER :: KITER, NDXZPL
   DOUBLE PRECISION :: XARG, XMAXTMP, XRISE, ZPLM, DHPOLD,&
   &SVPM, SWPM, UPM, TGPM, PTPM, PTP

!     Variable Initializations
   MODNAM = 'ADELTAH'


   IF( (STABLE  .OR.  (UNSTAB  .AND.  (HS .GE. ZI)))  .AND.&
   &(XARG .GE. XMAX) )THEN
!        Use final stable plume rise (DHF) calculated in DISTF (DHP)
!        at XMAX
      DHP = DHFAER

   ELSE IF( (STABLE  .OR. (UNSTAB  .AND.  (HS .GE. ZI))) .AND.&
   &(XARG .LT. XMAX) ) THEN
!----    Compute stable plume rise for the distance XARG   --- CALL ASBLRIS
!        Use iterative approach to plume rise calculations.
!        First compute temporary distance to "final rise" based on current
!        values of UP and BVPRIM.  Then, don't pass a distance larger than
!        XMAXTMP to ASBLRIS.  This avoids potential for math error in
!        SUB. ASBLRIS for distances beyond the value of XMAX computed
!----    iteratively outside the receptor loop in SUB. ADISTF.

      XMAXTMP = UP * 4.0D0 * DATAN(1.0D0)/ BVPRIM
      XRISE   = MIN( XARG, XMAXTMP )
      CALL ASBLRIS ( XRISE )
      KITER = 0

50    ZPLM = HSP + 0.5D0 * DHP
      DHPOLD = DHP

!----    Locate index below ZPLM

      CALL LOCATE(GRIDHT, 1, MXGLVL, ZPLM, NDXZPL)

!----    Get Wind speed at ZPLM; replace UP, SVP, SWP.  Also, replace TGP,
!        vertical potential temperature gradient, if stable.

      CALL GINTRP( GRIDHT(NDXZPL), GRIDSV(NDXZPL),&
      &GRIDHT(NDXZPL+1), GRIDSV(NDXZPL+1), ZPLM, SVPM )
      CALL GINTRP( GRIDHT(NDXZPL), GRIDSW(NDXZPL),&
      &GRIDHT(NDXZPL+1), GRIDSW(NDXZPL+1),&
      &ZPLM, SWPM )

      CALL GINTRP( GRIDHT(NDXZPL), GRIDWS(NDXZPL),&
      &GRIDHT(NDXZPL+1), GRIDWS(NDXZPL+1), ZPLM, UPM )

      SVPM = MAX( SVPM, SVMIN, SVUMIN*UPM )
      SWPM = MAX( SWPM, SWMIN )

      IF( L_VECTORWS )THEN
         UPM = DSQRT( UPM*UPM + 2.0D0*SVPM*SVPM )
      ENDIF
      UPM  = MAX( UPM, WSMIN )

!RWB     Use average of stack top and midpoint wind speeds and others.
      UP = 0.5D0 * (US + UPM)
      SVP = 0.5D0 * (SVS + SVPM)
      SWP = 0.5D0 * (SWS + SWPM)

      CALL GINTRP( GRIDHT(NDXZPL), GRIDTG(NDXZPL),&
      &GRIDHT(NDXZPL+1), GRIDTG(NDXZPL+1), ZPLM, TGPM )
      CALL GINTRP( GRIDHT(NDXZPL), GRIDPT(NDXZPL),&
      &GRIDHT(NDXZPL+1), GRIDPT(NDXZPL+1), ZPLM, PTPM )
!RWB     Use average of stack top and midpoint temperature gradients.
      TGP = 0.5D0 * (TGS + TGPM)
      PTP = 0.5D0 * (PTS + PTPM)
      BVF = DSQRT( G * TGP / PTP)
      IF(BVF .LT. 1.0D-10) BVF = 1.0D-10
      BVPRIM  = 0.7D0 * BVF

!        Repeat calculation of temporary distance to "final rise" using
!        current values of UP and BVPRIM.
      XMAXTMP = UP * 4.0D0 * DATAN(1.0D0)/ BVPRIM
      XRISE   = MIN( XARG, XMAXTMP )
      CALL ASBLRIS ( XRISE )

      KITER = KITER + 1

!RJP     Add temporary debugging statements

      IF(ARCFTDEBUG) THEN
         WRITE(ARCFTDBG,6001) KITER,DHPOLD, DHP, ZPLM, UP,TGP
6001     FORMAT(/,5X,'OPTH2 ITER. #',I1,': OLD DELH = ',&
         &F6.1,' M; NEW DELH = ',F6.1,' M; MET LEVEL = ',&
         &F6.1,' M; NEW Upl = ',F5.2,' M/S; NEW DTHDZ = ',&
         &F7.4,' K/M')
      ENDIF

!        Check for convergence
      IF(DHP.GT.0.0D0 .AND. DABS((DHPOLD-DHP)/DHP).LT.0.001D0 .AND.&
      &KITER .GE. 5)THEN
         IF( DHP .LE. 1.0D-5 )THEN
            DHP = 1.0D-5
         ENDIF
         GO TO 60
      ELSEIF(KITER .LT. 10)THEN
         GO TO 50
      ENDIF

      IF(KITER .GE. 5) THEN
         DHP = 0.5D0 * (DHP + DHPOLD)
         IF(ARCFTDEBUG) WRITE(ARCFTDBG,6002) DHP
6002     FORMAT(/,5X,'OPTH2 ITERATION FAILED TO CONVERGE; PLUME',&
         &' RISE SET AT ',F6.1,' METERS.',/)
         GO TO 60
      ELSE
         GO TO 50
      ENDIF

60    CONTINUE

!RWB     After completing iteration, reset UP, SVP, SWP and TGP to stack top
!RWB     values for subsequent distance-dependent plume rise calcs.
      UP  = US
      SVP = SVS
      SWP = SWS
      TGP = TGS
      PTP = PTS
      BVF = DSQRT( G * TGP / PTP )
      IF(BVF .LT. 1.0D-10) BVF = 1.0D-10
      BVPRIM  = 0.7D0 * BVF
!crfl-3/6/95 Make sure SBL rise is not greater than CBL rise.
      CALL ACBLPRD(XARG)
      DHP = MIN(DHP,DHP1)
      DHP = MIN(DHP,DHFAER)

   ELSEIF( UNSTAB )THEN
!        (i.e., for UNSTABle cases, with HS < ZI)

!        Compute  plume rise for the direct plume          --- CALL ACBLPRD
      CALL ACBLPRD ( XARG )

!        Compute  plume rise for the indirect plume        --- CALL ACBLPRN
      CALL ACBLPRN ( XARG )

      IF( PPF .GT. 0.0D0 )THEN
!           Compute plume rise for the penetrated plume    --- CALL ACBLPR3
         CALL ACBLPR3

      ELSE
!           No plume penetration - plume rise is zero for this source
         DHP3 = 0.0D0

      ENDIF

   ENDIF

   RETURN
END


SUBROUTINE ASBLRIS ( XARG )
!***********************************************************************
!             ASBLRIS Module of the AMS/EPA Regulatory Model - AERMOD
!
!   PURPOSE:  To calculate plume rise for the stable boundary layer
!             or releases above the convective boundary layer for
!             Aircraft Sources
!
!   PROGRAMMER: Gavendra Pandey, UNC-IE, Chapel Hill, NC, USA &
!               Akula Venkatram, UC-Riverside, CA, USA.
!
!   DATE:    April 01, 2023
!
!
!   INPUTS:  Brunt-Vaisala frequency, S
!            Buoyancy flux, FB
!            Momentum Plume Rise, HPM
!            Maximum Plume Radius, RP0
!            Downwind distance, XARG
!            Wind speed at stack top, UP
!            Aircraft Speed, VAA
!
!   OUTPUTS: Plume Rise, DHP (m)
!
!   CALLED FROM:  VCALC, ACALC
!
!   Assumptions:  Wind speed is nonzero at stack top
!
!   References:   "Accounting for Plume Rise of Aircraft Emissions in AERMOD"
!                 G.Pandey, A. Venkatram, & S. Arunachalam; In Review 2023
!                 "Dispersion in the Stable Boundary Layer",
!                 A. Venkatram, 2/12/93
!
!CC The framework of the plume rise calculations is adopted from PRISE.f CC
!***********************************************************************

!     Variable Declarations
   USE MAIN1
   IMPLICIT NONE
   CHARACTER MODNAM*12
   DOUBLE PRECISION :: XARG, TERMA, TERMC, TERMD, TERME
   DOUBLE PRECISION :: XLN, DELHNN

!     Variable Initializations
   MODNAM = 'ASBLRIS'

!     Call MOMENTUM_PLUMERISE to calculate the RP0 (plume radius) and
!     (HPM) horizontal momentum-induced plume rise

   CALL MOMENTUM_PLUMERISE (XARG)                                   ! CALL MOMENTUM_PLUMERISE

!     For turbofan and non-turbofan/shaft-based engines
   IF (BYPR .GT. 0.0D0 .OR. BYPR .EQ. -999.0D0 .AND.&
   &FB .GT. 1.0D-10) THEN

!       Calculation of DHP is based on the equation 6 of Pandey et al. (2023)
      DHP = ((RP0/BETA1)**3.0D0 + (1.5D0/BETA1**(2.0D0))*&
      &FB*((XARG/UP)**2.0D0)/(VAA+UP))**(THIRD)- RP0/BETA1 +&
      &HPM                                                   ! Momentum Plume Rise

! ---   Apply lower limit on stable plume rise based on Equation 98
!       of the MFD

      XLN = FB/((UP+VAA)*USTAR*USTAR)
      DELHNN = 1.2D0*XLN**0.6D0 * (HSP + 1.2D0*XLN)**0.4D0

      DHP = MIN( DHP, DHFAER, DELHNN )
      DHP = MIN(DHP, ABS(ZI-HS))

!      Turbofan Engines (having zero thrust) and Shaft-based Engines
!      (having negligible or zero buoyancy flux), in that case total plume rise
!      equal to momentum plume rise only)
   ELSE

      DHP = HPM
      DHP = MIN(DHP, ABS(ZI-HS))

   END IF

!     For Airborne Aircraft Sources
   IF (HS .GT. 12) THEN
      DHP = MIN(DHP, ABS(ZI-HS))
      DHP = MAX(HPM,(DHP-HDISP))
   END IF

   RETURN
END

SUBROUTINE ACBLPRD ( XARG )
!***********************************************************************
!             ACBLPRD Module of the AMS/EPA Regulatory Model - AERMOD
!
!   PURPOSE: To calculate plume rise for the direct plume in the
!            convective boundary layer for Aircraft Sources
!
!   PROGRAMMER: Gavendra Pandey, UNC-IE, Chapel Hill, NC, USA &
!               Akula Venkatram, UC-Riverside, CA, USA.
!
!   DATE:    April 01, 2023
!
!
!   INPUTS:  Buoyancy flux, FB
!            Momentum Plume Rise, HPM
!            Maximum Plume Radius, RP0
!            Downwind distance, XARG
!            Wind speed at stack top, UP
!            Aircraft Speed, VAA
!
!   OUTPUTS: Plume Rise, DHP1 (m)
!
!   CALLED FROM:  ADELTAH, VCALC, ACALC
!
!   Assumptions:  Wind speed is nonzero at stack top
!                 BETA1 = 0.6D0 (assigned in MODULE MAIN1)
!
!   References:   "Accounting for Plume Rise of Aircraft Emissions in AERMOD"
!                 G.Pandey, A. Venkatram, & S. Arunachalam; In Review 2023
!
!CC The framework of the plume rise calculations is adopted from PRISE.f CC
!***********************************************************************

!     Variable Declarations
   USE MAIN1
   IMPLICIT NONE
   CHARACTER MODNAM*12
   DOUBLE PRECISION :: XARG

!     Variable Initializations
   MODNAM = 'ACBLPRD'

!     Call MOMENTUM_PLUMERISE to calculate the RP0 (plume radius) and
!     (HPM) horizontal momentum-induced plume rise

   CALL MOMENTUM_PLUMERISE (XARG)                                   ! CALL MOMENTUM_PLUMERISE

!     For turbofan and non-turbofan/shaft-based engines
   IF (BYPR .GT. 0.0D0 .OR. BYPR .EQ. -999.0D0 .AND.&
   &FB .GT. 1.0D-10) THEN

!      Calculation of DHP is based on the equation 6 of Pandey et al. (2023)
      DHP1 = ((RP0/BETA1)**3.0D0 + (1.5D0/BETA1**(2.0D0))*&
      &FB*((XARG/UP)**2.0D0)/(VAA+UP))**(THIRD)- RP0/BETA1 +&
      &HPM                                                    ! Momentum Plume Rise

      DHP1 = MIN(DHP1, ABS(ZI-HS))

!      Turbofan Engines (having zero thrust) and Shaft-based Engines
!      (having negligible or zero buoyancy flux), in that case total plume rise
!      equal to momentum plume rise only)
   ELSE

      DHP1 = HPM
      DHP1 = MIN(DHP1, ABS(ZI-HS))

   END IF

!     For Airborne Aircraft Sources
   IF (HS .GT. 12) THEN

      DHP1 = MIN(DHP1, ABS(ZI-HS))
      DHP1 = MAX(HPM,(DHP1-HDISP))

   END IF

   RETURN
END

SUBROUTINE ACBLPRN ( XARG )
!***********************************************************************
!             ACBLPRN Module of the AMS/EPA Regulatory Model - AERMOD
!
!   PURPOSE: To calculate plume rise for the indirect plume in the
!            convective boundary layer for Aircraft Sources
!
!   PROGRAMMER: Gavendra Pandey, UNC-IE, Chapel Hill, NC, USA &
!               Akula Venkatram, UC-Riverside, CA, USA.
!
!   DATE:     April 01, 2023
!
!   CHANGES:  Added Aircraft speed along with wind speed to calculate the
!             aircraft related plume rise
!
!             Equation for DHP2 revised per memorandum from Jeff Weil
!             to AERMIC dated June 20, 1994.
!
!             Added XARG as formal argument for new formulation.
!             Roger Brode, PES, Inc. - 12/5/94
!
!   INPUTS:
!
!   OUTPUTS: Plume Rise, DHP2 (m)
!
!   CALLED FROM:   DELTAH
!
!   Assumptions:
!
!   References:   "A Dispersion Model for the Convective Boundary Layer",
!                 J. Weil, 8/17/93
!
!CC The framework of the plume rise calculations is adopted from PRISE.f CC
!***********************************************************************

!     Variable Declarations
   USE MAIN1
   IMPLICIT NONE
   CHARACTER MODNAM*12
   DOUBLE PRECISION :: XARG, RSUBH, RYRZ, DELHI

!     Variable Initializations
   MODNAM = 'ACBLPRN'

   RSUBH = BETA2 * (ZI - HSP)
   RYRZ  = RSUBH * RSUBH + 0.25D0*ASUBE*(LAMDAY**1.5D0) *&
   &WSTAR*WSTAR*XARG*XARG/(UP*UP)
   DELHI = DSQRT((2.D0*FB*ZI)/(ALPHAR*(UP+VAA)*RYRZ))*(XARG/UP)
   DHP2  = DELHI

   RETURN
END

SUBROUTINE ACBLPR3
!***********************************************************************
!             ACBLPR3 Module of the AMS/EPA Regulatory Model - AERMOD
!
!   PURPOSE: To calculate plume rise for the penetrated plume in the
!            convective boundary layer for aircraft sources
!
!   PROGRAMMER: Jim Paumier and Roger Brode, PES, Inc
!
!   DATE:    September 30, 1993
!
!   INPUTS:  The ratio of delta(Hsub_e) to delta(Hsub_h), HEDHH
!            Mixing height, ZI
!            Source release height, HS
!
!   OUTPUTS: Plume Rise, DHP3 (m)
!
!   CALLED FROM:   DELTAH
!
!   Assumptions:
!
!   References:   "A Dispersion Model for the Convective Boundary
!                  Layer", J. Weil, 8/17/93
!                 "Plume Penetration of the CBL and Source 3: Source
!                  Strength and Plume Rise", J. Weil, 9/1/93
!
!CC The framework of the plume rise calculations is adopted from PRISE.f CC
!***********************************************************************

!     Variable Declarations
   USE MAIN1
   IMPLICIT NONE
   CHARACTER MODNAM*12

!     Variable Initializations
   MODNAM = 'CBLPR3'

!     The plume rise for the penetrated source is delta(Hsub_3), given
!     by Eq. 9 in Jeff Weil's 9/1/93 document.  The variable HEDHH is
!     delta(Hsub_e)/delta(Hsub_h), calculated from Eq. 26a of Jeff Weil's
!     8/17/93 document, where delta(Hsub_h) is ZI-HS.

   IF (PPF .EQ. 1.0D0) THEN
      DHP3 = HEDHH * (ZI-HSP)
   ELSE
      DHP3 = 0.75D0 * (ZI-HSP) * HEDHH + 0.5D0 * (ZI-HSP)
   END IF

   RETURN

END
