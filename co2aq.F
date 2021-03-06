* 
*  co2aq.F
* 
*  Andreas Schmittner (aschmitt@coas.oregonstate.edu)
*  Aug 17th 2007
* 
*  Returns CO2(aq) of sea water
* 


      SUBROUTINE co2aq_init(id)

      INCLUDE 'ferret_cmn/EF_Util.cmn'

      INTEGER id, arg

      CALL ef_version_test(ef_version)

* **********************************************************************
*                                            USER CONFIGURABLE PORTION |
*                                                                      |
*                                                                      V

      CALL ef_set_desc(id,
     .  'hi returns CO2(aq) (mol m-3) using OCMIP routines' )
      CALL ef_set_num_args(id, 5)
      CALL ef_set_axis_inheritance_6d(id,
     .                                IMPLIED_BY_ARGS, IMPLIED_BY_ARGS,
     .                                IMPLIED_BY_ARGS, IMPLIED_BY_ARGS,
     .                                IMPLIED_BY_ARGS, IMPLIED_BY_ARGS)
      CALL ef_set_piecemeal_ok_6d(id, YES, YES, YES, YES, YES, YES)

      arg = 1
      CALL ef_set_arg_name(id, arg, 'TEMP')
      CALL ef_set_arg_unit(id, arg, 'deg C')
      CALL ef_set_arg_desc(id, arg, 'temperature')
      CALL ef_set_axis_influence_6d(id, arg,
     .                              YES, YES, YES, YES, YES, YES)

      arg = 2
      CALL ef_set_arg_name(id, arg, 'SALT')
      CALL ef_set_arg_unit(id, arg, 'su')
      CALL ef_set_arg_desc(id, arg, 'salinity')
      CALL ef_set_axis_influence_6d(id, arg,
     .                              YES, YES, YES, YES, YES, YES)

      arg = 3
      CALL ef_set_arg_name(id, arg, 'DIC')
      CALL ef_set_arg_unit(id, arg, 'mol/m^3')
      CALL ef_set_arg_desc(id, arg, 'dissolved inorganic carbon')
      CALL ef_set_axis_influence_6d(id, arg,
     .                              YES, YES, YES, YES, YES, YES)

      arg = 4
      CALL ef_set_arg_name(id, arg, 'ALK')
      CALL ef_set_arg_unit(id, arg, 'mol/m^3')
      CALL ef_set_arg_desc(id, arg, 'total alkalinity')
      CALL ef_set_axis_influence_6d(id, arg,
     .                              YES, YES, YES, YES, YES, YES)

      arg = 5
      CALL ef_set_arg_name(id, arg, 'PRES')
      CALL ef_set_arg_unit(id, arg, 'bar')
      CALL ef_set_arg_desc(id, arg, 'pressure')
      CALL ef_set_axis_influence_6d(id, arg,
     .                              YES, YES, YES, YES, YES, YES)

*                                                                      ^
*                                                                      |
*                                            USER CONFIGURABLE PORTION |
* **********************************************************************

      RETURN
      END


*
*  In this subroutine we compute the result
*
      SUBROUTINE co2aq_compute(id, arg_1, arg_2, arg_3, arg_4, arg_5
     .,result)

      IMPLICIT NONE
      INCLUDE 'ferret_cmn/EF_Util.cmn'
      INCLUDE 'ferret_cmn/EF_mem_subsc.cmn'

      INTEGER id

      REAL arg_1(mem1lox:mem1hix, mem1loy:mem1hiy, mem1loz:mem1hiz,
     .           mem1lot:mem1hit, mem1loe:mem1hie, mem1lof:mem1hif)
      REAL arg_2(mem2lox:mem2hix, mem2loy:mem2hiy, mem2loz:mem2hiz,
     .           mem2lot:mem2hit, mem2loe:mem2hie, mem2lof:mem2hif)
      REAL arg_3(mem3lox:mem3hix, mem3loy:mem3hiy, mem3loz:mem3hiz,
     .           mem3lot:mem3hit, mem3loe:mem3hie, mem3lof:mem3hif)
      REAL arg_4(mem4lox:mem4hix, mem4loy:mem4hiy, mem4loz:mem4hiz,
     .           mem4lot:mem4hit, mem4loe:mem4hie, mem4lof:mem4hif)
      REAL arg_5(mem5lox:mem5hix, mem5loy:mem5hiy, mem5loz:mem5hiz,
     .           mem5lot:mem5hit, mem5loe:mem5hie, mem5lof:mem5hif)

      REAL result(memreslox:memreshix, memresloy:memreshiy,
     .            memresloz:memreshiz, memreslot:memreshit,
     .            memresloe:memreshie, memreslof:memreshif)

* After initialization, the 'res_' arrays contain indexing information
* for the result axes.  The 'arg_' arrays will contain the indexing
* information for each variable's axes.

      INTEGER res_lo_ss(6),
     .        res_hi_ss(6),
     .        res_incr (6)
      INTEGER arg_lo_ss(6,EF_MAX_ARGS),
     .        arg_hi_ss(6,EF_MAX_ARGS),
     .        arg_incr (6,EF_MAX_ARGS)

      REAL bad_flag(EF_MAX_ARGS), bad_flag_result

* **********************************************************************
*                                            USER CONFIGURABLE PORTION |
*                                                                      |
*                                                                      V

      INTEGER i,  j,  k,  l,  m,  n
      INTEGER i1, j1, k1, l1, m1, n1
      INTEGER i2, j2, k2, l2, m2, n2
      INTEGER i3, j3, k3, l3, m3, n3
      INTEGER i4, j4, k4, l4, m4, n4
      INTEGER i5, j5, k5, l5, m5, n5
      real pt_in,sit_in,atmpres,phlo,phhi,co2ccn
      real co2star, dco2star, dpco2, pco2surf, ph

      CALL ef_get_res_subscripts_6d(id, res_lo_ss, res_hi_ss, res_incr)
      CALL ef_get_arg_subscripts_6d(id, arg_lo_ss, arg_hi_ss, arg_incr)
      CALL ef_get_bad_flags(id, bad_flag, bad_flag_result)

      co2ccn=280.
      pt_in=0.5125e-3           !mol/m^3
      atmpres=1.0               !atm
      sit_in=7.6875e-03         !mol/m^3
      phlo=6.
      phhi=9.

      n1 = arg_lo_ss(F_AXIS,ARG1)
      n2 = arg_lo_ss(F_AXIS,ARG2)
      n3 = arg_lo_ss(F_AXIS,ARG3)
      n4 = arg_lo_ss(F_AXIS,ARG4)
      n5 = arg_lo_ss(F_AXIS,ARG5)
      DO 600 n = res_lo_ss(F_AXIS), res_hi_ss(F_AXIS)

       m1 = arg_lo_ss(E_AXIS,ARG1)
       m2 = arg_lo_ss(E_AXIS,ARG2)
       m3 = arg_lo_ss(E_AXIS,ARG3)
       m4 = arg_lo_ss(E_AXIS,ARG4)
       m5 = arg_lo_ss(E_AXIS,ARG5)
       DO 500 m = res_lo_ss(E_AXIS), res_hi_ss(E_AXIS)

        l1 = arg_lo_ss(T_AXIS,ARG1)
        l2 = arg_lo_ss(T_AXIS,ARG2)
        l3 = arg_lo_ss(T_AXIS,ARG3)
        l4 = arg_lo_ss(T_AXIS,ARG4)
        l5 = arg_lo_ss(T_AXIS,ARG5)
        DO 400 l = res_lo_ss(T_AXIS), res_hi_ss(T_AXIS)

         k1 = arg_lo_ss(Z_AXIS,ARG1)
         k2 = arg_lo_ss(Z_AXIS,ARG2)
         k3 = arg_lo_ss(Z_AXIS,ARG3)
         k4 = arg_lo_ss(Z_AXIS,ARG4)
         k5 = arg_lo_ss(Z_AXIS,ARG5)
         DO 300 k = res_lo_ss(Z_AXIS), res_hi_ss(Z_AXIS)

          j1 = arg_lo_ss(Y_AXIS,ARG1)
          j2 = arg_lo_ss(Y_AXIS,ARG2)
          j3 = arg_lo_ss(Y_AXIS,ARG3)
          j4 = arg_lo_ss(Y_AXIS,ARG4)
          j5 = arg_lo_ss(Y_AXIS,ARG5)
          DO 200 j = res_lo_ss(Y_AXIS), res_hi_ss(Y_AXIS)

           i1 = arg_lo_ss(X_AXIS,ARG1)
           i2 = arg_lo_ss(X_AXIS,ARG2)
           i3 = arg_lo_ss(X_AXIS,ARG3)
           i4 = arg_lo_ss(X_AXIS,ARG4)
           i5 = arg_lo_ss(X_AXIS,ARG5)
           DO 100 i = res_lo_ss(X_AXIS), res_hi_ss(X_AXIS)

            IF ( (arg_1(i1,j1,k1,l1,m1,n1) .EQ. bad_flag(ARG1)) .OR.
     .           (arg_2(i2,j2,k2,l2,m2,n2) .EQ. bad_flag(ARG2)) .OR.
     .           (arg_3(i3,j3,k3,l3,m3,n3) .EQ. bad_flag(ARG3)) .OR.
     .           (arg_4(i4,j4,k4,l4,m4,n4) .EQ. bad_flag(ARG4)) .OR.
     .           (arg_5(i5,j5,k5,l5,m5,n5) .EQ. bad_flag(ARG5)) ) THEN

               result(i,j,k,l,m,n) = bad_flag_result

            ELSE

               call co2calc(arg_1(i1,j1,k1,l1,m1,n1)
     &              ,arg_2(i2,j2,k2,l2,m2,n2),arg_3(i3,j3,k3,l3,m3,n3)
     &                    ,arg_4(i4,j4,k4,l4,m4,n4),pt_in,sit_in
     &                    ,phlo,phhi,ph,co2ccn,atmpres
     &                    ,arg_5(i5,j5,k5,l5,m5,n5),co2star
     &                    ,dco2star,pCO2surf,dpco2)

               result(i,j,k,l,m,n) = co2star

            ENDIF

            i1 = i1 + arg_incr(X_AXIS,ARG1)
            i2 = i2 + arg_incr(X_AXIS,ARG2)
            i3 = i3 + arg_incr(X_AXIS,ARG3)
            i4 = i4 + arg_incr(X_AXIS,ARG4)
            i5 = i5 + arg_incr(X_AXIS,ARG5)
 100       CONTINUE
     
           j1 = j1 + arg_incr(Y_AXIS,ARG1)
           j2 = j2 + arg_incr(Y_AXIS,ARG2)
           j3 = j3 + arg_incr(Y_AXIS,ARG3)
           j4 = j4 + arg_incr(Y_AXIS,ARG4)
           j5 = j5 + arg_incr(Y_AXIS,ARG5)
 200      CONTINUE

          k1 = k1 + arg_incr(Z_AXIS,ARG1)
          k2 = k2 + arg_incr(Z_AXIS,ARG2)
          k3 = k3 + arg_incr(Z_AXIS,ARG3)
          k4 = k4 + arg_incr(Z_AXIS,ARG4)
          k5 = k5 + arg_incr(Z_AXIS,ARG5)
 300     CONTINUE

         l1 = l1 + arg_incr(T_AXIS,ARG1)
         l2 = l2 + arg_incr(T_AXIS,ARG2)
         l3 = l3 + arg_incr(T_AXIS,ARG3)
         l4 = l4 + arg_incr(T_AXIS,ARG4)
         l5 = l5 + arg_incr(T_AXIS,ARG5)
 400    CONTINUE

        m1 = m1 + arg_incr(E_AXIS,ARG1)
        m2 = m2 + arg_incr(E_AXIS,ARG2)
        m3 = m3 + arg_incr(E_AXIS,ARG3)
        m4 = m4 + arg_incr(E_AXIS,ARG4)
        m5 = m5 + arg_incr(E_AXIS,ARG5)
 500   CONTINUE

       n1 = n1 + arg_incr(F_AXIS,ARG1)
       n2 = n2 + arg_incr(F_AXIS,ARG2)
       n3 = n3 + arg_incr(F_AXIS,ARG3)
       n4 = n4 + arg_incr(F_AXIS,ARG4)
       n5 = n5 + arg_incr(F_AXIS,ARG5)
 600  CONTINUE

*                                                                      ^
*                                                                      |
*                                            USER CONFIGURABLE PORTION |
* **********************************************************************

      RETURN
      END

      subroutine co2calc(t,s,dic_in,ta_in,pt_in,sit_in
     &                  ,phlo,phhi,ph,xco2_in,atmpres,ocnpres
     &                  ,co2star,dco2star,pCO2surf,dpco2)

!-----------------------------------------------------------------------
! subroutine CO2CALC

! PURPOSE
!        Calculate delta co2* from total alkalinity and total CO2 at
! temperature (t), salinity (s) and "atmpres" atmosphere total pressure.

! USAGE
!       call co2calc(t,s,dic_in,ta_in,pt_in,sit_in
!    &                  ,phlo,phhi,ph,xco2_in,atmpres
!    &                  ,co2star,dco2star,pCO2surf,dpco2)

! INPUT
!        dic_in  = total inorganic carbon (mol/m^3)
!                where 1 T = 1 metric ton = 1000 kg
!        ta_in   = total alkalinity (eq/m^3)
!        pt_in   = inorganic phosphate (mol/m^3)
!        sit_in  = inorganic silicate (mol/m^3)
!        t       = temperature (degrees C)
!        s       = salinity (PSU)
!        phlo    = lower limit of pH range
!        phhi    = upper limit of pH range
!        xco2_in =atmospheric mole fraction CO2 in dry air (ppmv)
!        atmpres = atmospheric pressure in atmospheres
!                 (1 atm==1013.25mbar)

!       Note: arguments dic_in, ta_in, pt_in, sit_in, and xco2_in are
!             used to initialize variables dic, ta, pt, sit, and xco2.
!             * Variables dic, ta, pt, and sit are in the common block
!               "species".
!             * Variable xco2 is a local variable.
!             * Variables with "_in" suffix have different units
!               than those without.

! OUTPUT
!        co2star  = CO2*water (mol/m^3)
!        dco2star = delta CO2 (mol/m^3)
!       pco2surf = oceanic pCO2 (ppmv)
!       dpco2    = Delta pCO2, i.e, pCO2ocn - pCO2atm (ppmv)

! IMPORTANT: Some words about units - (JCO, 4/4/1999)
!     - Models carry tracers in mol/m^3 (on a per volume basis)
!     - Conversely, this routine, which was written by observationalists
!       (C. Sabine and R. Key), passes input arguments in umol/kg
!       (i.e., on a per mass basis)
!     - I have changed things slightly so that input arguments are in
!       mol/m^3,
!     - Thus, all input concentrations (dic_in, ta_in, pt_in, and st_in)
!       should be given in mol/m^3; output arguments "co2star" and
!       "dco2star" are likewise in mol/m^3.

! FILES and PROGRAMS NEEDED
!        drtsafe
!        ta_iter_1

!-----------------------------------------------------------------------

        real invtk,is,is2
        real k0,k1,k2,kw,kb,ks,kf,k1p,k2p,k3p,ksi
        common /const/k0,k1,k2,kw,kb,ks,kf,k1p,k2p,k3p,ksi,ff,htotal
        common /species/bt,st,ft,sit,pt,dic,ta
        external ta_iter_1

!       ----------------------------------------------------------------
!       Change units from the input of mol/m^3 -> mol/kg:
!       (1 mol/m^3)  x (1 m^3/1024.5 kg)
!       where the ocean's mean surface density is 1024.5 kg/m^3
!       Note: mol/kg are actually what the body of this routine uses
!       for calculations.
!       ----------------------------------------------------------------
        permil = 1.0 / 1024.5
!       To convert input in mol/m^3 -> mol/kg
        pt=pt_in*permil
        sit=sit_in*permil
        ta=ta_in*permil
        dic=dic_in*permil

!       ----------------------------------------------------------------
!       Change units from uatm to atm. That is, atm is what the body of
!       this routine uses for calculations.
!       ----------------------------------------------------------------
        permeg=1.e-6
!       To convert input in uatm -> atm
        xco2=xco2_in*permeg
!       ----------------------------------------------------------------

!***********************************************************************
! Calculate all constants needed to convert between various measured
! carbon species. References for each equation are noted in the code.
! Once calculated, the constants are
! stored and passed in the common block "const". The original version of
! this code was based on the code by Dickson in Version 2 of "Handbook
! of Methods for the Analysis of the Various Parameters of the Carbon
! Dioxide System in Seawater", DOE, 1994 (SOP No. 3, p25-26).

! Derive simple terms used more than once

        tk = 273.15 + t
        tk100 = tk/100.0
        tk1002=tk100*tk100
        invtk=1.0/tk
        dlogtk=log(tk)
        is=19.924*s/(1000.-1.005*s)
        is2=is*is
        sqrtis=sqrt(is)
        s2=s*s
        sqrts=sqrt(s)
        s15=s**1.5
        scl=s/1.80655

! f = k0(1-pH2O)*correction term for non-ideality

! Weiss & Price (1980,Mar. Chem.,8,347-359; Eq 13 with table 6 values)

        ff = exp(-162.8301 + 218.2968/tk100  +
     &                90.9241*log(tk100) - 1.47696*tk1002 +
     &                s * (.025695 - .025225*tk100 +
     &                0.0049867*tk1002))

! K0 from Weiss 1974

        k0 = exp(93.4517/tk100 - 60.2409 + 23.3585 * log(tk100) +
     &                s * (.023517 - 0.023656 * tk100 + 0.0047036
     &                * tk1002))

! k1 = [H][HCO3]/[H2CO3]
! k2 = [H][CO3]/[HCO3]

! Millero p.664 (1995) using Mehrbach et al. data on seawater scale

        k1=10**(-1*(3670.7*invtk - 62.008 + 9.7944*dlogtk -
     &                0.0118 * s + 0.000116*s2))

        dv1 = -25.5 + 0.1271*t
        dk1 = -3.08e-3 + 0.0877e-3*t
        R = 83.131
        k1 = k1*exp((-dv1*ocnpres+0.5*dk1*ocnpress**2)/(R*tk))

        k2=10**(-1*(1394.7*invtk + 4.777 -
     &                0.0184*s + 0.000118*s2))

        dv2 = -15.82 - 0.0219*t
        dk2 = 1.13e-3 - 0.1475e-3*t
        k2 = k2*exp((-dv2*ocnpres+0.5*dk2*ocnpress**2)/(R*tk))


! kb = [H][BO2]/[HBO2]

! Millero p.669 (1995) using data from Dickson (1990)

        kb=exp((-8966.90 - 2890.53*sqrts - 77.942*s +
     &                1.728*s15 - 0.0996*s2)*invtk +
     &                (148.0248 + 137.1942*sqrts + 1.62142*s) +
     &                (-24.4344 - 25.085*sqrts - 0.2474*s) *
     &                dlogtk + 0.053105*sqrts*tk)

        t2 = t*t
        dvb = -29.48 + 0.1622*t + 2.608e-3*t2
        dkb = -2.84e-3
        kb = kb*exp((-dvb*ocnpres+0.5*dkb*ocnpress**2)/(R*tk))

! k1p = [H][H2PO4]/[H3PO4]

! DOE(1994) eq 7.2.20 with footnote using data from Millero (1974)

        k1p = exp(-4576.752*invtk + 115.525 - 18.453 * dlogtk +
     &                (-106.736*invtk + 0.69171) * sqrts +
     &                (-0.65643*invtk - 0.01844) * s)

! k2p = [H][HPO4]/[H2PO4]

! DOE(1994) eq 7.2.23 with footnote using data from Millero (1974))

        k2p = exp(-8814.715*invtk + 172.0883 - 27.927 * dlogtk +
     &                (-160.340*invtk + 1.3566) * sqrts +
     &                (0.37335*invtk - 0.05778) * s)

!-----------------------------------------------------------------------
! k3p = [H][PO4]/[HPO4]

! DOE(1994) eq 7.2.26 with footnote using data from Millero (1974)

        k3p = exp(-3070.75*invtk - 18.141 +
     &                (17.27039*invtk + 2.81197) *
     &                sqrts + (-44.99486*invtk - 0.09984) * s)

!-----------------------------------------------------------------------
! ksi = [H][SiO(OH)3]/[Si(OH)4]

! Millero p.671 (1995) using data from Yao and Millero (1995)

        ksi = exp(-8904.2*invtk + 117.385 - 19.334 * dlogtk +
     &                (-458.79*invtk + 3.5913) * sqrtis +
     &                (188.74*invtk - 1.5998) * is +
     &                (-12.1652*invtk + 0.07871) * is2 +
     &                log(1.0-0.001005*s))

!-----------------------------------------------------------------------
! kw = [H][OH]

! Millero p.670 (1995) using composite data

        kw = exp(-13847.26*invtk + 148.9652 - 23.6521 * dlogtk +
     &                (118.67*invtk - 5.977 + 1.0495 * dlogtk) *
     &                sqrts - 0.01615 * s)

        dvw = -25.6 + 0.2324*t - 3.6246e-3*t2
        dkw = -5.13e-3 + 0.0794e-3*t
        kw = kw*exp((-dvw*ocnpres+0.5*dkw*ocnpress**2)/(R*tk))
!-----------------------------------------------------------------------
! ks = [H][SO4]/[HSO4]

! Dickson (1990, J. chem. Thermodynamics 22, 113)

        ks=exp(-4276.1*invtk + 141.328 - 23.093*dlogtk +
     &                (-13856*invtk + 324.57 - 47.986*dlogtk) * sqrtis +
     &                (35474*invtk - 771.54 + 114.723*dlogtk) * is -
     &                2698*invtk*is**1.5 + 1776*invtk*is2 +
     &                log(1.0 - 0.001005*s))

        dvs = -18.03 + 0.0466*t + 0.316e-3*t2
        dks = -4.53e-3 + 0.09e-3*t
        ks = ks*exp((-dvs*ocnpres+0.5*dks*ocnpress**2)/(R*tk))

!-----------------------------------------------------------------------
! kf = [H][F]/[HF]

! Dickson and Riley (1979) -- change pH scale to total

        kf=exp(1590.2*invtk - 12.641 + 1.525*sqrtis +
     &                log(1.0 - 0.001005*s) +
     &                log(1.0 + (0.1400/96.062)*(scl)/ks))

        dvf = -9.78 - 0.009*t - 0.942e-3*t2
        dkf = -3.91e-3 + 0.054e-3*t
        kf = kf*exp((-dvf*ocnpres+0.5*dkf*ocnpress**2)/(R*tk))
!-----------------------------------------------------------------------
! Calculate concentrations for borate, sulfate, and fluoride

! Uppstrom (1974)
        bt = 0.000232 * scl/10.811
! Morris & Riley (1966)
        st = 0.14 * scl/96.062
! Riley (1965)
        ft = 0.000067 * scl/18.9984
!***********************************************************************

! Calculate [H+] total when DIC and TA are known at T, S and 1 atm.
! The solution converges to err of xacc. The solution must be within
! the range x1 to x2.

! If DIC and TA are known then either a root finding or iterative method
! must be used to calculate htotal. In this case we use the
! Newton-Raphson "safe" method taken from "Numerical Recipes" (function
! "rtsafe.f" with error trapping removed).

! As currently set, this procedure iterates about 12 times. The x1 and
! x2 values set below will accomodate ANY oceanographic values. If an
! initial guess of the pH is known, then the number of iterations can be
! reduced to about 5 by narrowing the gap between x1 and x2. It is
! recommended that the first few time steps be run with x1 and x2 set as
! below. After that, set x1 and x2 to the previous value of the pH +/-
! ~0.5. The current setting of xacc will result in co2star accurate to 3
! significant figures (xx.y). Making xacc bigger will result in faster
! convergence also, but this is not recommended (xacc of 10**-9 drops
! precision to 2 significant figures).

! Parentheses added around negative exponents (Keith Lindsay)

        x1 = 10.0**(-phhi)
        x2 = 10.0**(-phlo)
        xacc = 1.e-10
        htotal = drtsafe(ta_iter_1,x1,x2,xacc)

! Calculate [CO2*] as defined in DOE Methods Handbook 1994 Ver.2,
! ORNL/CDIAC-74, Dickson and Goyet, eds. (Ch 2 p 10, Eq A.49)

        htotal2=htotal*htotal
        co2star=dic*htotal2/(htotal2 + k1*htotal + k1*k2)
        co2starair=xco2*ff*atmpres
        dco2star=co2starair-co2star
        ph=-log10(htotal)

!       ---------------------------------------------------------------
!c      Add two output arguments for storing pCO2surf
!c      Should we be using K0 or ff for the solubility here?
!       ---------------------------------------------------------------
        pCO2surf = co2star / ff
        dpCO2    = pCO2surf - xco2*atmpres

!  Convert units of output arguments
!      Note: co2star and dco2star are calculated in mol/kg within this
!      routine. Thus Convert now from mol/kg -> mol/m^3
       co2star  = co2star / permil
       dco2star = dco2star / permil

!      Note: pCO2surf and dpCO2 are calculated in atm above.
!      Thus convert now to uatm
       pCO2surf = pCO2surf / permeg
       dpCO2    = dpCO2 / permeg

        return
        end

!_ ---------------------------------------------------------------------
!_ RCS lines preceded by "c_ "
!_ ---------------------------------------------------------------------
!_
!_ $Source: /www/html/ipsl/OCMIP/phase2/simulations/Abiotic/Cchem/RCS/
!_          drtsafe.f,v $
!_ $Revision: 1.1 $
!_ $Date: 1999/04/03 22:00:42 $   ;  $State: Exp $
!_ $Author: orr $ ;  $Locker:  $
!_
!_ ---------------------------------------------------------------------
!_ $Log: drtsafe.f,v $
!_ Revision 1.1  1999/04/03 22:00:42  orr
!_ Initial revision
!_
!_ ---------------------------------------------------------------------
!_
      real FUNCTION DRTSAFE(FUNCD,X1,X2,XACC)

!     File taken from Numerical Recipes. Modified  R.M.Key 4/94

      MAXIT=100
      call FUNCD(X1,FL,DF)
      call FUNCD(X2,FH,DF)
      if (FL .lt. 0.0) then
        XL=X1
        XH=X2
      else
        XH=X1
        XL=X2
        SWAP=FL
        FL=FH
        FH=SWAP
      endif
      DRTSAFE=.5*(X1+X2)
      DXOLD=ABS(X2-X1)
      DX=DXOLD
      call FUNCD(DRTSAFE,F,DF)
      do 100, J=1,MAXIT
        if (((DRTSAFE-XH)*DF-F)*((DRTSAFE-XL)*DF-F) .ge. 0.0 .or.
     &              ABS(2.0*F) .gt. ABS(DXOLD*DF)) then
          DXOLD=DX
          DX=0.5*(XH-XL)
          DRTSAFE=XL+DX
          if (XL .eq. DRTSAFE) return
        else
          DXOLD=DX
          DX=F/DF
          TEMP=DRTSAFE
          DRTSAFE=DRTSAFE-DX
          if (TEMP .eq. DRTSAFE) return
        endif
        if (ABS(DX) .lt. XACC) return
        call FUNCD(DRTSAFE,F,DF)
        if (F .lt. 0.0) then
          XL=DRTSAFE
          FL=F
        else
          XH=DRTSAFE
          FH=F
        endif
  100 continue
      return
      end

!_ ---------------------------------------------------------------------
!_ RCS lines preceded by "c_ "
!_ ---------------------------------------------------------------------
!_
!_ $Source: /www/html/ipsl/OCMIP/phase2/simulations/Abiotic/Cchem/RCS/
!_          ta_iter_1.f,v $
!_ $Revision: 1.2 $
!_ $Date: 1999/09/01 17:55:41 $   ;  $State: Exp $
!_ $Author: orr $ ;  $Locker:  $
!_
!_ ---------------------------------------------------------------------
!_ $Log: ta_iter_1.f,v $
!_ Revision 1.2  1999/09/01 17:55:41  orr
!_ Fixed sign error in dfn/dx following remarks of C. Voelker
!_ (10/Aug/1999)
!_
!_ Revision 1.1  1999/04/03 22:00:42  orr
!_ Initial revision
!_
!_ ---------------------------------------------------------------------
!_
        subroutine ta_iter_1(x,fn,df)
        real k12,k12p,k123p
        real k0,k1,k2,kw,kb,ks,kf,k1p,k2p,k3p,ksi
        common /const/k0,k1,k2,kw,kb,ks,kf,k1p,k2p,k3p,ksi,ff,htotal
        common /species/bt,st,ft,sit,pt,dic,ta

! This routine expresses TA as a function of DIC, htotal and constants.
! It also calculates the derivative of this function with respect to
! htotal. It is used in the iterative solution for htotal. In the call
! "x" is the input value for htotal, "fn" is the calculated value for TA
! and "df" is the value for dTA/dhtotal

        x2=x*x
        x3=x2*x
        k12 = k1*k2
        k12p = k1p*k2p
        k123p = k12p*k3p
        c = 1.0 + st/ks
        a = x3 + k1p*x2 + k12p*x + k123p
        a2=a*a
        da = 3.0*x2 + 2.0*k1p*x + k12p
        b = x2 + k1*x + k12
        b2=b*b
        db = 2.0*x + k1

!     fn = hco3+co3+borate+oh+hpo4+2*po4+silicate+hfree+hso4+hf+h3po4-ta

        fn = k1*x*dic/b +
     &             2.0*dic*k12/b +
     &             bt/(1.0 + x/kb) +
     &             kw/x +
     &             pt*k12p*x/a +
     &             2.0*pt*k123p/a +
     &             sit/(1.0 + x/ksi) -
     &             x/c -
     &             st/(1.0 + ks/x/c) -
     &             ft/(1.0 + kf/x) -
     &             pt*x3/a -
     &             ta

!        df = dfn/dx

        df = ((k1*dic*b) - k1*x*dic*db)/b2 -
     &             2.0*dic*k12*db/b2 -
     &             bt/kb/(1.0+x/kb)**2. -
     &             kw/x2 +
     &             (pt*k12p*(a - x*da))/a2 -
     &             2.0*pt*k123p*da/a2 -
     &             sit/ksi/(1.0+x/ksi)**2. -
     &             1.0/c +
     &             st*(1.0 + ks/x/c)**(-2.)*(ks/c/x2) +
     &             ft*(1.0 + kf/x)**(-2.)*kf/x2 -
     &             pt*x2*(3.0*a-x*da)/a2

        return
        end
