* 
*  o2sat.F
* 
*  Andreas Schmittner (aschmitt@coas.oregonstate.edu)
*  Jul 24, 2017
* 
*  Returns oxygen saturation concentration of sea water
* 


      SUBROUTINE o2sat_init(id)

      INCLUDE 'ferret_cmn/EF_Util.cmn'

      INTEGER id, arg

      CALL ef_version_test(ef_version)

* **********************************************************************
*                                            USER CONFIGURABLE PORTION |
*                                                                      |
*                                                                      V

      CALL ef_set_desc(id,
     .  'returns oxygen saturation (mol/m^3) using OCMIP routines' )
      CALL ef_set_num_args(id, 2)
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

*                                                                      ^
*                                                                      |
*                                            USER CONFIGURABLE PORTION |
* **********************************************************************

      RETURN
      END


*
*  In this subroutine we compute the result
*
      SUBROUTINE o2sat_compute(id, arg_1, arg_2, result)

      IMPLICIT NONE
      INCLUDE 'ferret_cmn/EF_Util.cmn'
      INCLUDE 'ferret_cmn/EF_mem_subsc.cmn'

      INTEGER id

      REAL arg_1(mem1lox:mem1hix, mem1loy:mem1hiy, mem1loz:mem1hiz,
     .           mem1lot:mem1hit, mem1loe:mem1hie, mem1lof:mem1hif)
      REAL arg_2(mem2lox:mem2hix, mem2loy:mem2hiy, mem2loz:mem2hiz,
     .           mem2lot:mem2hit, mem2loe:mem2hie, mem2lof:mem2hif)

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
      real o2sato

      CALL ef_get_res_subscripts_6d(id, res_lo_ss, res_hi_ss, res_incr)
      CALL ef_get_arg_subscripts_6d(id, arg_lo_ss, arg_hi_ss, arg_incr)
      CALL ef_get_bad_flags(id, bad_flag, bad_flag_result)

      n1 = arg_lo_ss(F_AXIS,ARG1)
      n2 = arg_lo_ss(F_AXIS,ARG2)
      DO 600 n = res_lo_ss(F_AXIS), res_hi_ss(F_AXIS)

       m1 = arg_lo_ss(E_AXIS,ARG1)
       m2 = arg_lo_ss(E_AXIS,ARG2)
       DO 500 m = res_lo_ss(E_AXIS), res_hi_ss(E_AXIS)

        l1 = arg_lo_ss(T_AXIS,ARG1)
        l2 = arg_lo_ss(T_AXIS,ARG2)
        DO 400 l = res_lo_ss(T_AXIS), res_hi_ss(T_AXIS)

         k1 = arg_lo_ss(Z_AXIS,ARG1)
         k2 = arg_lo_ss(Z_AXIS,ARG2)
         DO 300 k = res_lo_ss(Z_AXIS), res_hi_ss(Z_AXIS)

          j1 = arg_lo_ss(Y_AXIS,ARG1)
          j2 = arg_lo_ss(Y_AXIS,ARG2)
          DO 200 j = res_lo_ss(Y_AXIS), res_hi_ss(Y_AXIS)

           i1 = arg_lo_ss(X_AXIS,ARG1)
           i2 = arg_lo_ss(X_AXIS,ARG2)
           DO 100 i = res_lo_ss(X_AXIS), res_hi_ss(X_AXIS)

            IF ( (arg_1(i1,j1,k1,l1,m1,n1) .EQ. bad_flag(ARG1)) .OR.
     .           (arg_2(i2,j2,k2,l2,m2,n2) .EQ. bad_flag(ARG2)) ) THEN

               result(i,j,k,l,m,n) = bad_flag_result

            ELSE

               result(i,j,k,l,m,n) = o2sato(arg_1(i1,j1,k1,l1,m1,n1)
     &                    ,arg_2(i2,j2,k2,l2,m2,n2))

            ENDIF

            i1 = i1 + arg_incr(X_AXIS,ARG1)
            i2 = i2 + arg_incr(X_AXIS,ARG2)
 100       CONTINUE
     
           j1 = j1 + arg_incr(Y_AXIS,ARG1)
           j2 = j2 + arg_incr(Y_AXIS,ARG2)
 200      CONTINUE

          k1 = k1 + arg_incr(Z_AXIS,ARG1)
          k2 = k2 + arg_incr(Z_AXIS,ARG2)
 300     CONTINUE

         l1 = l1 + arg_incr(T_AXIS,ARG1)
         l2 = l2 + arg_incr(T_AXIS,ARG2)
 400    CONTINUE

        m1 = m1 + arg_incr(E_AXIS,ARG1)
        m2 = m2 + arg_incr(E_AXIS,ARG2)
 500   CONTINUE

       n1 = n1 + arg_incr(F_AXIS,ARG1)
       n2 = n2 + arg_incr(F_AXIS,ARG2)
 600  CONTINUE

*                                                                      ^
*                                                                      |
*                                            USER CONFIGURABLE PORTION |
* **********************************************************************

      RETURN
      END

      function o2sato(T,S)
c
C ********************************************************************
C                                     
C Computes the oxygen saturation concentration at 1 atm total pressure
c in mol/m^3 given the temperature (t, in deg C) and the salinity (s,
c in permil). 
C
C FROM GARCIA AND GORDON (1992), LIMNOLOGY and OCEANOGRAPHY.
C THE FORMULA USED IS FROM PAGE 1310, EQUATION (8).
c
C *** NOTE: THE "A3*TS^2" TERM (IN THE PAPER) IS INCORRECT. ***
C *** IT SHOULDN'T BE THERE.                                ***
C
C o2sato IS DEFINED BETWEEN T(freezing) <= T <= 40(deg C) AND
c 0 permil <= S <= 42 permil
C C
C CHECK VALUE:  T = 10.0 deg C, S = 35.0 permil, 
c o2sato = 0.282015 mol/m^3
C
C ********************************************************************
c
      DATA A0/ 2.00907   /,A1/ 3.22014   /, A2/ 4.05010 /,
     $     A3/ 4.94457   /,A4/-2.56847E-1/, A5/ 3.88767 /
      DATA B0/-6.24523E-3/,B1/-7.37614E-3/,
     $     B2/-1.03410E-2/,B3/-8.17083E-3/
      DATA C0/-4.88682E-7/
C
      TT  = 298.15-T
      TK  = 273.15+T
      TS  = ALOG(TT/TK)
      TS2 = TS**2
      TS3 = TS**3
      TS4 = TS**4
      TS5 = TS**5
      CO  = A0 + A1*TS + A2*TS2 + A3*TS3 + A4*TS4 + A5*TS5
     $     + S*(B0 + B1*TS + B2*TS2 + B3*TS3)
     $     + C0*(S*S)
      o2sato = EXP(CO)
c
c  Convert from ml/l to mol/m^3
c
      o2sato = o2sato/22391.6*1000.0
C
      RETURN
      END
