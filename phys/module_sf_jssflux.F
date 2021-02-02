!
!
!
!
module module_sf_jssflux
contains
!
!-------------------------------------------------------------------
!
   subroutine jssflux_single(skintemp,xtime,starthour,sfcwrap, minhfx, minwind, u3d, v3d, t3d, qv3d, p3d, dz8w,                           &
                     cp, rovcp, xlv, psfc, cpm, xland,                          &
                     psim, psih, hfx, qfx, lh, tsk, flhc, flqc,                 &
                     znt, gz1oz0, wspd,                                         &
                     julian_in, karman, p1000mb,                                &
                     itimestep,chklowq,                                          &
                     ids, ide, jds, jde, kds, kde,                              &
                     ims, ime, jms, jme, kms, kme,                              &
                     its, ite, jts, jte, kts, kte   )
!-------------------------------------------------------------------
      implicit none
!-------------------------------------------------------------------
!
   integer, intent(in)   ::                       ids, ide, jds, jde, kds, kde, &
                                                  ims, ime, jms, jme, kms, kme, &
                                       its, ite, jts, jte, kts, kte, itimestep        
!   
   real, intent(in)      ::         minwind, minhfx, cp, rovcp, xlv, julian_in, karman, p1000mb
   real, intent(in)      ::         xtime
   integer, intent(in)      ::      sfcwrap, starthour, skintemp
!
   real, dimension( ims:ime, kms:kme, jms:jme )                               , &
            intent(in)   ::                                                u3d, &
                                                                           v3d, &
                                                                           t3d, &
                                                                          qv3d, &
                                                                           p3d, &
                                                                          dz8w
   real, dimension( ims:ime, jms:jme )                                        , &
            intent(in)   ::                                               psfc, &
                                                                         xland, &
                                                                          flhc, &
                                                                          flqc 
!
   real, dimension( ims:ime, jms:jme )                                        , &
            intent(inout)::                                                cpm, &
                                                                           znt, &
                                                                        gz1oz0, &
                                                                          wspd, &
                                                                          psim, &
                                                                          psih, &
                                                                           hfx, &
                                                                           qfx, &
                                                                            lh, &
                                                                           tsk,&
                                                                        chklowq           
! local vars

   character*256 :: filesh0, filesh1, filelh0, filelh1,fractout
   character(len=2) :: hr0,hr1,mn0,mn1
   integer, parameter    ::                                       n_max = 1200
   integer               ::                                 i, j, n, nm, nt, m
   real, parameter       ::                                           den = 1.
   real :: mn, fract
   integer :: hr
   real                  ::                         julian_s, julian_e, fc_int, &
                                                            fm, fh, ch, dtdiff
   !real, dimension( 1:n_max ) ::                     fc_qfx, fc_hfx, fc_julian !JP 0 ->1
   real, dimension(ids:ide, jds:jde) :: fc_hfx, fc_hfx0, fc_hfx1, fc_lh, fc_lh0, fc_lh1
   real                       ::                     qfx_interp,hfx_interp ! JP
   real, dimension( its:ite, jts:jte) ::                                   u2d, &
                                                                           v2d, &
                                                                           t2d, &
                                                                          qv2d, &
                                                                           p2d, &
                                                                        dz8w1d, &
                                                                            za, &
                                                                           thx, &
                                                                          thgb
   logical               ::                                        end_of_file
!
!-----open scmflx_bdy and read the julian_s, julian_e, fc_int
!

	hr = int(xtime / 60.0) 
	mn = xtime - hr*60

	hr = hr + starthour
	if (hr.ge.24) then
		hr = hr - 24
	endif

	if (mn.lt.30) then
		write(hr0,'(I2.2)') hr
		write(hr1,'(I2.2)') hr
		write(mn0,'(I2.2)') 0
		write(mn1,'(I2.2)') 30
	else
		write(hr0,'(I2.2)') hr
		write(mn0,'(I2.2)') 30
		write(mn1,'(I2.2)') 0

		if (hr.eq.23) then
			write(hr1,'(I2.2)') 0
		else
			write(hr1,'(I2.2)') hr + 1
		endif

	endif

	filesh0 = 'jsssh_bdy_'//trim(hr0)//'_'//trim(mn0)
	filesh1 = 'jsssh_bdy_'//trim(hr1)//'_'//trim(mn1)
	filelh0 = 'jsslh_bdy_'//trim(hr0)//'_'//trim(mn0)
	filelh1 = 'jsslh_bdy_'//trim(hr1)//'_'//trim(mn1)

	!print *, trim(filesh0)
	!print *, trim(filesh1)
	!print *, trim(filelh0)
	!print *, trim(filelh1)
	!call wrf_debug(100, filesh0)
	!call wrf_debug(100, filesh1)
	!call wrf_debug(100, filelh0)
	!call wrf_debug(100, filelh1)

	open(unit=11, file=filesh0, form='formatted', status='old')
	read(11,*) fc_hfx0
	close(11)

	open(unit=11, file=filesh1, form='formatted', status='old')
	read(11,*) fc_hfx1
	close(11)

	open(unit=11, file=filelh0, form='formatted', status='old')
	read(11,*) fc_lh0
	close(11)

	open(unit=11, file=filelh1, form='formatted', status='old')
	read(11,*) fc_lh1
	close(11)

	fract = modulo(xtime,30.0) / 30.0
	!print *, 'fract',fract

	fc_hfx = fc_hfx0 + (fc_hfx1 - fc_hfx0) * fract
	fc_lh = fc_lh0 + (fc_lh1 - fc_lh0) *  fract


	fc_hfx = fc_hfx(:,jde:jds:-1)
	fc_lh = fc_lh(:,jde:jds:-1)

!
!-----compute surface moisture and heat fluxes, in the unit of [W m-2]
!

!-----compute skin temperature
!
   do j=jts,jte
     do i=its,ite
       u2d(i,j)=u3d(i,1,j)
       v2d(i,j)=v3d(i,1,j)
       t2d(i,j)=t3d(i,1,j)
       qv2d(i,j)=qv3d(i,1,j)
       p2d(i,j)=p3d(i,1,j)
       dz8w1d(i,j)=dz8w(i,1,j)
       za(i,j)=0.5*dz8w1d(i,j)
     enddo
   enddo 

    CALL wrf_debug( 100, 'jss before loop' )

   do j=jts, jte
     do i=its, ite

!
!-----compute surface moisture flux
!
       !lh(i,j)=xlv*qfx(i,j)
	   lh(i,j) = fc_lh(i,j)
	   qfx(i,j) = fc_lh(i,j)/xlv
!


!-----compute surface heat flux
!
       cpm(i,j)=cp*(1.+0.8*qv2d(i,j))
!      print*,'i j cpm xland qv2d',i,j,cpm(i,j),xland(i,j), qv2d(i,j)
!      print*,hfx_interp

		hfx(i,j)=max(fc_hfx(i,j),minhfx) !does removing this work?
		!hfx(i,j)=fc_hfx(i,j)

     enddo
   enddo

    CALL wrf_debug( 100, 'jss after loop' )
!
   
   if (itimestep .eq. 1) then
     psih=0.0
     psim=0.0

   endif
     chklowq=1.0 !JP


	if (sfcwrap.eq.1) then

	   CALL jss_wrapper_surface(  hfx,				 &
								  ids, ide, jds, jde, kds, kde,      &
								  ims, ime, jms, jme, kms, kme,      &
							 	  its, ite, jts, jte, kts, kte      )


	   CALL jss_wrapper_surface(  qfx,				 &
								  ids, ide, jds, jde, kds, kde,      &
								  ims, ime, jms, jme, kms, kme,      &
							 	  its, ite, jts, jte, kts, kte      )

	  endif

   
  if (skintemp.eq.0) then
	   do j=jts,jte
		 do i=its,ite
		   gz1oz0(i,j)=alog(za(i,j)/znt(i,j))
		   fh=gz1oz0(i,j)-psih(i,j)
		   fm=gz1oz0(i,j)-psim(i,j)
		   ch=karman**2/fh/fm
		   !wspd(i,j)=sqrt(u2d(i,j)**2+v2d(i,j)**2)
		   wspd(i,j)=max(sqrt(u2d(i,j)**2+v2d(i,j)**2),minwind)
		   dtdiff=-hfx(i,j)/den/cpm(i,j)/ch/wspd(i,j)
		   tsk(i,j)=t2d(i,j)-dtdiff
		 enddo
	   enddo
   endif
   
   end subroutine jssflux_single




subroutine jssskintemp(xtime,starthour,startday,startmonth,startyear, &
					 sfcwrap,tsk, tsread0, tsread1, &
                     ids, ide, jds, jde, kds, kde,                              &
                     ims, ime, jms, jme, kms, kme,                              &
                     its, ite, jts, jte, kts, kte   )
!-------------------------------------------------------------------
      implicit none
!-------------------------------------------------------------------
!
   integer, intent(in)   ::                       ids, ide, jds, jde, kds, kde, &
                                                  ims, ime, jms, jme, kms, kme, &
                                       its, ite, jts, jte, kts, kte        
!   
   real, intent(in)      ::         xtime
   integer, intent(in)      ::      sfcwrap, starthour, startday, startmonth, startyear
!
 !
   real, dimension( ims:ime, jms:jme )                                        , &
            intent(inout)::                                                tsk,&
																		tsread0,&
																		tsread1
                                                                           
! local vars

   character*256 :: filesh0, filesh1, filelh0, filelh1,fractout
   character(len=2) :: hr0,hr1,mn0,mn1,dy0,dy1,mo0,mo1
   character(len=4) :: yr
   integer, parameter    ::                                       n_max = 1200
   integer               ::                                 i, j, n, nm, nt, m
   integer               ::                                 newread, month0, &
															month1
   real, parameter       ::                                           den = 1.
   real :: mn, fract
   integer :: hr, day
   real                  ::                         julian_s, julian_e, fc_int, &
                                                            fm, fh, ch, dtdiff
   !real, dimension( 1:n_max ) ::                     fc_qfx, fc_hfx, fc_julian !JP 0 ->1
   real, dimension(ids:ide, jds:jde) :: fc_tsk, fc_tsk0, fc_tsk1
   real                       ::                     tsk_interp
    logical               ::                                        end_of_file
!
!-----open scmflx_bdy and read the julian_s, julian_e, fc_int
!


	hr = int(xtime / 60.0) 
	mn = xtime - hr*60
	day = startday
	month0 = startmonth

	hr = hr + starthour
	if (hr.ge.24) then
		hr = hr - 24
		day = day + 1
		call jss_monthwrap( startmonth, month0, day )
	elseif (hr.ge.48) then
		hr = hr - 48
		day = day + 2
		call jss_monthwrap( startmonth, month0, day )
	endif

	write(yr,'(I4.4)') startyear
	write(mo0,'(I2.2)') month0
	write(hr0,'(I2.2)') hr
	write(dy0,'(I2.2)') day

	if (hr.eq.23) then
		write(hr1,'(I2.2)') 0
		write(dy1,'(I2.2)') day + 1
		call jss_monthwrap( startmonth, month1, day+1 )
		write(mo1,'(I2.2)') month1
	else
		write(hr1,'(I2.2)') hr + 1
		write(dy1,'(I2.2)') day
		write(mo1,'(I2.2)') month0
	endif



	newread = 0
	if (mn.eq.0) then
		newread = 1
	endif

	!print *, yr,mo,dy0,hr0
	!print *, yr,mo,dy1,hr1
	!print *, hr,mn,day

	fract = modulo(xtime,60.0) / 60.0


	if (newread.eq.1) then
		filesh0 = 'jsstsk_bdy_'//trim(yr)//'-'//trim(mo0)//'-'//trim(dy0)//'-'//trim(hr0)//'-00'
		filesh1 = 'jsstsk_bdy_'//trim(yr)//'-'//trim(mo1)//'-'//trim(dy1)//'-'//trim(hr1)//'-00'

		print *, trim(filesh0)
		print *, trim(filesh1)
		!print *, trim(filelh0)
		!print *, trim(filelh1)
		!call wrf_debug(100, filesh0)
		!call wrf_debug(100, filesh1)
		!call wrf_debug(100, filelh0)
		!call wrf_debug(100, filelh1)

		open(unit=11, file=filesh0, form='formatted', status='old')
		read(11,*) fc_tsk0
		close(11)

		open(unit=11, file=filesh1, form='formatted', status='old')
		read(11,*) fc_tsk1
		close(11)

		!open(unit=11, file=filelh0, form='formatted', status='old')
		!read(11,*) fc_lh0
		!close(11)

		!open(unit=11, file=filelh1, form='formatted', status='old')
		!read(11,*) fc_lh1
		!close(11)

		!print *, 'fract',fract

		fc_tsk0 = fc_tsk0(:,jde:jds:-1)
		fc_tsk1 = fc_tsk1(:,jde:jds:-1)


	endif

	!fc_hfx = fc_hfx0 + (fc_hfx1 - fc_hfx0) * fract
	!fc_lh = fc_lh0 + (fc_lh1 - fc_lh0) *  fract


	!fc_hfx = fc_hfx(:,jde:jds:-1)
	!fc_lh = fc_lh(:,jde:jds:-1)

!
   do j=jts, jte
     do i=its, ite

!
!-----compute surface moisture flux
!
	   !lh(i,j) = fc_lh(i,j)
	   !qfx(i,j) = fc_lh(i,j)/xlv
!


!-----compute surface heat flux
!
		!cd(i,j)=fc_hfx(i,j) 

		if (newread.eq.1) then
			tsk(i,j)=fc_tsk0(i,j)  + (fc_tsk1(i,j) - fc_tsk0(i,j))*fract
			tsread0(i,j) = fc_tsk0(i,j)
			tsread1(i,j) = fc_tsk1(i,j)
		else
			tsk(i,j)=tsread0(i,j)  + (tsread1(i,j) - tsread0(i,j))*fract
		endif

     enddo
   enddo



	if (sfcwrap.eq.1) then

	   CALL jss_wrapper_surface(  tsk,				 &
								  ids, ide, jds, jde, kds, kde,      &
								  ims, ime, jms, jme, kms, kme,      &
							 	  its, ite, jts, jte, kts, kte      )


	  endif

   
end subroutine jssskintemp



subroutine jssexcoef(xtime,starthour,sfcwrap,chs, &
                     ids, ide, jds, jde, kds, kde,                              &
                     ims, ime, jms, jme, kms, kme,                              &
                     its, ite, jts, jte, kts, kte   )
!-------------------------------------------------------------------
      implicit none
!-------------------------------------------------------------------
!
   integer, intent(in)   ::                       ids, ide, jds, jde, kds, kde, &
                                                  ims, ime, jms, jme, kms, kme, &
                                       its, ite, jts, jte, kts, kte        
!   
   real, intent(in)      ::         xtime
   integer, intent(in)      ::      sfcwrap, starthour
!
 !
   real, dimension( ims:ime, jms:jme )                                        , &
            intent(inout)::                                                chs
                                                                           
! local vars

   character*256 :: filesh0, filesh1, filelh0, filelh1,fractout
   character(len=2) :: hr0,hr1,mn0,mn1
   integer, parameter    ::                                       n_max = 1200
   integer               ::                                 i, j, n, nm, nt, m
   real, parameter       ::                                           den = 1.
   real :: mn, fract
   integer :: hr
   real                  ::                         julian_s, julian_e, fc_int, &
                                                            fm, fh, ch, dtdiff
   !real, dimension( 1:n_max ) ::                     fc_qfx, fc_hfx, fc_julian !JP 0 ->1
   real, dimension(ids:ide, jds:jde) :: fc_tsk, fc_tsk0, fc_tsk1
   real                       ::                     tsk_interp
    logical               ::                                        end_of_file
!
!-----open scmflx_bdy and read the julian_s, julian_e, fc_int
!

	hr = int(xtime / 60.0) 
	mn = xtime - hr*60

	hr = hr + starthour
	if (hr.ge.24) then
		hr = hr - 24
	endif

	if (mn.lt.30) then
		write(hr0,'(I2.2)') hr
		write(hr1,'(I2.2)') hr
		write(mn0,'(I2.2)') 0
		write(mn1,'(I2.2)') 30
	else
		write(hr0,'(I2.2)') hr
		write(mn0,'(I2.2)') 30
		write(mn1,'(I2.2)') 0

		if (hr.eq.23) then
			write(hr1,'(I2.2)') 0
		else
			write(hr1,'(I2.2)') hr + 1
		endif

	endif

	filesh0 = 'jsscm_bdy_'//trim(hr0)//'_'//trim(mn0)
	filesh1 = 'jsscm_bdy_'//trim(hr1)//'_'//trim(mn1)

	!print *, trim(filesh0)
	!print *, trim(filesh1)
	!print *, trim(filelh0)
	!print *, trim(filelh1)
	!call wrf_debug(100, filesh0)
	!call wrf_debug(100, filesh1)
	!call wrf_debug(100, filelh0)
	!call wrf_debug(100, filelh1)

	open(unit=11, file=filesh0, form='formatted', status='old')
	read(11,*) fc_tsk0
	close(11)

	open(unit=11, file=filesh1, form='formatted', status='old')
	read(11,*) fc_tsk1
	close(11)


	fract = modulo(xtime,30.0) / 30.0
	!print *, 'fract',fract

	fc_tsk = fc_tsk0 + (fc_tsk1 - fc_tsk0) * fract


	fc_tsk = fc_tsk(:,jde:jds:-1)


   do j=jts, jte
     do i=its, ite

	   chs(i,j) = fc_tsk(i,j)

     enddo
   enddo



	if (sfcwrap.eq.1) then

	   CALL jss_wrapper_surface(  chs,				 &
								  ids, ide, jds, jde, kds, kde,      &
								  ims, ime, jms, jme, kms, kme,      &
							 	  its, ite, jts, jte, kts, kte      )


	  endif

   
end subroutine jssexcoef





SUBROUTINE jss_wrapper_surface( flux,                  &
								ids,ide, jds,jde, kds,kde,           &
								ims,ime, jms,jme, kms,kme,           &
								its,ite, jts,jte, kts,kte           )

!-----------------------------------------------------------------------
! Begin declarations.

	use module_dm, only: wrf_dm_sum_real !jss
    USE module_configure, ONLY : grid_config_rec_type, model_config_rec

   IMPLICIT NONE

   INTEGER ,          INTENT(IN   )           :: ids, ide, jds, jde, kds, kde, &
                                                 ims, ime, jms, jme, kms, kme, &
                                                 its, ite, jts, jte, kts, kte

   REAL, DIMENSION( ims:ime , jms:jme ) , INTENT(INOUT) ::     flux

! LOCAL VARS

   INTEGER :: i, j, k
   REAL    :: totflux, meanflux, np2d

! End declarations.
!-----------------------------------------------------------------------

	totflux = 0.0

	DO j = jts, jte
	DO i = its, ite
		totflux = totflux + flux(i,j)
	ENDDO
	ENDDO

	np2d = float((ide-ids)*(jde-jds))

	totflux = wrf_dm_sum_real( totflux )

	meanflux = totflux / np2d

	DO j = jts, jte
	DO i = its, ite
		flux(i,j) = meanflux
	ENDDO
	ENDDO


END SUBROUTINE jss_wrapper_surface

SUBROUTINE jss_monthwrap( mo0,mo1,day )

!-----------------------------------------------------------------------
! Begin declarations.

   IMPLICIT NONE

   INTEGER ,          INTENT(IN   )           :: mo0,day
   INTEGER ,          INTENT(INOUT)           :: mo1


! LOCAL VARS

   INTEGER :: lastday

! End declarations.
!-----------------------------------------------------------------------


	if (mo0.eq.1) then !jan
		lastday = 31
	elseif (mo0.eq.2) then !feb
		lastday = 28
	elseif (mo0.eq.3) then !mar
		lastday = 31
	elseif (mo0.eq.4) then !apr
		lastday = 30
	elseif (mo0.eq.5) then !may
		lastday = 31
	elseif (mo0.eq.6) then !jun
		lastday = 30
	elseif (mo0.eq.7) then !jul
		lastday = 31
	elseif (mo0.eq.8) then !aug
		lastday = 31
	elseif (mo0.eq.9) then !sep
		lastday = 30
	elseif (mo0.eq.10) then !oct
		lastday = 31
	elseif (mo0.eq.11) then !nov
		lastday = 30
	elseif (mo0.eq.12) then !dec
		lastday = 31
	endif

	mo1 = mo0
	if (day.gt.lastday) then
		mo1 = mo0 + 1
	endif

END SUBROUTINE jss_monthwrap


end module module_sf_jssflux
